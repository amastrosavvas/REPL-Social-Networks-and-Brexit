# LOAD PACKAGES  ---------------------------------------------------------------

packages <-
  c(
    "dplyr",
    "tidyr",
    "haven",
    "expss",
    "stringr"
  )

for (p in packages){ 
  if (! (p %in% installed.packages())){
    install.packages(p)
  }
}

library(dplyr) 

# CREATE MASTER DF  ------------------------------------------------------------

regdat <- 
  readRDS(file = "./output/datasets/main/regdat.RDS") %>%
  mutate(across(everything(), ~expss::unlab(.)))

# Read in data from BES waves 7-9 in list, rename dfs
besfiles <- 
  paste0(
    "./rawdata/voting/",
    c(
      "BES2015_W9_v23.0.dta",
      "BES2015_W8_v23.0.dta",
      "BES2015_W7_v23.0.dta"
    )
  )

besdat <- lapply(besfiles, function(x) haven::read_dta(x))
names(besdat) <- c("w9", "w8", "w7")

# Select variables
besdat <- lapply(
  besdat,
  function(x){
    x %>%
      select(matches("^id$|^wt$|oslaua|^euRefVote$|^age$|^gender$|^p_education$|^p_work_stat$|^riskPoverty$|^parentsForeign$|infoSourceInternet"))
  }
)

# Merge information from all waves on wave 8/7 respondents
besw9dat <- 
  besdat[["w9"]] %>%
  left_join(select(besdat[["w8"]], "id", !matches(colnames(select(besdat[["w9"]], -"id")))), by = "id") %>%
  left_join(select(besdat[["w7"]], "id", !matches(colnames(select(., -"id")))), by = "id") 

# Replace 9999 with NA, drop observations with no voting or residence info

besw9dat[besw9dat==9999] <- NA

besw9dat <-
  besw9dat %>%
  filter(
    !is.na(euRefVote), 
    euRefVote != 2, 
    !is.na(oslaua)
  )

# Drop "won't vote" from outcome and respondents with no LAD info, discretise vars
besw9dat <-
  besw9dat %>%
  mutate(
    topqual = case_when(
      p_education == 8 ~ 1, #gcse-dg
      p_education == 9 ~ 2, #gcse-ac
      p_education == 11 ~ 3, # alvl
      p_education == 16 ~ 4, # undergrad
      p_education == 17 ~ 5, # postgrad
      is.na(p_education) ~ NA_real_,
      TRUE ~ 0 # other
    ),
    gender = case_when(
      gender == 1 ~ 1,
      gender == 2 ~ 0,
      is.na(gender) ~ NA_real_
    ),
    unemployed = case_when(
      p_work_stat == 6 ~ 1,
      is.na(p_work_stat) ~  NA_real_,
      TRUE ~ 0
    ),
    student = case_when(
      p_work_stat == 4 ~ 1,
      is.na(p_work_stat) ~  NA_real_,
      TRUE ~ 0
    ),
    riskPoverty = case_when(
      riskPoverty %in% c(3, 4, 5) ~ 1,
      is.na(riskPoverty) ~  NA_real_,
      TRUE ~ 0
    ),
    bothparsbrit = case_when(
      parentsForeign == 0 ~ 1,
      is.na(parentsForeign) ~  NA_real_,
      TRUE ~ 0
    ),
    polintov1hr  = case_when(
      infoSourceInternet %in% c(3, 4, 5) ~ 1,
      is.na(infoSourceInternet) & is.na(infoSourceInternet) ~  NA_real_,
      TRUE ~ 0
    )
  ) %>%
  select(-p_education, -p_work_stat, -parentsForeign, -infoSourceInternet)


# Match BES LAD codes to LAD20X, ITL321X, and zones; remove NA zones (e.g. Scotland/NI)

besw9dat <- besw9dat %>% mutate(oslaua = haven::as_factor(oslaua)) # BES LAD values to labels
geolkp <- readRDS(file = "./output/lookups/geolkp.RDS")

besw9dat <- # Join by name
  besw9dat %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("ITL321XCD", "ITL321XNM", "LAD20XNM")]),
    by = c("oslaua" = "LAD20XNM")
  )

oslaua_lkup_na <- # Lookup for NA matches using fuzzy join
  filter(besw9dat, is.na(ITL321XCD)) %>%
  select(oslaua) %>%
  distinct() %>%
  fuzzyjoin::fuzzy_left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("ITL321XCD", "ITL321XNM")]), 
    match_fun = function(x, y){stringr::str_detect(y, x)},
    by = c("oslaua" = "ITL321XNM")
  )
  
besw9dat <- # Match NAs
  besw9dat %>%
  left_join(
    oslaua_lkup_na, 
    by = c("oslaua")
  ) %>% 
  mutate(
    ITL321XCD.x = ifelse(is.na(ITL321XCD.x), ITL321XCD.y, ITL321XCD.x),
    ITL321XNM.x = ifelse(is.na(ITL321XNM.x), ITL321XNM.y, ITL321XNM.x),
  ) %>%
  select(-ITL321XCD.y, -ITL321XNM.y) %>%
  rename(
    ITL321XCD = ITL321XCD.x,
    ITL321XNM = ITL321XNM.x
  ) %>%
  filter(!grepl("^S|^N", ITL321XCD)) # Remove Scotland/NI

# Standardise, classes
besw9dat <-
  besw9dat %>% 
  mutate(
    id = factor(id),
    wt = as.numeric(wt),
    oslaua = factor(oslaua),
    ITL321XCD = factor(ITL321XCD),
    ITL321XNM = factor(ITL321XNM),
    euRefVote = as.integer(euRefVote),
    age = as.integer(age),
    gender = factor(gender, levels = 0:1, labels = c("Female", "Male")),
    topqual = factor(topqual, levels = 0:5, labels = c("Other", "GCSE D-G", "GCSE A-C", "A-level", "Undergraduate", "Postgraduate")),
    unemployed = factor(unemployed, levels = 0:1, labels = c("No", "Yes")),
    riskPoverty = factor(riskPoverty, levels = 0:1, labels = c("No", "Yes")),
    student = factor(student, levels = 0:1, labels = c("No", "Yes")),
    bothparsbrit = factor(bothparsbrit, levels = 0:1, labels = c("No", "Yes")),
    polintov1hr = factor(polintov1hr, levels = 0:1, labels = c("No", "Yes"))
  ) %>%
  select(
    id, wt, oslaua, ITL321XCD, ITL321XCD, ITL321XNM, euRefVote, age, gender, topqual,
    unemployed, riskPoverty, student, bothparsbrit, polintov1hr
  )

# Join with shock data
besw9dat <-
  besw9dat %>%
  left_join(select(regdat, ITL321XCD, zones_ha, zones_la, contains("shk_")), by = "ITL321XCD")

# Labels
besw9dat = expss::apply_labels(
  besw9dat,
  id = "ID",
  wt = "BES wave 9 weight",
  oslaua = "BES local authority code",
  ITL321XCD = "Harmonised ITL3 region code",
  ITL321XNM = "Harmonised ITL3 region name",
  euRefVote = "Intention to vote Leave",
  age = "Age",
  gender = "Gender",
  topqual = "Highest qualification",
  unemployed = "Unemployed",
  riskPoverty = "At risk of poverty",
  student = "Student",
  bothparsbrit = "Both parents UK-born",
  polintov1hr = "Followed political news online for over an hour in last 7 days",
  shk_GBR = "Import shock: Within-region",
  shk_USA = "Import shock: Within-region instrument",
  rshk_GBR = "Res. import shock: Within-region",
  rshk_USA = "Res. import shock: Within-region instrument",
  shk_GBRZ = "Import shock: Within-region (standardised)",
  shk_USAZ = "Import shock: Within-region instrument (standardised)",
  lgrshk_GBR_wall_SCI_c = "Res. import shock: social lag (SCI-weighted)",
  lgrshk_GBR_w1_5_SCI_c = "Res. import shock: 1st to 5th social neighbour (SCI-weighted)",
  lgrshk_USA_wall_SCI_c = "Res. import shock instrument: social lag (SCI-weighted)",
  lgrshk_USA_w1_5_SCI_c = "Res. import shock instrument: 1st to 5th social neighbour (SCI-weighted)",
  lgrshk_GBR_weq1_5_SCI_c = "Res. import shock: 1st to 5th social neighbour (equally weighted)",
  lgrshk_GBR_weq1_5_SCI_c = "Res. import shock: 1st to 5th social neighbour (equally weighted)",
  lgrshk_USA_weq1_5_SCI_c = "Res. import shock instrument: 1st to 5th social neighbour (equally weighted)",
  lgrshk_GBR_wall_LCDi_c = "Res. import shock: spatial lag (LCDi-weighted)",
  lgrshk_GBR_w1_5_LCDi_c = "Res. import shock: 1st to 5th geographic neighbour (LCDi-weighted)",
  lgrshk_USA_wall_LCDi_c = "Res. import shock instrument: spatial lag (LCDi-weighted)",
  lgrshk_USA_w1_5_LCDi_c = "Res. import shock instrument: 1st to 5th geographic neighbour (LCDi-weighted)",
  shk_GBR = "Import shock: Within-region (standardised)",
  shk_USA = "Import shock: Within-region instrument (standardised)",
  rshk_GBRZ = "Res. import shock: Within-region (standardised)",
  rshk_USAZ = "Res. import shock: Within-region instrument (standardised)",
  lgrshk_GBR_wall_SCI_cZ = "Res. import shock: social lag (SCI-weighted; standardised)",
  lgrshk_GBR_w1_5_SCI_cZ = "Res. import shock: 1st to 5th social neighbour (SCI-weighted; standardised)",
  lgrshk_USA_wall_SCI_cZ = "Res. import shock instrument: social lag (SCI-weighted; standardised)",
  lgrshk_USA_w1_5_SCI_cZ = "Res. import shock instrument: 1st to 5th social neighbour (SCI-weighted; standardised)",
  lgrshk_GBR_weq1_5_SCI_cZ = "Res. import shock: 1st to 5th social neighbour (equally weighted; standardised)",
  lgrshk_USA_weq1_5_SCI_cZ = "Res. import shock instrument: 1st to 5th social neighbour (equally weighted; standardised)",
  lgrshk_GBR_wall_LCDi_cZ = "Res. import shock: spatial lag (LCDi-weighted; standardised)",
  lgrshk_GBR_w1_5_LCDi_cZ = "Res. import shock: 1st to 5th geographic neighbour (LCDi-weighted; standardised)",
  lgrshk_USA_wall_LCDi_cZ = "Res. import shock instrument: spatial lag (LCDi-weighted; standardised)",
  lgrshk_USA_w1_5_LCDi_cZ = "Res. import shock instrument: 1st to 5th geographic neighbour (LCDi-weighted; standardised)"
)

# SAVE OUTPUT & CLEAN UP ENVIRONMENT -------------------------------------------

haven::write_dta(data=besw9dat, path="./output/datasets/main/besw9dat.dta")
saveRDS(besw9dat, file = "./output/datasets/main/besw9dat.RDS")

rm(list = ls()) 
gc()