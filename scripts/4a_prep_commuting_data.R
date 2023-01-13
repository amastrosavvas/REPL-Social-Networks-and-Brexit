# LOAD PACKAGES  ---------------------------------------------------------------

packages <-
  c(
    "dplyr",
    "stringr",
    "tidyr"
  )

for (p in packages){ 
  if (! (p %in% installed.packages())){
    install.packages(p)
  }
}

library(dplyr)                    

# PREPARE COMMUTING DATA -------------------------------------------------------

geolkp <- readRDS(file = "./output/lookups/geolkp.RDS")

# Read in UK Census commuting tables for annual
commuting_files <- 
  list.files(
    path = "./rawdata/commuting",
    pattern = "*.csv",
    full.names = TRUE
  )

commdat <- 
  lapply(commuting_files, function(x){read.csv(x)}) 

names(commdat) <- # Data for each year labelled according to filename
  stringr::str_extract(commuting_files, "1991|2001|2011")

# Extract origin-destination LAD codes/names from strings; remove notes rows
commdat <- 
  lapply(
    commdat,
    function(x){
      
      x <-
        x %>%
        filter(!grepl("^\\*|^Notes", Origins)) %>% 
        mutate(
          LADCD_O = stringr::str_extract(Origins, "(?<=;\\s)(.*)"),
          LADNM_O = stringr::str_extract(Origins, "(?<=:\\s)(.*)(?=;)"),
          LADCD_D = stringr::str_extract(Destinations, "(?<=;\\s)(.*)"),
          LADNM_D = stringr::str_extract(Destinations, "(?<=:\\s)(.*)(?=;)")
        ) %>%
        rename(commuters = 3) %>%
        select(LADCD_O, LADNM_O, LADCD_D, LADNM_D, commuters)
    }
  )

# Append year column to each commuting table
for (i in 1:length(commdat)){commdat[[i]]$Year <- names(commdat)[[i]]} 

# Bind commuting across years; Remove NI; scale up 1991 data 
commdat[["dir annual by LAD GBR"]] <- 
  commdat %>% 
  bind_rows() %>%
  filter(!grepl("^N|^95", LADCD_O), !grepl("^N|^95", LADCD_D)) %>%
  mutate(commuters = case_when(Year == "1991" ~ commuters*10L, TRUE ~ commuters))

commdat <- commdat[!names(commdat) %in% c("1991", "2001", "2011")]

# Replace empty entries for origin-dest-year or dest-origin-year cells with zero
commdat[["dir annual by LAD GBR"]] <-
  commdat[["dir annual by LAD GBR"]] %>%
  group_by(Year) %>%
  tidyr::complete(tidyr::nesting(LADCD_O, LADNM_O), tidyr::nesting(LADCD_D, LADNM_D)) %>%
  mutate(commuters = case_when(is.na(commuters) ~ 0L, TRUE ~ commuters)) %>% 
  ungroup()

# Set aside 1991 table (for impshock adjustment); get LAD20X/ITL21X versions
commdat[["dir 1991 by LAD GBR"]]  <- 
  commdat[["dir annual by LAD GBR"]]  %>%
  filter(Year == "1991")

commdat[["dir 1991 by LAD20X GBR"]] <-
  commdat[["dir 1991 by LAD GBR"]] %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LADCD", "LAD20XCD", "LAD20XNM")]),
    by = c("LADCD_O" = "LADCD")
  ) %>% 
  rename(LAD20XCD_O = LAD20XCD, LAD20XNM_O = LAD20XNM) %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LADCD", "LAD20XCD", "LAD20XNM")]),
    by = c("LADCD_D" = "LADCD")
  ) %>% 
  rename(LAD20XCD_D = LAD20XCD, LAD20XNM_D = LAD20XNM) %>%
  group_by(Year, LAD20XCD_O, LAD20XNM_O, LAD20XCD_D, LAD20XNM_D) %>%
  summarise(commuters = sum(commuters)) %>%
  ungroup()

commdat[["dir 1991 by ITL21X GBR"]] <-
  commdat[["dir 1991 by LAD GBR"]] %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LADCD", "ITL321XCD", "ITL321XNM")]),
    by = c("LADCD_O" = "LADCD")
  ) %>% 
  rename(ITL321XCD_O = ITL321XCD, ITL321XNM_O = ITL321XNM) %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LADCD", "ITL321XCD", "ITL321XNM")]),
    by = c("LADCD_D" = "LADCD")
  ) %>% 
  rename(ITL321XCD_D = ITL321XCD, ITL321XNM_D = ITL321XNM) %>%
  group_by(Year, ITL321XCD_O, ITL321XNM_O, ITL321XCD_D, ITL321XNM_D) %>%
  summarise(commuters = sum(commuters)) %>%
  ungroup()

# Get LAD commuting flows for England and Wales only
commdat[["dir annual by LAD EW"]] <-
  commdat[["dir annual by LAD GBR"]] %>%
  filter(!grepl("^S|^60", LADCD_O), !grepl("^S|^60", LADCD_D))

# Get LAD20X commuting flows for England and Wales
commdat[["dir annual by LAD20X EW"]] <-
  commdat[["dir annual by LAD EW"]] %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LADCD", "LAD20XCD", "LAD20XNM")]),
    by = c("LADCD_O" = "LADCD")
  ) %>% 
  rename(LAD20XCD_O = LAD20XCD, LAD20XNM_O = LAD20XNM) %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LADCD", "LAD20XCD", "LAD20XNM")]),
    by = c("LADCD_D" = "LADCD")
  ) %>% 
  rename(LAD20XCD_D = LAD20XCD, LAD20XNM_D = LAD20XNM) %>%
  group_by(Year, LAD20XCD_O, LAD20XNM_O, LAD20XCD_D, LAD20XNM_D) %>%
  summarise(commuters = sum(commuters)) %>%
  ungroup()

# Get ITL21X commuting flows for England and Wales
commdat[["dir annual by ITL21X EW"]] <-
  commdat[["dir annual by LAD EW"]] %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LADCD", "ITL321XCD", "ITL321XNM")]),
    by = c("LADCD_O" = "LADCD")
  ) %>% 
  rename(ITL321XCD_O = ITL321XCD, ITL321XNM_O = ITL321XNM) %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LADCD", "ITL321XCD", "ITL321XNM")]),
    by = c("LADCD_D" = "LADCD")
  ) %>% 
  rename(ITL321XCD_D = ITL321XCD, ITL321XNM_D = ITL321XNM) %>%
  group_by(Year, ITL321XCD_O, ITL321XNM_O, ITL321XCD_D, ITL321XNM_D) %>%
  summarise(commuters = sum(commuters)) %>%
  ungroup()

# Get gross commuting flows for England and Wales; get LAD20X/ITL21X versions
commdat[["undir annual by LAD EW"]] <-
  commdat[["dir annual by LAD EW"]] %>%
  left_join(., .,  by = c(
    "LADCD_O" = "LADCD_D", 
    "LADNM_O" = "LADNM_D", 
    "LADCD_D" = "LADCD_O", 
    "LADNM_D" = "LADNM_O", 
    "Year" = "Year"
  )) %>% mutate(
    commuters = case_when(
      LADCD_O == LADCD_D ~ as.integer((commuters.x + commuters.y)/2),
      TRUE ~ as.integer(commuters.x + commuters.y)
    )
  ) %>% 
  select(-commuters.x, -commuters.y)

commdat[["undir annual by LAD20X EW"]] <-
  commdat[["dir annual by LAD20X EW"]] %>%
  left_join(., .,  by = c(
    "LAD20XCD_O" = "LAD20XCD_D", 
    "LAD20XNM_O" = "LAD20XNM_D", 
    "LAD20XCD_D" = "LAD20XCD_O", 
    "LAD20XNM_D" = "LAD20XNM_O", 
    "Year" = "Year"
  )) %>% mutate(
    commuters = case_when(
      LAD20XCD_O == LAD20XCD_D ~ as.integer((commuters.x + commuters.y)/2),
      TRUE ~ as.integer(commuters.x + commuters.y)
    )
  ) %>% 
  select(-commuters.x, -commuters.y)

commdat[["undir annual by ITL21X EW"]] <-
  commdat[["dir annual by ITL21X EW"]] %>%
  left_join(., .,  by = c(
    "ITL321XCD_O" = "ITL321XCD_D", 
    "ITL321XNM_O" = "ITL321XNM_D", 
    "ITL321XCD_D" = "ITL321XCD_O", 
    "ITL321XNM_D" = "ITL321XNM_O", 
    "Year" = "Year"
  )) %>% mutate(
    commuters = case_when(
      ITL321XCD_O == ITL321XCD_D ~ as.integer((commuters.x + commuters.y)/2),
      TRUE ~ as.integer(commuters.x + commuters.y)
    )
  ) %>% 
  select(-commuters.x, -commuters.y)


# SAVE OUTPUT & CLEAN UP ENVIRONMENT -------------------------------------------

saveRDS(commdat, file="./output/datasets/commdat.RDS")

rm(list = ls()) 
gc()