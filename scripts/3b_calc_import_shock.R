# LOAD PACKAGES  ---------------------------------------------------------------

packages <-
  c(
    "dplyr",
    "tidyr"
  )

for (p in packages){ 
  if (! (p %in% installed.packages())){
    install.packages(p)
  }
}

library(dplyr)   

# CALCULATE IMPORT SHOCK -------------------------------------------------------

emp91dat <- readRDS(file = "./output/datasets/emp91dat.RDS")
tradedat <- readRDS(file = "./output/datasets/tradedat.RDS")
tradedat <-
  tradedat[["vol/gbp NACE RptAgg imp CHN"]] %>%
  select(
    reporterISO, 
    NACE1CD, 
    refYear, 
    chg_16y
  )

shockdat <- list()

# Calculate emp_ik (region-ind emp), emp_i (region emp in all ind), emp_k (ind emp)
shockdat[["terms by ITL21X-NACE"]] <-
  emp91dat[["by ITL21X-NACE GBR"]] %>%
  group_by(NACE1CD) %>%
  mutate(emp_k = sum(emp)) %>%
  ungroup() %>%
  group_by(ITL321XCD) %>%
  mutate(emp_i = sum(emp)) %>%
  rename(emp_ik = emp) %>%
  ungroup()

shockdat[["terms by LAD20X-NACE"]] <-
  emp91dat[["by LAD20X-NACE GBR"]] %>%
  group_by(NACE1CD) %>%
  mutate(emp_k = sum(emp)) %>%
  ungroup() %>%
  group_by(LAD20XCD) %>%
  mutate(emp_i = sum(emp)) %>%
  rename(emp_ik = emp) %>%
  ungroup()
  
# Join UK-CN and HI/US-CN import change data with emp data; remove unmatched rows
# NAs in reporterISO are due to
# (a) non-mnf industries in emp data and 
# (b) industries 173, 223, 275, 284, 285 existing in Nomis but not in WITS (and thus Comtrade)
shockdat[["terms by ITL21X-NACE"]] <-
  shockdat[["terms by ITL21X-NACE"]] %>%
  left_join(tradedat, by = c("NACE1CD")) %>%
  filter(!is.na(reporterISO))

shockdat[["terms by LAD20X-NACE"]] <-
  shockdat[["terms by LAD20X-NACE"]] %>%
  left_join(tradedat, by = c("NACE1CD")) %>%
  filter(!is.na(reporterISO))

# Calculate region-year-shocks using UK and HI/USA imports
shockdat[["ITL21X-year shocks"]] <-
  shockdat[["terms by ITL21X-NACE"]] %>%
  group_by(refYear, ITL321XCD, ITL321XNM, reporterISO) %>%
  summarise(
    shk = sum((emp_ik / emp_i) * (chg_16y / emp_k), na.rm = F)
  )%>%
  ungroup() 

shockdat[["LAD20X-year shocks"]] <-
  shockdat[["terms by LAD20X-NACE"]] %>%
  group_by(refYear, LAD20XCD, LAD20XNM, reporterISO) %>%
  summarise(
    shk = sum((emp_ik / emp_i) * (chg_16y / emp_k), na.rm = F)
  )%>%
  ungroup() 

# Calculate regional shocks using UK and HI/USA imports
shockdat[["ITL21X shocks"]] <-
  shockdat[["ITL21X-year shocks"]] %>%
  group_by(ITL321XCD, ITL321XNM, reporterISO) %>%
  summarise( 
    shk = mean(shk, na.rm = T)
  ) %>% 
  ungroup()

shockdat[["LAD20X shocks"]] <-
  shockdat[["LAD20X-year shocks"]] %>%
  group_by(LAD20XCD, LAD20XNM, reporterISO) %>%
  summarise(    
    shk = mean(shk, na.rm = T)
  ) %>% 
  ungroup()

# Unstack UK/HI/USA import-based shocks
shockdat[["ITL21X shocks unstacked"]] <-
  shockdat[["ITL21X shocks"]] %>%
  tidyr::pivot_wider(names_from = c("reporterISO"), values_from = c("shk")) %>%
  rename(
    shk_GBR = GBR,
    shk_HI = HI,
    shk_HIUSA = HIUSA,
    shk_USA = USA
  )

shockdat[["LAD20X shocks unstacked"]] <-
  shockdat[["LAD20X shocks"]] %>%
  tidyr::pivot_wider(names_from = c("reporterISO"), values_from = c("shk")) %>%
  rename(
    shk_GBR = GBR,
    shk_HI = HI,
    shk_HIUSA = HIUSA,
    shk_USA = USA
  )

# SAVE OUTPUT & CLEAN UP ENVIRONMENT -------------------------------------------

saveRDS(shockdat, file="./output/datasets/shockdat.RDS")

rm(list = ls())
gc()


