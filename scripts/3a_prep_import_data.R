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

# PREPARE IMPORT DATA ------------------------------------------------------------

indlkp <- readRDS(file = "./output/lookups/indlkp.RDS")

# ******************************************************************************
# CPI and GBR import currency conversion factor
# ******************************************************************************

cpi_tb <-
  as_tibble(
    read.csv(
      "./rawdata/other/ONS - CPI 00 All items 2015 = 100.csv",
      skip = 7,
      nrows = 33,
      col.names = c("Year", "CPI.All.Items.2015.100")
    )
  )

cpi_tb <-
  cpi_tb %>%
  mutate(
    across(Year, as.integer),
    across(CPI.All.Items.2015.100, ~ as.numeric(.) / 100)   # Decimalise
  ) %>%
  filter(Year %in% c(1991:2007)) 

xchg_tb <- 
  as_tibble(
    read.csv(
      "./rawdata/other/COMTRADE - GBR currency conversion factors.csv",
      skip = 1,
      header = T
    )
  )

xchg_tb <-
  xchg_tb %>%
  filter(Trade.Flow == "Import") %>%
  select(Reporter, Currency.Conversion.Factor) %>%
  rename(Currency.Conversion.GBP.to.USD = Currency.Conversion.Factor) %>%
  mutate(
    Year = as.integer(gsub("\\D","", Reporter)),
    Currency.Conversion.GBP.to.USD = as.numeric(Currency.Conversion.GBP.to.USD)
  ) %>%
  select(Year, Currency.Conversion.GBP.to.USD) 

# ******************************************************************************
# Import data
# ******************************************************************************

impdat <- list()

# Read import data files 
# (2001-2007 UK-CN & USA-CN imports by SITC3; 2001-2007 UK-WLD & USA-WLD total imports)
comtrade_files <- 
  list.files(
    path = paste0(getwd(), "/rawdata/imports"),
    pattern = "^COMTRADE",
    full.names = TRUE
  )

impdat[["raw SITC *"]] <- lapply(comtrade_files, function(x){read.csv(x)}) %>% bind_rows()

# Match SITC3 to NACE1; append UK CPI and GBP conversion factors; calc deflated and GBP series
impdat[["vol/gbp SITC-NACE *"]] <-
  impdat[["raw SITC *"]] %>%
  select(
    Year, Reporter.ISO, Partner.ISO, 
    Aggregate.Level, Commodity.Code, Commodity, 
    Trade.Value..US..
  ) %>%
  left_join(indlkp[["SITC3-NACE1 *"]], by = c("Commodity.Code" = "SITC3CD"))  %>%
  left_join(xchg_tb, by = "Year") %>%
  left_join(cpi_tb, by = "Year") %>%
  mutate(
    Trade.Value..GBP.. = Trade.Value..US../Currency.Conversion.GBP.to.USD,
    Trade.Volume..GBP.. = Trade.Value..GBP.. * CPI.All.Items.2015.100
  )

# For each year and reporter, remove SITC aggregates for which lower levels exist
impdat[["vol/gbp SITC-NACE lowlvl"]] <- impdat[["vol/gbp SITC-NACE *"]]

for (i in 2:5){
  impdat[["vol/gbp SITC-NACE lowlvl"]] <-
    impdat[["vol/gbp SITC-NACE lowlvl"]] %>% 
    group_by(Year, Reporter.ISO, Partner.ISO) %>%
    mutate(
      idig = ifelse(Aggregate.Level == i, substring(Commodity.Code, 1, i - 1), NA)
    ) %>%
    filter(!((Aggregate.Level == i - 1) & (Commodity.Code %in% idig))) %>%
    select(-idig) %>%
    ungroup()
}

# Aggregate SITC to NACE, apportioning 1-n matches using 1991 GBR emp weights
impdat[["vol/gbp NACE appt lowlvl"]] <-
  impdat[["vol/gbp SITC-NACE lowlvl"]] %>%
  left_join(
    indlkp[["SITC3-wNACE1 * 1-n"]][, c("SITC3CD", "NACE1CD", "wemp91")],
    by = c("Commodity.Code" = "SITC3CD", "NACE1CD")
  ) %>%
  filter( # Exclude totals and unmatched special transactions
    !grepl("TOTAL|^9", Commodity.Code) 
  ) %>%
  mutate( # Assign weight of 1 for 1-1 SITC-NACE matches
    wemp91 = case_when(is.na(wemp91) ~ 1, TRUE ~ wemp91)
  ) %>%
  group_by(Year, Reporter.ISO, Partner.ISO, NACE1CD, NACE1NM) %>%
  summarise(
    Trade.Value..US.. = sum(Trade.Value..US.. * wemp91),
    Trade.Value..GBP.. = sum(Trade.Value..GBP.. * wemp91),
    Trade.Volume..GBP.. = sum(Trade.Volume..GBP.. * wemp91)
  ) %>%
  ungroup()

# Keep only manufacturing industries; enter 0s for nace-year cells where there is no Comtrade data
impdat[["vol/gbp NACE appt lowlvl mnf 0fill"]] <-
  impdat[["vol/gbp NACE appt lowlvl"]] %>%
  filter(grepl(paste0("^", 15:36, collapse = "|"), NACE1CD)) %>%
  group_by(Reporter.ISO, Partner.ISO, NACE1CD, NACE1NM) %>%
  tidyr::complete(Year = tidyr::full_seq(1991:2007, 1)) %>%
  replace(is.na(.), 0) %>%
  ungroup()

# Calculate 5-year change and 1991-2007 change
impdat[["vol/gbp NACE appt lowlvl mnf 0fill"]] <-
  impdat[["vol/gbp NACE appt lowlvl mnf 0fill"]] %>%
  group_by(Reporter.ISO, Partner.ISO, NACE1CD, NACE1NM) %>%
  arrange(Year) %>%
  mutate(
    Trade.Value..US..16y = Trade.Value..US.. - lag(Trade.Value..US.., 16),
    Trade.Value..GBP..16y = Trade.Value..GBP.. - lag(Trade.Value..GBP.., 16),
    Trade.Volume..GBP..16y = Trade.Volume..GBP.. - lag(Trade.Volume..GBP.., 16)
  ) %>% 
  ungroup()
  
# Create tables for CN imports as % of all imports for each reporter
impdat[["total % CHN/WLD"]] <-
  impdat[["raw SITC *"]] %>%
  filter(Commodity.Code == "TOTAL") %>%
  select(Year, Reporter.ISO, Partner.ISO, Commodity.Code, Trade.Value..US..) %>%
  group_by(Year, Reporter.ISO) %>%
  summarise(
    Trade.Value..US..pcCHN = 
      100* Trade.Value..US..[Partner.ISO == "CHN"]/Trade.Value..US..[Partner.ISO == "WLD"]
  ) %>%
  ungroup()

# Calculate % of the value of all commodities that had 1-1, 1-n, or no SITC-NACE match
impdat[["% unique"]]<- 
  impdat[["vol/gbp SITC-NACE lowlvl"]] %>%
  filter(Partner.ISO == "CHN", Commodity.Code != "TOTAL") %>%
  group_by(Year, Reporter.ISO, Commodity.Code) %>%
  mutate(
    flag = case_when(
      n()>1 ~ "non-unq", 
      is.na(NACE1CD) ~ "non-match", 
      TRUE == 1 ~ "unq"
    )
  ) %>%
  ungroup() %>%
  select(-NACE1CD, -NACE1NM) %>% # Remove duplicates for non-unq
  distinct() %>%
  group_by(Year, flag) %>%
  summarise(Trade.Value..US.. = sum(Trade.Value..US..)) %>%
  mutate(Trade.Value..US..pcALL = Trade.Value..US../sum(Trade.Value..US..)) %>%
  ungroup()

# SAVE OUTPUT & CLEAN UP ENVIRONMENT -------------------------------------------

saveRDS(impdat, file="./output/datasets/impdat.RDS")
saveRDS(cpi_tb, file="./output/datasets/cpi_tb.RDS")
saveRDS(xchg_tb, file="./output/datasets/xchg_tb.RDS")

rm(list = ls())
gc()