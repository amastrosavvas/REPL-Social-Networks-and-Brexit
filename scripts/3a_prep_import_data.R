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
      header = T
    )
  )

xchg_tb <-
  xchg_tb %>%
  select(notes.period, notes.importConvFactor, notes.exportConvFactor) %>%
  rename(
    Year = notes.period,
    Imp.Currency.Conversion.GBP.to.USD = notes.importConvFactor,
    Exp.Currency.Conversion.GBP.to.USD = notes.exportConvFactor
  ) %>%
  mutate(
    Imp.Currency.Conversion.GBP.to.USD = as.numeric(Imp.Currency.Conversion.GBP.to.USD),
    Exp.Currency.Conversion.GBP.to.USD = as.numeric(Exp.Currency.Conversion.GBP.to.USD),
  ) %>%
  select(Year, Imp.Currency.Conversion.GBP.to.USD, Exp.Currency.Conversion.GBP.to.USD) 

# ******************************************************************************
# Trade data
# ******************************************************************************

tradedat <- list()

# Read import and export data files 
comtrade_files <- 
  c(
    list.files(
      path = paste0(getwd(), "/rawdata/imports"),
      pattern = "^COMTRADE",
      full.names = TRUE
    ),
    list.files(
      path = paste0(getwd(), "/rawdata/exports"),
      pattern = "^COMTRADE",
      full.names = TRUE
    )
  )

tradedat[["raw SITC *"]] <- 
  lapply(comtrade_files, function(x){read.csv(x)}) %>% 
  bind_rows() %>%
  filter(
    customsDesc == "TOTAL CPC",
    motDesc == "TOTAL MOT",
    aggrLevel != "0"
  ) %>%
  select(-X) %>%
  distinct() 

tradedat[["raw SITC lowlvl"]]  <-  tradedat[["raw SITC *"]]

for (i in 2:5){ # Retain only lowest level STIC Rev. 3 with year-reporter-partner
  tradedat[["raw SITC lowlvl"]] <-
    tradedat[["raw SITC lowlvl"]] %>% 
    group_by(refYear, reporterISO, partnerISO, flowCode) %>%
    mutate(
      idig = ifelse(aggrLevel == i, substring(cmdCode, 1, i - 1), NA)
    ) %>%
    filter(!((aggrLevel == i - 1) & (cmdCode %in% idig))) %>%
    select(-idig) %>%
    ungroup()
}

tradedat[["vol/gbp NACE"]] <-
  tradedat[["raw SITC lowlvl"]] %>%
  select(
    refYear, reporterISO, partnerISO, 
    cmdCode, cmdDesc, aggrLevel, primaryValue, flowCode
  ) %>%   # Append CPI and exchange rate data
  left_join(xchg_tb, by = c("refYear" = "Year")) %>%
  left_join(cpi_tb, by = c("refYear" = "Year")) %>%
  left_join( # Match SITC3 to NACE1
    select(indlkp[["SITC3-NACE1 *"]], SITC3CD, NACE1CD,  NACE1NM),
    by = c("cmdCode" = "SITC3CD")
  ) %>%
  left_join( # Append SITC3-to-NACE1 weights for 1-to-n matches
    select(indlkp[["SITC3-wNACE1 * 1-n"]], SITC3CD, NACE1CD, NACE1NM, wemp91),
    by = c("cmdCode" = "SITC3CD", "NACE1CD", "NACE1NM")
  ) %>%
  mutate(
    wemp91 = ifelse(is.na(wemp91), 1, wemp91)
  ) %>% # Remove completely unmatched NACE1CD imports (special transactions)
  filter(!is.na(NACE1CD)) %>% # Assign currency conversion factor according to flow type
  mutate(
    Currency.Conversion.GBP.to.USD = case_when(
      flowCode == "M" ~ Imp.Currency.Conversion.GBP.to.USD,
      flowCode == "X" ~ Exp.Currency.Conversion.GBP.to.USD
    )
  ) %>%
  mutate( # Convert to GBP, deflate, and apportion 1-to-n
    volgbp = CPI.All.Items.2015.100*(primaryValue/Currency.Conversion.GBP.to.USD),
    volgbp = volgbp * wemp91
  ) %>%
  select(refYear, reporterISO, partnerISO, cmdCode, NACE1CD, NACE1NM, volgbp, flowCode) %>%
  distinct() %>%
  group_by(refYear, reporterISO, partnerISO, NACE1CD, NACE1NM, flowCode) %>%
  summarise(
    volgbp = sum(volgbp)
  ) %>%
  ungroup()

# Keep only manufacturing industries; enter 0s for nace-year cells where there is no Comtrade data
tradedat[["vol/gbp NACE mnf 0fill"]] <-
  tradedat[["vol/gbp NACE"]] %>%
  filter(grepl(paste0("^", 15:36, collapse = "|"), NACE1CD)) %>%
  group_by(reporterISO, partnerISO, NACE1CD, NACE1NM, flowCode) %>%
  tidyr::complete(refYear = c(1991, 2007)) %>%
  replace(is.na(.), 0) %>%
  ungroup()

# Get table 1 data: total GBR-CHN and GBR-WLD export and import changes
tradedat[["vol/gbp tab1"]] <-
  tradedat[["vol/gbp NACE mnf 0fill"]] %>%
  filter(reporterISO == "GBR") %>%
  group_by(refYear, reporterISO, partnerISO, flowCode) %>%
  summarise(volgbp = sum(volgbp)) %>%
  ungroup() %>%
  group_by(reporterISO, partnerISO, flowCode) %>%
  arrange(refYear) %>%
  mutate(
    pc_chg_16y = 100*(volgbp - lag(volgbp))/lag(volgbp)
  ) %>% 
  ungroup()

tradedat[["vol/gbp tab1"]] <- # Edit for in-text presentation
  tradedat[["vol/gbp tab1"]] %>%
  mutate(
    flow = case_when(
      partnerISO == "CHN" & flowCode == "M" ~ "Imports from China",
      partnerISO == "CHN" & flowCode == "X" ~ "Exports to China",
      partnerISO == "W00" & flowCode == "M" ~ "Imports from World",
      partnerISO == "W00" & flowCode == "X" ~ "Exports to World",
    )
  )

tradedat[["vol/gbp tab1"]] <-
  rbind(
    tradedat[["vol/gbp tab1"]], 
    mutate(filter(tradedat[["vol/gbp tab1"]], refYear == 2007), volgbp = NA, refYear = "Growth 1991-2017")
  ) %>%
  mutate(
    volgbp = volgbp/10^9,
    value = ifelse(refYear == "Growth 1991-2017", round(pc_chg_16y, 0), volgbp)
  ) %>%
  select(refYear, flow, value) %>%
  tidyr::pivot_wider(id_cols = refYear, names_from = flow)  %>%
  rename(Year = refYear)

# Aggregate reporter import data to high-income (HI) USA and both (HIUSA), calc changes
tradedat[["vol/gbp NACE RptAgg imp CHN"]] <-
  tradedat[["vol/gbp NACE mnf 0fill"]] %>%
  mutate( # Identify USA and HI reporter data
    reporterISO = case_when(
      reporterISO == "GBR" ~ "GBR",
      reporterISO == "USA" ~ "USA",
      TRUE ~ "HI"
    )
  ) %>%
  filter( # Drop exports and world partner
    flowCode == "M", 
    partnerISO != "W00"
  ) %>% # Aggregate data and get changes for GBR, USA, HI
  group_by(refYear, reporterISO, partnerISO, NACE1CD, NACE1NM) %>%
  summarise(volgbp = sum(volgbp)) %>%
  ungroup() %>%
  group_by(reporterISO, partnerISO, NACE1CD, NACE1NM) %>%
  arrange(refYear) %>%
  mutate(
    chg_16y = volgbp - lag(volgbp)
  ) %>%
  ungroup()

hiusa_sum <- # Aggregate data and get changes for HIUSA and append 
  tradedat[["vol/gbp NACE RptAgg imp CHN"]] %>%
  filter(reporterISO %in% c("USA", "HI")) %>%
  mutate(reporterISO = "HIUSA") %>%
  group_by(refYear, reporterISO, partnerISO, NACE1CD, NACE1NM) %>%
  summarise(volgbp = sum(volgbp)) %>%
  ungroup() %>%
  group_by(reporterISO, partnerISO, NACE1CD, NACE1NM) %>%
  arrange(refYear) %>%
  mutate(
    chg_16y = volgbp - lag(volgbp)
  ) %>%
  ungroup()

tradedat[["vol/gbp NACE RptAgg imp CHN"]] <-
  rbind( tradedat[["vol/gbp NACE RptAgg imp CHN"]], hiusa_sum)
  

# SAVE OUTPUT & CLEAN UP ENVIRONMENT -------------------------------------------

saveRDS(tradedat, file="./output/datasets/tradedat.RDS")
saveRDS(cpi_tb, file="./output/datasets/cpi_tb.RDS")
saveRDS(xchg_tb, file="./output/datasets/xchg_tb.RDS")
write.csv(tradedat[["vol/gbp tab1"]], "./output/tables/impexptab.csv", row.names = FALSE)

rm(list = ls())
gc()