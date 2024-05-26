# LOAD PACKAGES  ---------------------------------------------------------------

packages <-
  c(
    "dplyr",
    "readr",
    "tidyr",
    "readxl",
    "haven"
  )

for (p in packages){ 
  if (! (p %in% installed.packages())){
    install.packages(p)
  }
}

library(dplyr)                    

# PREPARE NODE DATA ------------------------------------------------------------

geolkp <- readRDS(file = "./output/lookups/geolkp.RDS")

# ******************************************************************************
# EU Referendum 2016 data
# ******************************************************************************

euref16dat <- list()

euref16dat[["by LAD EW"]] <-
  readr::read_csv(
    "./rawdata/voting/UK ELECTORAL COMMISSION - 2016 - EURef results.csv",
    col_types = readr::cols(
      .default = "d", 
      Region_Code = "c", 
      Region = "c", 
      Area_Code = "c", 
      Area = "c"
    )
  ) %>%
  rename(LADCD = Area_Code, LADNM = Area) %>%
  filter(
    !grepl("^N|^S|^GI", LADCD)  # Drop NI/Scotland/GI
  ) %>%
  select(LADCD, LADNM, Leave, Valid_Votes, Votes_Cast, Electorate) %>%
  distinct()

euref16dat[["by LAD20X EW"]] <- # %leave and %turnout by LAD20X
  euref16dat[["by LAD EW"]] %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LADCD", "LAD20XCD", "LAD20XNM")]),
    by = "LADCD"
  ) %>%
  group_by(LAD20XCD, LAD20XNM) %>%
  summarise(
    pc_leave = sum(Leave)/sum(Valid_Votes),
    pc_to = sum(Votes_Cast)/sum(Electorate),
    Votes_Cast = sum(Votes_Cast),
    Electorate = sum(Electorate)
  ) %>%
  ungroup()

euref16dat[["by ITL21X EW"]] <- # %leave and %turnout by ITL21X
  euref16dat[["by LAD EW"]] %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LADCD", "ITL321XCD", "ITL321XNM")]),
    by = "LADCD"
  ) %>%
  group_by(ITL321XCD, ITL321XNM) %>%
  summarise(
    pc_leave = sum(Leave)/sum(Valid_Votes),
    pc_to = sum(Votes_Cast)/sum(Electorate),
    Votes_Cast = sum(Votes_Cast),
    Electorate = sum(Electorate)
  ) %>%
  ungroup()
  

# ******************************************************************************
# 1991 Census data
# ******************************************************************************

cens91dat <- list()

cens91dat[["by LAD EW"]] <-
  readr::read_csv(
    "./rawdata/other/ONS - 1991 Census statistics by LAD.csv",
    col_types = readr::cols(.default = "c", OBS_VALUE = "d")
  ) %>%
  rename(LADCD = GEOGRAPHY_CODE, LADNM = GEOGRAPHY_NAME) %>%
  filter(
    !grepl("^N|^S", LADCD)  # Drop NI/Scotland
  ) %>%
  mutate( # Assign aggregation labels to cells
    CELL_NAME = case_when(
      CELL_NAME == "L01:64 (RESIDENTS 1991: 1991 BASE (1+2+3+4) : Total persons )" ~ "totpop_l01",
      CELL_NAME == "L02:56 (Aged 16 - 17 : Total persons )" ~ "aged16_17_l02",
      CELL_NAME == "L02:67 (Aged 18 - 19 : Total persons )" ~ "aged18_34_l02",
      CELL_NAME == "L02:78 (Aged 20 - 24 : Total persons )" ~ "aged18_34_l02",
      CELL_NAME == "L02:89 (Aged 25 - 29 : Total persons )" ~ "aged18_34_l02",
      CELL_NAME == "L02:100 (Aged 30 - 34 : Total persons )" ~ "aged18_34_l02",
      CELL_NAME == "L02:111 (Aged 35 - 39 : Total persons )" ~ "aged35_64_l02",
      CELL_NAME == "L02:122 (Aged 40 - 44 : Total persons )" ~ "aged35_64_l02",
      CELL_NAME == "L02:133 (Aged 45 - 49 : Total persons )" ~ "aged35_64_l02",
      CELL_NAME == "L02:144 (Aged 50 - 54 : Total persons )" ~ "aged35_64_l02",
      CELL_NAME == "L02:155 (Aged 55 - 59 : Total persons )" ~ "aged35_64_l02",
      CELL_NAME == "L02:166 (Aged 60 - 64 : Total persons )" ~ "aged35_64_l02",
      CELL_NAME == "L02:177 (Aged 65 - 69 : Total persons )" ~ "aged65ov_l02",
      CELL_NAME == "L02:188 (Aged 70 - 74 : Total persons )" ~ "aged65ov_l02",
      CELL_NAME == "L02:199 (Aged 75 - 79 : Total persons )" ~ "aged65ov_l02",
      CELL_NAME == "L02:210 (Aged 80 - 84 : Total persons )" ~ "aged65ov_l02",
      CELL_NAME == "L02:221 (Aged 85 - 89 : Total persons )" ~ "aged65ov_l02",
      CELL_NAME == "L02:232 (Aged 90 & over : Total persons )" ~ "aged65ov_l02",
      CELL_NAME == "L07:28 (Born in Outside United Kingdom : Persons )" ~ "foreign_l07",
      CELL_NAME == "L08:1 (Total persons : Total Aged 16 and Over )"  ~ "aged16ov_l08",
      CELL_NAME == "L08:20 (Persons Economically Active : Total Aged 16 and Over )" ~ "ea_l08",
      CELL_NAME == "L08:134 (Persons EA unemployed : Total Aged 16 and Over )" ~ "unemp_l08",
      CELL_NAME == "L73:1 (Total persons : Total persons )" ~ "aged16ov_l73",
      CELL_NAME == "L73:5 (Total persons : Manufacturing metal etc )" ~ "mnfemp_l73",
      CELL_NAME == "L73:6 (Total persons : Other manufacturing )" ~ "mnfemp_l73",
      CELL_NAME == "L73:7 (Total persons : Construction )" ~ "conemp_l73",
      CELL_NAME == "L73:8 (Total persons : Distribution and catering )" ~ "disemp_l73",
      CELL_NAME == "L73:9 (Total persons : Transport )" ~ "traemp_l73",
      CELL_NAME == "L73:10 (Total persons : Banking, finance etc )" ~ "banemp_l73",
      CELL_NAME == "L73:11 (Total persons : Other services )" ~ "othemp_l73",
      CELL_NAME == "L84:1 (All persons aged 18 and over : Total persons )" ~ "aged18ov_l84",
      CELL_NAME == "L84:7 (Level a (higher degree) : Total persons )" ~ "aedu_l84",
      CELL_NAME == "L84:22 (Level b (degree) : Total persons )" ~ "bedu_l84",
      CELL_NAME == "L84:37 (Level c (diploma etc.) : Total persons )" ~ "cedu_l84",
      TRUE ~ CELL_NAME
    )
  ) %>% # Aggregate cells that are split in levels (m/f, level a/b/c etc) 
  group_by(LADCD, LADNM, CELL_NAME) %>%
  summarise(OBS_VALUE = sum(OBS_VALUE)) %>%
  ungroup() 

cens91dat[["by LAD20X EW"]] <- # 1991 census %statistics by LAD20X
  cens91dat[["by LAD EW"]] %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LADCD", "LAD20XCD", "LAD20XNM")]),
    by = "LADCD"
  ) %>%
  group_by(LAD20XCD, LAD20XNM, CELL_NAME) %>% # aggregate persons by LAD20X
  summarise(OBS_VALUE = sum(OBS_VALUE))  %>% 
  ungroup() %>%
  tidyr::spread(CELL_NAME, OBS_VALUE) %>% # unstack columns
  mutate(
    pca_uni = 100*(aedu_l84 / aged18ov_l84),
    pcp_aged65ov = 100*(aged65ov_l02 / totpop_l01),
    pcp_foreign = 100*(foreign_l07 / totpop_l01),
    pc16_mnf = 100*(mnfemp_l73 / aged16ov_l73)
  ) %>%
  select(LAD20XCD, LAD20XNM, which(grepl("^pc",colnames(.))))
  
cens91dat[["by ITL21X EW"]] <- # 1991 census %statistics by ITL21X
  cens91dat[["by LAD EW"]] %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LADCD", "ITL321XCD", "ITL321XNM")]),
    by = "LADCD"
  ) %>%
  group_by(ITL321XCD, ITL321XNM, CELL_NAME) %>% # aggregate persons by ITL21X
  summarise(OBS_VALUE = sum(OBS_VALUE))  %>% 
  ungroup() %>%
  tidyr::spread(CELL_NAME, OBS_VALUE) %>% # unstack columns
  mutate(
    pca_uni = 100*(aedu_l84 / aged18ov_l84),
    pcp_aged65ov = 100*(aged65ov_l02 / totpop_l01),
    pcp_foreign = 100*(foreign_l07 / totpop_l01),
    pc16_mnf = 100*(mnfemp_l73 / aged16ov_l73),
  ) %>%
  select(ITL321XCD, ITL321XNM, which(grepl("^pc",colnames(.))))


# ******************************************************************************
# 2011 Census data
# ******************************************************************************

cens11dat <- list()

cens11dat[["by LAD EW"]] <-
  readr::read_csv(
    "./rawdata/other/ONS - 2011 Census statistics by LAD.csv",
    col_types = readr::cols(.default = "c", OBS_VALUE = "d")
  ) %>%
  rename(LADCD = GEOGRAPHY_CODE, LADNM = GEOGRAPHY_NAME) %>%
  filter(
    !grepl("^N|^S", LADCD)  # Drop NI/Scotland
  ) %>%
  mutate( # Assign aggregation labels to cells
    CELL_NAME = case_when(
      CELL_NAME == "All categories: Country of birth" ~ "totpop",
      CELL_NAME == "Europe: United Kingdom: Total" ~ "local_born",
      TRUE ~ CELL_NAME
    )
  ) %>% # Aggregate cells that are split in levels (m/f, level a/b/c etc) 
  group_by(LADCD, LADNM, CELL_NAME) %>%
  summarise(OBS_VALUE = sum(OBS_VALUE)) %>%
  ungroup() 

cens11dat[["by LAD20X EW"]] <- # 1991 census %statistics by LAD20X
  cens11dat[["by LAD EW"]] %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LADCD", "LAD20XCD", "LAD20XNM")]),
    by = "LADCD"
  ) %>%
  group_by(LAD20XCD, LAD20XNM, CELL_NAME) %>% # aggregate persons by LAD20X
  summarise(OBS_VALUE = sum(OBS_VALUE))  %>% 
  ungroup() %>%
  tidyr::spread(CELL_NAME, OBS_VALUE) %>% # unstack columns
  mutate(
    pcp_foreign = 100*(totpop - local_born) / totpop
  ) %>%
  select(LAD20XCD, LAD20XNM, which(grepl("^pc",colnames(.))))
  
cens11dat[["by ITL21X EW"]] <- # 1991 census %statistics by ITL21X
  cens11dat[["by LAD EW"]] %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LADCD", "ITL321XCD", "ITL321XNM")]),
    by = "LADCD"
  ) %>%
  group_by(ITL321XCD, ITL321XNM, CELL_NAME) %>% # aggregate persons by ITL21X
  summarise(OBS_VALUE = sum(OBS_VALUE))  %>% 
  ungroup() %>%
  tidyr::spread(CELL_NAME, OBS_VALUE) %>% # unstack columns
  mutate(
    pcp_foreign11 = 100*(totpop - local_born) / totpop
  ) %>%
  select(ITL321XCD, ITL321XNM, which(grepl("^pc",colnames(.))))

# ******************************************************************************
# Regional GVA by NUTS3 data
# ******************************************************************************

gvadat <- list() 

# Get GVA data
gvadat[["GVA by NUTS3 EW"]] <-
  readxl::read_excel(
    "./rawdata/other/ONS - Regional GVA by NUTS3.xls",
    sheet = "Table 1",
    col_names = F
  ) %>% 
  tidyr::drop_na()

# Get GVA per head data
gvadat[["GVA phead by NUTS3 EW"]] <-
  readxl::read_excel(
    "./rawdata/other/ONS - Regional GVA by NUTS3.xls",
    sheet = "Table 2",
    col_names = F
  ) %>% 
  tidyr::drop_na()


# Rename columns, remove first row and column
gvadat <-
  lapply(
    gvadat,
    function(x){
      setNames(x, x[1,])
    }
  )

gvadat <-
  lapply(
    gvadat,
    function(x){
      x[-1,-1]
    }
  )

# Rename columns, remove Scotland/NI data
gvadat <-
  lapply(
    gvadat,
    function(x){
      x %>%
        rename(
          "NUTS316CD" = "NUTS code",
          "NUTS316NM" = "Region name"
        ) %>%
        filter(
          nchar(NUTS316CD) == 5,
          !grepl("^UKM|^UKN", NUTS316CD)
        )
    }
  )

# Stack years
gvadat <-
  lapply(
    gvadat,
    function(x){
      data.frame(x[1:2], stack(x[3:ncol(x)])) 
    }
  )

# Rename vars, rescale, convert to numeric
gvadat[["GVA by NUTS3 EW"]] <-
  gvadat[["GVA by NUTS3 EW"]] %>%
  rename(
    "gva" = "values",
    "year" = "ind"
  ) %>%
  mutate(
    gva = as.numeric(gva),
    gva = gva*10^6
  )

gvadat[["GVA phead by NUTS3 EW"]] <-
  gvadat[["GVA phead by NUTS3 EW"]] %>%
  rename(
    "gvaph" = "values",
    "year" = "ind"
  ) %>%
  mutate(
    gvaph = as.numeric(gvaph)
  )

# by ITL321XCD
gvadat[["GVA and phead by ITL21X EW"]] <-
  gvadat[["GVA by NUTS3 EW"]] %>%
  left_join(gvadat[["GVA phead by NUTS3 EW"]]) %>%
  mutate(heads = gva/gvaph) %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("NUTS316CD", "ITL321XCD", "ITL321XNM")]),
    by = "NUTS316CD"
  ) %>%
  select(-gvaph, -NUTS316CD, -NUTS316NM) %>%
  group_by(ITL321XCD, ITL321XNM, year) %>%
  summarise(
    gva = sum(gva),
    heads = sum(heads)
  ) %>%
  ungroup() %>%
  mutate(gvaph = gva/heads)

gvadat[["CRI by ITL21X EW"]] <-
  gvadat[["GVA and phead by ITL21X EW"]] %>%
  filter(year %in% c("1997", "2016")) %>%
  group_by(year) %>%
  mutate(gvaph_yrmd = median(gvaph)) %>%
  ungroup() %>%
  mutate(gvaph_rt = gvaph/gvaph_yrmd) %>%
  group_by(ITL321XCD, ITL321XNM) %>% 
  arrange(year, .by_group = TRUE) %>%
  mutate(cri = (gvaph_rt/lag(gvaph_rt) - 1) * 100) %>%
  tidyr::drop_na() %>%
  select(ITL321XCD, ITL321XNM, cri)

# ******************************************************************************
# Mid-2020 population estimates
# ******************************************************************************

popdat <- list()

popdat[["2020 by LAD EW"]] <-
  readr::read_csv(
    "./rawdata/other/ONS - mid-2020 population estimates by LAD.csv",
    col_types = readr::cols(.default = "c", OBS_VALUE = "d")
  ) %>%
  rename(
    LADCD = GEOGRAPHY_CODE, 
    LADNM = GEOGRAPHY_NAME, 
    Year = DATE_NAME, 
    pop = OBS_VALUE
  ) %>%
  filter(!grepl("^N|^S", LADCD)) %>%  # Drop NI/Scotland
  select(Year, LADCD, LADNM, pop)

popdat[["2020 by LAD20X EW"]] <- # population by LAD20X (pre-2020 data to be added later)
  popdat[["2020 by LAD EW"]] %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LADCD", "LAD20XCD", "LAD20XNM")]),
    by = "LADCD"
  ) %>%
  group_by(Year, LAD20XCD, LAD20XNM) %>%
  summarise(pop = sum(pop)) %>%
  ungroup()

popdat[["2020 by ITL21X EW"]] <- # population by ITL21X (pre-2020 data to be added later)
  popdat[["2020 by LAD EW"]] %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LADCD", "ITL321XCD", "ITL321XNM")]),
    by = "LADCD"
  ) %>%
  group_by(Year, ITL321XCD, ITL321XNM) %>%
  summarise(pop = sum(pop)) %>%
  ungroup()

  
# ******************************************************************************
# Annual Employment Survey 1991 employment by LAD and SIC92(NACE1) data
# ******************************************************************************

emp91dat <- list()

em91_files <- 
  list.files(
    path = "./rawdata/employment",
    pattern = "ONS - 1991 employment by 3-dig SIC92 and LAD *",
    full.names = TRUE
  )

emp91dat[["by LAD-NACE GBR"]] <- 
  lapply(
    em91_files, 
    function(x){
      readr::read_csv(
        x, 
        col_types = readr::cols(.default = "c", OBS_VALUE = "d")
      )
    }
  ) %>% 
  bind_rows() %>%
  rename(
    LADCD = GEOGRAPHY_CODE,
    LADNM = GEOGRAPHY_NAME,
    emp = OBS_VALUE
  ) %>%
  mutate(
    NACE1CD = substring(INDUSTRY_NAME,1,3),
    NACE1NM = substring(INDUSTRY_NAME,7,nchar(INDUSTRY_NAME))
  ) %>%
  select(LADCD, LADNM, NACE1CD, NACE1NM, emp) %>%
  filter(!NACE1CD == "010")  # Remove NAs for DEFRA Agricultural data

emp91dat[["by LAD20X-NACE GBR"]] <- # 1991 employment by LAD20X
  emp91dat[["by LAD-NACE GBR"]] %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LADCD", "LAD20XCD", "LAD20XNM")]),
    by = "LADCD"
  ) %>%
  group_by(LAD20XCD, LAD20XNM, NACE1CD, NACE1NM) %>%
  summarise(emp = sum(emp)) %>%
  ungroup()

emp91dat[["by ITL21X-NACE GBR"]] <- # 1991 employment by ITL21X
  emp91dat[["by LAD-NACE GBR"]] %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LADCD", "ITL321XCD", "ITL321XNM")]),
    by = "LADCD"
  ) %>%
  group_by(ITL321XCD, ITL321XNM, NACE1CD, NACE1NM) %>%
  summarise(emp = sum(emp)) %>%
  ungroup()

# ******************************************************************************
# Internet Users 2016 data
# ******************************************************************************

intdat <- 
 readxl::read_excel(
    "./rawdata/other/ONS - Internet users.xlsx",
    sheet = "6a",
    col_names = F
  )

intdat <- intdat[,colSums(is.na(intdat))<nrow(intdat)]

colnames(intdat) <- c(
  "NUTS16CD", 
  "NUT16NM",
  "actintusers_2014",
  "actintusers_2015",
  "actintusers_2016",
  "actintusers_2017",
  "actintusers_2018",
  "actintusers_2019",
  "actintusers_2020",
  "othintusers_2014",
  "othintusers_2015",
  "othintusers_2016",
  "othintusers_2017",
  "othintusers_2018",
  "othintusers_2019",
  "othintusers_2020"
)

intdat <-
  intdat %>%
  filter(nchar(NUTS16CD) == 5) %>%
  left_join(
    unique(geolkp[["ITL21X-wNUTS16 EW"]][, c("NUTS316CD", "ITL321XCD", "ITL321XNM", "wpop20")]),
    by = c("NUTS16CD" = "NUTS316CD")
  )  %>%
  filter(!grepl("^UKN|^UKM", NUTS16CD)) %>%
  mutate(
    intdata_2016or17 =  as.numeric(ifelse(actintusers_2016 == "-", "2017", "2016")),
    actintusers_2016or17 = as.numeric(ifelse(actintusers_2016 == "-", actintusers_2017, actintusers_2016)),
    othintusers_2016or17 = as.numeric(ifelse(othintusers_2016 == "-", othintusers_2017, othintusers_2016)),
    actintusers_2016or17_pc = 100*actintusers_2016or17/(actintusers_2016or17 + othintusers_2016or17)
  )  %>%
  left_join(select(geolkp[["ITL21X-wNUTS16 EW"]], ITL321XCD, NUTS316CD, wpop20, by = c("NUTS16CD" = "NUTS316CD"))) %>%
  group_by(ITL321XCD, ITL321XNM) %>%
  summarise( # Use pop-weights for aggregating the 2 ITL-X regions
    actintusers_2016or17_pc = sum(actintusers_2016or17_pc*wpop20),
    intdata_2016or17 = median(intdata_2016or17)
  ) %>%
  ungroup() %>%
  select(ITL321XCD, ITL321XNM, actintusers_2016or17_pc, intdata_2016or17) %>%
  filter(!is.na(ITL321XCD))

# ******************************************************************************
# Fetzer (2019) data - Austerity exposure, EP vote shares, LAD
# ******************************************************************************

temp <- tempfile()
unzip(zipfile = "./rawdata/other/fetzdat.zip", exdir = temp)

fetzdatl <- 
  haven::read_dta(paste0(temp, "/data files/DISTRICT.dta")) %>%
  select(
    code, TurnoutPct, UKIPPct,ConPct, LabPct, LDPct, GreenPct, EU2016RefElectorate, POPULATION,  year, 
    totalimpact_finlosswapyr, 
  ) %>%
  filter(year %in% c(2009, 2014)) %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LADCD", "ITL321XCD", "ITL321XNM")]),
    by = c("code" = "LADCD")
  ) %>%
  group_by(ITL321XCD, ITL321XNM, year) %>%
  filter(!is.na(totalimpact_finlosswapyr)) %>%
  summarise(
    pc_to = sum(POPULATION*TurnoutPct*100)/sum(POPULATION),
    pc_ukip = sum(POPULATION*UKIPPct)/sum(POPULATION), # already in %
    pc_con = sum(POPULATION*ConPct*100)/sum(POPULATION),
    pc_lab = sum(POPULATION*LabPct*100)/sum(POPULATION),
    pc_ldp = sum(POPULATION*LDPct*100)/sum(POPULATION),
    pc_green = sum(POPULATION*GreenPct*100)/sum(POPULATION),
    totalimpact_finlosswapyr = sum(totalimpact_finlosswapyr*POPULATION)/sum(POPULATION)
  ) %>%
  ungroup()

fetzdatw <-
  fetzdatl %>%
  tidyr::pivot_wider(
    values_from = c("totalimpact_finlosswapyr"), 
    names_from = "year"
  ) %>%
  select("ITL321XCD", "ITL321XNM", "2009") %>%
  rename(totalimpact_finlosswapyr = "2009") %>%
  filter(!is.na(totalimpact_finlosswapyr))
  

# SAVE OUTPUT & CLEAN UP ENVIRONMENT -------------------------------------------

saveRDS(euref16dat, file="./output/datasets/euref16dat.RDS")
saveRDS(cens91dat, file="./output/datasets/cens91dat.RDS")
saveRDS(cens11dat, file="./output/datasets/cens11dat.RDS")
saveRDS(popdat, file="./output/datasets/popdat.RDS")
saveRDS(emp91dat, file="./output/datasets/emp91dat.RDS")
saveRDS(gvadat, file="./output/datasets/gvadat.RDS")
saveRDS(intdat, file="./output/datasets/intdat.RDS")
saveRDS(fetzdatl, file="./output/datasets/fetzdatl.RDS")
saveRDS(fetzdatw, file="./output/datasets/fetzdatw.RDS")

rm(list = ls())
gc()

