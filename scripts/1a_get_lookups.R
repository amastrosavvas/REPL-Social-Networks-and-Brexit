# LOAD PACKAGES ----------------------------------------------------------------

packages <-
  c(
    "dplyr",
    "readr",
    "readxl",
    "sf"
  )

for (p in packages){ 
  if (! (p %in% installed.packages())){
    install.packages(p)
  }
}

library(dplyr)

# CONSTRUCT GEOGRAPHICAL LOOK-UP TABLES ----------------------------------------

geolkp <- list()

# ******************************************************************************
# Construct LAD (Dec 2020) to LAD (previous) lookup
# ******************************************************************************

# Import geography code/name change lookup from CHD
geolkp[["LAD-LAD_P"]][[1]] <- 
  readr::read_csv(
    unz("./rawdata/lookups/ONS - Code History Database (Dec 2020 v2).zip", "Changes_V2.csv"),
    col_types = readr::cols(.default = "c")
  ) 

# Import geography code/name attributes from CHD
geolkp[["LAD-LAD_P"]][[2]] <-
  readr::read_csv(
    unz("./rawdata/lookups/ONS - Code History Database (Dec 2020 v2).zip", "ChangeHistory_V2.csv"),
    col_types = readr::cols(.default = "c")
  )

# Keep only LAD codes/names and append status column to lookup
geolkp[["LAD-LAD_P"]][[1]] <- 
  geolkp[["LAD-LAD_P"]][[1]]  %>%
  select(-GEOGNMW, -GEOGNMW_P) %>%
  filter(grepl('E06|E07|E08|E09|S12|W06', GEOGCD)) 

geolkp[["LAD-LAD_P"]] <- 
  left_join(
    geolkp[["LAD-LAD_P"]][[1]],
    geolkp[["LAD-LAD_P"]][[2]][,c("GEOGCD", "GEOGNM", "STATUS")],
    by = c("GEOGCD", "GEOGNM")
  ) %>%
  rename(
    LADCD = GEOGCD,
    LADNM = GEOGNM,
    LADCD_P = GEOGCD_P,
    LADNM_P = GEOGNM_P
  ) %>%
  distinct()

# Identify n-to-1 changes; for boundary changes, flag secondary succeeding areas
geolkp[["LAD-LAD_P n-1"]] <- 
  geolkp[["LAD-LAD_P"]] %>%
  group_by(LADCD_P) %>%
  filter(n()>1) %>%
  mutate(
    flag = case_when(
      (LADNM != LADNM_P) & !grepl("Name|re-coding", SI_TITLE) ~ "ignore"
    )
  ) %>%
  ungroup()

# Derive columns for corresponding LAD code/names as of Dec 2020:
# (a) 1-to-n boundary changes are restricted to the primary succeeding area
geolkp[["LAD20-LAD"]] <- 
  geolkp[["LAD-LAD_P"]] %>%
  left_join(geolkp[["LAD-LAD_P n-1"]]) %>% 
  filter(is.na(flag)) 

# (b) Codes/names appearing in 'previous' columns are matched to live equivs
# (c) Live codes/names are the Dec 2020 LAD version and are set as such
geolkp[["LAD20-LAD"]] <-
  geolkp[["LAD20-LAD"]] %>%
  left_join( 
    .[.$STATUS == "live", c("LADCD", "LADCD_P", "LADNM", "LADNM_P")], 
    by = c("LADCD" = "LADCD_P", "LADNM" = "LADNM_P")
  ) %>%
  rename(LAD20CD = LADCD.y, LAD20NM = LADNM.y) %>%
  mutate( 
    LAD20CD = case_when(STATUS == "live" ~ LADCD, TRUE ~ LAD20CD),
    LAD20NM = case_when(STATUS == "live" ~ LADNM, TRUE ~ LAD20NM)
  ) 

# (d) NAs due to >1 changes over lifetime are matched to successors' LAD20
geolkp[["LAD20-LAD"]] <-
  geolkp[["LAD20-LAD"]] %>%
  left_join( # 1st iteration
    geolkp[["LAD20-LAD"]][,c("LADCD_P", "LAD20CD", "LADNM_P", "LAD20NM")], 
    by = c("LADCD" = "LADCD_P" , "LADNM" = "LADNM_P")
  ) %>%
  mutate(
    LAD20CD.x = case_when(is.na(LAD20CD.x) ~ LAD20CD.y, TRUE ~ LAD20CD.x),
    LAD20NM.x = case_when(is.na(LAD20NM.x) ~ LAD20NM.y, TRUE ~ LAD20NM.x)
  ) %>%
  select(-LAD20CD.y, -LAD20NM.y) %>%
  rename(LAD20CD = LAD20CD.x, LAD20NM = LAD20NM.x) %>%
  left_join( # 2nd iteration
    .[,c("LADCD_P", "LAD20CD", "LADNM_P", "LAD20NM")], 
    by = c("LADCD" = "LADCD_P" , "LADNM" = "LADNM_P")
  ) %>%
  mutate(
    LAD20CD.x = case_when(is.na(LAD20CD.x) ~ LAD20CD.y, TRUE ~ LAD20CD.x),
    LAD20NM.x = case_when(is.na(LAD20NM.x) ~ LAD20NM.y, TRUE ~ LAD20NM.x)
  ) %>%
  select(-LAD20CD.y, -LAD20NM.y) %>%
  rename(LAD20CD = LAD20CD.x, LAD20NM = LAD20NM.x)

# Finalise LAD20-LAD code lookup by stacking LAD and LAD_P columns
geolkp[["LAD20-LAD"]] <-
  geolkp[["LAD20-LAD"]] %>%
  select(LADCD, LADCD_P, LAD20CD, LAD20NM)

geolkp[["LAD20-LAD"]] <-
  as_tibble(
    data.frame(
      geolkp[["LAD20-LAD"]][3:4], 
      stack(geolkp[["LAD20-LAD"]][1:2])
    )
  )

geolkp[["LAD20-LAD"]] <-
  geolkp[["LAD20-LAD"]] %>%
  rename(LADCD = values) %>%
  select(-ind) %>%
  distinct() %>%
  select(LAD20CD, LAD20NM, LADCD) %>% 
  filter(!is.na(LAD20CD)) 

# Add equivalent codes
equivs <- 
  readr::read_csv(
    unz("./rawdata/lookups/ONS - Code History Database (Dec 2020 v2).zip", "Equivalents_V2.csv"),
    col_types = readr::cols(.default = "c")
  ) %>% 
  filter(grepl('E06|E07|E08|E09|S12|W06', GEOGCD), grepl("live", STATUS)) %>%
  select(GEOGCD, GEOGNM, GEOGCDO) %>% 
  rename(LAD20CD = GEOGCD, LAD20NM = GEOGNM, LADCD = GEOGCDO) %>%
  distinct()

geolkp[["LAD20-LAD with equivs"]] <- 
  rbind(geolkp[["LAD20-LAD"]], equivs) %>% 
  distinct()

# Add equivalent codes for old scottish districts
scot_equivs <- 
  geolkp[["LAD20-LAD with equivs"]]  %>%
  filter(grepl("^S", LAD20CD) & grepl("^00", LADCD)) %>%
  mutate(LADCD = gsub("00", "60", LADCD))

geolkp[["LAD20-LAD with equivs"]] <-
  rbind(geolkp[["LAD20-LAD with equivs"]], scot_equivs) %>%
  filter(!grepl("Eilean Siar", LAD20NM)) # Na-h Eilean Siar is retained as name

# ******************************************************************************
# Construct LAD (Dec 2020) to CMLAD2011 lookup 
# ******************************************************************************

# Read LAD20 and CMLAD11 boundaries; convert to OS CRS
temp <- tempfile()
unzip("./rawdata/geometries/ONS - LAD (Dec 2020) boundaries.zip", exdir = temp)
lad20_sf <- 
  sf::st_read(temp) %>% 
  sf::st_transform(27700) %>%
  select(LAD20CD, LAD20NM, geometry)

temp <- tempfile()
unzip("./rawdata/geometries//ONS - CMLAD 2011 boundaries.zip", exdir = temp)
cmlad11_sf <- 
  sf::st_read(temp) %>% 
  sf::st_transform(27700) %>%
  select(cmlad11cd, cmlad11nm, geometry) 

# Construct LAD20-CMLAD11 lookup by point-in-polygon; identify n-to-1 cases
geolkp[["LAD20-CMLAD11 EW"]] <-
  sf::st_join(
    sf::st_point_on_surface(lad20_sf[grepl("^E|^W", lad20_sf$LAD20CD),]), cmlad11_sf
  ) %>%
  sf::st_drop_geometry()

geolkp[["LAD20-CMLAD11 EW n-1"]] <- geolkp[["LAD20-CMLAD11 EW"]] %>%
  group_by(cmlad11cd) %>%
  filter(n()>1) %>%
  ungroup()

# ******************************************************************************
# Construct LAD20X to LAD lookup
#
# Notes: 
# LAD20X is equal to LAD20 except where there are n-to-1 matches with CMLAD11,
# in which case LAD20 is replaced with CMLAD11.
# ******************************************************************************

geolkp[["LAD20X-LAD"]] <- geolkp[["LAD20-LAD with equivs"]] %>%
  left_join(geolkp[["LAD20-CMLAD11 EW n-1"]], by = c("LAD20CD", "LAD20NM")) %>%
  mutate(
    LAD20CD = case_when(!is.na(cmlad11cd) ~ cmlad11cd, TRUE ~ LAD20CD),
    LAD20NM = case_when(!is.na(cmlad11nm) ~ cmlad11nm, TRUE ~ LAD20NM)
  ) %>%
  rename(LAD20XCD = LAD20CD, LAD20XNM = LAD20NM) %>%
  select(LAD20XCD, LAD20XNM, LADCD)

# Add n-1 LAD20XCD to LAD CD
geolkp[["LAD20X-LAD"]] <- rbind(
  geolkp[["LAD20X-LAD"]],
  geolkp[["LAD20-CMLAD11 EW n-1"]] %>% 
    select(cmlad11cd, cmlad11nm) %>%
    rename(LAD20XCD = cmlad11cd, LAD20XNM = cmlad11nm) %>%
    mutate(LADCD = LAD20XCD) %>%
    distinct()
)

# ******************************************************************************
# Construct LAD20X to ITL21 lookup
# ******************************************************************************

# Load LAD20-ITL21 lookup; match LAD20 to LAD20X; identify 1-to-n cases for EW
geolkp[["LAD20X-ITL21"]] <- 
  readxl::read_excel("./rawdata/lookups/ONS - LAD (Dec 2020) to ITL (Jan 2021) lookup.xlsx", sheet = 1)

geolkp[["LAD20X-ITL21"]]  <-
  geolkp[["LAD20X-ITL21"]] %>%
  filter(!grepl("^TLN", ITL321CD)) %>% # # Drop NI
  left_join(geolkp[["LAD20X-LAD"]], by = c("LAD20CD" = "LADCD")) %>%
  select(
    LAD20XCD, LAD20XNM, ITL321CD, ITL321NM, ITL221CD, ITL221NM, ITL121CD, ITL121NM
  ) %>% 
  distinct()

geolkp[["LAD20X-ITL21 EW 1-n"]] <- 
  geolkp[["LAD20X-ITL21"]] %>%
  group_by(LAD20XCD) %>%
  filter(!grepl("^TLM", ITL321CD)) %>% # Drop Scotland
  filter(n()>1) %>%
  ungroup()

# ******************************************************************************
# Construct NUTS 2016 to ITL21 lookup
# ******************************************************************************

# Load NUTS16 - NUTS21 (equiv. to ITL21) lookup; identify boundary changes
geolkp[["NUTS16-ITL21"]] <- 
  readxl::read_excel(
    "./rawdata/lookups/EUROSTAT - NUTS16 to NUTS21 changes lookup.xlsx", 
    sheet = "Changes detailed NUTS 2016-2021"
  )

geolkp[["NUTS16-ITL21"]] <-
  geolkp[["NUTS16-ITL21"]] %>%
  filter(
    .$'NUTS level' == 3,
    grepl("^UK", .$'Code 2016'),        # Keep UK NUTS
    !grepl("^UKN|^UKZ", .$'Code 2016'),      # Drop NI/Extra-Regio
  ) %>%
  select("Code 2016", "Code 2021", "Change comments", contains("NUTS level 3")) %>%
  rename(
    NUTS316CD = "Code 2016",  
    NUTS321CD = "Code 2021",
    NUTS321NM = contains("NUTS level 3")) %>%
  mutate(
    ITL321CD = gsub("UK", "TL", NUTS321CD),
    ITL321NM = NUTS321NM
  )

geolkp[["NUTS16-ITL21 EW bdchg"]] <- geolkp[["NUTS16-ITL21"]] %>%
  filter(grepl("boundary change", .$"Change comments")) 

# ******************************************************************************
# Construct master geography lookup (ITL21X-ITL21-NUTS16-LAD20X-LAD20-LAD)
#
# Notes:
# ITL21X is equal to ITL21 except:
# (a) Areas in Scotland are in LAD20
# (b) Some ITL21 in E&W are aggregated for correspondence with LAD20X and NUTS16
# ******************************************************************************

# Derive ITL21 aggregates based on LAD20X-ITL21 1-n and NUTS16-ITL21 bdchanges
geolkp[["ITL21X-*-LAD"]] <-
  geolkp[["LAD20X-ITL21 EW 1-n"]] %>%
  select(ITL321CD, ITL321NM) %>%
  rbind(geolkp[["NUTS16-ITL21 EW bdchg"]][,c("ITL321CD", "ITL321NM")]) %>%
  group_by(substring(ITL321CD, 1, 4)) %>%
  mutate(
    ITL321XCD = paste0(substring(ITL321CD, 1, 4), "X"), 
    ITL321XNM = paste0(ITL321NM, collapse = " + ") 
  ) %>%
  ungroup() %>%
  select(ITL321XCD, ITL321XNM, ITL321CD, ITL321NM)


geolkp[["ITL21X-*-LAD"]] <-
  geolkp[["ITL21X-*-LAD"]] %>%
  left_join( # Append NUTS16 equivs of ITL21 aggregates
    geolkp[["NUTS16-ITL21"]][,c("NUTS316CD", "ITL321CD", "ITL321NM")],
    by = c("ITL321CD", "ITL321NM") 
  ) %>%
  bind_rows( # Append remaining ITL21/NUTS not covered by aggregates
    geolkp[["NUTS16-ITL21"]][
      !geolkp[["NUTS16-ITL21"]]$ITL321CD %in% .$ITL321CD,
      c("NUTS316CD", "ITL321CD", "ITL321NM")
    ]
  ) %>%
  left_join( # Append LAD20X equivs 
    geolkp[["LAD20X-ITL21"]][,c("LAD20XCD", "LAD20XNM", "ITL321CD")],
    by = c("ITL321CD")
  ) %>%
  left_join( # Append LAD equivs 
    geolkp[["LAD20X-LAD"]][,c("LAD20XCD", "LAD20XNM", "LADCD")],
    by = c("LAD20XCD", "LAD20XNM")
  ) %>%
  left_join( # Append LAD20 equivs
    geolkp[["LAD20-LAD with equivs"]][,c("LAD20CD", "LAD20NM", "LADCD")],
    by = c("LADCD")
  ) %>%  
  mutate( # Set ITLX values for non-aggs (LAD20 for Scotland, ITL for rest)
    ITL321XCD = case_when(
      grepl("^TLM", ITL321CD) ~ LAD20CD,
      is.na(ITL321XCD) ~ ITL321CD,
      TRUE ~ ITL321XCD
    ),
    ITL321XNM = case_when(
      grepl("^TLM", ITL321CD) ~ LAD20NM,
      is.na(ITL321XNM) ~ ITL321NM,
      TRUE ~ ITL321XNM
    )
  )

# ******************************************************************************
# Construct ITL21X-NUTS16 "weighted" lookup for England and Wales
#
# Note: 
# Includes weight of each NUTS16 within each ITL21X based on 2020 population est.
# This table is used to aggregate SCI data from the NUTS16 to the ITL21X level.
# ******************************************************************************

# Get ITL21X-NUTS16 matches
geolkp[["ITL21X-wNUTS16 EW"]] <-
  unique(geolkp[["ITL21X-*-LAD"]][, c("ITL321XCD", "ITL321XNM", "NUTS316CD")]) %>%
  filter(!grepl("^S", ITL321XCD)) # Drop Scotland

# Import ONS LAD population estimates; convert to NUTS16
ladpop20 <- readr::read_csv(
  "./rawdata/other/ONS - mid-2020 population estimates by LAD.csv",
   col_types = readr::cols(.default = "c", OBS_VALUE = "d")
)

ladpop20 <-
  ladpop20 %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("NUTS316CD", "LADCD")]),
    by = c("GEOGRAPHY_CODE" = "LADCD")
  ) %>% 
  mutate( # Fix for West/East Northamptonshire
    NUTS316CD = case_when(
      GEOGRAPHY_NAME == "West Northamptonshire" ~ "UKF24",
      GEOGRAPHY_NAME == "North Northamptonshire" ~ "UKF25",
      TRUE ~ NUTS316CD
    )
  )

# Join population data with ITL21-NUTS16 matches
geolkp[["ITL21X-wNUTS16 EW"]] <-
  geolkp[["ITL21X-wNUTS16 EW"]] %>%
  left_join(ladpop20[,c("NUTS316CD", "OBS_VALUE")], by = "NUTS316CD") %>%
  rename(pop20 = OBS_VALUE) %>%
  group_by(ITL321XCD, ITL321XNM, NUTS316CD) %>%
  summarise(pop20 = sum(pop20)) %>%
  ungroup()

# Calculate relative weight of NUTS16 in ITL21X
geolkp[["ITL21X-wNUTS16 EW"]] <-
  geolkp[["ITL21X-wNUTS16 EW"]] %>%
  group_by(ITL321XCD, ITL321XNM) %>%
  mutate(wpop20 = pop20/sum(pop20)) %>%
  ungroup()

# CONSTRUCT INDUSTRY-PRODUCT LOOK-UP TABLES ------------------------------------

indlkp <- list()

# ******************************************************************************
# Construct SITC Rev. 3 - NACE Rev. 1 
# ******************************************************************************

# Import SITC3-NACE1 lookup from WB; Identify 1-to-n cases
indlkp[["SITC3-NACE1"]] <-
  readr::read_csv(
    unz("./rawdata/lookups/WORLD BANK - SITC Rev.3 to NACE Rev.1 lookup.zip", "JobID-74_Concordance_S3_to_NC.CSV"),
    col_types = readr::cols(.default = "c")
  )

indlkp[["SITC3-NACE1"]] <-
  indlkp[["SITC3-NACE1"]] %>%
  rename(
    SITC3CD = "SITC Revision 3 Product Code",
    SITC3NM = "SITC Revision 3 Product Description",
    NACE1CD = "NACE Revision 1 Product Code",
    NACE1NM = "NACE Revision 1 Product Description"
  )

indlkp[["SITC3-NACE1 1-n"]] <- indlkp[["SITC3-NACE1"]] %>%
  group_by(SITC3CD) %>%
  filter(n()>1) %>%
  ungroup()

# Create SITC3-NACE1 lookup for all SITC3 levels (1,2,3,4,5-digit); Identify 1-to-n
indlkp[["SITC3-NACE1 *"]] <- list()
for (i in seq(1, 5)) {
  indlkp[["SITC3-NACE1 *"]][[i]] <- 
    indlkp[["SITC3-NACE1"]] %>%
    select(SITC3CD, NACE1CD, NACE1NM) %>%
    mutate(SITC3CD = substr(SITC3CD, 1, i)) %>%
    distinct()
}

indlkp[["SITC3-NACE1 *"]] <-
  indlkp[["SITC3-NACE1 *"]] %>%
  bind_rows %>% 
  distinct() 

indlkp[["SITC3-NACE1 * 1-n"]] <- 
  indlkp[["SITC3-NACE1 *"]] %>%
  group_by(SITC3CD) %>%
  filter(n()>1)

# ******************************************************************************
# Construct SITC3-NACE1 "weighted" lookup 
#
# Note: 
# Includes weight of each NACE1 within each SITC3 based on 1991 GBR employment.
# 
# This table is used to apportion imports to industries where there are 1-n matches.
# ******************************************************************************

em91_files <- 
  list.files(
    path = "./rawdata/employment",
    pattern = "ONS - 1991 employment by 3-dig SIC92 and LAD *",
    full.names = TRUE
  )

lademp91 <- 
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
    emp91 = OBS_VALUE
  )  %>%
  mutate(NACE1CD = substring(INDUSTRY_NAME,1,3)) %>%
  group_by(NACE1CD) %>%
  summarise(emp91 = sum(emp91)) %>%
  ungroup()
  
# Join employment data to SITC3-NACE1 matches; calculate weights
indlkp[["SITC3-wNACE1 * 1-n"]] <- indlkp[["SITC3-NACE1 * 1-n"]] %>%
  left_join(lademp91, by = "NACE1CD") %>%
  group_by(SITC3CD) %>%
  mutate(wemp91 = emp91/sum(emp91)) %>%
  ungroup()
 
# SAVE OUTPUT & CLEAN UP ENVIRONMENT -------------------------------------------

saveRDS(geolkp, file="./output/lookups/geolkp.RDS")
saveRDS(indlkp, file="./output/lookups/indlkp.RDS")

rm(list = ls())
gc()



