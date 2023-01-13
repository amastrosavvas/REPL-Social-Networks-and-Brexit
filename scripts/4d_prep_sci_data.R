# LOAD PACKAGES  ---------------------------------------------------------------

packages <-
  c(
    "dplyr",
    "data.table"
  )

for (p in packages){ 
  if (! (p %in% installed.packages())){
    install.packages(p)
  }
}

library(dplyr)                    

# PREPARE EDGE DATA ------------------------------------------------------------

scidat <- list()
geolkp <- readRDS(file = "./output/lookups/geolkp.RDS")

# Read raw SCI as of Aug 2020
scidat[["by NUTS16 EW"]] <-
  data.table::fread(
    cmd = "unzip -p ./rawdata/sci/gadm1_nuts3_counties_gadm1_nuts3_counties_Aug2020.tsv.zip",
    sep="\t",
    header=TRUE
  ) %>%
  filter(
    grepl("^UK", user_loc),    # Keep only UK (also picks up Ukraine)
    grepl("^UK", fr_loc),
    !grepl("^UKN", user_loc),  # Drop NI
    !grepl("^UKN", fr_loc), 
    !grepl("^UKM", user_loc),  # Drop Scotland
    !grepl("^UKM", fr_loc), 
    !grepl("^UKR", user_loc),  # Drop Ukraine
    !grepl("^UKR", fr_loc)
  ) 

# Aggregate SCI from NUTS16 to ITL21X using pop20 weights from geolkp

scidat[["by ITL21X EW incl self"]] <-
  scidat[["by NUTS16 EW"]] %>%
  left_join(geolkp[["ITL21X-wNUTS16 EW"]], by = c("user_loc" = "NUTS316CD")) %>%
  group_by(fr_loc, ITL321XCD, ITL321XNM) %>%
  rename(ITL321XCD_O = ITL321XCD, ITL321XNM_O = ITL321XNM) %>%
  ungroup() %>%
  left_join(geolkp[["ITL21X-wNUTS16 EW"]], by = c("fr_loc" = "NUTS316CD")) %>%
  group_by(ITL321XCD_O, ITL321XNM_O, ITL321XCD, ITL321XNM) %>%
  rename(ITL321XCD_D = ITL321XCD, ITL321XNM_D = ITL321XNM) %>%
  ungroup() %>%
  select(-user_loc, -fr_loc, -wpop20.x, -wpop20.y) %>%
  group_by(ITL321XCD_O, ITL321XNM_O, ITL321XCD_D, ITL321XNM_D) %>%
  summarise(
    scaled_sci = sum(pop20.x/sum(pop20.x) * pop20.y/sum(pop20.y) * scaled_sci)
  ) %>% 
  ungroup() 

scidat[["by ITL21X EW"]]  <-
  scidat[["by ITL21X EW incl self"]] %>%
  mutate(scaled_sci = ifelse(ITL321XCD_O == ITL321XCD_D, 0, scaled_sci))

scidat[["by ITL21X EW self"]] <-
  scidat[["by ITL21X EW incl self"]] %>%
  filter(ITL321XCD_O == ITL321XCD_D) %>%
  rename(
    "ITL321XCD" = "ITL321XCD_O",
    "ITL321XNM" = "ITL321XNM_O"
  ) %>%
  select(ITL321XCD, ITL321XNM, scaled_sci)

# SAVE OUTPUT & CLEAN UP ENVIRONMENT -------------------------------------------

saveRDS(scidat, file="./output/datasets/scidat.RDS")

rm(list = ls()) 
gc()

