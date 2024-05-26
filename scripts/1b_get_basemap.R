# LOAD PACKAGES ----------------------------------------------------------------

packages <-
  c(
    "dplyr",
    "sf"
  )

for (p in packages){ 
  if (! (p %in% installed.packages())){
    install.packages(p)
  }
}

library(dplyr)

# CONSTRUCT ITL21X AND LAD20X SHAPEFILES --------------------------------------

# Read geographic lookups
geolkp <- readRDS(file = "./output/lookups/geolkp.RDS")

# Read LAD20 map
temp <- tempfile()
unzip(zipfile = "./rawdata/geometries/ONS - LAD (Dec 2020) boundaries.zip", exdir = temp)
lad20_sf <- sf::st_read(temp) 

# Append ITL21X (AND LAD20X) codes to LAD20 boundary data
lad20_sf <- 
  lad20_sf %>%
  filter(!grepl("^N|^S", LAD20CD)) %>% # Drop NI/Scotland
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LADCD", "LAD20XCD", "LAD20XNM", "ITL321XCD", "ITL321XNM")]),
    by = c("LAD20CD"="LADCD")
  )

# Aggregate boundaries to ITL21X
itl321x_sf <- lad20_sf %>%
  sf::st_buffer(dist = 0.001) %>%
  select(ITL321XCD, ITL321XNM, BNG_E, BNG_N, geometry) %>%
  group_by(ITL321XCD, ITL321XNM) %>%
  summarise(
    BNG_E = mean(BNG_E),
    BNG_N = mean(BNG_N),
    geometry = sf::st_union(geometry)
  ) %>%
  ungroup()

# Aggregate boundaries to LAD20X
lad20x_sf <- lad20_sf %>%
  sf::st_buffer(dist = 0.001) %>%
  select(LAD20XCD, LAD20XNM, BNG_E, BNG_N, geometry) %>%
  group_by(LAD20XCD, LAD20XNM) %>%
  summarise(
    BNG_E = mean(BNG_E),
    BNG_N = mean(BNG_N),
    geometry = sf::st_union(geometry)
  ) %>%
  ungroup()
  
# SAVE OUTPUT & CLEAN UP ENVIRONMENT -------------------------------------------

saveRDS(lad20_sf, file="./output/geometries/lad20_sf.RDS")
saveRDS(lad20x_sf, file="./output/geometries/lad20x_sf.RDS")
saveRDS(itl321x_sf, file="./output/geometries/itl321x_sf.RDS")

rm(list = ls())
gc()