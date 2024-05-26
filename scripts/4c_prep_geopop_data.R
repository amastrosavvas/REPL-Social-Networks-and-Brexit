# LOAD PACKAGES  ---------------------------------------------------------------

packages <-
  c(
    "dplyr",
    "gdistance",
    "nngeo",
    "raster",
    "reshape2",
    "sf",
    "stringr",
    "tidyr"
  )

for (p in packages){ 
  if (! (p %in% installed.packages())){
    install.packages(p)
  }
}

library(dplyr)                    

# PREPARE GEO-DISTANCE AND POPULATION DATA -------------------------------------

geolkp <- readRDS(file = "./output/lookups/geolkp.RDS")
popdat <- readRDS(file = "./output/datasets/popdat.RDS")


# Read ED/LSOA population and boundary data chuncks from 1991-2001-2011 Census
geopop_files <- 
  list.files(
    path = "./rawdata/geometries",
    pattern = "boundaries and population",
    full.names = TRUE
  )

geodat <- 
  lapply(
    geopop_files, 
    function(x){
      
      out <- sf::st_make_valid(sf::st_read(x)) # read and fix geometries
      out <- out[!sf::st_is_empty(out),] # remove empty geometries
      out <- sf::st_transform(out, 27700) # convert to OS grid
      out <- sf::st_point_on_surface(out) #  get surface centroids
      
      return(out)
      
    }
  ) 

geodat <- geodat %>% bind_rows() # bind 1991/2001/2011 chunks

geodat <- geodat %>%
  mutate(
    Description = gsub("Value for.*", "", Description),
    Year = stringr::str_extract(Description, "(?<=for\\s)(\\d*)"), # extract year
    pop = as.integer(stringr::str_extract(Description, "(\\d*)(?=\\sfor)")) # extract population
  ) %>%
  dplyr::select(-Description)

# Point-in-polygon of 1991-2001-2011 ED/LSOA by spatial join with ITLX and LAD20X
itl321x_sf <- readRDS(file = "./output/geometries/itl321x_sf.RDS")
lad20x_sf <- readRDS(file = "./output/geometries/lad20x_sf.RDS")

geodat <- geodat %>%
  sf::st_join(itl321x_sf[, c("ITL321XCD", "ITL321XNM", "geometry")]) %>%
  sf::st_join(lad20x_sf[, c("LAD20XCD", "LAD20XNM", "geometry")]) %>%
  distinct()

itl321x_missing <- unlist(nngeo::st_nn(geodat[is.na(geodat$ITL321XCD),], itl321x_sf, k =1))
lad20x_missing <- unlist(nngeo::st_nn(geodat[is.na(geodat$LAD20XCD),], itl321x_sf, k =1))

# For unmatched points-in-polygon, match by nearest polygon
geodat[is.na(geodat$ITL321XCD), "ITL321XCD"] <- 
  itl321x_sf[itl321x_missing, "ITL321XCD"][[1]]

geodat[is.na(geodat$ITL321XNM), "ITL321XNM"] <- 
  itl321x_sf[itl321x_missing, "ITL321XNM"][[1]]

geodat[is.na(geodat$LAD20XCD), "LAD20XCD"] <- 
  lad20x_sf[lad20x_missing, "LAD20XCD"][[1]]

geodat[is.na(geodat$LAD20XNM), "LAD20XNM"] <- 
  lad20x_sf[lad20x_missing, "LAD20XNM"][[1]]

# Calculate population weighted LAD20X centroids for each year and GCD
lad20x_ctd <-
  geodat %>%
  mutate(
    Easting = sf::st_coordinates(.)[,1],
    Northing = sf::st_coordinates(.)[,2],
  ) %>%
  sf::st_drop_geometry() %>%
  group_by(Year, LAD20XCD, LAD20XNM) %>%
  summarise(
    Easting = weighted.mean(Easting, pop +  10^-10),    
    Northing = weighted.mean(Northing, pop + 10^-10),
    pop = sum(pop),
  ) %>% 
  ungroup() %>%
  sf::st_as_sf(coords = c("Easting", "Northing"), crs = 27700) %>%
  sf::st_transform(4326) # Convert to longlat to get GC distance

# Calculate population weighted ITL321X centroids for each year and GCD
itl321x_ctd <-
  geodat %>%
  mutate(
    Easting = sf::st_coordinates(.)[,1],
    Northing = sf::st_coordinates(.)[,2],
  ) %>%
  sf::st_drop_geometry() %>%
  group_by(Year, ITL321XCD, ITL321XNM) %>%
  summarise(
    Easting = weighted.mean(Easting, pop +  10^-10),    
    Northing = weighted.mean(Northing, pop + 10^-10),
    pop = sum(pop),
  ) %>% 
  ungroup() %>%
  sf::st_as_sf(coords = c("Easting", "Northing"), crs = 27700) %>%
  sf::st_transform(4326) # Convert to longlat to get GC distance

# Derive pairwise population sum tables
popdat[["1991-2011 O-D by LAD20X EW"]] <-
  sf::st_drop_geometry(lad20x_ctd) %>%
  setNames(c("Year", paste0(names(.)[2:length(names(.))], '_O'))) %>%
  tidyr::crossing(sf::st_drop_geometry(lad20x_ctd[, c("LAD20XCD", "LAD20XNM")])) %>%
  rename(LAD20XCD_D = LAD20XCD, LAD20XNM_D = LAD20XNM) %>%
  left_join(
    sf::st_drop_geometry(lad20x_ctd), 
    by = c("Year" = "Year", "LAD20XCD_D" = "LAD20XCD", "LAD20XNM_D" = "LAD20XNM")
  ) %>%
  rename(pop_D = pop) 

popdat[["1991-2011 O-D by ITL21X EW"]] <-
  sf::st_drop_geometry(itl321x_ctd) %>%
  setNames(c("Year", paste0(names(.)[2:length(names(.))], '_O'))) %>%
  tidyr::crossing(sf::st_drop_geometry(itl321x_ctd[, c("ITL321XCD", "ITL321XNM")])) %>%
  rename(ITL321XCD_D = ITL321XCD, ITL321XNM_D = ITL321XNM) %>%
  left_join(
    sf::st_drop_geometry(itl321x_ctd), 
    by = c("Year" = "Year", "ITL321XCD_D" = "ITL321XCD", "ITL321XNM_D" = "ITL321XNM")
  ) %>%
  rename(pop_D = pop) 

# Buffer islands; derive EW outline; rasterise outline (~ 1kmx1km resolution)
itl21xbf_sf <- itl321x_sf 

itl21xbf_sf[itl21xbf_sf$ITL321XCD == "TLJ34", "geometry"] <-
  sf::st_buffer(itl21xbf_sf[itl21xbf_sf$ITL321XCD == "TLJ34", "geometry"], dist = 5000)

itl21xbf_sf[itl21xbf_sf$ITL321XCD == "TLL11", "geometry"] <-
  sf::st_buffer(itl21xbf_sf[itl21xbf_sf$ITL321XCD == "TLL11", "geometry"], dist = 350)

ew_sf <-
  itl21xbf_sf %>% 
  summarise() %>%  
  sf::st_transform(4326)

ew_raster <- raster::raster(x = raster::extent(ew_sf), nrow = 650,  ncol = 550, crs = "+proj=longlat +datum=WGS84")
ew_raster <- raster::rasterize(x = ew_sf, y = ew_raster, field = 1)
ew_raster[is.na(ew_raster)] <- 0

# Derive and correct transition matrix
ew_raster_tr <- gdistance::transition(ew_raster, transitionFunction = mean, directions = 16) 
ew_raster_tr <- gdistance::geoCorrection(ew_raster_tr, type = "c")

# Calculate least cost distances
lad20x_dist <- # LAD20X-LAD20X using 1991 centroids
  gdistance::costDistance(
    ew_raster_tr,
    as(sf::as_Spatial(lad20x_ctd[lad20x_ctd$Year == "1991",]), "SpatialPoints"),
    as(sf::as_Spatial(lad20x_ctd[lad20x_ctd$Year == "1991",]), "SpatialPoints")
  )

colnames(lad20x_dist) <- lad20x_ctd[lad20x_ctd$Year == "1991",]$LAD20XCD
rownames(lad20x_dist) <- lad20x_ctd[lad20x_ctd$Year == "1991",]$LAD20XCD

itl321x_dist <- # ITL21X-ITL21X using 1991 centroids
  gdistance::costDistance(
    ew_raster_tr,
    as(sf::as_Spatial(itl321x_ctd[itl321x_ctd$Year == "1991",]), "SpatialPoints"),
    as(sf::as_Spatial(itl321x_ctd[itl321x_ctd$Year == "1991",]), "SpatialPoints")
  )

colnames(itl321x_dist) <- itl321x_ctd[itl321x_ctd$Year == "1991",]$ITL321XCD
rownames(itl321x_dist) <- itl321x_ctd[itl321x_ctd$Year == "1991",]$ITL321XCD

# Convert distance matrices to data frame format
geodat <- list()

geodat[["1991 by LAD20X EW"]] <-
  setNames(reshape2::melt(lad20x_dist), c("LAD20XCD_O", "LAD20XCD_D", "geo_d"))

geodat[["1991 by ITL21X EW"]] <-
  setNames(reshape2::melt(itl321x_dist), c("ITL321XCD_O", "ITL321XCD_D", "geo_d"))

geodat[["1991 by ITL21X EW London"]] <-
  geodat[["1991 by ITL21X EW"]] %>%
  filter(ITL321XCD_D == "TLI3X") %>%
  select(ITL321XCD_O, geo_d) %>%
  rename(
    ITL321XCD = ITL321XCD_O,
    km_to_london = geo_d
  ) %>%
  mutate(
    km_to_london = km_to_london/1000
  )


# SAVE OUTPUT & CLEAN UP ENVIRONMENT -------------------------------------------

saveRDS(geodat, file="./output/datasets/geodat.RDS")
saveRDS(popdat, file="./output/datasets/popdat2.RDS")
saveRDS(ew_raster, file="./output/geometries/ew_raster.RDS")
saveRDS(lad20x_ctd, file="./output/geometries/lad20x_ctd.RDS")
saveRDS(itl321x_ctd, file="./output/geometries/itl321x_ctd.RDS")

rm(list = ls()) 
gc()
