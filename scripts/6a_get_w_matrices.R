# LOAD PACKAGES  ---------------------------------------------------------------

packages <-
  c(
    "dplyr",
    "expss"
  )

for (p in packages){ 
  if (! (p %in% installed.packages())){
    install.packages(p)
  }
}

library(dplyr)            

# PREPARE MATRIX DATA ----------------------------------------------------------

geodat <- readRDS(file="./output/datasets/geodat.RDS")
scidat <- readRDS(file = "./output/datasets/scidat.RDS")
commdat <- readRDS(file = "./output/datasets/commdat2.RDS")

nndat <-
  scidat[["by ITL21X EW"]] %>%
  left_join(
    geodat[["1991 by ITL21X EW"]]
  ) %>%
  left_join( # get origin commuting zones
    commdat[["T-S adj clusters by ITL21X EW"]],
    by = c("ITL321XCD_O" = "ITL321XCD")
  ) %>%
  rename( 
    "zones_ha_O" = "zones_ha",
    "zones_la_O" = "zones_la"
  ) %>%
  left_join( # get destination commuting zones
    commdat[["T-S adj clusters by ITL21X EW"]],
    by = c("ITL321XCD_D" = "ITL321XCD")
  ) %>%
  rename(
    "zones_ha_D" = "zones_ha",
    "zones_la_D" = "zones_la"
  ) %>%
  left_join( # get mean commuting dissimilarity
    commdat[["T-S average by ITL21X EW"]][, c("ITL321XCD_O", "ITL321XCD_D", "dissimilarity_avg")]
  ) %>%
  rename(
    SCI = scaled_sci,
    LCD = geo_d
  ) %>%
  filter( # remove connections with self
    ITL321XNM_O != ITL321XNM_D
  ) %>%
  mutate( # calc inverse geographic distance / `commuting-discounted' measures
    SCI_c = ifelse(zones_ha_O == zones_ha_D | dissimilarity_avg < 0.98, NA, SCI), 
    LCDi = 1/LCD, 
    LCD_c = ifelse(zones_ha_O == zones_ha_D | dissimilarity_avg < 0.98, NA, LCD),
    LCDi_c = ifelse(zones_ha_O == zones_ha_D | dissimilarity_avg < 0.98, NA, LCDi)
  ) %>%
  group_by(ITL321XCD_O) %>%
  mutate(
    across(c("LCDi", "LCDi_c", "SCI", "SCI_c"), ~dense_rank(desc(.)), .names = "n_{col}"),
    sall_SCI_c = ifelse(!is.na(n_SCI_c), 1, 0),
    s1_5_SCI_c = ifelse(n_SCI_c %in% 1:5, 1, 0),  
    sall_LCDi_c = ifelse(!is.na(n_LCDi_c), 1, 0),
    s1_5_LCDi_c = ifelse(n_LCDi_c %in% 1:5, 1, 0),
    across( # Weights of neighbours within sets
      c("sall_SCI_c",  "s1_5_SCI_c",),
      ~.*SCI/sum(.*SCI),
      .names = "{sub('s', 'w', col)}"
    ),
    across( 
      c("sall_LCDi_c",  "s1_5_LCDi_c"),
      ~.*LCDi/sum(.*LCDi),
      .names = "{sub('s', 'w', col)}"
    ),
    across( # Equal weights within sets
      c(contains("s1_5_")),
      ~./sum(.),
      .names = "{sub('s', 'weq', col)}"
    )
  ) %>% 
  ungroup()

nndat = expss::apply_labels(
  nndat,
  ITL321XCD_O = "Origin harmonised ITL3 region code",
  ITL321XNM_O = "Origin harmonised ITL3 region name",
  ITL321XCD_D = "Destination harmonised ITL3 region code",
  ITL321XNM_D = "Destination harmonised ITL3 region name",
  zones_ha_O = "Origin commuting zone",
  zones_ha_D = "Destination commuting zone",
  zones_la_O = "Origin lower commuting zone",
  zones_la_D  = "Destination lower commuting zone",
  dissimilarity_avg = "Commuting dissimilarity (mean across 1991,2001,2011)",
  SCI = "Social Connectedness Index",
  LCD = "Least-cost geographic distance",
  LCDi = "Inverse least-cost geographic distance",
  SCI_c = "Social Connectedness Index (commuting discounted)",
  n_SCI = "Destination rank as neighbour of origin by SCI",
  n_LCDi = "Destination rank as neighbour of origin by LCDi",
  n_SCI_c = "Destination rank as neighbour of origin by SCI_c",
  n_LCDi_c = "Destination rank as neighbour of origin by LCDi_c",
  sall_SCI_c = "Destination in set of all neighbours of origin by SCI_c",  
  sall_LCDi_c = "Destination in set of all neighbours of origin by LCDi_c",  
  s1_5_LCDi_c = "Destination in set of 1-5 nearest neighbours of origin by LCDi_c", 
  wall_SCI_c = "Weights within set of all neighbours of origin by SCI_c",  
  w1_5_SCI_c = "Weights within set of 1-5 nearest neighbours of origin by SCI_c", 
  wall_LCDi_c = "Weights within set of all neighbours of origin by LCDi_c",  
  w1_5_LCDi_c = "Weights within set of 1-5 nearest neighbours of origin by LCDi_c", 
  weq1_5_SCI_c = "Equal weights - set of 1-5 nearest neighbours of origin by SCI_c", 
  weq1_5_LCDi_c = "Equal weights - set of 1-5 nearest neighbours of origin by LCDi_c"
)


# SAVE OUTPUT & CLEAN UP ENVIRONMENT -------------------------------------------

saveRDS(nndat, file = "./output/datasets/nndat.RDS")

rm(list = ls())
gc()
