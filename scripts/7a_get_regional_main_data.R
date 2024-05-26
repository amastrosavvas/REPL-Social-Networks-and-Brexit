# LOAD PACKAGES  ---------------------------------------------------------------

packages <-
  c(
    "dplyr",
    "haven",
    "expss"
  )

for (p in packages){ 
  if (! (p %in% installed.packages())){
    install.packages(p)
  }
}

library(dplyr) 

# REGDAT PREP  -----------------------------------------------------------------


commdat <- readRDS(file = "./output/datasets/commdat2.RDS")[["T-S adj clusters by ITL21X EW"]]
geodat <- readRDS(file = "./output/datasets/geodat.RDS")[["1991 by ITL21X EW London"]]
euref16dat <- readRDS(file = "./output/datasets/euref16dat.RDS")[["by ITL21X EW"]] %>% 
  select(ITL321XCD, Votes_Cast, Electorate, pc_leave)
cens91dat <- readRDS(file = "./output/datasets/cens91dat.RDS")[["by ITL21X EW"]]
migshockdat <- readRDS(file = "./output/datasets/migshockdat.RDS")
lagaustshockdat <- readRDS(file = "./output/datasets/lagaustshockdat.RDS")
gvadat <- readRDS(file = "./output/datasets/gvadat.RDS")[["CRI by ITL21X EW"]]
popdat <- readRDS(file = "./output/datasets/popdat2.RDS")[["1991-2011 O-D by ITL21X EW"]] %>%
  filter(Year == 2001) %>%
  select(ITL321XCD_O, ITL321XNM_O, pop_O) %>%
  rename(ITL321XCD=1, ITL321XNM=2, pop2001=3) %>%
  distinct()
intdat <- readRDS(file = "./output/datasets/intdat.RDS")
fetzdatw <- readRDS(file = "./output/datasets/fetzdatw.RDS")
itl321x_sf <- 
  readRDS(file = "./output/geometries/itl321x_sf.RDS") %>%
  mutate(
    area_size = sf::st_area(geometry)
  ) %>%
  select(ITL321XCD, area_size) %>%
  sf::st_drop_geometry()

shockdat <- readRDS(file = "./output/datasets/shockdat2.RDS")[["ITL21X shocks unstacked"]]
shockdat_adj <- readRDS(file = "./output/datasets/shockdat2.RDS")[["ITL21X shocks unstacked adj EW + lags"]]

regdat <- 
  commdat %>%
  left_join(euref16dat) %>%
  left_join(geodat) %>%
  left_join(cens91dat) %>%
  left_join(migshockdat) %>%
  left_join(lagaustshockdat) %>%
  left_join(gvadat) %>%
  left_join(popdat) %>%
  left_join(intdat) %>%
  left_join(fetzdatw) %>%
  left_join(shockdat) %>%
  left_join(shockdat_adj) %>%
  left_join(itl321x_sf)

rm(commdat, geodat, euref16dat, cens91dat, migshockdat, lagaustshockdat, gvadat, intdat, itl321x_sf, fetzdatw, shockdat, shockdat_adj)


# MIGR DAT PREP  ---------------------------------------------------------------

nndat <- readRDS(file = "./output/datasets/nndat.RDS") %>%
  mutate(
    ITL321XCD_O = as.character(ITL321XCD_O),
    across(contains("ITL321X"), ~as.character(.))
  )

migrdat <- readRDS(file = "./output/datasets/migrdat.RDS")

# Get 2002-2020 migration data for endogeneity analysis

mainmigr0220 <-
  nndat %>%
  left_join(
    migrdat[["undir 2002-2020 by ITL21X EW"]]
  ) %>%
  select(
    ITL321XCD_O, ITL321XNM_O, ITL321XCD_D, ITL321XNM_D,
    migrants, n_SCI_c
  ) %>%
  rename(
    grossmigrants0220 = migrants
  ) %>% 
  rename("n_SCI_c_O" = "n_SCI_c") %>% 
  filter(!is.na(n_SCI_c_O)) 

mainmigr0220_agg_all_c <-
  mainmigr0220 %>%
  group_by(ITL321XCD_O, ITL321XNM_O) %>%
  summarise(
    nn = max(n_SCI_c_O, na.rm = T),
    grossmigrants0220_all = sum(grossmigrants0220)
  ) %>%
  ungroup() %>%
  left_join(popdat, by = c("ITL321XCD_O" = "ITL321XCD", "ITL321XNM_O" = "ITL321XNM")) %>%
  rename(pop2001_O = pop2001) %>%
  mutate(
    grossmigrants0220_all_pc = grossmigrants0220_all/pop2001_O,
    avg_grossmigrants0220_all_pc = grossmigrants0220_all_pc/nn
  )%>%
  rename(
    ITL321XCD = ITL321XCD_O,
    ITL321XNM = ITL321XNM_O
  ) %>%
  select(-pop2001_O, -nn)

mainmigr0220_agg_5_c <-
  mainmigr0220 %>%
  filter(n_SCI_c_O <= 5) %>%
  group_by(ITL321XCD_O, ITL321XNM_O) %>%
  summarise(
    nn = max(n_SCI_c_O, na.rm = T),
    grossmigrants0220_5 = sum(grossmigrants0220)
  ) %>%
  ungroup() %>%
  left_join(popdat, by = c("ITL321XCD_O" = "ITL321XCD", "ITL321XNM_O" = "ITL321XNM")) %>%
  rename(pop2001_O = pop2001) %>%
  mutate(
    grossmigrants0220_5_pc = grossmigrants0220_5/pop2001_O,
    avg_grossmigrants0220_5_pc = grossmigrants0220_5_pc/nn
  )%>%
  rename(
    ITL321XCD = ITL321XCD_O,
    ITL321XNM = ITL321XNM_O
  ) %>%
  select(-pop2001_O, -nn)


mainmigr0220 <-
  mainmigr0220 %>%
  left_join(popdat, by = c("ITL321XCD_D" = "ITL321XCD", "ITL321XNM_D" = "ITL321XNM")) %>%
  rename(pop2001_D = pop2001) %>%
  left_join(popdat, by = c("ITL321XCD_O" = "ITL321XCD", "ITL321XNM_O" = "ITL321XNM")) %>%
  rename(pop2001_O = pop2001) %>%
  mutate(
    pop2001_O_pctile = as.numeric(ntile(pop2001_O, 100)),
    pop2001_D_pctile = as.numeric(ntile(pop2001_D, 100)),
    grossmigrants0220Z = (grossmigrants0220 -mean(grossmigrants0220, na.rm = T))/sd(grossmigrants0220, na.rm =T)
  )

# Get 2002-2016 migration data for economic spillover analysis

mainmigr0216 <-
  nndat %>%
  left_join(
    migrdat[["undir 2002-2016 by ITL21X EW"]]
  ) %>%
  select(
    ITL321XCD_O, ITL321XNM_O, ITL321XCD_D, ITL321XNM_D,
    migrants, n_SCI_c
  ) %>%
  rename(
    grossmigrants0216 = migrants
  ) %>% 
  rename("n_SCI_c_O" = "n_SCI_c") %>% 
  filter(!is.na(n_SCI_c_O)) 

mainmigr0216_agg_all_c <-
  mainmigr0216 %>%
  group_by(ITL321XCD_O, ITL321XNM_O) %>%
  summarise(
    nn = max(n_SCI_c_O, na.rm = T),
    grossmigrants0216_all = sum(grossmigrants0216)
  ) %>%
  ungroup() %>%
  left_join(popdat, by = c("ITL321XCD_O" = "ITL321XCD", "ITL321XNM_O" = "ITL321XNM")) %>%
  rename(pop2001_O = pop2001) %>%
  mutate(
    grossmigrants0216_all_pc = grossmigrants0216_all/pop2001_O,
    avg_grossmigrants0216_all_pc = grossmigrants0216_all_pc/nn
  )%>%
  rename(
    ITL321XCD = ITL321XCD_O,
    ITL321XNM = ITL321XNM_O
  ) %>%
  select(-pop2001_O, -nn)

mainmigr0216_agg_5_c <-
  mainmigr0216 %>%
  filter(n_SCI_c_O <= 5) %>%
  group_by(ITL321XCD_O, ITL321XNM_O) %>%
  summarise(
    nn = max(n_SCI_c_O, na.rm = T),
    grossmigrants0216_5 = sum(grossmigrants0216)
  ) %>%
  ungroup() %>%
  left_join(popdat, by = c("ITL321XCD_O" = "ITL321XCD", "ITL321XNM_O" = "ITL321XNM")) %>%
  rename(pop2001_O = pop2001) %>%
  mutate(
    grossmigrants0216_5_pc = grossmigrants0216_5/pop2001_O,
    avg_grossmigrants0216_5_pc = grossmigrants0216_5_pc/nn
  )%>%
  rename(
    ITL321XCD = ITL321XCD_O,
    ITL321XNM = ITL321XNM_O
  ) %>%
  select(-pop2001_O, -nn)



# Join with other regional data
regdat <-
  regdat %>%
  left_join(mainmigr0220_agg_all_c, by = c("ITL321XCD", "ITL321XNM")) %>%
  left_join(mainmigr0220_agg_5_c, by = c("ITL321XCD", "ITL321XNM")) %>%
  left_join(mainmigr0216_agg_all_c, by = c("ITL321XCD", "ITL321XNM")) %>%
  left_join(mainmigr0216_agg_5_c, by = c("ITL321XCD", "ITL321XNM")) 


# FINALISE REGDAT  -------------------------------------------------------------

# Standardise / rescale variables 
regdat <- 
  regdat %>%
  mutate(
    pc_leave = 100*pc_leave,
  ) %>% 
  mutate(
    across(c("ITL321XCD", "ITL321XNM", "zones_ha", "zones_la"), ~as.factor(.)),
    across(-c("ITL321XCD", "ITL321XNM", "zones_ha", "zones_la"), ~as.numeric(.)),
    across(-c("ITL321XCD", "ITL321XNM", "zones_ha", "zones_la", "Votes_Cast", "Electorate", "pop2001", "pc_leave", "intdata_2016or17", "area_size"), .fns = list(Z = ~(.-mean(.))/sd(.)))
  ) 

names(regdat) <- gsub("_Z", "Z", names(regdat))

# Long Fetzer dataset

fetzdatl <- 
  readRDS(file = "./output/datasets/fetzdatl.RDS") %>%
  left_join(
    select(regdat, ITL321XCD, rshk_GBRZ, rshk_HIZ, lgrshk_GBR_w1_5_SCI_cZ, lgrshk_GBR_wall_SCI_cZ, totalimpact_finlosswapyrZ, lgrshk_HI_w1_5_SCI_cZ, lgrshk_HI_wall_SCI_cZ, pc16_mnfZ, zones_ha, zones_la)
  ) %>%
  mutate(
    rshk_GBRZ_09 = ifelse(year == 2009, rshk_GBRZ, 0),
    rshk_HIZ_09 = ifelse(year == 2009, rshk_HIZ, 0),
    lgrshk_GBR_w1_5_SCI_cZ_09 = ifelse(year == 2009, lgrshk_GBR_w1_5_SCI_cZ, 0),
    lgrshk_HI_w1_5_SCI_cZ_09 = ifelse(year == 2009,  lgrshk_HI_w1_5_SCI_cZ, 0),
    lgrshk_GBR_wall_SCI_cZ_09 = ifelse(year == 2009, lgrshk_GBR_wall_SCI_cZ, 0),
    lgrshk_HI_wall_SCI_cZ_09 = ifelse(year == 2009,  lgrshk_HI_wall_SCI_cZ, 0),
    rshk_GBRZ_14 = ifelse(year == 2014, rshk_GBRZ, 0),
    rshk_HIZ_14 = ifelse(year == 2014, rshk_HIZ, 0),
    lgrshk_GBR_w1_5_SCI_cZ_14 = ifelse(year == 2014, lgrshk_GBR_w1_5_SCI_cZ, 0),
    lgrshk_HI_w1_5_SCI_cZ_14 = ifelse(year == 2014,  lgrshk_HI_w1_5_SCI_cZ, 0),
    lgrshk_GBR_wall_SCI_cZ_14 = ifelse(year == 2014, lgrshk_GBR_wall_SCI_cZ, 0),
    lgrshk_HI_wall_SCI_cZ_14 = ifelse(year == 2014,  lgrshk_HI_wall_SCI_cZ, 0),
    austerity_post10Z = ifelse(year == 2014,  totalimpact_finlosswapyrZ, 0)
  )

# Labels
regdat <- expss::apply_labels(
  regdat,
  ITL321XCD = "Harmonised ITL3 region code",
  ITL321XNM = "Harmonised ITL3 region name",
  zones_ha = "Commuting zone",
  zones_la = "Lower commuting zone",
  Votes_Cast = "Votes cast in EURef 2016",
  Electorate = "Electorate in EURef 2016",
  pc_leave = "Leave vote share",
  pop2001 = "2001 population",
  pcp_aged65ov = "Population share over 65 1991",
  pcp_foreign = "Population share foreign 1991",
  pc16_mnf = "Employment share in manufacturing 1991",
  cri = "Change in relative income vs median 1997-2016",
  km_to_london = "Geographic distance to London (km)",
  km_to_londonZ = "Geographic distance to London (km; standardised)",
  totalimpact_finlosswapyr = "Austerity shock",
  totalimpact_finlosswapyrZ = "Austerity shock (standardised)",
  migshock = "Population share foreign, growth rate 2011-1991",
  migshockZ = "Population share foreign, growth rate 2011-1991 (standardised)",
  pcp_aged65ovZ = "Population share over 65 1991 (standardised)",
  pcp_foreignZ = "Population share foreign 1991 (standardised)",
  pc16_mnfZ = "Employment share in manufacturing 1991 (standardised)",
  criZ = "Change in relative income vs median 1997-2016 (standardised)",
  grossmigrants0216_all = "Gross migration (out of CZ) 2002-2016",
  grossmigrants0216_all_pc = "Gross migration (out of CZ) 2002-2016 (% of 2001 pop)",
  grossmigrants0216_allZ = "Gross migration (out of CZ) 2002-2016 (standardised)",
  grossmigrants0216_all_pcZ = "Gross migration (out of CZ) 2002-2016 (% of 2001 pop; standardised)",
  grossmigrants0216_5 = "Gross migration (5 social neighbours) 2002-2016",
  grossmigrants0216_5_pc = "Gross migration (5 social neighbours) 2002-2016 (% of 2001 pop)",
  grossmigrants0216_5Z = "Gross migration (5 social neighbours) 2002-2016 (standardised)",
  grossmigrants0216_5_pcZ = "Gross migration (5 social neighbours) 2002-2016 (% of 2001 pop; standardised)",
  avg_grossmigrants0216_all_pc = "Average gross migration (out of CZ) 2002-2016 (% of 2001 pop)",
  avg_grossmigrants0216_all_pcZ = "Average gross migration (out of CZ) 2002-2016 (% of 2001 pop; standardised)",
  avg_grossmigrants0216_5_pc = "Average gross migration (5 social neighbours) 2002-2016 (% of 2001 pop)",
  avg_grossmigrants0216_5_pcZ = "Average gross migration (5 social neighbours) 2002-2016 (% of 2001 pop; standardised)",
  shk_GBR = "Import shock: Within-region",
  shk_USA = "Import shock: Within-region USA instrument",
  shk_HI = "Import shock: Within-region HI instrument",
  shk_HIUSA = "Import shock: Within-region HI-USA instrument",
  shk_GBRZ = "Import shock: Within-region (standardised)",
  shk_USAZ = "Import shock: Within-region USA instrument (standardised)",
  shk_HIZ = "Import shock: Within-region HI instrument (standardised)",
  shk_HIUSAZ = "Import shock: Within-region HI-USA instrument (standardised)",
  rshk_GBR = "Res. import shock: Within-region",
  rshk_USA = "Res. import shock: Within-region USA instrument",
  rshk_HI = "Res. import shock: Within-region HI instrument",
  rshk_HIUSA = "Res. import shock: Within-region HI-USA instrument",
  rshk_GBRZ = "Res. import shock: Within-region (standardised)",
  rshk_USAZ = "Res. import shock: Within-region USA instrument (standardised)",
  rshk_HIZ = "Res. import shock: Within-region HI instrument (standardised)",
  rshk_HIUSAZ = "Res. import shock: Within-region HI-USA instrument (standardised)",
  lgrshk_GBR_wall_SCI_c = "Res. import shock: social lag (SCI-weighted)",
  lgrshk_GBR_w1_5_SCI_c = "Res. import shock: 1st to 5th social neighbour (SCI-weighted)",
  lgrshk_USA_wall_SCI_c = "Res. import shock USA instrument: social lag (SCI-weighted)",
  lgrshk_USA_w1_5_SCI_c = "Res. import shock USA instrument: 1st to 5th social neighbour (SCI-weighted)",
  lgrshk_HI_wall_SCI_c = "Res. import shock HI instrument: social lag (SCI-weighted)",
  lgrshk_HI_w1_5_SCI_c = "Res. import shock HI instrument: 1st to 5th social neighbour (SCI-weighted)",
  lgrshk_HIUSA_wall_SCI_c = "Res. import shock HI-USA instrument: social lag (SCI-weighted)",
  lgrshk_HIUSA_w1_5_SCI_c = "Res. import shock HI-USA instrument: 1st to 5th social neighbour (SCI-weighted)",
  lgrshk_GBR_wall_SCI_cZ = "Res. import shock: social lag (SCI-weighted; standardised)",
  lgrshk_GBR_w1_5_SCI_cZ = "Res. import shock: 1st to 5th social neighbour (SCI-weighted; standardised)",
  lgrshk_USA_wall_SCI_cZ = "Res. import shock USA instrument: social lag (SCI-weighted; standardised)",
  lgrshk_USA_w1_5_SCI_cZ = "Res. import shock USA instrument: 1st to 5th social neighbour (SCI-weighted; standardised)",
  lgrshk_HI_wall_SCI_cZ = "Res. import shock HI instrument: social lag (SCI-weighted; standardised)",
  lgrshk_HI_w1_5_SCI_cZ = "Res. import shock HI instrument: 1st to 5th social neighbour (SCI-weighted; standardised)",
  lgrshk_HIUSA_wall_SCI_cZ = "Res. import shock HI-USA instrument: social lag (SCI-weighted; standardised)",
  lgrshk_HIUSA_w1_5_SCI_cZ = "Res. import shock HI-USA instrument: 1st to 5th social neighbour (SCI-weighted; standardised)",
  lgrshk_GBR_wall_LCDi_c = "Res. import shock: spatial lag (LCDi-weighted)",
  lgrshk_GBR_w1_5_LCDi_c = "Res. import shock: 1st to 5th geographic neighbour (LCDi-weighted)",
  lgrshk_USA_wall_LCDi_c = "Res. import shock USA instrument: spatial lag (LCDi-weighted)",
  lgrshk_USA_w1_5_LCDi_c = "Res. import shock USA instrument: 1st to 5th geographic neighbour (LCDi-weighted)",
  lgrshk_HI_wall_LCDi_c = "Res. import shock HI instrument: spatial lag (LCDi-weighted)",
  lgrshk_HI_w1_5_LCDi_c = "Res. import shock HI instrument: 1st to 5th geographic neighbour (LCDi-weighted)",
  lgrshk_HIUSA_wall_LCDi_c = "Res. import shock HI-USA instrument: spatial lag (LCDi-weighted)",
  lgrshk_HIUSA_w1_5_LCDi_c = "Res. import shock HI-USA instrument: 1st to 5th geographic neighbour (LCDi-weighted)",
  lgrshk_GBR_wall_LCDi_cZ = "Res. import shock: spatial lag (LCDi-weighted; standardised)",
  lgrshk_GBR_w1_5_LCDi_cZ = "Res. import shock: 1st to 5th geographic neighbour (LCDi-weighted; standardised)",
  lgrshk_USA_wall_LCDi_cZ = "Res. import shock USA instrument: spatial lag (LCDi-weighted; standardised)",
  lgrshk_USA_w1_5_LCDi_cZ = "Res. import shock USA instrument: 1st to 5th geographic neighbour (LCDi-weighted; standardised)",
  lgrshk_HI_wall_LCDi_cZ = "Res. import shock HI instrument: spatial lag (LCDi-weighted; standardised)"
)

# ENDOG SCI ANALYSIS DATA  -----------------------------------------------------

endwdat <-
  nndat %>%
  select(ITL321XCD_O, ITL321XNM_O, ITL321XCD_D, ITL321XNM_D,  SCI_c, SCI, n_SCI_c, zones_ha_D, zones_la_D) %>%
  left_join(
    mutate(regdat[, c("ITL321XCD", "rshk_GBRZ",  "rshk_HIZ" )], ITL321XCD = as.character(ITL321XCD)),
    by = c("ITL321XCD_D" = "ITL321XCD")
  )  %>%
  left_join(mainmigr0220) %>%
  filter(!is.na(SCI_c)) %>%
  mutate(
    ITL321XCD_O_zones_ha_D = paste0(ITL321XCD_O, zones_ha_D),
    ITL321XCD_O_zones_la_D = paste0(ITL321XCD_O, zones_la_D),
    across(contains("ITL321X"), ~as.factor(.)),
    across(contains("SCI"), .fns = list(Z = ~(.-mean(., na.rm = T))/sd(., na.rm =T)))
  ) 

names(endwdat) <- gsub("_Z", "Z", names(endwdat))

endwdat$decile <- as.numeric(ntile(endwdat$pop2001_D, 10))


endwdat = expss::apply_labels(
  endwdat,
  ITL321XCD_O = "Origin harmonised ITL3 region code",
  ITL321XNM_O = "Origin harmonised ITL3 region name",
  ITL321XCD_D = "Destination harmonised ITL3 region code",
  ITL321XNM_D = "Destination harmonised ITL3 region name"
)

endsdat <-
  nndat %>%
  select(ITL321XCD_O, ITL321XNM_O, ITL321XCD_D, ITL321XNM_D, SCI_c, SCI, n_SCI_c, zones_ha_D, zones_la_D) %>%
  filter(!is.na(SCI_c)) %>%
  left_join(
    mutate(regdat[, c("ITL321XCD", "rshk_GBRZ", "rshk_USAZ", "rshk_HIZ", "rshk_HIUSAZ")], ITL321XCD = as.character(ITL321XCD)),
    by = c("ITL321XCD_D" = "ITL321XCD")
  ) %>%
  left_join(mainmigr0220) %>%
  mutate(
    ITL321XCD_O_zones_ha_D = paste0(ITL321XCD_O, zones_ha_D),
    ITL321XCD_O_zones_la_D = paste0(ITL321XCD_O, zones_la_D),
    ITL321XCD_O = as.factor(ITL321XCD_O),
    across(contains("ITL321X"), ~as.factor(.)),
    in_nn5_SCI_c = ifelse(n_SCI_c %in% 1:5, 1, 0),
    in_nn10_SCI_c = ifelse(n_SCI_c %in% 1:10, 1, 0),
    in_nn15_SCI_c = ifelse(n_SCI_c %in% 1:15, 1, 0),
    in_nn20_SCI_c = ifelse(n_SCI_c %in% 1:20, 1, 0),
    in_nn25_SCI_c = ifelse(n_SCI_c %in% 1:25, 1, 0),
    in_nn30_SCI_c = ifelse(n_SCI_c %in% 1:30, 1, 0),
    in_nn35_SCI_c = ifelse(n_SCI_c %in% 1:35, 1, 0),
    in_nn40_SCI_c = ifelse(n_SCI_c %in% 1:40, 1, 0),
    in_nn45_SCI_c = ifelse(n_SCI_c %in% 1:45, 1, 0),
    in_nn50_SCI_c = ifelse(n_SCI_c %in% 1:50, 1, 0)
  )

endsdat = expss::apply_labels(
  endsdat,
  ITL321XCD_O = "Origin harmonised ITL3 region code",
  ITL321XNM_O = "Origin harmonised ITL3 region name",
  ITL321XCD_D = "Destination harmonised ITL3 region code",
  ITL321XNM_D = "Destination harmonised ITL3 region name"
)

# SAVE OUTPUT & CLEAN UP ENVIRONMENT -------------------------------------------

haven::write_dta(data=regdat, path="./output/datasets/main/regdat.dta")
saveRDS(regdat, file = "./output/datasets/main/regdat.RDS")
haven::write_dta(data=endwdat, path="./output/datasets/main/endwdat.dta")
saveRDS(endwdat, file = "./output/datasets/main/endwdat.RDS")
haven::write_dta(data=endsdat, path="./output/datasets/main/endsdat.dta")
saveRDS(endsdat, file = "./output/datasets/main/endsdat.RDS")
haven::write_dta(data=fetzdatl, path="./output/datasets/main/fetzdatl.dta")
saveRDS(fetzdatl, file = "./output/datasets/main/fetzdatl.RDS")
rm(list = ls()) 
gc()