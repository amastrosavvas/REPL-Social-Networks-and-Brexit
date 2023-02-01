# LOAD PACKAGES  ---------------------------------------------------------------

packages <-
  c(
    "dplyr",
    "haven",
    "expss"
  )

if(length(setdiff(packages, installed.packages())) > 0){
  stop("The following required packages are not installed:\n\n  ",
      paste(setdiff(packages, installed.packages()), collapse= "\n  "),
      "\n\nPlease install these and rerun the script."
    )
}

library(dplyr) 

# REGDAT PREP  -----------------------------------------------------------------

commdat <- readRDS(file = "./output/datasets/commdat2.RDS")[["T-S adj clusters by ITL21X EW"]]
euref16dat <- readRDS(file = "./output/datasets/euref16dat.RDS")[["by ITL21X EW"]] %>% 
  select(ITL321XCD, Votes_Cast, Electorate, pc_leave)
cens91dat <- readRDS(file = "./output/datasets/cens91dat.RDS")[["by ITL21X EW"]]
gvadat <- readRDS(file = "./output/datasets/gvadat.RDS")[["CRI by ITL21X EW"]]
popdat <- readRDS(file = "./output/datasets/popdat2.RDS")[["1991-2011 O-D by ITL21X EW"]] %>%
  filter(Year == 2001) %>%
  select(ITL321XCD_O, ITL321XNM_O, pop_O) %>%
  rename(ITL321XCD=1, ITL321XNM=2, pop2001=3) %>%
  distinct()

shockdat <- readRDS(file = "./output/datasets/shockdat2.RDS")[["ITL21X shocks unstacked"]]
shockdat_adj <- readRDS(file = "./output/datasets/shockdat2.RDS")[["ITL21X shocks unstacked adj EW + lags"]]

regdat <- 
  commdat %>%
  left_join(euref16dat) %>%
  left_join(cens91dat) %>%
  left_join(gvadat) %>%
  left_join(popdat) %>%
  left_join(shockdat) %>%
  left_join(shockdat_adj)

rm(commdat, euref16dat, cens91dat, gvadat, shockdat, shockdat_adj)


# MIGR DAT PREP  ---------------------------------------------------------------

nndat <- readRDS(file = "./output/datasets/nndat.RDS") %>%
  mutate(
    ITL321XCD_O = as.character(ITL321XCD_O),
    across(contains("ITL321X"), ~as.character(.))
  )

migrdat <- readRDS(file = "./output/datasets/migrdat.RDS")

# Get out-migration data (directed origin to destination) and nn relations

mainmigr <-
  nndat %>%
  left_join(
    migrdat[["dir 2002-2016 by ITL21X EW"]]
  ) %>%
  rename("outmigrants0216" = "migrants") %>%
  select(
    ITL321XCD_O, ITL321XNM_O, ITL321XCD_D, ITL321XNM_D,
    outmigrants0216, n_SCI_c
  )

rm(migrdat)


# Get in migration data (join outmigrants on destination)
mainmigr <-
  mainmigr %>% 
  rename("n_SCI_c_O" = "n_SCI_c") %>%
  left_join(
    ., 
    .[, c("ITL321XCD_O", "ITL321XNM_O", "ITL321XCD_D", "ITL321XNM_D",
          "outmigrants0216")], 
    by = c(
      "ITL321XCD_O" = "ITL321XCD_D", 
      "ITL321XCD_D" = "ITL321XCD_O",
      "ITL321XNM_O" = "ITL321XNM_D", 
      "ITL321XNM_D" = "ITL321XNM_O"
      
    )
  ) %>% 
  rename(
    "outmigrants0216" = "outmigrants0216.x",
    "inmigrants0216" = "outmigrants0216.y"
  ) %>%
  mutate(
    netinmigrants0216 = inmigrants0216 - outmigrants0216
  )



# Restrict to 5 nn, aggregate flows
mainmigr <-
  mainmigr %>%
  filter(n_SCI_c_O %in% 1:5) %>%
  select(-n_SCI_c_O) %>%
  group_by(ITL321XCD_O, ITL321XNM_O) %>%
  summarise(
    inmigrants0216 = sum(inmigrants0216),
    outmigrants0216 = sum(outmigrants0216),
    netinmigrants0216 = sum(netinmigrants0216)
  ) 

# Join with other regional data
regdat <-
  regdat %>%
  left_join(mainmigr, by = c("ITL321XCD" = "ITL321XCD_O", "ITL321XNM" = "ITL321XNM_O"))


# FINALISE REGDAT  -------------------------------------------------------------

# Standardise / rescale variables 
regdat <- 
  regdat %>%
  mutate(
    pc_leave = 100*pc_leave,
    across(contains("migrants"), .fns = list(pc = ~100*./pop2001))
  ) %>% 
  mutate(
    across(c("ITL321XCD", "ITL321XNM", "zones_ha", "zones_la"), ~as.factor(.)),
    across(-c("ITL321XCD", "ITL321XNM", "zones_ha", "zones_la"), ~as.numeric(.)),
    across(-c("ITL321XCD", "ITL321XNM", "zones_ha", "zones_la", "Votes_Cast", "Electorate", "pop2001", "pc_leave"), .fns = list(Z = ~(.-mean(.))/sd(.)))
  ) 

names(regdat) <- gsub("_Z", "Z", names(regdat))

# Labels
regdat = expss::apply_labels(
  regdat,
  ITL321XCD = "Harmonised ITL3 region code",
  ITL321XNM = "Harmonised ITL3 region name",
  zones_ha = "Commuting zone",
  zones_la = "Lower commuting zone",
  Votes_Cast = "Votes cast in EURef 2016",
  Electorate = "Electorate in EURef 2016",
  pc_leave = "Leave vote share",
  pop2001 = "2001 population",
  ukip09ep_pc = "UKIP vote share EP 2009",
  pcp_aged65ov = "Population share over 65 1991",
  pcp_foreign = "Population share foreign 1991",
  pc16_mnf = "Employment share in mnf 1991",
  cri = "Change in rel income vs median 1997-2016",
  pcp_aged65ovZ = "Population share over 65 1991 (standardised)",
  pcp_foreignZ = "Population share foreign 1991 (standardised)",
  pc16_mnfZ = "Employment share in mnf 1991 (standardised)",
  criZ = "Change in rel income vs median 1997-2016 (standardised)",
  pcp_foreignZ = "Population share foreign 1991 (standardised)",
  inmigrants0216 = "In-migration from 5 nrst. social neighbs 2002-2016", 
  outmigrants0216 = "Out-migration from 5 nrst. social neighbs 2002-2016",
  netinmigrants0216 = "Net in-migration from 5 nrst. social neighbs 2002-2016",
  inmigrants0216_pc = "In-migration from 5 nrst. social neighbs 2002-2016 (% of 2001 pop)", 
  outmigrants0216_pc = "Out-migration from 5 nrst. social neighbs 2002-2016 (% of 2001 pop)",
  netinmigrants0216_pc = "Net in-migration from 5 nrst. social neighbs 2002-2016 (% of 2001 pop)",
  inmigrants0216Z = "In-migration from 5 nrst. social neighbs 2002-2016 (standardised)", 
  outmigrants0216Z = "Out-migration from 5 nrst. social neighbs 2002-2016 (standardised)",
  netinmigrants0216Z = "Net in-migration from 5 nrst. social neighbs 2002-2016(standardised)",
  inmigrants0216_pcZ = "In-migration from 5 nrst. social neighbs 2002-2016 (% of 2001 pop; standardised)", 
  outmigrants0216_pcZ = "Out-migration from 5 nrst. social neighbs 2002-2016 (% of 2001 pop; standardised)",
  netinmigrants0216_pcZ = "Net in-migration from 5 nrst. social neighbs 2002-2016 (% of 2001 pop; standardised)",
  shk_GBR = "Import shock: Within-region",
  shk_USA = "Import shock: Within-region instrument",
  rshk_GBR = "Res. import shock: Within-region",
  rshk_USA = "Res. import shock: Within-region instrument",
  shk_GBRZ = "Import shock: Within-region (standardised)",
  shk_USAZ = "Import shock: Within-region instrument (standardised)",
  lgrshk_GBR_w1_5_SCI_c = "Res. import shock: 1st to 5th social neighbour (SCI-weighted)",
  lgrshk_GBR_w6_10_SCI_c = "Res. import shock: 6th to 10th social neighbour (SCI-weighted)", 
  lgrshk_USA_w1_5_SCI_c = "Res. import shock instrument: 1st to 5th social neighbour (SCI-weighted)",
  lgrshk_USA_w6_10_SCI_c = "Res. import shock instrument: 6th to 10th social neighbour (equally weighted)",
  lgrshk_GBR_weq1_5_SCI_c = "Res. import shock: 1st to 5th social neighbour (equally weighted)",
  lgrshk_GBR_weq6_10_SCI_c = "Res. import shock: 6th to 10th social neighbour (equally weighted)", 
  lgrshk_USA_weq1_5_SCI_c = "Res. import shock instrument: 1st to 5th social neighbour (equally weighted)",
  lgrshk_USA_weq6_10_SCI_c = "Res. import shock instrument: 6th to 10th social neighbour (equally weighted)",
  lgrshk_GBR_w1_5_LCDi_c = "Res. import shock: 1st to 5th geographic neighbour (LCDi-weighted)",
  lgrshk_GBR_w6_10_LCDi_c = "Res. import shock: 6tt to 10th geographic neighbour (LCDi-weighted)",
  lgrshk_USA_w1_5_LCDi_c = "Res. import shock instrument: 1st to 5th geographic neighbour (LCDi-weighted)",
  lgrshk_USA_w6_10_LCDi_c = "Res. import shock instrument: 6th to 10th geographic neighbour (LCDi-weighted)",
  shk_GBR = "Import shock: Within-region (standardised)",
  shk_USA = "Import shock: Within-region instrument (standardised)",
  rshk_GBRZ = "Res. import shock: Within-region (standardised)",
  rshk_USAZ = "Res. import shock: Within-region instrument (standardised)",
  lgrshk_GBR_w1_5_SCI_cZ = "Res. import shock: 1st to 5th social neighbour (SCI-weighted; standardised)",
  lgrshk_GBR_w6_10_SCI_cZ = "Res. import shock: 6th to 10th social neighbour (SCI-weighted; standardised)", 
  lgrshk_USA_w1_5_SCI_cZ = "Res. import shock instrument: 1st to 5th social neighbour (SCI-weighted; standardised)",
  lgrshk_USA_w6_10_SCI_cZ = "Res. import shock instrument: 6th to 10th social neighbour (equally weighted; standardised)",
  lgrshk_GBR_weq1_5_SCI_cZ = "Res. import shock: 1st to 5th social neighbour (equally weighted; standardised)",
  lgrshk_GBR_weq6_10_SCI_cZ = "Res. import shock: 6th to 10th social neighbour (equally weighted; standardised)", 
  lgrshk_USA_weq1_5_SCI_cZ = "Res. import shock instrument: 1st to 5th social neighbour (equally weighted; standardised)",
  lgrshk_USA_weq6_10_SCI_cZ = "Res. import shock instrument: 6th to 10th social neighbour (equally weighted; standardised)",
  lgrshk_GBR_w1_5_LCDi_cZ = "Res. import shock: 1st to 5th geographic neighbour (LCDi-weighted; standardised)",
  lgrshk_USA_w1_5_LCDi_cZ = "Res. import shock instrument: 1st to 5th geographic neighbour (LCDi-weighted; standardised)",
  lgrshk_GBR_w6_10_LCDi_cZ = "Res. import shock: 6th to 10th geographic neighbour (LCDi-weighted; standardised)",
  lgrshk_USA_w6_10_LCDi_cZ = "Res. import shock instrument: 6th to 10th geographic neighbour (LCDi-weighted; standardised)"
)

# ENDOG SCI ANALYSIS DATA  -----------------------------------------------------

endwdat <-
  nndat %>%
  select(ITL321XCD_O, ITL321XNM_O, ITL321XCD_D, ITL321XNM_D, SCI_c, n_SCI_c, zones_la_D) %>%
  filter(n_SCI_c %in% 1:10) %>%
  left_join(
    mutate(regdat[, c("ITL321XCD", "rshk_GBRZ", "rshk_USAZ")], ITL321XCD = as.character(ITL321XCD)),
    by = c("ITL321XCD_D" = "ITL321XCD")
  )  %>%
  mutate(
    log_SCI_c = log(SCI_c),
    ITL321XCD_O = as.factor(ITL321XCD_O),
   across(contains("ITL321X"), ~as.factor(.))
  )

endwdat = expss::apply_labels(
  endwdat,
  ITL321XCD_O = "Origin harmonised ITL3 region code",
  ITL321XNM_O = "Origin harmonised ITL3 region name",
  ITL321XCD_D = "Destination harmonised ITL3 region code",
  ITL321XNM_D = "Destination harmonised ITL3 region name",
  log_SCI_c = "log. Social Connectedness Index (commuting discounted)"
)

endsdat <-
  nndat %>%
  select(ITL321XCD_O, ITL321XNM_O, ITL321XCD_D, ITL321XNM_D, SCI_c, SCI, n_SCI_c, zones_ha_D, zones_la_D) %>%
 # filter(!is.na(SCI_c)) %>%
  left_join(
    mutate(regdat[, c("ITL321XCD", "rshk_GBRZ", "rshk_USAZ")], ITL321XCD = as.character(ITL321XCD)),
    by = c("ITL321XCD_D" = "ITL321XCD")
  ) %>%
  mutate(
    ITL321XCD_O = as.factor(ITL321XCD_O),
    across(contains("ITL321X"), ~as.factor(.)),
    in_nn10_SCI_c = ifelse(n_SCI_c %in% 1:10, 1, 0)
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
rm(list = ls()) 
gc()
