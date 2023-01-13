# LOAD PACKAGES  ---------------------------------------------------------------

packages <-
  c(
    "dplyr",
    "reshape2",
    "forcats"
  )

for (p in packages){ 
  if (! (p %in% installed.packages())){
    install.packages(p)
  }
}

library(dplyr)                    

# DERIVE COMMUTING ZONES -------------------------------------------------------

commdat <- readRDS(file = "./output/datasets/commdat.RDS")

commdat[["resworkers annual by ITL21X EW"]] <- 
  commdat[["undir annual by ITL21X EW"]] %>%
  filter(ITL321XCD_O == ITL321XCD_D) %>%
  select(Year, ITL321XCD_O, ITL321XNM_O, commuters) %>%
  rename(
    "ITL321XCD" = "ITL321XCD_O",
    "ITL321XNM" = "ITL321XNM_O",
    "resworkers" = "commuters"
  )

commdat[["minresworkers annual by ITL21X EW"]] <-
  commdat[["undir annual by ITL21X EW"]] %>%
  left_join(
    commdat[["resworkers annual by ITL21X EW"]],
    by = c("Year" = "Year", "ITL321XCD_O" = "ITL321XCD", "ITL321XNM_O" = "ITL321XNM")
  ) %>%
  left_join(
    commdat[["resworkers annual by ITL21X EW"]],
    by = c("Year" = "Year", "ITL321XCD_D" = "ITL321XCD", "ITL321XNM_D" = "ITL321XNM")
  ) %>%
  rowwise() %>%
  mutate(minresworkers = min(resworkers.x, resworkers.y)) %>%
  select(-resworkers.x, -resworkers.y)

commdat[["T-S annual by ITL21X EW"]] <-
  commdat[["minresworkers annual by ITL21X EW"]] %>%
  mutate(
    similarity = commuters/minresworkers,
    similarity_adj = case_when(
      similarity > 1 ~ 1,
      TRUE ~ similarity
    ),
    dissimilarity = 1 - similarity_adj
  )

commdat[["T-S average by ITL21X EW"]] <-
  commdat[["T-S annual by ITL21X EW"]] %>%
  group_by(ITL321XCD_O, ITL321XNM_O, ITL321XCD_D, ITL321XNM_D) %>%
  summarise(
    dissimilarity_avg = mean(dissimilarity)
  ) %>%
  ungroup()

diss_mx <- reshape2::acast(commdat[["T-S average by ITL21X EW"]], ITL321XCD_O ~ ITL321XCD_D, value.var = "dissimilarity_avg")
hclust_avg <- hclust(as.dist(diss_mx), method = "average")
cut_avg_98 <- cutree(hclust_avg, h = 0.98)
cut_avg_95 <- cutree(hclust_avg, h = 0.95)

commdat[["T-S clusters by ITL21X EW"]] <-
  data.frame(
    ITL321XCD = names(cut_avg_98),
    zones_h = as.vector(cut_avg_98),
    zones_l = as.vector(cut_avg_95)
    )

# Zone adjusting, naming and ordering
euref16dat <- readRDS(file = "./output/datasets/euref16dat.RDS")

commdat[["T-S adj clusters by ITL21X EW"]] <- 
  commdat[["T-S clusters by ITL21X EW"]] 

commdat[["T-S clusters by ITL21X EW"]] <-
  commdat[["T-S clusters by ITL21X EW"]] %>%
  left_join(euref16dat[["by ITL21X EW"]][, c("ITL321XCD", "Electorate")], by = "ITL321XCD") %>%
  group_by(zones_h) %>%
  mutate(Electorate = sum(Electorate)) %>%
  ungroup() %>%
  mutate(
    Electorate = forcats::fct_rev(as.factor(Electorate)),
    zones_h = paste0("Z", as.numeric(Electorate))
  ) %>%
  select(-Electorate) %>%
  left_join(euref16dat[["by ITL21X EW"]][, c("ITL321XCD", "Electorate")], by = "ITL321XCD") %>%
  group_by(zones_l) %>%
  mutate(Electorate = sum(Electorate)) %>%
  ungroup() %>%
  group_by(zones_h) %>%
  arrange(desc(Electorate)) %>%
  mutate(
    zones_l = paste0(zones_h, "-", as.numeric(forcats::fct_rev(as.factor(Electorate))))
  ) %>%
  ungroup() %>%
  select(-Electorate)

commdat[["T-S adj clusters by ITL21X EW"]] <-
  commdat[["T-S adj clusters by ITL21X EW"]] %>%
  left_join(euref16dat[["by ITL21X EW"]][, c("ITL321XCD", "Electorate")], by = "ITL321XCD") %>%
  mutate(
    zones_ha = ifelse(ITL321XCD == "TLJ34", 14, zones_h) # Isle of Wight adjustment
  ) %>%
  group_by(zones_ha) %>%
  mutate(Electorate = sum(Electorate)) %>%
  ungroup() %>%
  mutate(
    Electorate = forcats::fct_rev(as.factor(Electorate)),
    zones_ha = paste0("Z", as.numeric(Electorate))
  ) %>%
  select(-Electorate) %>%
  left_join(euref16dat[["by ITL21X EW"]][, c("ITL321XCD", "Electorate")], by = "ITL321XCD") %>%
  group_by(zones_l) %>%
  mutate(Electorate = sum(Electorate)) %>%
  ungroup() %>%
  group_by(zones_ha) %>%
  arrange(desc(Electorate)) %>%
  mutate(
    zones_la = paste0(zones_ha, "-", as.numeric(forcats::fct_rev(as.factor(Electorate))))
  ) %>%
  ungroup() %>%
  select(-zones_h, -zones_l, -Electorate)

  
# SAVE OUTPUT & CLEAN UP ENVIRONMENT -------------------------------------------

saveRDS(commdat, file="./output/datasets/commdat2.RDS")

rm(list = ls()) 
gc()