# LOAD PACKAGES  ---------------------------------------------------------------

packages <-
  c(
    "dplyr",
    "tidyr",
    "haven",
    "expss",
    "rstatix",
    "reshape2"
  )

for (p in packages){ 
  if (! (p %in% installed.packages())){
    install.packages(p)
  }
}

library(dplyr) 

# COVARIATE TABLE  -------------------------------------------------------------

regdat <- 
  readRDS(file = "./output/datasets/main/regdat.RDS") %>%
  mutate(across(everything(), ~expss::unlab(.)))

covtab <-
  regdat %>%
  arrange(lgrshk_GBR_w1_5_SCI_c) %>%
  mutate(Group = ifelse(row_number() >= n() - 14, "Most indirectly exposed", "Other")) %>%
  arrange(rshk_GBR) %>%
  mutate(Group = ifelse(row_number() >= n() - 14, "Most directly exposed", Group)) %>%
  filter(Group != "Other") %>%
  select(
    Group, rshk_GBR, lgrshk_GBR_w1_5_SCI_c, lgrshk_GBR_wall_SCI_c,  pc16_mnf, cri, pcp_aged65ov, pcp_foreign, km_to_london, actintusers_2016or17_pc
  )

covtab_melted <- reshape2::melt(covtab,id.vars = "Group")

pwc1 <- covtab_melted %>% group_by(variable) %>% 
  rstatix::pairwise_t_test(value ~ Group, p.adjust.method = "fdr")

covtab <-
  covtab %>% 
  group_by(Group) %>%
  summarise_all(mean) %>%
  ungroup()

covtab_p <- reshape2::melt(covtab,id.vars = "Group") %>%
  tidyr::pivot_wider(id_cols = variable, names_from = Group) %>%
  left_join(select(pwc1, variable, p.adj, p.adj.signif), by = "variable") %>%
  mutate(
    variable = case_when(
      variable == "rshk_GBR" ~ "Import-shock exposure: Within-region",
      variable == "lgrshk_GBR_w1_5_SCI_c" ~ "lgrshk_GBR_wall_SCI_c: 1st to 5th neighbour",
      variable == "lgrshk_GBR_w6_10_SCI_c" ~ "lgrshk_GBR_wall_SCI_c: 6th to 10th neighbour",
      variable == "km_to_london" ~ "Kilometres to London",
      variable == "pcp_foreign" ~ "Pop. share foreign-born, 1991",
      variable == "migshock" ~ "Foreign-born share growth rate, 1991-2011: Within-region",
      variable == "lgd_migshock_w1_10_SCI_c" ~ "Foreign-born share growth rate, 1991-2011: 1st to 10th neighbour", 
      variable == "pc16_mnf" ~ "Manufacturing emp. share, 1991", 
      variable == "totalimpact_finlosswapyr" ~ "Exposure to austerity, 2010: Within-region", 
      variable == "lg_austshock_1_10_SCI_c" ~ "Exposure to austerity, 2010: 1st to 10th neighbour", 
      variable == "cri" ~ "Change in relative incom, 1997-2015", 
      variable == "grossmigrants0216_pc" ~ "Gross migration rate (out of CZ), 2002-2016", 
      variable == "grossmigrants0216_10_pc" ~ "Gross migration rate (10 social neighbours), 2002-2016", 
      variable == "actintusers_2016or17_pc" ~ "Pop. share active internet users, 2016/17", 
      variable == "pcp_aged65ov" ~ "Pop. share over 65, 1991",
      TRUE ~ variable
    ) 
  ) %>% 
  select(-p.adj.signif) %>%
  rename(
    Variable = variable,
    "Adj. p-value" = p.adj
  ) %>%
  mutate(
    across(c("Most directly exposed", "Most indirectly exposed"), ~round(., 2))
  )

# SAVE OUTPUT & CLEAN UP ENVIRONMENT -------------------------------------------

write.csv(covtab_p, file = "./output/tables/covtab.csv", row.names = FALSE)

rm(list = ls()) 
gc()