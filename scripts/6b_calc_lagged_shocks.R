# LOAD PACKAGES  ---------------------------------------------------------------

packages <-
  c(
    "dplyr",
    "tidyr",
    "reshape2"
  )

for (p in packages){ 
  if (! (p %in% installed.packages())){
    install.packages(p)
  }
}

library(dplyr)   

# CALCULATE RESIDENTIAL IMPORT SHOCKS ------------------------------------------

shockdat <- readRDS(file = "./output/datasets/shockdat.RDS")
commdat <- readRDS(file="./output/datasets/commdat2.RDS")
nndat <- readRDS(file="./output/datasets/nndat.RDS")

# Get 1991 ITL21X GBR commuting flow matrix and row-normalise
comm91_mx <- 
  reshape2::acast(
    commdat[["dir 1991 by ITL21X GBR"]],
    ITL321XCD_O ~ ITL321XCD_D,
    value.var = "commuters"
  )

comm91_mx <-
  t(apply(comm91_mx , 1, function(x)(x/sum(x))))

# Order shock data rows to match commuting matrix; get res. import shocks
shockdat[["ITL21X shocks unstacked adj"]] <-
  shockdat[["ITL21X shocks unstacked"]][match(rownames(comm91_mx), shockdat[["ITL21X shocks unstacked"]]$ITL321XCD),]  

shockdat[["ITL21X shocks unstacked adj"]] <-
  shockdat[["ITL21X shocks unstacked adj"]] %>%
  mutate(across(shk_GBR:shk_USA, ~as.vector(.%*%t(comm91_mx)), .names = "r{.col}")) %>%
  select(-shk_HIUSA, -shk_USA)

# Get England and Wales version
shockdat[["ITL21X shocks unstacked adj EW"]] <-
  shockdat[["ITL21X shocks unstacked adj"]] %>% 
  filter(!grepl("^S", ITL321XCD)) 

# CALCULATE LAGGED IMPORT SHOCKS -----------------------------------------------

# Append shock data to neighbour data (to destinations)
nndat <-
  nndat %>%
  mutate(
    ITL321XCD_O = as.character(ITL321XCD_O),
    ITL321XCD_D = as.character(ITL321XCD_D)
  ) %>%
  left_join(shockdat[["ITL21X shocks unstacked adj EW"]], by = c("ITL321XCD_D" = "ITL321XCD")) %>%
  select(-ITL321XNM) 


# Calculate res. import shock lags

lagshocks <-
  lapply(
    colnames(nndat)[grepl("rshk|shk_HI$|shk_GBR$", colnames(nndat))], 
    function(x){
      
      nndat %>%
        group_by(ITL321XCD_O) %>%
        summarise(
          across(
            c(
              contains("wall_SCI_c"), contains("w1_5_SCI_c"),  
              contains("wall_LCDi_c"), contains("w1_5_LCDi_c"), 
              contains("weq1_5_SCI_c")
            ),
            ~sum(.*!!sym(x), na.rm = T), 
            .names = "lg{x}_{.col}"
          )
        ) %>%
        ungroup()
      
    }
  ) 


lagshocks <-
  Reduce(function(x, y) left_join(x, y, by = "ITL321XCD_O"), lagshocks) 
 
shockdat[["ITL21X shocks unstacked adj EW + lags"]] <-
  shockdat[["ITL21X shocks unstacked adj EW"]] %>%
  left_join(lagshocks, by = c("ITL321XCD" = "ITL321XCD_O")) 

# CALCULATE LAGGED MIGRATION SHOCKS --------------------------------------------

# Get within-region data
cens91dat <- readRDS(file = "./output/datasets/cens91dat.RDS")[["by ITL21X EW"]]
cens11dat <- readRDS(file = "./output/datasets/cens11dat.RDS")[["by ITL21X EW"]]

migshockdat <-
  cens91dat %>%
  left_join(cens11dat, by = c("ITL321XCD", "ITL321XNM")) %>%
  mutate(d_pcp_foreign91_11_pc = 100*(pcp_foreign11 - pcp_foreign)/pcp_foreign) %>%
  select(ITL321XCD, ITL321XNM, d_pcp_foreign91_11_pc)

nndat <-
  nndat %>%
  left_join(migshockdat, by = c("ITL321XCD_D" = "ITL321XCD")) %>%
  select(-ITL321XNM) 

lagmigshocks <-
  lapply(
    colnames(nndat)[grepl("foreign91", colnames(nndat))], 
    function(x){
      
      nndat %>%
        group_by(ITL321XCD_O) %>%
        summarise(
          across(
            c(
              contains("wall_SCI_c"), contains("w1_5_SCI_c")
            ),
            ~sum(.*!!sym(x), na.rm = T), 
            .names = "lg{x}_{.col}"
          )
        ) %>%
        ungroup()
      
    }
  ) 

lagmigshocks <-
  Reduce(function(x, y) left_join(x, y, by = "ITL321XCD_O"), lagmigshocks) 

migshockdat <- 
  migshockdat %>%
  left_join(lagmigshocks, by = c("ITL321XCD" = "ITL321XCD_O")) %>%
  rename(
    migshock = d_pcp_foreign91_11_pc,
    lgd_migshock_w1_5_SCI_c = lgd_pcp_foreign91_11_pc_w1_5_SCI_c,
    lgd_migshock_wall_SCI_c = lgd_pcp_foreign91_11_pc_wall_SCI_c
  )

# CALCULATE LAGGED AUSTERITY SHOCKS --------------------------------------------

fetzdatw <- readRDS(file = "./output/datasets/fetzdatw.RDS")

austshockdat <-
  fetzdatw %>%
  select(ITL321XCD, ITL321XNM, totalimpact_finlosswapyr)

nndat <-
  nndat %>%
  left_join(austshockdat, by = c("ITL321XCD_D" = "ITL321XCD")) %>%
  select(-ITL321XNM) 

lagaustshocks  <-
  lapply(
    colnames(nndat)[grepl("finlosswapyr", colnames(nndat))], 
    function(x){
      
      nndat %>%
        group_by(ITL321XCD_O) %>%
        summarise(
          across(
            c(
              contains("wall_SCI_c"), contains("w1_5_SCI_c")
            ),
            ~sum(.*!!sym(x), na.rm = T), 
            .names = "lg{x}_{.col}"
          )
        ) %>%
        ungroup()
      
    }
  ) 


lagaustshocks <-
  Reduce(function(x, y) left_join(x, y, by = "ITL321XCD_O"), lagaustshocks) 

lagaustshockdat <- 
  austshockdat %>%
  left_join(lagaustshocks, by = c("ITL321XCD" = "ITL321XCD_O")) %>%
  rename(
    lg_austshock_w1_5_SCI_c = lgtotalimpact_finlosswapyr_w1_5_SCI_c,
    lg_austshock_wall_SCI_c = lgtotalimpact_finlosswapyr_wall_SCI_c
  ) %>%
  select(ITL321XCD, ITL321XNM, lg_austshock_w1_5_SCI_c, lg_austshock_wall_SCI_c)
# SAVE OUTPUT & CLEAN UP ENVIRONMENT -------------------------------------------

saveRDS(shockdat, file="./output/datasets/shockdat2.RDS")
saveRDS(migshockdat, file="./output/datasets/migshockdat.RDS")
saveRDS(lagaustshockdat, file="./output/datasets/lagaustshockdat.RDS")

rm(list = ls())
gc()



