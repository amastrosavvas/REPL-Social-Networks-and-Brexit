# LOAD PACKAGES  ---------------------------------------------------------------

packages <-
  c(
    "dplyr",
    "tidyr",
    "reshape2"
  )

if(length(setdiff(packages, installed.packages())) > 0){
  stop("The following required packages are not installed:\n\n  ",
      paste(setdiff(packages, installed.packages()), collapse= "\n  "),
      "\n\nPlease install these and rerun the script."
    )
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
  select(-shk_GBR, -shk_USA)

# Get England and Wales version
shockdat[["ITL21X shocks unstacked adj EW"]] <-
  shockdat[["ITL21X shocks unstacked adj"]] %>% 
  filter(!grepl("^S", ITL321XCD)) 

# CALCULATE IMPORT SHOCKS IN NEIGHBOUR BINS ------------------------------------

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
    colnames(nndat)[grepl("rshk", colnames(nndat))], 
    function(x){
      
      nndat %>%
        group_by(ITL321XCD_O) %>%
        summarise(
          across(
            c(
              contains("w1_5_SCI_c"), contains("w6_10_SCI_c"), contains("weq1_5_SCI_c"), contains("weq6_10_SCI_c"),
              contains("w1_5_LCDi_c"), contains("w6_10_LCDi_c")
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

# SAVE OUTPUT & CLEAN UP ENVIRONMENT -------------------------------------------

saveRDS(shockdat, file="./output/datasets/shockdat2.RDS")

rm(list = ls())
gc()



