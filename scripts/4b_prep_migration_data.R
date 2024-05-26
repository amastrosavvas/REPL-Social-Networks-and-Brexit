# LOAD PACKAGES  ---------------------------------------------------------------

packages <-
  c(
    "dplyr",
    "readr",
    "readxl",
    "stringr",
    "tidyr"
  )

for (p in packages){ 
  if (! (p %in% installed.packages())){
    install.packages(p)
  }
}

library(dplyr)                    

# PREPARE MIGRATION DATA -------------------------------------------------------

geolkp <- readRDS(file = "./output/lookups/geolkp.RDS")

# 2002-2016 migration data zip files
migr0216_zipfiles <-
  list.files(
    path = paste0(getwd(), "/rawdata/migration"),
    pattern = ".zip",
    full.names = TRUE
  )

# 2017-2020 migration data xls/xlsx files
migr1720_files <-
  list.files(
    path = paste0(getwd(), "/rawdata/migration"),
    pattern = ".xls|xlsx",
    full.names = TRUE
  )

# Read 2002-2016 LAD-LAD commuting matrices only from each zipfile
migrdat <- lapply(
  migr0216_zipfiles, 
  function(x){
    
    # Identify matrix file names
    datfiles <- unzip(x, list = TRUE)
    lamxfile <- 
      datfiles[[1]][grepl("^Table 2a|^lasquarematrix|^LA square matrix", datfiles[[1]])]
    
    # Read CSV matrices
    if (grepl(".csv", lamxfile)){
      out <- readr::read_csv(unz(x, lamxfile), col_names = F)
    }
    
    # Read Excel matrices
    if (grepl(".xls|.xlsx", lamxfile)){ 
      
      # Unzip file (readxl cannot read unz connections)
      y <- unzip(zipfile = x, files = lamxfile, exdir=tempfile())
      
      # Identify and read matrices from each file (some in separated in two parts/sheets) as list
      sheets <- which(stringr::str_detect(readxl::excel_sheets(y), paste0(paste0("^", 2002:2020, collapse = "|"), "|^Part")))
      out <- lapply(sheets, function(x){readxl::read_excel(y, sheet = x, col_names = F)})
      
    }
    return(out)
  }
)

# Read 2017-2020 LAD-LAD commuting matrices and append to 2002-2016 data
migrdat <- append(
  migrdat,
  lapply(
    migr1720_files, 
    function(x){
      readxl::read_excel(
        x, 
        sheet = which(stringr::str_detect(readxl::excel_sheets(x), "2017-T7|-T7b")),
        col_names = F
      )
    }
  )
)

# Unlist matrices where these come in multiple parts/sheets
migrdat <- append(
  migrdat[sapply(migrdat, is.data.frame)],
  unlist(migrdat[!sapply(migrdat, is.data.frame)], recursive = FALSE)
)

# Name matrices by corresponding year by identifying years in titles
migrnm_pattn <- 
  paste0(
    "^", 
    c(
      "Registered during the year ending",
      "Moves within the UK during the year ending",
      "Internal migration between"
    ),
    collapse = "|"
  )

migrnm <- 
  sapply(
    migrdat, 
    function(x){
      x[sapply(x, grepl, pattern = migrnm_pattn)]
    }
  )

names(migrdat) <- gsub("\\D", "", migrnm)

# Tidy 2002-2020 matrices
migrdat <- lapply(
  migrdat,
  function(x){
    
    # Drop rows/cols listing new codes (for matrices displaying both old-new)
    row.is.new <- apply(x, 1, function(z){any(z == "New code" & !is.na(z))})
    col.is.new <- apply(x, 2, function(z){any(z == "New code" & !is.na(z))})
    
    x <- x[!row.is.new, !col.is.new]
    
    # Set aside rows where LAD code, name, and first value exist
    x1 <- as.data.frame(x[!is.na(x[,1]) & !is.na(x[,2]) & !is.na(x[,3]),])
    x1 <- x1[,apply(x1, 2, function(z){any(!is.na(z))})] # remove cols with NAs
    ladcdnm <- c(x1[[1]], x1[[2]]) # set aside patterns of LAD codes and names
    rownames(x1) <- paste0(x1[[1]], ":", x1[[2]]) # concat code-name cols and set to rowname
    x1 <- x1[,3:ncol(x1)] # remove code-name cols
    
    # Extract column names from original data
    x2 <- x[,colnames(x) %in% colnames(x1)] 
    x2 <- x2[apply(x2, 1, function(z) any(z %in% ladcdnm)),]
    colnames(x1) <- paste0(x2[1,], ":", x2[2,])
    
    # Convert matrices to pairwise column tables
    x1 <- as.data.frame(as.table(as.matrix(x1)))
    
    # Separte origin and destination names-cols; remove Scot/NI/Totals; format cells
    x1 <-
      x1 %>%
      tidyr::separate(Var1, c("LADNM_D", "LADCD_D"), sep = ":") %>%
      tidyr::separate(Var2, c("LADNM_O", "LADCD_O"), sep = ":") %>%
      rename(migrants = Freq) %>%
      filter(
        !grepl("Scotland|Northern Ireland|^Total|^Destination", LADNM_O, ignore.case = T),
        !grepl("Scotland|Northern Ireland|^Total|^Destination", LADNM_D, ignore.case = T),
        !grepl("^Total", LADCD_O, ignore.case = T),
        !grepl("^Total", LADCD_D, ignore.case = T)
      ) %>% mutate(
        migrants = gsub(",", "", migrants),
        migrants = gsub("-", 0, migrants),
        migrants = as.numeric(migrants)
      )
    return(x1)
    
  }
)

# Append year column to each migration table
for (i in 1:length(migrdat)){migrdat[[i]]$Year <- names(migrdat)[[i]]} 

migrdat[["dir annual by LAD EW"]] <- 
  migrdat %>% 
  bind_rows() 

migrdat <- migrdat["dir annual by LAD EW"]

migrdat[["undir annual by LAD EW"]] <-
  migrdat[["dir annual by LAD EW"]] %>%
  left_join(
    ., 
    .[, c("LADCD_O", "LADCD_D", "migrants", "Year")], 
    by = c(
      "LADCD_O" = "LADCD_D", 
      "LADCD_D" = "LADCD_O", 
      "Year" = "Year"
    )
  ) %>% mutate(
    migrants = case_when(
      LADCD_O == LADCD_D ~ as.integer((migrants.x + migrants.y)/2),
      TRUE ~ as.integer(migrants.x + migrants.y)
    )
  ) %>% 
  select(-migrants.x, -migrants.y)

migrdat[["dir annual by LAD20X EW"]] <-
  migrdat[["dir annual by LAD EW"]] %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LADCD", "LAD20XCD", "LAD20XNM")]),
    by = c("LADCD_O" = "LADCD")
  ) %>% 
  rename(LAD20XCD_O = LAD20XCD, LAD20XNM_O = LAD20XNM) %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LADCD", "LAD20XCD", "LAD20XNM")]),
    by = c("LADCD_D" = "LADCD")
  ) %>% 
  rename(LAD20XCD_D = LAD20XCD, LAD20XNM_D = LAD20XNM) %>%
  group_by(Year, LAD20XCD_O, LAD20XNM_O, LAD20XCD_D, LAD20XNM_D) %>%
  summarise(migrants = sum(migrants)) %>%
  ungroup() 


migrdat[["undir annual by LAD20X EW"]] <-
  migrdat[["undir annual by LAD EW"]] %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LADCD", "LAD20XCD", "LAD20XNM")]),
    by = c("LADCD_O" = "LADCD")
  ) %>% 
  rename(LAD20XCD_O = LAD20XCD, LAD20XNM_O = LAD20XNM) %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LADCD", "LAD20XCD", "LAD20XNM")]),
    by = c("LADCD_D" = "LADCD")
  ) %>% 
  rename(LAD20XCD_D = LAD20XCD, LAD20XNM_D = LAD20XNM) %>%
  group_by(Year, LAD20XCD_O, LAD20XNM_O, LAD20XCD_D, LAD20XNM_D) %>%
  summarise(migrants = sum(migrants)) %>%
  ungroup() 

migrdat[["dir annual by ITL21X EW"]] <-
  migrdat[["dir annual by LAD20X EW"]] %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LAD20XCD", "ITL321XCD", "ITL321XNM")]),
    by = c("LAD20XCD_O" = "LAD20XCD")
  ) %>% 
  rename(ITL321XCD_O = ITL321XCD, ITL321XNM_O = ITL321XNM) %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LAD20XCD", "ITL321XCD", "ITL321XNM")]),
    by = c("LAD20XCD_D" = "LAD20XCD")
  ) %>%
  rename(ITL321XCD_D = ITL321XCD, ITL321XNM_D = ITL321XNM) %>%
  group_by(Year, ITL321XCD_O, ITL321XNM_O, ITL321XCD_D, ITL321XNM_D) %>%
  summarise(migrants = sum(migrants)) %>%
  ungroup() 

migrdat[["undir annual by ITL21X EW"]] <-
  migrdat[["undir annual by LAD20X EW"]] %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LAD20XCD", "ITL321XCD", "ITL321XNM")]),
    by = c("LAD20XCD_O" = "LAD20XCD")
  ) %>% 
  rename(ITL321XCD_O = ITL321XCD, ITL321XNM_O = ITL321XNM) %>%
  left_join(
    unique(geolkp[["ITL21X-*-LAD"]][, c("LAD20XCD", "ITL321XCD", "ITL321XNM")]),
    by = c("LAD20XCD_D" = "LAD20XCD")
  ) %>%
  rename(ITL321XCD_D = ITL321XCD, ITL321XNM_D = ITL321XNM) %>%
  group_by(Year, ITL321XCD_O, ITL321XNM_O, ITL321XCD_D, ITL321XNM_D) %>%
  summarise(migrants = sum(migrants)) %>%
  ungroup() 


migrdat[["dir 2002-2016 by ITL21X EW"]] <-
  migrdat[["dir annual by ITL21X EW"]] %>%
  filter(Year <= 2016) %>%
  group_by(across(c(-Year, -migrants))) %>%
  summarise(migrants = sum(migrants)) %>%
  ungroup()

migrdat[["undir 2002-2016 by ITL21X EW"]] <-
  migrdat[["undir annual by ITL21X EW"]] %>%
  filter(Year <= 2016) %>%
  group_by(across(c(-Year, -migrants))) %>%
  summarise(migrants = sum(migrants)) %>%
  ungroup()

migrdat[["dir 2002-2020 by ITL21X EW"]] <-
  migrdat[["dir annual by ITL21X EW"]] %>%
  filter(Year <= 2020) %>%
  group_by(across(c(-Year, -migrants))) %>%
  summarise(migrants = sum(migrants)) %>%
  ungroup()

migrdat[["undir 2002-2020 by ITL21X EW"]] <-
  migrdat[["undir annual by ITL21X EW"]] %>%
  filter(Year <= 2020) %>%
  group_by(across(c(-Year, -migrants))) %>%
  summarise(migrants = sum(migrants)) %>%
  ungroup()

# SAVE OUTPUT & CLEAN UP ENVIRONMENT -------------------------------------------

saveRDS(migrdat, file="./output/datasets/migrdat.RDS")

rm(list = ls()) 
gc()
