# LOAD PACKAGES ----------------------------------------------------------------

packages <-
  c(
    "dplyr",
    "rvest",
    "httr",
    "stringr"
  )

for (p in packages){ 
  if (! (p %in% installed.packages())){
    install.packages(p)
  }
}

library(dplyr)


# ENTER COMTRADE API KEY -------------------------------------------------------

comtrade_key <- ""

# DEFINE FUNCTIONS  ------------------------------------------------------------

# Function to get data files that can be directly downloaded from the web
get.download <- 
  function(url, fpath, overwrite){
    tryCatch(
      {
        if(overwrite == T | (!file.exists(fpath) & overwrite == F)){
          download.file(url, destfile= fpath, mode = "wb")
          cat("File", paste0("'", fpath, "'"), "downloaded from: \n", url,"\n")
          
        } else if (file.exists(fpath) & overwrite == F) {
          cat("Did not overwrite existing file", paste0("'", fpath, "'"), "\n")
        }
      }, 
      error=function(cond) {message(cond, "\n")},
      warning=function(cond) {message(cond, "\n")}
    )
  }

# Function to get data from the COMTRADE API "https://comtradeapi.un.org/data/v1/get/"
get.Comtrade <- 
  function(reporterCode, partnerCode, flowCode, period, fpath, overwrite, meta){
    tryCatch(
      {
        if(overwrite == T | (!file.exists(fpath) & overwrite == F)){
          
          if (meta == F){
            output <- httr::GET(
              "https://comtradeapi.un.org/data/v1/get/C/A/S3",
              query = list(
                  period = period,
                  reporterCode = reporterCode,
                  partnerCode = partnerCode, 
                  flowCode = flowCode,
                  includeDesc = T
              ),
              httr::add_headers(
              .headers = 
                c(
                  "Ocp-Apim-Subscription-Key" = comtrade_key, 
                  "Cache-Control" = "no-cache" 
                )
              )
            )
            
            content_lst <-
              lapply(
                httr::content(output)$data, 
                function(x) {
                  as.data.frame(t(unlist(x))) %>%
                    select(
                      any_of(
                        c(
                          "typeCode", "freqCode", "refYear", "reporterISO", "partnerISO", "partner2ISO", "classificationCode", 
                          "cmdCode", "cmdDesc", "aggrLevel", "isLeaf", "primaryValue", "customsDesc", "motDesc", "flowCode"
                        )
                      )
                    )
                }
              ) 
            
          } else if (meta == T) {
            output <- httr::GET(
              "https://comtradeapi.un.org/data/v1/getMetadata/C/A/SS",
              query = list(
                reporterCode = reporterCode
              ),
              httr::add_headers(
                .headers = 
                  c(
                    "Ocp-Apim-Subscription-Key" = comtrade_key, 
                    "Cache-Control" = "no-cache" 
                  )
              )
            )
            
            content_lst <-
              lapply(
                httr::content(output)$data[-1], function(x) {as.data.frame(t(unlist(x)))}
              ) %>%
              Filter(\(df) df$period <= 2007, .)
            
          }
          

          content_df <- do.call(rbind, content_lst)
          
          write.csv(content_df,  file = fpath)
          
          cat("File", paste0("'", fpath, "'"), "downloaded from: \n COMTRADE API \n")
          
        } else if (file.exists(fpath) & overwrite == F) {
          cat("Did not overwrite existing file", paste0("'", fpath, "'"), "\n")
        }
      }, 
      error=function(cond) {message(cond, "\n")},
      warning=function(cond) {message(cond, "\n")}
    )
  } 

# Function to get commuting data from WICID database session
get.WICID <- function(url, fpath, dat, geog, label, code, style, overwrite){
  tryCatch(
    {
      if(overwrite == T | (!file.exists(fpath) & overwrite == F)){
          
        wicid_ses <- rvest::session(url)
          
        # Values for label/code selection of form
        geog_dd1 <- paste0("geoglab[", geog, "][1]")
        geog_dd2 <- paste0("geoglab[", geog, "][2]")
          
        # Session navigation
        wicid_ses %>% 
          rvest::session_follow_link("Start a new session - no username or password required") %>%
          rvest::session_jump_to("/cider/wicid/query/select/data/data.php") %>%
          rvest::session_jump_to("quick.php?mode=null") %>%
          rvest::session_jump_to( # dataset selection
            paste0(
              "/flowdata/cider/wicid/query/select/data/quick.php?quick=",
              dat, 
              "&total=1"
            )
          ) %>% 
          rvest::session_jump_to("/cider/wicid//query/select/geog/index.php?type=orig") %>%
          rvest::session_jump_to("type_list.php?mode=quick&listtype=0") %>%
          rvest::session_jump_to(paste0("type_confirm.php?set_geog=", geog))%>% # geog selection
          rvest::session_jump_to("update_quick.php") %>%
          rvest::session_jump_to("/cider/wicid//query/select/geog/index.php?type=dest") %>%
          rvest::session_jump_to("copy.php") %>%
          rvest::session_jump_to("update_copy.php?mode=o2d") %>%
          rvest::session_jump_to("/cider/wicid//query/refine.php") %>%
          rvest::session_jump_to("run.php") %>%
          rvest::session_jump_to("/cider/wicid/output/index.php") %>%
          rvest::session_jump_to("./tabular/index.php?mode=plan") %>% 
          rvest::session_jump_to("/flowdata/cider/wicid/output/tabular/index.php?mode=labels") %>%
          rvest::session_submit( # code/label selection form
            x = ., 
            form = rvest::html_form_set(
              rvest::html_form(.)[[1]],
              !!geog_dd1 := label,
              !!geog_dd2 := code 
            )
          ) %>%
          rvest::session_submit( # style/format selection form
            x = ., 
            form = rvest::html_form_set(
              rvest::html_form(.)[[1]],
              "out_style" = style,
              "out_fmt" = "csv"
            )
          ) %>%
          rvest::session_jump_to("output_tab_dload.php") %>%
          rvest::session_submit(x = ., form = rvest::html_form(.)[[1]]) -> results
          
        writeBin(results$response$content, fpath)
          
        cat("File", paste0("'", fpath, "'"), "downloaded from: \n", url,"\n")
          
      } else if (file.exists(fpath) & overwrite == F) {
          cat("Did not overwrite existing file", paste0("'", fpath, "'"), "\n")
      }
    }, 
    error=function(cond) {message(cond, "\n")},
    warning=function(cond) {message(cond, "\n")}
  )
}

# Function to get data from the NOMIS API
get.Nomis <- 
  function(url, fpath, dat, geo, date, cell, item, variable, measures, select, gender, sex, c_age, industry, rur_urb, sel, overwrite){
    tryCatch(
      {
        if(overwrite == T | (!file.exists(fpath) & overwrite == F)){
          
          string<- paste0(
            url,
            "dataset/",dat,"?",
            ifelse(missing(geo), "", paste0("geography=",geo,"&")),
            ifelse(missing(date), "", paste0("date=",date,"&")),
            ifelse(missing(cell), "", paste0("cell=",cell,"&")),
            ifelse(missing(variable), "", paste0("variable=",variable,"&")),
            ifelse(missing(item), "", paste0("item=",item,"&")),
            ifelse(missing(measures), "", paste0("measures=",measures,"&")),
            ifelse(missing(select), "", paste0("select=",select,"&")),
            ifelse(missing(gender), "", paste0("gender=",gender,"&")),
            ifelse(missing(sex), "", paste0("sex=",sex,"&")),
            ifelse(missing(c_age), "", paste0("c_age=",c_age,"&")),
            ifelse(missing(industry), "", paste0("industry=",industry,"&")),
            ifelse(missing(rur_urb), "", paste0("rural_urban=",rur_urb,"&"))
          )
          
          
          if (grepl(".csv", fpath)){
            write.csv(read.csv(string,header=TRUE), file = fpath)
            cat("File", paste0("'", fpath, "'"), "downloaded from: \n", string,"\n")
          }
          
          if (!grepl(".csv", fpath)){
            download.file(string, destfile = fpath)
            cat("File", paste0("'", fpath, "'"), "downloaded from: \n", string,"\n")
          }
        
        } else if (file.exists(fpath) & overwrite == F) {
          cat("Did not overwrite existing file", paste0("'", fpath, "'"), "\n")
        }
      }, 
      error=function(cond) {message(cond, "\n")},
      warning=function(cond) {message(cond, "\n")}
    )
  }
    
# GET DIRECT DOWNLOAD FILES ----------------------------------------------------

# Increase timeout limit
options(timeout = max(1000, getOption("timeout")))

# Code History Database (Dec 2020) for the UK v2 (ONS Geoportal)
get.download(
  url = "https://www.arcgis.com/sharing/rest/content/items/4ea95d1f8e8c4b7bbda9c61375427706/data", 
  fpath = "./rawdata/lookups/ONS - Code History Database (Dec 2020 v2).zip",
  overwrite = T
)

# LAD (Dec 2020) to LAU1 to ITL3/2/1 (Jan 2021) lookup in the UK v2 (ONS Geoportal)
get.download(
  url = "https://www.arcgis.com/sharing/rest/content/items/cdb629f13c8f4ebc86f30e8fe3cddda4/data", 
  fpath = "./rawdata/lookups/ONS - LAD (Dec 2020) to ITL (Jan 2021) lookup.xlsx",
  overwrite = T
)

# Changes between NUTS 2021 and NUTS 2016 (Eurostat)
get.download(
  url = "https://ec.europa.eu/eurostat/documents/345175/629341/NUTS2021.xlsx", 
  fpath = "./rawdata/lookups/EUROSTAT - NUTS16 to NUTS21 changes lookup.xlsx",
  overwrite = T
)

# SITC Rev. 3 - NACE Rev. 1 lookup (WITS, World Bank)
get.download(
  url = "http://wits.worldbank.org/data/public/concordance/Concordance_S3_to_NC.zip", 
  fpath = "./rawdata/lookups/WORLD BANK - SITC Rev.3 to NACE Rev.1 lookup.zip",
  overwrite = T
)

# Full set of EU Referendum result data (Electoral Commission UK)
get.download(
  url = "https://www.electoralcommission.org.uk/sites/default/files/2019-07/EU-referendum-result-data.csv", 
  fpath = "./rawdata/voting/UK ELECTORAL COMMISSION - 2016 - EURef results.csv",
  overwrite = T
)

# Regional GVA by NUTS3 (ONS)
get.download(
  url = "https://www.ons.gov.uk/file?uri=/economy/grossvalueaddedgva/datasets/regionalgrossvalueaddedincomeapproach/current/gvaireferencetables.xls",
  fpath = "./rawdata/other/ONS - Regional GVA by NUTS3.xls",
  overwrite = T
)


# LAD (Dec 2020) boundaries UK BUC (ONS Geoportal)
get.download(
  url = "https://opendata.arcgis.com/api/v3/datasets/0c94ff9e45a84f20b380875869e06e5f_0/downloads/data?format=shp&spatialRefId=27700&where=1%3D1", 
  fpath = "./rawdata/geometries/ONS - LAD (Dec 2020) boundaries.zip",
  overwrite = T
)

# Census Merged LAD 2011 -  SUPER GENERALISED (ONS Geoportal)
get.download(
  url = "https://opendata.arcgis.com/api/v3/datasets/42a6cfa42e6d4070bfdf2e403bb68265_0/downloads/data?format=shp&spatialRefId=27700&where=1%3D1", 
  fpath = "./rawdata/geometries/ONS - CMLAD 2011 boundaries.zip",
  overwrite = T
)

# CPI 00: All items 2015 = 100 (ONS)
 get.download(
   url = "https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/d7bt/mm23",
   fpath = "./rawdata/other/ONS - CPI 00 All items 2015 = 100.csv",
   overwrite = T
 )
 
 # Internet Users 2016 (ONS)
 get.download(
   url = "https://www.ons.gov.uk/file?uri=/businessindustryandtrade/itandinternetindustry/datasets/internetusers/current/internetusers2020.xlsx",
   fpath = "./rawdata/other/ONS - Internet users.xlsx",
   overwrite = T
 )
 
 # Fetzer (2019) data
 get.download(
   url =  "https://www.aeaweb.org/doi/10.1257/aer.20181164.data",
   fpath = "./rawdata/other/fetzdat.zip",
   overwrite = T
 )
 
 # British Election Study Waves 7-9 [Needs login]
 # get.download(
 #   url = "https://www.britishelectionstudy.com/wp-content/uploads/2022/10/BES2015_W7_v23.0.dta",
 #   fpath = "./rawdata/voting/BES2015_W7_v23.0.dta",
 #   overwrite = T
 # )
 # 
 # get.download(
 #   url = "https://www.britishelectionstudy.com/wp-content/uploads/2022/10/BES2015_W8_v23.0.dta",
 #   fpath = "./rawdata/voting/BES2015_W8_v23.0.dta",
 #   overwrite = T
 # )
 # 
 # get.download(
 #   url = "https://www.britishelectionstudy.com/wp-content/uploads/2022/10/BES2015_W9_v23.0.dta",
 #   fpath = "./rawdata/voting/BES2015_W9_v23.0.dta",
 #   overwrite = T
 # )

# ******************************************************************************
# Annual LAD internal migration matrices, 2002-2020, England & Wales (ONS)
# ******************************************************************************

## Get 2002-2011 matrices from the National Archives 
migr0211_html <- 
  rvest::read_html( # Read master webpage 
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20160105222741mp_/http://www.ons.gov.uk/ons/rel/migration1/internal-migration-by-local-authorities-in-england-and-wales/index.html"
  )

migr0211_pub_pg <- # Get urls to annual publication webpages
  migr0211_html %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>%
  stringr::str_subset("internal-migration-by-local-authorities-in-england-and-wales") %>%
  stringr::str_subset(paste(as.character(2002:2011), collapse = "|")) %>%
  .[!grepl("research-series", .)] %>%
  paste0("https://webarchive.nationalarchives.gov.uk", .)

migr0211_ref_pg <- 
  sapply( # Get urls to reference table pages 
    migr0211_pub_pg, 
    USE.NAMES = FALSE,
    function(pp){

        rvest::read_html(pp) %>%
        rvest::html_nodes("a") %>% 
        rvest::html_attr("href") %>%
        stringr::str_subset("re-reference-tables") %>%
        paste0("https://webarchive.nationalarchives.gov.uk", .)
    
    }
  )

migr0211_files  <- 
  sapply( # Get urls to reference table files
    migr0211_ref_pg, 
    USE.NAMES = FALSE,
    function(rp){
      
      file_url <- 
        rvest::read_html(rp) %>%
        rvest::html_nodes("a") %>% 
        rvest::html_attr("href") %>%
        stringr::str_subset(".zip") %>%
        paste0("https://webarchive.nationalarchives.gov.uk", .)
      
      return(file_url) 
    }
  )

# Unlist reference tables (>1 for some years); drop redundant table
migr0211_files <- unlist(migr0211_files) 
migr0211_files <-
  migr0211_files[!grepl("mid-2011-part-2.zip", migr0211_files)]

# Download 2002-2011 migration matrices
for (file_url in migr0211_files){
  get.download(
    url = file_url, 
    fpath = paste0("./rawdata/migration/", sub(".*/", "", file_url)),
    overwrite = T
  )
}


## Get 2012-2020 matrices from the ONS website
migr1220_url <- 
  "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/matricesofinternalmigrationmovesbetweenlocalauthoritiesandregionsincludingthecountriesofwalesscotlandandnorthernireland"

migr1220_files <- 
  rvest::read_html(migr1220_url) %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>%
  stringr::str_subset(".zip|.xlsx") %>%
  paste0("https://www.ons.gov.uk", .)

# Download 2012-20 migration matrices
for (file_url in migr1220_files){
  get.download(
    url = file_url, 
    fpath = paste0("./rawdata/migration/", sub(".*/", "", file_url)),
    overwrite = T
  )
  
  Sys.sleep(5)
}

# GET SESSION ACCESSIBLE FILES -------------------------------------------------

# ******************************************************************************
# LAD commuting matrices, 1991-2001-2011 Census, England & Wales (WICID, UKDS)
# ******************************************************************************

# 1991 commuting data
get.WICID(
  url = "https://wicid.ukdataservice.ac.uk/flowdata/cider/wicid/query.php",
  dat = "34", 
  geog = "28", 
  label = "label", 
  code = "opcs_code", 
  style = "tally", 
  fpath = "./rawdata/commuting/UK DATA SERVICE - 1991 commuting flows by IDT01 (Set SWS C).csv",
  overwrite = T
)

# 2001 commuting data
get.WICID(
  url = "https://wicid.ukdataservice.ac.uk/flowdata/cider/wicid/query.php",
  dat = "23", 
  geog = "28", 
  label = "label", 
  code = "opcs_code", 
  style = "tally", 
  fpath = "./rawdata/commuting/UK DATA SERVICE - 2001 commuting flows by IDT01 (Set SWS 1).csv",
  overwrite = T
)

# 2011 commuting data
get.WICID(
  url = "https://wicid.ukdataservice.ac.uk/flowdata/cider/wicid/query.php",
  dat = "292", 
  geog = "96", 
  label = "label", 
  code = "opcs_code", 
  style = "tally", 
  fpath = "./rawdata/commuting/UK DATA SERVICE - 2011 commuting flows by CMLAD11 (Table WU02UK).csv",
  overwrite = T
)

# GET API ACCESSIBLE FILES -----------------------------------------------------

# ******************************************************************************
# LAD population estimates 2020 for England, Scotland & Wales (Nomis API)
# ******************************************************************************

get.Nomis(
  url = "https://www.nomisweb.co.uk/api/v01/",
  dat= "NM_2002_1.data.csv", 
  geo= "1811939329...1811939332,1811939334...1811939336,1811939338...1811939428,1811939436...1811939442,1811939768,1811939769,1811939443...1811939497,1811939499...1811939501,1811939503,1811939505...1811939507,1811939509...1811939517,1811939519,1811939520,1811939524...1811939570,1811939575...1811939599,1811939601...1811939628,1811939630...1811939634,1811939636...1811939647,1811939649,1811939655...1811939664,1811939667...1811939680,1811939682,1811939683,1811939685,1811939687...1811939704,1811939707,1811939708,1811939710,1811939712...1811939717,1811939719,1811939720,1811939722...1811939730,1811939757...1811939767", 
  date= "2020", 
  gender = "0",
  c_age = "200",
  measures = "20100",
  select = paste(
    "date_name",
    "geography_name",
    "geography_code",
    "gender_name",
    "c_age_name",
    "measures_name",
    "obs_value",
    "obs_status_name",
    sep = ","
  ),
  fpath = "./rawdata/other/ONS - mid-2020 population estimates by LAD.csv",
  overwrite = T
)

# ******************************************************************************
# LAD employment data in 1991 for England, Scotland & Wales (AES, Nomis API)
# ******************************************************************************

geos <- 1946157057:1946157436
i <- 0

for (geochunk in split(geos, ceiling(seq_along(geos)/100))){ # 25k CSV cell API limit (100geo*~200SIC)
  i <- i + 1
  
  get.Nomis(
    url = "https://www.nomisweb.co.uk/api/v01/",
    dat = "NM_194_1.data.csv", 
    geo = paste(geochunk[1], geochunk[length(geochunk)], sep = "..."),
    date = "1991", 
    sex = "7",
    industry = paste(
      "12582922...12582927",
      "12582932",
      "12582962",
      "12583013...12583015",
      "12583023",
      "12583024",
      "12583032",
      "12583043",
      "12583044",
      "12583053...12583057",
      "12583063...12583072",
      "12583083...12583089",
      "12583093...12583095",
      "12583103...12583105",
      "12583113...12583117",
      "12583123,12583124",
      "12583133...12583135",
      "12583143...12583145",
      "12583153...12583159",
      "12583163",
      "12583164",
      "12583173...12583180",
      "12583183...12583187",
      "12583193...12583199",
      "12583203...12583209",
      "12583212",
      "12583223...12583228",
      "12583233...12583235",
      "12583243...12583247",
      "12583253...12583255",
      "12583263...12583267",
      "12583273...12583278",
      "12583283",
      "12583284",
      "12583313...12583315",
      "12583322",
      "12583363...12583367",
      "12583413...12583417",
      "12583423...12583429",
      "12583433...12583439",
      "12583463...12583467",
      "12583513...12583515",
      "12583523",
      "12583524",
      "12583533...12583535",
      "12583543...12583546",
      "12583553",
      "12583554",
      "12583563",
      "12583564",
      "12583572",
      "12583583",
      "12583584",
      "12583613...12583615",
      "12583623...12583626",
      "12583633...12583638",
      "12583643",
      "12583644",
      "12583653...12583660",
      "12583663...12583665",
      "12583713...12583716",
      "12583763...12583765",
      "12583812",
      "12583823...12583825",
      "12583833...12583839",
      "12583842",
      "12583862",
      "12583902",
      sep = ","
    ),
    measures = "20100",
    fpath = paste0("./rawdata/employment/ONS - 1991 employment by 3-dig SIC92 and LAD ", i , ".csv"),
    overwrite = T
  )
}

# ******************************************************************************
# LAD 1991 census statistics in 1991 for England & Wales (Nomis API)
# ******************************************************************************

get.Nomis(
  url = "https://www.nomisweb.co.uk/api/v01/",
  dat = "NM_35_1.data.csv", 
  geo = "1946157057...1946157436",
  date = "1991", 
  cell = paste(
    "201394689",
    "201459201",
    "201459457",
    "201459713",
    "201459969",
    "201460225",
    "201460481",
    "201460737",
    "201460993",
    "201461249",
    "201461505",
    "201461761",
    "201462017",
    "201462273",
    "201462529",
    "201462785",
    "201463041",
    "201463297",
    "201787905",
    "201851137",
    "201851393",
    "201852929",
    "206110977",
    "206110981",
    "206110982",
    "206831873",
    "206832385",
    "206833665",
    "206834945",
    "206110983...206110987",
    sep = ","
  ),
  measures = "20100",
  select = paste(
    "date_name",
    "geography_name",
    "geography_code",
    "cell_name",
    "measures_name",
    "obs_value",
    "obs_status_name",
    sep = ","
  ),
  fpath = "./rawdata/other/ONS - 1991 Census statistics by LAD.csv",
  overwrite = T
)

# ******************************************************************************
# LAD 2011 census statistics in 2011 for England & Wales (Nomis API)
# ******************************************************************************

get.Nomis(
  url = "https://www.nomisweb.co.uk/api/v01/",
  dat = "NM_524_1.data.csv", 
  geo = "1946157057...1946157404",
  date = "2011", 
  cell = paste("0", "2", sep = ","),
  rur_urb = 0,
  measures = "20100",
  select = paste(
    "date_name",
    "geography_name",
    "geography_code",
    "cell_name",
    "measures_name",
    "obs_value",
    "obs_status_name",
    sep = ","
  ),
  fpath = "./rawdata/other/ONS - 2011 Census statistics by LAD.csv",
  overwrite = T
)

# ******************************************************************************
# UK-CN and USA-CN trade in goods, by SITC Rev. 3 (UN Comtrade API)
# ******************************************************************************



for (yr in c(1991,2007)){
  
  # Imports to GBR from China 1991,2007
  get.Comtrade(
    reporterCode = 826,
    partnerCode = 156, 
    flowCode = "M", 
    period = yr, 
    fpath =  paste0("./rawdata/imports/COMTRADE - GBR CHN Imports - " , yr, ".csv"), 
    overwrite = T,
    meta = F
  )
  
  Sys.sleep(10)
  
  # Imports to GBR from world 1991,2007
  get.Comtrade(
    reporterCode = 826,
    partnerCode = 0, 
    flowCode = "M", 
    period = yr, 
    fpath =  paste0("./rawdata/imports/COMTRADE - GBR WLD Imports - " , yr, ".csv"), 
    overwrite = T,
    meta = F
  )
  
  Sys.sleep(10)
  
  # Imports to USA from China 1991,2007
  get.Comtrade(
    reporterCode = 842,
    partnerCode = 156, 
    flowCode = "M", 
    period = yr, 
    fpath =  paste0("./rawdata/imports/COMTRADE - USA CHN Imports - " , yr, ".csv"), 
    overwrite = T,
    meta = F
  )
  
  Sys.sleep(10)
  
  # Imports to Australia from China 1991,2007
  get.Comtrade(
    reporterCode = 36,
    partnerCode = 156, 
    flowCode = "M", 
    period = yr, 
    fpath =  paste0("./rawdata/imports/COMTRADE - AUS CHN Imports - " , yr, ".csv"), 
    overwrite = T,
    meta = F
  )
  
  Sys.sleep(10)
  
  # Imports to Denmark from China 1991,2007
  get.Comtrade(
    reporterCode = 208,
    partnerCode = 156, 
    flowCode = "M", 
    period = yr, 
    fpath =  paste0("./rawdata/imports/COMTRADE - DK CHN Imports - " , yr, ".csv"), 
    overwrite = T,
    meta = F
  )
  
  Sys.sleep(10)
  
  # Imports to Finland from China 1991,2007
  get.Comtrade(
    reporterCode = 246,
    partnerCode = 156, 
    flowCode = "M", 
    period = yr, 
    fpath =  paste0("./rawdata/imports/COMTRADE - FIN CHN Imports - " , yr, ".csv"), 
    overwrite = T,
    meta = F
  )
  
  Sys.sleep(10)
  
  # Imports to Germany from China 1991,2007
  get.Comtrade(
    reporterCode = 276,
    partnerCode = 156, 
    flowCode = "M", 
    period = yr, 
    fpath =  paste0("./rawdata/imports/COMTRADE - GER CHN Imports - " , yr, ".csv"), 
    overwrite = T,
    meta = F
  )
  
  Sys.sleep(10)
  
  # Imports to Japan from China 1991,2007
  get.Comtrade(
    reporterCode = 392,
    partnerCode = 156, 
    flowCode = "M", 
    period = yr, 
    fpath =  paste0("./rawdata/imports/COMTRADE - JPN CHN Imports - " , yr, ".csv"), 
    overwrite = T,
    meta = F
  )
  
  Sys.sleep(10)
  
  # Imports to New Zealand from China 1991,2007
  get.Comtrade(
    reporterCode = 554,
    partnerCode = 156, 
    flowCode = "M", 
    period = yr, 
    fpath =  paste0("./rawdata/imports/COMTRADE - NZ CHN Imports - " , yr, ".csv"), 
    overwrite = T,
    meta = F
  )
  
  Sys.sleep(10)
  
  # Imports to Spain from China 1991,2007
  get.Comtrade(
    reporterCode = 724,
    partnerCode = 156, 
    flowCode = "M", 
    period = yr, 
    fpath =  paste0("./rawdata/imports/COMTRADE - ESP CHN Imports - " , yr, ".csv"), 
    overwrite = T,
    meta = F
  )
  
  Sys.sleep(10)
  
  # Imports to Switzerland from China 1991,2007
  get.Comtrade(
    reporterCode = 757,
    partnerCode = 156, 
    flowCode = "M", 
    period = yr, 
    fpath =  paste0("./rawdata/imports/COMTRADE - SWZ CHN Imports - " , yr, ".csv"), 
    overwrite = T,
    meta = F
  )
  
  Sys.sleep(10)
  
  # Exports to GBR from China 1991,2007
  get.Comtrade(
    reporterCode = 826,
    partnerCode = 156, 
    flowCode = "X", 
    period = yr, 
    fpath =  paste0("./rawdata/exports/COMTRADE - GBR CHN Exports - " , yr, ".csv"), 
    overwrite = T,
    meta = F
  )
  
  Sys.sleep(10)
  
  # Exports to GBR from world 1991,2007
  get.Comtrade(
    reporterCode = 826,
    partnerCode = 0, 
    flowCode = "X", 
    period = yr, 
    fpath =  paste0("./rawdata/exports/COMTRADE - GBR WLD Exports - " , yr, ".csv"), 
    overwrite = T,
    meta = F
  )
  
  Sys.sleep(10)
  
}

# GBR import currency conversion factors (UN Comtrade Explanatory Notes)
get.Comtrade(
  reporterCode = 826,
  fpath =  paste0("./rawdata/other/COMTRADE - GBR currency conversion factors.csv"), 
  overwrite = T,
  meta = T
)

# ******************************************************************************
# Boundary data from NOMIS API
# ******************************************************************************

# Enumeration districts (1991 Census) boundaries and population  (ONS Nomis)
geos <- 29360129:29469700
i <- 0

for (geochunk in split(geos, ceiling(seq_along(geos)/999))){ # 1k KML cell API limit
  i <- i + 1
  get.Nomis(
    url = "https://www.nomisweb.co.uk/api/v01/",
    dat= "NM_38_1.data.kml", 
    geo = paste(geochunk[1], geochunk[length(geochunk)], sep = "..."), 
    date= "1991", 
    cell = "268503553",
    measures = "20100",
    fpath = paste0("./rawdata/geometries/ONS- 1991 Census ED boundaries and population ", i, ".kml"),
    overwrite = T
  )
  
  Sys.sleep(2)
}

# LSOA (2001 Census) boundaries and population (ONS Nomis)
geos <- 1275068417:1275102794
i <- 0

for (geochunk in split(geos, ceiling(seq_along(geos)/999))){ # 1k KML cell API limit
  i <- i + 1
  get.Nomis(
    url = "https://www.nomisweb.co.uk/api/v01/",
    dat= "NM_1634_1.data.kml", 
    geo= paste(geochunk[1], geochunk[length(geochunk)], sep = "..."),
    date= "2001", 
    cell = "0",
    measures = "20100",
    fpath = paste0("./rawdata/geometries/ONS- 2001 Census LSOA boundaries and population ", i, ".kml"),
    overwrite = T
  )
  
  Sys.sleep(2)
}

#  LSOA (2011 Census) boundaries and population (ONS Nomis)
geos <- 1249902593:1249937345
i <- 0

for (geochunk in split(geos, ceiling(seq_along(geos)/999))){ # 1k KML cell API limit
  i <- i + 1
  get.Nomis(
    url = "https://www.nomisweb.co.uk/api/v01/",
    dat = "NM_144_1.data.kml", 
    geo = paste(geochunk[1], geochunk[length(geochunk)], sep = "..."),
    date= "2001", 
    cell = "0",
    rur_urb = "0",
    measures = "20100",
    fpath = paste0("./rawdata/geometries/ONS- 2011 Census LSOA boundaries and population ", i, ".kml"),
    overwrite = T
  )
  
  Sys.sleep(2)
}

# CLEAN UP ENVIRONMENT ---------------------------------------------------------

rm(list = ls())
gc()






