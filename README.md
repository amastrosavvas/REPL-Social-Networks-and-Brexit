## Introduction

This repository provides replication materials for tables and plots in the paper:

> Mastrosavvas, A. (2024) “Social Networks and Brexit: Evidence from a Trade Shock”, *Regional Science and Urban Economics*, Forthcoming, Available at: [https://doi.org/10.1016/j.regsciurbeco.2024.104024](https://doi.org/10.1016/j.regsciurbeco.2024.104024).

Date: 2024-05-21 

## Overview of scripts
* The R scripts numbered 0-9 perform all the prerequisite data collection and pre-processing steps, and produce the plots and some supplementary tables:
  
  * `0a_create_folders.R` creates the file structure of the repository.
  * `0b_get_data.R` collects most of the prerequisite data using publicly available APIs and downloads (~15-30mins). 
  * `1a_get_lookups.R` produces geographic and product-industry lookups.
  * `1b_get_basemap.R` produces geometries for harmonised spatial classifications.
  * `2_prep_node_data.R` pre-processes data on regional voting, employment, demographics, and growth.
  * `3a_prep_import_data.R` pre-processes data on national imports.
  * `3b_calc_import_shock.R` produces measures of regional exposure to import competition.
  * `4a_prep_commuting_data.R` pre-processes data on interregional commuting.
  * `4b_prep_migration_data.R` pre-processes data on interregional migration.
  * `4c_prep_geopop_data.R` produces measures of geographic distance.
  * `4d_prep_sci_data.R` pre-processes data on interregional social ties.
  * `5a_get_cz_clusters.R` delineates commuting zones.
  * `6a_get_w_matrices.R` produces social and spatial weight matrices.
  * `6b_calc_lagged_import_shocks.R` produces socially and spatially lagged measures of regional exposure to import competition.
  * `7a_get_regional_main_data.R` produces the regional dataset used in the main analysis.
  * `7b_get_indiv_main_data.R` produces the individual-level dataset used in the main analysis.
  * `8_comp_table.R` produces table comparing covariate means for most and least exposed regions to the main treatment.
  * `9_get_plots.R` produces the figures presented in the paper.
    
* The results tables included in the paper are produced by the Stata script `10_main_analysis.do`. 
* To reproduce the results in the paper, the scripts need to be run in order i.e. from 0 to 10.
 
## Before running

* Open the R project file `REPL-Social-Networks-and-Brexit.Rproj` in an R session before running R scripts.
  
* Each R script checks your system for any missing R package dependencies and installs them. The relevant code can be found at the top of each script. Ensure that you are satisfied with installing the relevant packages if not already installed before running the script.
  
* The script `0a_create_folders.R` has already been run in the provided repository, so this can be skipped.

* Register with the [British Election Study](https://www.britishelectionstudy.com/wp-login.php?action=register) and download survey data for waves 7, 8, and 9 of the BES Internet Panel (in .dta format). Ensure that these are saved within the  `rawdata/voting` folder and that their respective names match those in the scripts (BES2015_w7_v23.0.dta, BES2015_w8_v23.0.dta, BES2015_w9_v23.0.dta at the time of publication).
    
* Before running the R script `0b_get_data.R` ensure that the following is in place:
  
  * Obtain a COMTRADE API authentication key by registering with the [COMTRADE database](https://comtradedeveloper.un.org/signin?returnUrl=%2F) and enter it in `comtrade_key` at the top of the script.

* Before running the Stata script `10_main_analysis.do`:
   *  Ensure that the repository is set as the working directory in Stata.
   *  Ensure that the relevant Stata package dependencies are installed.

 ## Additional notes on input data

 * The R script `0b_get_data.R` downloads all but two prerequisite datasets:
    * The British Election Study data, which need to be obtained manually and by registration as described in the previous section.
    * The Social Connectedness Index (SCI), which has already been provided in this repository within the rawdata/sci folder. While the SCI is publicly available via the [Humanitarian Data Exchange](https://data.humdata.org/dataset/social-connectedness-index?) under a CC0 license, later versions of the index, which are generally based on snapshots of the Facebook user population at later dates, replace previous versions. Consequently, only the provided version of the SCI should be used for replicating results.
  
  * Note that the API structures and web addresses via which the R script `0b_get_data.R` accesses data may be subject to changes over time. In such cases, the code may be need to be modified to obtain some datasets.

  * Note that some of the underlying datasets (e.g. on trade flows) may be subject to revisions enacted by the statistical agencies that make them available. This could drive some discrepancies in the replicated results if and when such revisions are enacted, though these are likely to be very limited.

 ## Storage

 It is advised that at at least 2GB of storage is made available.

 ## System and version information

 Platform: aarch64-apple-darwin20 (64-bit) 
 
 Version: R version 4.2.2; Stata version 17.0
 
 
