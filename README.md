## Scripts
* The tables included in the paper are produced by the Stata script `9_main_analysis.do`. 
* The R scripts numbered 1-8 perform all preceeding data processing steps:
  * `1a_get_lookups.R` produces spatial and product-industry lookups.
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
  * `6b_calc_lagged_import_shocks.R` produces socially and spatially lagged measures of regional exposure to import competition
  * `7a_get_regional_main_data.R` produces the regional dataset used in the main analysis.
  * `7b_get_indiv_main_data.R` produces the individual-level dataset used in the main analysis.
  * `8_get_plots.R` produces plots and maps included in the paper.
