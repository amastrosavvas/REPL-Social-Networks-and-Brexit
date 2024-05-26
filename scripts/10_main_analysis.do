*=============================*
* Regional tables             * 
*=============================*

*----------- Table 1 ----------* 
use ./output/datasets/main/regdat.dta,clear

* Regional main OLS/IV specs

reghdfe pc_leave rshk_GBRZ , absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/Table1.xls, nocons e(r2_within) label replace 

reghdfe pc_leave rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ, absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/Table1.xls, nocons e(r2_within) label append

reghdfe pc_leave rshk_GBRZ lgrshk_GBR_wall_SCI_cZ, absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/Table1.xls, nocons e(r2_within) label append

ivreghdfe pc_leave (rshk_GBRZ = rshk_HIZ), absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/Table1.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ = rshk_HIZ lgrshk_HI_w1_5_SCI_cZ), absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/Table1.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_wall_SCI_cZ = rshk_HIZ lgrshk_HI_wall_SCI_cZ), absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/Table1.xls, addstat(kpstat, e(widstat)) label append

*----------- Table F1 ----------* 
use ./output/datasets/main/regdat.dta,clear

* Summary statistics 
outreg2 using ./output/tables/TableF1.xls, replace sum(log) keep(rshk_GBR lgrshk_GBR_w1_5_SCI_c lgrshk_GBR_wall_SCI_c  grossmigrants0220_5_pc grossmigrants0220_all_pc  grossmigrants0216_5_pc grossmigrants0216_all_pc  pc16_mnf cri pcp_aged65ov pcp_foreign km_to_london actintusers_2016or17_pc)

*----------- Table F2 ----------* 
use ./output/datasets/main/regdat.dta,clear

* Regional main OLS/IV specs, place-of-work import-shock exposure 

reghdfe pc_leave shk_GBRZ, absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/TableF2.xls, nocons e(r2_within) label replace 

reghdfe pc_leave shk_GBRZ lgshk_GBR_w1_5_SCI_cZ, absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/TableF2.xls, nocons e(r2_within) label append

reghdfe pc_leave shk_GBRZ lgshk_GBR_wall_SCI_cZ, absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/TableF2.xls, nocons e(r2_within) label append

ivreghdfe pc_leave (shk_GBRZ = shk_HIZ), absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF2.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (shk_GBRZ lgshk_GBR_w1_5_SCI_cZ = shk_HIZ lgshk_HI_w1_5_SCI_cZ), absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF2.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (shk_GBRZ lgshk_GBR_wall_SCI_cZ = shk_HIZ lgshk_HI_wall_SCI_cZ), absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF2.xls, addstat(kpstat, e(widstat)) label append

*----------- Table F3 ----------* 
use ./output/datasets/main/regdat.dta,clear

* Regional main OLS/IV specs, USA instruments

reghdfe pc_leave rshk_GBRZ , absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/TableF3.xls, nocons e(r2_within) label replace 

reghdfe pc_leave rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ, absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/TableF3.xls, nocons e(r2_within) label append

reghdfe pc_leave rshk_GBRZ lgrshk_GBR_wall_SCI_cZ, absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/TableF3.xls, nocons e(r2_within) label append

ivreghdfe pc_leave (rshk_GBRZ = rshk_USAZ), absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF3.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ = rshk_USAZ lgrshk_USA_w1_5_SCI_cZ), absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF3.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_wall_SCI_cZ = rshk_USAZ lgrshk_USA_wall_SCI_cZ), absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF3.xls, addstat(kpstat, e(widstat)) label append

*----------- Table F4 ----------* 
use ./output/datasets/main/regdat.dta,clear

* Regional main OLS/IV specs, HI + USA instruments

reghdfe pc_leave rshk_GBRZ , absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/TableF4.xls, nocons e(r2_within) label replace 

reghdfe pc_leave rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ, absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/TableF4.xls, nocons e(r2_within) label append

reghdfe pc_leave rshk_GBRZ lgrshk_GBR_wall_SCI_cZ, absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/TableF4.xls, nocons e(r2_within) label append

ivreghdfe pc_leave (rshk_GBRZ = rshk_HIUSAZ), absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF4.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ = rshk_HIUSAZ lgrshk_HIUSA_w1_5_SCI_cZ), absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF4.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_wall_SCI_cZ = rshk_HIUSAZ lgrshk_HIUSA_wall_SCI_cZ), absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF4.xls, addstat(kpstat, e(widstat)) label append

*----------- Table F5 ----------*  
use ./output/datasets/main/regdat.dta,clear

* Regional main OLS/IV specs, weighted by Votes_Cast

reghdfe pc_leave rshk_GBRZ [aweight = Votes_Cast], absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/TableF5.xls, nocons e(r2_within) label replace 

reghdfe pc_leave rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ [aweight = Votes_Cast], absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/TableF5.xls, nocons e(r2_within) label append

reghdfe pc_leave rshk_GBRZ lgrshk_GBR_wall_SCI_cZ [aweight = Votes_Cast], absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/TableF5.xls, nocons e(r2_within) label append

ivreghdfe pc_leave (rshk_GBRZ = rshk_HIZ) [aweight = Votes_Cast], absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF5.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ = rshk_HIZ lgrshk_HI_w1_5_SCI_cZ) [aweight = Votes_Cast], absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF5.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_wall_SCI_cZ = rshk_HIZ lgrshk_HI_wall_SCI_cZ) [aweight = Votes_Cast], absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF5.xls, addstat(kpstat, e(widstat)) label append

*----------- Table F6 ----------* 
use ./output/datasets/main/regdat.dta,clear

* Robustness to equal weights (social neighbours)

reghdfe pc_leave rshk_GBRZ, absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/TableF6.xls, nocons e(r2_within) label replace 

reghdfe pc_leave rshk_GBRZ lgrshk_GBR_weq1_5_SCI_cZ, absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/TableF6.xls, nocons e(r2_within) label append

ivreghdfe pc_leave (rshk_GBRZ = rshk_HIZ), absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF6.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_weq1_5_SCI_cZ = rshk_HIZ lgrshk_HI_weq1_5_SCI_cZ), absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF6.xls, addstat(kpstat, e(widstat)) label append

*----------- Table F7 ----------* 
use ./output/datasets/main/regdat.dta,clear

* Regional robustness 2SLS (social lag) 1

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_wall_SCI_cZ = rshk_HIZ lgrshk_HI_wall_SCI_cZ) pc16_mnfZ, absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF7.xls, addstat(kpstat, e(widstat)) label replace

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_wall_SCI_cZ = rshk_HIZ lgrshk_HI_wall_SCI_cZ) pc16_mnfZ criZ, absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF7.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_wall_SCI_cZ = rshk_HIZ lgrshk_HI_wall_SCI_cZ) pc16_mnfZ criZ grossmigrants0216_all_pcZ, absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF7.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_wall_SCI_cZ = rshk_HIZ lgrshk_HI_wall_SCI_cZ) pcp_aged65ovZ pcp_foreignZ, absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF7.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_wall_SCI_cZ = rshk_HIZ lgrshk_HI_wall_SCI_cZ) km_to_londonZ, absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF7.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_wall_SCI_cZ = rshk_HIZ lgrshk_HI_wall_SCI_cZ) actintusers_2016or17_pcZ, absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF7.xls, addstat(kpstat, e(widstat)) label append

*----------- Table 2 ----------* 
use ./output/datasets/main/regdat.dta,clear

* Regional main OLS/IV specs with gross migration controls

reghdfe pc_leave rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ grossmigrants0220_5_pcZ, absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/Table2.xls, nocons e(r2_within) label replace

reghdfe pc_leave rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ grossmigrants0220_all_pcZ, absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/Table2.xls, nocons e(r2_within) label append

reghdfe pc_leave rshk_GBRZ lgrshk_GBR_wall_SCI_cZ grossmigrants0220_all_pcZ, absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/Table2.xls, nocons e(r2_within) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ = rshk_HIZ lgrshk_HI_w1_5_SCI_cZ) grossmigrants0220_5_pcZ, absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/Table2.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ = rshk_HIZ lgrshk_HI_w1_5_SCI_cZ) grossmigrants0220_all_pcZ, absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/Table2.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_wall_SCI_cZ = rshk_HIZ lgrshk_HI_wall_SCI_cZ) grossmigrants0220_all_pcZ, absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/Table2.xls, addstat(kpstat, e(widstat)) label append

*----------- Table F8 ----------*  

* Most directly/indirectly exposed table (from R)

*----------- Table 3 ----------* 
use ./output/datasets/main/regdat.dta,clear

* Regional robustness 2SLS (social neighbours) 1

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ = rshk_HIZ lgrshk_HI_w1_5_SCI_cZ) pc16_mnfZ, absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/Table3.xls, addstat(kpstat, e(widstat)) label replace

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ = rshk_HIZ lgrshk_HI_w1_5_SCI_cZ) pc16_mnfZ criZ, absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/Table3.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ = rshk_HIZ lgrshk_HI_w1_5_SCI_cZ) pc16_mnfZ criZ grossmigrants0216_5_pcZ, absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/Table3.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ = rshk_HIZ lgrshk_HI_w1_5_SCI_cZ) pcp_aged65ovZ pcp_foreignZ, absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/Table3.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ = rshk_HIZ lgrshk_HI_w1_5_SCI_cZ) km_to_londonZ, absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/Table3.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ = rshk_HIZ lgrshk_HI_w1_5_SCI_cZ) actintusers_2016or17_pcZ, absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/Table3.xls, addstat(kpstat, e(widstat)) label append


*----------- Table F9 ----------* 
use ./output/datasets/main/regdat.dta,clear

* Regional robustness 2SLS 2 (social neighbours and social lag) 

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ = rshk_HIZ lgrshk_HI_w1_5_SCI_cZ) migshockZ lgd_migshock_w1_5_SCI_cZ, absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF9.xls, addstat(kpstat, e(widstat)) label replace

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ = rshk_HIZ lgrshk_HI_w1_5_SCI_cZ) migshockZ lgd_migshock_wall_SCI_cZ, absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF9.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_wall_SCI_cZ = rshk_HIZ lgrshk_HI_wall_SCI_cZ) migshockZ lgd_migshock_wall_SCI_cZ, absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF9.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ = rshk_HIZ lgrshk_HI_w1_5_SCI_cZ) totalimpact_finlosswapyrZ lg_austshock_w1_5_SCI_cZ, absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF9.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ = rshk_HIZ lgrshk_HI_w1_5_SCI_cZ) totalimpact_finlosswapyrZ lg_austshock_wall_SCI_cZ, absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF9.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_wall_SCI_cZ = rshk_HIZ lgrshk_HI_wall_SCI_cZ) totalimpact_finlosswapyrZ lg_austshock_wall_SCI_cZ, absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF9.xls, addstat(kpstat, e(widstat)) label append


*----------- Table F10 ----------* 
use ./output/datasets/main/fetzdatl.dta,clear

* Regional main OLS/IV specs, UKIP analysis

reghdfe pc_ukip rshk_GBRZ_09 rshk_GBRZ_14, absorb(zones_ha year) cluster(zones_la)
outreg2 using ./output/tables/TableF10.xls, nocons e(r2_within) label replace 

reghdfe pc_ukip rshk_GBRZ_09 lgrshk_GBR_w1_5_SCI_cZ_09 rshk_GBRZ_14 lgrshk_GBR_w1_5_SCI_cZ_14, absorb(zones_ha year) cluster(zones_la)
outreg2 using ./output/tables/TableF10.xls, nocons e(r2_within) label append

reghdfe pc_ukip rshk_GBRZ_09 lgrshk_GBR_wall_SCI_cZ_09 rshk_GBRZ_14 lgrshk_GBR_wall_SCI_cZ_14, absorb(zones_ha year) cluster(zones_la)
outreg2 using ./output/tables/TableF10.xls, nocons e(r2_within) label append

ivreghdfe pc_ukip (rshk_GBRZ_09 rshk_GBRZ_14 = rshk_HIZ_09 rshk_HIZ_14), absorb(zones_ha year) cluster(zones_la)
outreg2 using ./output/tables/TableF10.xls, nocons addstat(kpstat, e(widstat)) label append

ivreghdfe pc_ukip (rshk_GBRZ_09 lgrshk_GBR_w1_5_SCI_cZ_09 rshk_GBRZ_14 lgrshk_GBR_w1_5_SCI_cZ_14 = rshk_HIZ_09 lgrshk_HI_w1_5_SCI_cZ_09 rshk_HIZ_14 lgrshk_HI_w1_5_SCI_cZ_14), absorb(zones_ha year) cluster(zones_la)
outreg2 using ./output/tables/TableF10.xls, nocons addstat(kpstat, e(widstat)) label append

ivreghdfe pc_ukip (rshk_GBRZ_09 lgrshk_GBR_wall_SCI_cZ_09 rshk_GBRZ_14 lgrshk_GBR_wall_SCI_cZ_14 = rshk_HIZ_09 lgrshk_HI_wall_SCI_cZ_09 rshk_HIZ_14 lgrshk_HI_wall_SCI_cZ_14), absorb(zones_ha year) cluster(zones_la)
outreg2 using ./output/tables/TableF10.xls, nocons addstat(kpstat, e(widstat)) label append


*----------- Table F11 ----------* 
use ./output/datasets/main/fetzdatl.dta,clear

* Regional main OLS/IV specs, Other EP outcomes analysis

ivreghdfe pc_con (rshk_GBRZ_09 lgrshk_GBR_w1_5_SCI_cZ_09 rshk_GBRZ_14 lgrshk_GBR_w1_5_SCI_cZ_14 = rshk_HIZ_09 lgrshk_HI_w1_5_SCI_cZ_09 rshk_HIZ_14 lgrshk_HI_w1_5_SCI_cZ_14), absorb(zones_ha year) cluster(zones_la)
outreg2 using ./output/tables/TableF11.xls, nocons addstat(kpstat, e(widstat)) label replace

ivreghdfe pc_lab (rshk_GBRZ_09 lgrshk_GBR_w1_5_SCI_cZ_09 rshk_GBRZ_14 lgrshk_GBR_w1_5_SCI_cZ_14 = rshk_HIZ_09 lgrshk_HI_w1_5_SCI_cZ_09 rshk_HIZ_14 lgrshk_HI_w1_5_SCI_cZ_14), absorb(zones_ha year) cluster(zones_la)
outreg2 using ./output/tables/TableF11.xls, nocons addstat(kpstat, e(widstat)) label append

ivreghdfe pc_ldp (rshk_GBRZ_09 lgrshk_GBR_w1_5_SCI_cZ_09 rshk_GBRZ_14 lgrshk_GBR_w1_5_SCI_cZ_14 = rshk_HIZ_09 lgrshk_HI_w1_5_SCI_cZ_09 rshk_HIZ_14 lgrshk_HI_w1_5_SCI_cZ_14), absorb(zones_ha year) cluster(zones_la)
outreg2 using ./output/tables/TableF11.xls, nocons addstat(kpstat, e(widstat)) label append

ivreghdfe pc_green (rshk_GBRZ_09 lgrshk_GBR_w1_5_SCI_cZ_09 rshk_GBRZ_14 lgrshk_GBR_w1_5_SCI_cZ_14 = rshk_HIZ_09 lgrshk_HI_w1_5_SCI_cZ_09 rshk_HIZ_14 lgrshk_HI_w1_5_SCI_cZ_14), absorb(zones_ha year) cluster(zones_la)
outreg2 using ./output/tables/TableF11.xls, nocons addstat(kpstat, e(widstat)) label append

ivreghdfe pc_to (rshk_GBRZ_09 lgrshk_GBR_w1_5_SCI_cZ_09 rshk_GBRZ_14 lgrshk_GBR_w1_5_SCI_cZ_14 = rshk_HIZ_09 lgrshk_HI_w1_5_SCI_cZ_09 rshk_HIZ_14 lgrshk_HI_w1_5_SCI_cZ_14), absorb(zones_ha year) cluster(zones_la)
outreg2 using ./output/tables/TableF11.xls, nocons addstat(kpstat, e(widstat)) label append


*----------- Table F12 ----------* 
use ./output/datasets/main/regdat.dta,clear

* Regional main OLS/IV specs, geographic neighbours

reghdfe pc_leave rshk_GBRZ, absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/TableF12.xls, nocons e(r2_within) label replace 

reghdfe pc_leave rshk_GBRZ lgrshk_GBR_w1_5_LCDi_cZ, absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/TableF12.xls, nocons e(r2_within) label append

reghdfe pc_leave rshk_GBRZ lgrshk_GBR_wall_LCDi_cZ, absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/TableF12.xls, nocons e(r2_within) label append

ivreghdfe pc_leave (rshk_GBRZ = rshk_HIZ), absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF12.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_w1_5_LCDi_cZ = rshk_HIZ lgrshk_HI_w1_5_LCDi_cZ), absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF12.xls, addstat(kpstat, e(widstat)) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_wall_LCDi_cZ = rshk_HIZ lgrshk_HI_wall_LCDi_cZ), absorb(zones_ha) cluster(zones_la) ffirst
outreg2 using ./output/tables/TableF12.xls, addstat(kpstat, e(widstat)) label append


*=============================*
* Individual-level tables     * 
*=============================*

use ./output/datasets/main/besw9dat.dta,clear

*----------- Tables 4 / F13 ----------* 

* 4: Main Probit/IV Probit specs| A8: Average marginal effects

probit euRefVote  rshk_GBRZ  age i.gender i.topqual i.zones_ha [pweight=wt], cluster(zones_la)
outreg2 using ./output/tables/Table4.xls, nocons keep(rshk_GBRZ ) label replace  
*
margins, dydx(rshk_GBRZ) post
outreg2 using ./output/tables/TableF13.xls, nocons keep(rshk_GBRZ) label replace

probit euRefVote  rshk_GBRZ  lgrshk_GBR_w1_5_SCI_cZ age i.gender i.topqual i.zones_ha [pweight=wt], cluster(zones_la)
outreg2 using ./output/tables/Table4.xls, nocons keep(rshk_GBRZ  lgrshk_GBR_w1_5_SCI_cZ) label append  
*
margins, dydx(rshk_GBRZ   lgrshk_GBR_w1_5_SCI_cZ) post
outreg2 using ./output/tables/TableF13.xls, nocons keep(rshk_GBRZ  lgrshk_GBR_w1_5_SCI_cZ) label append

probit euRefVote  rshk_GBRZ  lgrshk_GBR_wall_SCI_cZ age i.gender i.topqual i.zones_ha [pweight=wt], cluster(zones_la)
outreg2 using ./output/tables/Table4.xls, nocons keep(rshk_GBRZ  lgrshk_GBR_wall_SCI_cZ) label append  
*
margins, dydx(rshk_GBRZ   lgrshk_GBR_wall_SCI_cZ) post
outreg2 using ./output/tables/TableF13.xls, nocons keep(rshk_GBRZ  lgrshk_GBR_wall_SCI_cZ) label append

ivreg2 euRefVote (rshk_GBRZ   = rshk_HIZ) age i.gender i.topqual i.zones_ha [pweight=wt], partial(i.gender i.topqual i.zones_ha) cluster(zones_la) ffirst
local kpstat = e(widstat)
ivprobit euRefVote (rshk_GBRZ  = rshk_HIZ) age i.gender i.topqual i.zones_ha [pweight=wt],  vce(cluster zones_la)
outreg2 using ./output/tables/Table4.xls, nocons keep(rshk_GBRZ) addstat(kpstat, `kpstat')  label append 
*
margins, dydx(rshk_GBRZ ) predict(pr fix(rshk_GBRZ )) post
outreg2 using ./output/tables/TableF13.xls, nocons keep(rshk_GBRZ) label append

ivreg2 euRefVote (rshk_GBRZ  lgrshk_GBR_w1_5_SCI_cZ  = rshk_HIZ  lgrshk_HI_w1_5_SCI_cZ) age i.gender i.topqual i.zones_ha [pweight=wt], partial(i.gender i.topqual i.zones_ha) cluster(zones_la) ffirst
local kpstat = e(widstat)
ivprobit euRefVote (rshk_GBRZ  lgrshk_GBR_w1_5_SCI_cZ = rshk_HIZ  lgrshk_HI_w1_5_SCI_cZ) age i.gender i.topqual i.zones_ha [pweight=wt],  vce(cluster zones_la)
outreg2 using ./output/tables/Table4.xls, nocons keep(rshk_GBRZ  lgrshk_GBR_w1_5_SCI_cZ) addstat(kpstat, `kpstat')  label append 
*
margins, dydx(rshk_GBRZ  lgrshk_GBR_w1_5_SCI_cZ) predict(pr fix(rshk_GBRZ   lgrshk_GBR_w1_5_SCI_cZ)) post
outreg2 using ./output/tables/TableF13.xls, nocons keep(rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ) label append

ivreg2 euRefVote (rshk_GBRZ  lgrshk_GBR_wall_SCI_cZ  = rshk_HIZ  lgrshk_HI_wall_SCI_cZ) age i.gender i.topqual i.zones_ha [pweight=wt], partial(i.gender i.topqual i.zones_ha) cluster(zones_la) ffirst
local kpstat = e(widstat)
ivprobit euRefVote (rshk_GBRZ  lgrshk_GBR_wall_SCI_cZ = rshk_HIZ  lgrshk_HI_wall_SCI_cZ) age i.gender i.topqual i.zones_ha [pweight=wt],  vce(cluster zones_la)
outreg2 using ./output/tables/Table4.xls, nocons keep(rshk_GBRZ  lgrshk_GBR_wall_SCI_cZ) addstat(kpstat, `kpstat')  label append 
*
margins, dydx(rshk_GBRZ  lgrshk_GBR_wall_SCI_cZ) predict(pr fix(rshk_GBRZ   lgrshk_GBR_wall_SCI_cZ)) post
outreg2 using ./output/tables/TableF13.xls, nocons keep(rshk_GBRZ lgrshk_GBR_wall_SCI_cZ) label append


*----------- Table 5 ----------* 
use ./output/datasets/main/besw9dat.dta,clear

* Probit robustness specs, social neighbours

probit euRefVote rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ##i.riskPoverty  age gender i.topqual i.zones_ha [pweight=wt], cluster(zones_la)
outreg2 using ./output/tables/Table5.xls, nocons keep(rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ##i.riskPoverty) label replace

probit euRefVote rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ##i.unemployed  age gender i.topqual i.zones_ha [pweight=wt], cluster(zones_la)
outreg2 using ./output/tables/Table5.xls, nocons keep(rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ##i.unemployed) label append

probit euRefVote rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ##i.student age gender i.topqual i.zones_ha [pweight=wt], cluster(zones_la)
outreg2 using ./output/tables/Table5.xls,  nocons keep(rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ##i.student)label append 

probit euRefVote rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ##i.polintov1hr age gender i.topqual i.zones_ha [pweight=wt], cluster(zones_la)
outreg2 using ./output/tables/Table5.xls, nocons keep(rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ##i.polintov1hr) label append 

probit euRefVote rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ##i.bothparsbrit age gender i.topqual i.zones_ha [pweight=wt], cluster(zones_la)
outreg2 using ./output/tables/Table5.xls, nocons keep(rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ##i.bothparsbrit) label append 


*----------- Table F14 ----------* 
use ./output/datasets/main/besw9dat.dta,clear

* Probit robustness specs, social lag

probit euRefVote rshk_GBRZ c.lgrshk_GBR_wall_SCI_cZ##i.riskPoverty  age gender i.topqual i.zones_ha [pweight=wt], cluster(zones_la)
outreg2 using ./output/tables/TableF14.xls, nocons keep(rshk_GBRZ c.lgrshk_GBR_wall_SCI_cZ##i.riskPoverty) label replace

probit euRefVote rshk_GBRZ c.lgrshk_GBR_wall_SCI_cZ##i.unemployed  age gender i.topqual i.zones_ha [pweight=wt], cluster(zones_la)
outreg2 using ./output/tables/TableF14.xls, nocons keep(rshk_GBRZ c.lgrshk_GBR_wall_SCI_cZ##i.unemployed) label append

probit euRefVote rshk_GBRZ c.lgrshk_GBR_wall_SCI_cZ##i.student age gender i.topqual i.zones_ha [pweight=wt], cluster(zones_la)
outreg2 using ./output/tables/TableF14.xls,  nocons keep(rshk_GBRZ c.lgrshk_GBR_wall_SCI_cZ##i.student)label append 

probit euRefVote rshk_GBRZ c.lgrshk_GBR_wall_SCI_cZ##i.polintov1hr age gender i.topqual i.zones_ha [pweight=wt], cluster(zones_la)
outreg2 using ./output/tables/TableF14.xls, nocons keep(rshk_GBRZ c.lgrshk_GBR_wall_SCI_cZ##i.polintov1hr) label append 

probit euRefVote rshk_GBRZ c.lgrshk_GBR_wall_SCI_cZ##i.bothparsbrit age gender i.topqual i.zones_ha [pweight=wt], cluster(zones_la)
outreg2 using ./output/tables/TableF14.xls, nocons keep(rshk_GBRZ c.lgrshk_GBR_wall_SCI_cZ##i.bothparsbrit) label append 

*----------- Table F15 ----------* 
use ./output/datasets/main/besw9dat.dta,clear

* IV LPM robustness specs, social neighbours

ivreg2 euRefVote (rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ c.lgrshk_GBR_w1_5_SCI_cZ#i.riskPoverty  = rshk_HIZ  c.lgrshk_HI_w1_5_SCI_cZ c.lgrshk_HI_w1_5_SCI_cZ#i.riskPoverty) i.riskPoverty age i.gender i.topqual i.zones_ha [pweight=wt], partial(i.gender i.topqual i.zones_ha) cluster(zones_la) ffirst
local kpstat = e(widstat)
outreg2 using ./output/tables/TableF15.xls, nocons addstat(kpstat, `kpstat')  label replace 

ivreg2 euRefVote (rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ c.lgrshk_GBR_w1_5_SCI_cZ#i.unemployed  = rshk_HIZ  c.lgrshk_HI_w1_5_SCI_cZ c.lgrshk_HI_w1_5_SCI_cZ#i.unemployed) i.unemployed age i.gender i.topqual i.zones_ha [pweight=wt], partial(i.gender i.topqual i.zones_ha) cluster(zones_la) ffirst
local kpstat = e(widstat)
outreg2 using ./output/tables/TableF15.xls, nocons addstat(kpstat, `kpstat')  label append 

ivreg2 euRefVote (rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ c.lgrshk_GBR_w1_5_SCI_cZ#i.student  = rshk_HIZ  c.lgrshk_HI_w1_5_SCI_cZ c.lgrshk_HI_w1_5_SCI_cZ#i.student) i.student age i.gender i.topqual i.zones_ha [pweight=wt], partial(i.gender i.topqual i.zones_ha) cluster(zones_la) ffirst
local kpstat = e(widstat)
outreg2 using ./output/tables/TableF15.xls, nocons addstat(kpstat, `kpstat')  label append 

ivreg2 euRefVote (rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ c.lgrshk_GBR_w1_5_SCI_cZ#i.polintov1hr  = rshk_HIZ  c.lgrshk_HI_w1_5_SCI_cZ c.lgrshk_HI_w1_5_SCI_cZ#i.polintov1hr) i.polintov1hr age i.gender i.topqual i.zones_ha [pweight=wt], partial(i.gender i.topqual i.zones_ha) cluster(zones_la) ffirst
local kpstat = e(widstat)
outreg2 using ./output/tables/TableF15.xls, nocons addstat(kpstat, `kpstat')  label append 

ivreg2 euRefVote (rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ c.lgrshk_GBR_w1_5_SCI_cZ#i.bothparsbrit  = rshk_HIZ  c.lgrshk_HI_w1_5_SCI_cZ c.lgrshk_HI_w1_5_SCI_cZ#i.bothparsbrit) i.bothparsbrit age i.gender i.topqual i.zones_ha [pweight=wt], partial(i.gender i.topqual i.zones_ha) cluster(zones_la) ffirst
local kpstat = e(widstat)
outreg2 using ./output/tables/TableF15.xls, nocons addstat(kpstat, `kpstat')  label append 


*----------- Table F16 ----------* 
use ./output/datasets/main/besw9dat.dta,clear

* IV LPM robustness specs, social lag

ivreg2 euRefVote (rshk_GBRZ c.lgrshk_GBR_wall_SCI_cZ c.lgrshk_GBR_wall_SCI_cZ#i.riskPoverty  = rshk_HIZ  c.lgrshk_HI_wall_SCI_cZ c.lgrshk_HI_wall_SCI_cZ#i.riskPoverty) i.riskPoverty age i.gender i.topqual i.zones_ha [pweight=wt], partial(i.gender i.topqual i.zones_ha) cluster(zones_la) ffirst
local kpstat = e(widstat)
outreg2 using ./output/tables/TableF16.xls, nocons addstat(kpstat, `kpstat')  label replace 

ivreg2 euRefVote (rshk_GBRZ c.lgrshk_GBR_wall_SCI_cZ c.lgrshk_GBR_wall_SCI_cZ#i.unemployed  = rshk_HIZ  c.lgrshk_HI_wall_SCI_cZ c.lgrshk_HI_wall_SCI_cZ#i.unemployed) i.unemployed age i.gender i.topqual i.zones_ha [pweight=wt], partial(i.gender i.topqual i.zones_ha) cluster(zones_la) ffirst
local kpstat = e(widstat)
outreg2 using ./output/tables/TableF16.xls, nocons addstat(kpstat, `kpstat')  label append 

ivreg2 euRefVote (rshk_GBRZ c.lgrshk_GBR_wall_SCI_cZ c.lgrshk_GBR_wall_SCI_cZ#i.student  = rshk_HIZ  c.lgrshk_HI_wall_SCI_cZ c.lgrshk_HI_wall_SCI_cZ#i.student) i.student age i.gender i.topqual i.zones_ha [pweight=wt], partial(i.gender i.topqual i.zones_ha) cluster(zones_la) ffirst
local kpstat = e(widstat)
outreg2 using ./output/tables/TableF16.xls, nocons addstat(kpstat, `kpstat')  label append 

ivreg2 euRefVote (rshk_GBRZ c.lgrshk_GBR_wall_SCI_cZ c.lgrshk_GBR_wall_SCI_cZ#i.polintov1hr  = rshk_HIZ  c.lgrshk_HI_wall_SCI_cZ c.lgrshk_HI_wall_SCI_cZ#i.polintov1hr) i.polintov1hr age i.gender i.topqual i.zones_ha [pweight=wt], partial(i.gender i.topqual i.zones_ha) cluster(zones_la) ffirst
local kpstat = e(widstat)
outreg2 using ./output/tables/TableF16.xls, nocons addstat(kpstat, `kpstat')  label append 

ivreg2 euRefVote (rshk_GBRZ c.lgrshk_GBR_wall_SCI_cZ c.lgrshk_GBR_wall_SCI_cZ#i.bothparsbrit  = rshk_HIZ  c.lgrshk_HI_wall_SCI_cZ c.lgrshk_HI_wall_SCI_cZ#i.bothparsbrit) i.bothparsbrit age i.gender i.topqual i.zones_ha [pweight=wt], partial(i.gender i.topqual i.zones_ha) cluster(zones_la) ffirst
local kpstat = e(widstat)
outreg2 using ./output/tables/TableF16.xls, nocons addstat(kpstat, `kpstat')  label append 


*----------- Table E1 ----------* 

* Endogenous weights OLS/IV specs

use ./output/datasets/main/endwdat.dta,clear

reghdfe grossmigrants0220Z rshk_GBRZ, absorb(ITL321XCD_O) cluster(ITL321XCD_O zones_la_D)
outreg2 using ./output/tables/TableE1.xls, nocons e(r2_within) label replace

ivreghdfe grossmigrants0220Z (rshk_GBRZ =rshk_HIZ), absorb(ITL321XCD_O) cluster(ITL321XCD_O zones_la_D) ffirst
outreg2 using ./output/tables/TableE1.xls, nocons  addstat(kpstat, e(widstat)) label append

reghdfe SCI_cZ rshk_GBRZ, absorb(ITL321XCD_O) cluster(ITL321XCD_O zones_la_D)
outreg2 using ./output/tables/TableE1.xls, nocons e(r2_within) label append

ivreghdfe SCI_cZ (rshk_GBRZ =rshk_HIZ), absorb(ITL321XCD_O) cluster(ITL321XCD_O zones_la_D) ffirst
outreg2 using ./output/tables/TableE1.xls, nocons  addstat(kpstat, e(widstat)) label append

reghdfe SCI_cZ rshk_GBRZ grossmigrants0220Z, absorb(ITL321XCD_O) cluster(ITL321XCD_O zones_la_D)
outreg2 using ./output/tables/TableE1.xls, nocons e(r2_within) label append

ivreghdfe SCI_cZ (rshk_GBRZ =rshk_HIZ) grossmigrants0220Z, absorb(ITL321XCD_O) cluster(ITL321XCD_O zones_la_D) ffirst
outreg2 using ./output/tables/TableE1.xls, nocons  addstat(kpstat, e(widstat)) label append

*----------- Table E2 ----------* 

* Endogenous sets OLS/IV specs

use ./output/datasets/main/endsdat.dta,clear

ivreghdfe in_nn5_SCI_c (rshk_GBRZ =rshk_HIZ), absorb(ITL321XCD_O) cluster(ITL321XCD_O zones_la_D) ffirst
outreg2 using ./output/tables/TableE2.xls, nocons  addstat(kpstat, e(widstat)) label replace

ivreghdfe in_nn10_SCI_c (rshk_GBRZ =rshk_HIZ), absorb(ITL321XCD_O) cluster(ITL321XCD_O zones_la_D) ffirst
outreg2 using ./output/tables/TableE2.xls, nocons  addstat(kpstat, e(widstat)) label append

ivreghdfe in_nn15_SCI_c (rshk_GBRZ =rshk_HIZ), absorb(ITL321XCD_O) cluster(ITL321XCD_O zones_la_D) ffirst
outreg2 using ./output/tables/TableE2.xls, nocons  addstat(kpstat, e(widstat)) label append

ivreghdfe in_nn20_SCI_c (rshk_GBRZ =rshk_HIZ), absorb(ITL321XCD_O) cluster(ITL321XCD_O zones_la_D) ffirst
outreg2 using ./output/tables/TableE2.xls, nocons  addstat(kpstat, e(widstat)) label append

ivreghdfe in_nn25_SCI_c (rshk_GBRZ =rshk_HIZ), absorb(ITL321XCD_O) cluster(ITL321XCD_O zones_la_D) ffirst
outreg2 using ./output/tables/TableE2.xls, nocons  addstat(kpstat, e(widstat)) label append



