*=============================*
* Regional tables             * 
*=============================*

use ./output/datasets/main/regdat.dta,clear


*----------- Table 1 ----------* 

* Regional main OLS/IV specs

reghdfe pc_leave rshk_GBRZ  lgrshk_GBR_w1_5_SCI_cZ, absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/Table1.xls, nocons e(r2_within) label replace 

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ = rshk_USAZ lgrshk_USA_w1_5_SCI_cZ), absorb(zones_ha) cluster(zones_la) ffirst
local swf1=e(first)[rownumb(e(first),"SWF"),colnumb(e(first),"rshk_GBRZ")]
local swf2=e(first)[rownumb(e(first),"SWF"),colnumb(e(first),"lgrshk_GBR_w1_5_SCI_cZ")]
outreg2 using ./output/tables/Table1.xls, addstat(swf1, `swf1', swf2, `swf2') label append

reghdfe pc_leave rshk_GBRZ  lgrshk_GBR_w1_5_SCI_cZ  lgrshk_GBR_w6_10_SCI_cZ, absorb(zones_ha) cluster(zones_la)
outreg2 using ./output/tables/Table1.xls, nocons  e(r2_within) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ lgrshk_GBR_w6_10_SCI_cZ = rshk_USAZ lgrshk_USA_w1_5_SCI_cZ lgrshk_USA_w6_10_SCI_cZ ), absorb(zones_ha) cluster(zones_la) ffirst
local swf1=e(first)[rownumb(e(first),"SWF"),colnumb(e(first),"rshk_GBRZ")]
local swf2=e(first)[rownumb(e(first),"SWF"),colnumb(e(first)," lgrshk_GBR_w1_5_SCI_cZ")]
local swf3=e(first)[rownumb(e(first),"SWF"),colnumb(e(first)," lgrshk_GBR_w6_10_SCI_cZ")]
outreg2 using ./output/tables/Table1.xls, nocons  addstat(swf1, `swf1', swf2, `swf2', swf3, `swf3') label append


*----------- Table 2 ----------* 

* Regional robustness OLS specs

reghdfe pc_leave rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ pc16_mnfZ, absorb(zones_ha) vce(cluster zones_la)
outreg2 using ./output/tables/Table2.xls, nocons label e(r2_within) replace

reghdfe pc_leave rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ pc16_mnfZ criZ, absorb(zones_ha) vce(cluster zones_la)
outreg2 using ./output/tables/Table2.xls, nocons label e(r2_within) append

reghdfe pc_leave rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ pc16_mnfZ criZ netinmigrants0216_pcZ, absorb(zones_ha) vce(cluster zones_la)
outreg2 using ./output/tables/Table2.xls, nocons label e(r2_within) append

reghdfe pc_leave rshk_GBRZ  lgrshk_GBR_w1_5_SCI_cZ pcp_aged65ovZ pcp_foreignZ, absorb(zones_ha) vce(cluster zones_la)
outreg2 using ./output/tables/Table2.xls, nocons label e(r2_within) append


*----------- Table D1 ----------* 
use ./output/datasets/main/endwdat.dta,clear

* Endogenous weights check OLS/IV specs

reghdfe log_SCI_c rshk_GBRZ, absorb(ITL321XCD_O) cluster(ITL321XCD_O ITL321XCD_D)
outreg2 using ./output/tables/TableD1.xls, nocons label e(r2_within) replace

ivreghdfe log_SCI_c (rshk_GBRZ =rshk_USAZ), absorb(ITL321XCD_O) cluster(ITL321XCD_O ITL321XCD_D) ffirst
outreg2 using ./output/tables/TableD1.xls, nocons label e(r2_within) append

use ./output/datasets/main/endsdat.dta,clear

reghdfe in_nn10_SCI_c rshk_GBRZ, absorb(ITL321XCD_O) cluster(ITL321XCD_O ITL321XCD_D)
outreg2 using ./output/tables/TableD1.xls, nocons label e(r2_within) append

ivreghdfe in_nn10_SCI_c (rshk_GBRZ =rshk_USAZ), absorb(ITL321XCD_O) cluster(ITL321XCD_O ITL321XCD_D) ffirst
outreg2 using ./output/tables/TableD1.xls, nocons label e(r2_within) append


*----------- Table E1 ----------* 

use ./output/datasets/main/regdat.dta,clear


* Regional main OLS/IV specs, weq

reghdfe pc_leave rshk_GBRZ lgrshk_GBR_weq1_5_SCI_cZ, absorb(zones_ha) cluster(zones_la)
outreg2 using ./output/tables/TableE1.xls, nocons e(r2_within) label replace 

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_weq1_5_SCI_cZ = rshk_USAZ lgrshk_USA_weq1_5_SCI_cZ), absorb(zones_ha) cluster(zones_la) ffirst
local swf1=e(first)[rownumb(e(first),"SWF"),colnumb(e(first),"rshk_GBRZ")]
local swf2=e(first)[rownumb(e(first),"SWF"),colnumb(e(first),"lgrshk_GBR_weq1_5_SCI_cZ")]
outreg2 using ./output/tables/TableE1.xls, nocons addstat(swf1, `swf1', swf2, `swf2') label append

reghdfe pc_leave rshk_GBRZ lgrshk_GBR_weq1_5_SCI_cZ lgrshk_GBR_weq6_10_SCI_cZ, absorb(zones_ha) cluster(zones_la)
outreg2 using ./output/tables/TableE1.xls, nocons e(r2_within) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_weq1_5_SCI_cZ lgrshk_GBR_weq6_10_SCI_cZ = rshk_USAZ lgrshk_USA_weq1_5_SCI_cZ lgrshk_USA_weq6_10_SCI_cZ), absorb(zones_ha) cluster(zones_la) ffirst
local swf1=e(first)[rownumb(e(first),"SWF"),colnumb(e(first),"rshk_GBRZ")]
local swf2=e(first)[rownumb(e(first),"SWF"),colnumb(e(first),"lgrshk_GBR_weq1_5_SCI_cZ")]
local swf3=e(first)[rownumb(e(first),"SWF"),colnumb(e(first),"lgrshk_GBR_weq6_10_SCI_cZ")]
outreg2 using ./output/tables/TableE1.xls, nocons addstat(swf1, `swf1', swf2, `swf2', swf3, `swf3') label append

*----------- Table E2 ----------* 

* Regional main OLS/IV specs, weighted by Votes_Cast

reghdfe pc_leave rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ [aweight = Votes_Cast], absorb(zones_ha) cluster(zones_la)
outreg2 using ./output/tables/TableE2.xls, nocons e(r2_within) label replace 

ivreghdfe pc_leave (rshk_GBRZ  lgrshk_GBR_w1_5_SCI_cZ = rshk_USAZ lgrshk_USA_w1_5_SCI_cZ) [aweight = Votes_Cast], absorb(zones_ha) cluster(zones_la) ffirst
local swf1=e(first)[rownumb(e(first),"SWF"),colnumb(e(first),"rshk_GBRZ")]
local swf2=e(first)[rownumb(e(first),"SWF"),colnumb(e(first),"lgrshk_GBR_w1_5_SCI_cZ")]
outreg2 using ./output/tables/TableE2.xls, nocons addstat(swf1, `swf1', swf2, `swf2') label append

reghdfe pc_leave rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ  lgrshk_GBR_w6_10_SCI_cZ [aweight = Votes_Cast], absorb(zones_ha) cluster(zones_la)
outreg2 using ./output/tables/TableE2.xls, nocons e(r2_within) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ lgrshk_GBR_w6_10_SCI_cZ = rshk_USAZ lgrshk_USA_w1_5_SCI_cZ lgrshk_USA_w6_10_SCI_cZ) [aweight = Votes_Cast], absorb(zones_ha) cluster(zones_la) ffirst
local swf1=e(first)[rownumb(e(first),"SWF"),colnumb(e(first),"rshk_GBRZ")]
local swf2=e(first)[rownumb(e(first),"SWF"),colnumb(e(first),"lgrshk_GBR_w1_5_SCI_cZ")]
local swf3=e(first)[rownumb(e(first),"SWF"),colnumb(e(first)," lgrshk_GBR_w6_10_SCI_cZ")]
outreg2 using ./output/tables/TableE2.xls, nocons addstat(swf1, `swf1', swf2, `swf2', swf3, `swf3') label append


*----------- Table E3 ----------* 

* Regional main OLS/IV specs, geographic neighbours
reghdfe pc_leave rshk_GBRZ  lgrshk_GBR_w1_5_LCDi_cZ, absorb(zones_ha) cluster(zones_la) 
outreg2 using ./output/tables/TableE3.xls, nocons e(r2_within) label replace 

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_w1_5_LCDi_cZ = rshk_USAZ lgrshk_USA_w1_5_LCDi_cZ), absorb(zones_ha) cluster(zones_la) ffirst
local swf1=e(first)[rownumb(e(first),"SWF"),colnumb(e(first),"rshk_GBRZ")]
local swf2=e(first)[rownumb(e(first),"SWF"),colnumb(e(first),"lgrshk_GBR_w1_5_LCDi_cZ")]
outreg2 using ./output/tables/TableE3.xls, addstat(swf1, `swf1', swf2, `swf2') label append

reghdfe pc_leave rshk_GBRZ  lgrshk_GBR_w1_5_LCDi_cZ  lgrshk_GBR_w6_10_LCDi_cZ, absorb(zones_ha) cluster(zones_la)
outreg2 using ./output/tables/TableE3.xls, nocons  e(r2_within) label append

ivreghdfe pc_leave (rshk_GBRZ lgrshk_GBR_w1_5_LCDi_cZ lgrshk_GBR_w6_10_LCDi_cZ = rshk_USAZ lgrshk_USA_w1_5_LCDi_cZ lgrshk_USA_w6_10_LCDi_cZ ), absorb(zones_ha) cluster(zones_la) ffirst
local swf1=e(first)[rownumb(e(first),"SWF"),colnumb(e(first),"rshk_GBRZ")]
local swf2=e(first)[rownumb(e(first),"SWF"),colnumb(e(first)," lgrshk_GBR_w1_5_LCDi_cZ")]
local swf3=e(first)[rownumb(e(first),"SWF"),colnumb(e(first)," lgrshk_GBR_w6_10_LCDi_cZ")]
outreg2 using ./output/tables/TableE3.xls, nocons  addstat(swf1, `swf1', swf2, `swf2', swf3, `swf3') label append

*=============================*
* Individual-level tables     * 
*=============================*

use ./output/datasets/main/besw9dat.dta,clear

*----------- Tables 3 / E4 ----------* 

* 4: Main Probit/IV Probit specs (ivreg specs for AP F stats) | A8: Average marginal effects

probit euRefVote  rshk_GBRZ  lgrshk_GBR_w1_5_SCI_cZ age i.gender i.topqual i.zones_ha [pweight=wt], cluster(zones_la)
outreg2 using ./output/tables/Table3.xls, nocons keep(rshk_GBRZ  lgrshk_GBR_w1_5_SCI_cZ) label replace  
*
margins, dydx(rshk_GBRZ   lgrshk_GBR_w1_5_SCI_cZ) post
outreg2 using ./output/tables/TableE4.xls, nocons keep(rshk_GBRZ  lgrshk_GBR_w1_5_SCI_cZ) label replace

ivreg2 euRefVote (rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ = rshk_USAZ  lgrshk_USA_w1_5_SCI_cZ) age i.gender i.topqual i.zones_ha [pweight=wt], partial(i.gender i.topqual i.zones_ha) cluster(zones_la) ffirst
local swf1=e(first)[rownumb(e(first),"SWF"),colnumb(e(first),"rshk_GBRZ")]
local swf2=e(first)[rownumb(e(first),"SWF"),colnumb(e(first)," lgrshk_GBR_w1_5_SCI_cZ")]
ivprobit euRefVote (rshk_GBRZ  lgrshk_GBR_w1_5_SCI_cZ = rshk_USAZ  lgrshk_USA_w1_5_SCI_cZ) age i.gender i.topqual i.zones_ha [pweight=wt],  vce(cluster zones_la)
outreg2 using ./output/tables/Table3.xls, nocons keep(rshk_GBRZ  lgrshk_GBR_w1_5_SCI_cZ) addstat(swf1, `swf1', swf2, `swf2')  label append 
*
margins, dydx(rshk_GBRZ  lgrshk_GBR_w1_5_SCI_cZ) predict(pr fix(rshk_GBRZ   lgrshk_GBR_w1_5_SCI_cZ)) post
outreg2 using ./output/tables/TableE4.xls, nocons keep(rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ) label append

probit euRefVote  rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ lgrshk_GBR_w6_10_SCI_cZ age i.gender i.topqual i.zones_ha [pweight=wt],  cluster(zones_la)
outreg2 using ./output/tables/Table3.xls, nocons keep(rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ lgrshk_GBR_w6_10_SCI_cZ) label append
*
margins, dydx(rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ lgrshk_GBR_w6_10_SCI_cZ) post
outreg2 using ./output/tables/TableE4.xls, nocons keep(rshk_GBRZ lgrshk_GBR_w1_5_SCI_cZ  lgrshk_GBR_w6_10_SCI_cZ) label append

ivreg2 euRefVote (rshk_GBRZ  lgrshk_GBR_w1_5_SCI_cZ  lgrshk_GBR_w6_10_SCI_cZ = rshk_USAZ  lgrshk_USA_w1_5_SCI_cZ  lgrshk_USA_w6_10_SCI_cZ) age i.gender i.topqual i.zones_ha [pweight=wt], partial(i.gender i.topqual i.zones_ha) cluster(zones_la) ffirst
local swf1=e(first)[rownumb(e(first),"SWF"),colnumb(e(first),"rshk_GBRZ")]
local swf2=e(first)[rownumb(e(first),"SWF"),colnumb(e(first),"lgrshk_GBR_w1_5_SCI_cZ")]
local swf3=e(first)[rownumb(e(first),"SWF"),colnumb(e(first),"lgrshk_GBR_w6_10_SCI_cZ")]
ivprobit euRefVote (rshk_GBRZ  lgrshk_GBR_w1_5_SCI_cZ  lgrshk_GBR_w6_10_SCI_cZ=rshk_USAZ  lgrshk_USA_w1_5_SCI_cZ  lgrshk_USA_w6_10_SCI_cZ) age i.gender i.topqual i.zones_ha [pweight=wt],  vce(cluster zones_la)
outreg2 using ./output/tables/Table3.xls, nocons keep(rshk_GBRZ  lgrshk_GBR_w1_5_SCI_cZ  lgrshk_GBR_w6_10_SCI_cZ) addstat(swf1, `swf1', swf2, `swf2', swf3, `swf3') label append  
*
margins, dydx(rshk_GBRZ  lgrshk_GBR_w1_5_SCI_cZ  lgrshk_GBR_w6_10_SCI_cZ) predict(pr fix(rshk_GBRZ   lgrshk_GBR_w1_5_SCI_cZ  lgrshk_GBR_w6_10_SCI_cZ)) post
outreg2 using ./output/tables/TableE4.xls, nocons keep(rshk_GBRZ  lgrshk_GBR_w1_5_SCI_cZ  lgrshk_GBR_w6_10_SCI_cZ ) label append


*----------- Table 4 ----------* 

* Probit robustness specs 

probit euRefVote rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ##i.riskPoverty  age gender i.topqual i.zones_ha [pweight=wt], cluster(zones_la)
outreg2 using ./output/tables/Table4.xls, nocons keep(rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ##i.riskPoverty) label replace

probit euRefVote rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ##i.unemployed  age gender i.topqual i.zones_ha [pweight=wt], cluster(zones_la)
outreg2 using ./output/tables/Table4.xls, nocons keep(rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ##i.unemployed) label append

probit euRefVote rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ##i.student age gender i.topqual i.zones_ha [pweight=wt], cluster(zones_la)
outreg2 using ./output/tables/Table4.xls,  nocons keep(rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ##i.student)label append 

probit euRefVote rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ##i.polintov1hr age gender i.topqual i.zones_ha [pweight=wt], cluster(zones_la)
outreg2 using ./output/tables/Table4.xls, nocons keep(rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ##i.polintov1hr) label append 

probit euRefVote rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ##i.bothparsbrit age gender i.topqual i.zones_ha [pweight=wt], cluster(zones_la)
outreg2 using ./output/tables/Table4.xls, nocons keep(rshk_GBRZ c.lgrshk_GBR_w1_5_SCI_cZ##i.bothparsbrit) label append 

