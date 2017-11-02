capture log close
log using "~/Documents/CRG/wash-benefits/kenya/src/primary/analysis/icc.log", replace
*********************************************
* WASH Benefits Kenya
* Primary outcome analysis 

* Estimate cluster-level ICCs for primary outcomes

* input: diarrhea.dta and endline-anthro.dta

* by Jade Benjamin-Chung (jadebc@berkeley.edu)
*********************************************
use "~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/diarrhea.dta", clear
keep if dcohort==1
keep if time>0
destring clusterid, replace
loneway diarr7 clusterid


use "~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/endline-anthro.dta", clear
drop if haz==.
keep if targetchild==1
destring clusterid, replace
loneway haz clusterid


log close
