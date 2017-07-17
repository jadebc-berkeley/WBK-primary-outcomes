capture log close
log using "~/documents/crg/wash-benefits/Kenya/src/primary/dm/6_mortality.log", replace

*********************************************
* WASH Benefits Kenya
* Primary outcome analysis

* Make analysis dataset: mortality

* Reads in raw data
* Outputs 1 dataset with individual level data
* from endline

* by Jade Benjamin-Chung (jadebc@berkeley.edu)
*********************************************
clear all
set more off
cd "~/Dropbox/WASHB-Kenya-Data/0-Untouched-Data/1-Main-survey/"


use "3_Endline/msP_mortality_20161103.dta", clear

* merge in clusterid
merge m:1 childid using "msP_child_IDchar_20161103.dta"
* drop if not in death dataset
drop if _m==2
drop _m

keep childid hhid clusterid childtype pregloss childdeath 

tostring clusterid, replace

* merge in treatment info
merge m:1 clusterid using "~/Dropbox/WASHB-Kenya-Data/0-Untouched-Data/0-Treatment-assignments/washb-kenya-tr.dta"

drop _m

order tr block clusterid hhid childid childtype pregloss childdeath 

* drop if not live birth
drop if pregloss ==1
drop pregloss

* recode childdeath
gen childdeathnew=childdeath
recode childdeathnew (2=0)
drop childdeath
ren childdeathnew childdeath

outsheet using "~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/mortality.csv", comma replace




log close
