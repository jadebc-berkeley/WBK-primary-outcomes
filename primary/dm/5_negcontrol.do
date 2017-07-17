capture log close
log using "~/documents/crg/wash-benefits/Kenya/src/primary/dm/5_negcontrol.log", replace

*********************************************
* WASH Benefits Kenya
* Primary outcome analysis

* Make analysis dataset: negative control
* Bruise/abrasion

* Reads in raw data
* Outputs 1 dataset with individual level data
* for midline and endline 

* by Jade Benjamin-Chung (jadebc@berkeley.edu)
*********************************************
clear all
set more off
cd "~/Dropbox/WASHB-Kenya-Data/0-Untouched-Data/1-Main-survey/"


*********************************************
* Prep midline child-level variables
*********************************************

* ---------------------------------------------
* Child diarrhea
* ---------------------------------------------
use "2_Midline/msP_ml_child_diarr_20161010.dta", clear
tostring hhid childid, replace

gen bruise2=0
replace bruise2=1 if bruisea ==1 | bruiseb == 1 | bruisec ==1
replace bruise2=. if (bruisea ==. | bruisea==99) & (bruiseb ==. | bruiseb==99) & (bruisec ==. | bruisec==99)

gen bruise7 = 0
replace bruise7 = 1 if bruise2==1 | bruised==1
replace bruise7 = . if bruise2==. & (bruised==. | bruised==99)


keep childid hhid clusterid childid bruise2 bruise7 

gen time=1


tempfile midline
save `midline'


*********************************************
* Prep endline child-level variables
*********************************************

* ---------------------------------------------
* Child diarrhea
* ---------------------------------------------
use "3_Endline/msP_el_child_diarr_20161010.dta", clear
tostring childid hhid, replace


gen bruise2=0
replace bruise2=1 if bruisea_ad ==1 | bruiseb_ad == 1 | bruisec_ad ==1
replace bruise2=. if (bruisea_ad ==. | bruisea_ad==99) & (bruiseb_ad ==. | bruiseb_ad==99) & (bruisec_ad ==. | bruisec_ad==99)

gen bruise7 = 0
replace bruise7 = 1 if bruise2==1 | bruised==1
replace bruise7 = . if bruise2==. & (bruised_ad==. | bruised_ad==99)

keep childid hhid clusterid childid bruise2 bruise7 

gen time =2

tempfile endline
save `endline'

*********************************************
* Combine individual-level variables from midline and endline
*********************************************
use `midline', clear
append using `endline'

tempfile childdata
save `childdata'

* ---------------------------------------------
* Child age and sex
* ---------------------------------------------
preserve
use "msP_child_IDchar_20161103.dta", clear
tostring childid hhid, replace

* indicators for child type
gen sibling = (childtype>2)
gen targetchild= (childtype<=2)
gen newbirth = (childtype==6)

* rename date vars
ren ms_bl_date date_base
ren ms_ml_ad_date date_mid
ren ms_el_ad_date date_end

* calculate age
ren DOB dob

recode sex (1=0) (2=1) 
label define sexl 0 "Male" 1 "Female"
label values sex sexl

keep clusterid compoundid hhid childid date* sibling targetchild newbirth date* dob sex

tempfile agesex
save `agesex'
restore

merge m:1 childid using `agesex'
drop _m

* survey date
gen date = .
replace date=date_base if time==0
replace date=date_mid if time==1
replace date=date_end if time==2

format date* %d
drop date_*

drop if bruise2==. & bruise7==. & date==.

gen month=month(date)

* create age at each time point
gen double aged=(date-dob)
gen double agem=(date-dob)/30.4167
gen double agey=(date-dob)/365.25

gen dcohort=0
replace dcohort=1 if agey<3 & targetchild==0 & bruise7!=. & time==0
replace dcohort=1 if targetchild==1 & bruise7!=. & time==1
replace dcohort=1 if targetchild==1 & bruise7!=. & time==2
replace dcohort=0 if agem==.


* ---------------------------------------------
* merge in block
* ---------------------------------------------
tostring clusterid, replace
merge m:1 clusterid using "/Volumes/0-Treatment-assignments/washb-kenya-tr.dta"

* drop if there are no children in that compound
* no data for clusterid 11027
drop if _m==2
drop _m

sort hhid childid time
order block clusterid compoundid hhid childid time targetchild tr date 

outsheet using "~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/bruise.csv", replace comma 
save  "~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/bruise.dta", replace 

log close
