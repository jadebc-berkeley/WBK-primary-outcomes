
capture log close
log using "~/documents/crg/wash-benefits/Kenya/src/primary/dm/3_flowchart.log", replace

*********************************************
* WASH Benefits Kenya

* Primary outcome analysis analysis
* Clean data for CONSORT flow chart

* by Jade
*********************************************
clear all
set more off
cd "~/Dropbox/WBK-primary-analysis/Data/Untouched/"

*********************************************
* hosuehold count
*********************************************

* ---------------------------------------------
* merge tx assignment into data that tracks
* households at each time point
* ---------------------------------------------

use "msP_household_tracking_20160909.dta", clear
tostring hhid clusterid, replace

* merge in block
merge m:1 clusterid using "washb-kenya-blind-tr.dta"
drop _m

keep clusterid hhid block tr indata*

outsheet using "~/dropbox/wash benefits/kenya/primary-analysis/data/temp/compounds.csv", replace comma


*********************************************
* reason for withdrawal
*********************************************

/* ---------------------------------------------
* prepare dataset with reasons for withdrawal
* & output csv to read into R
* ---------------------------------------------

use "~/Dropbox/WBK-primary-analysis/Data/Untouched/3_Endline/01. WASHB_Midline_Endline_data_count_cleaned.dta", clear

* merge in block
merge 1:1 dataid using "~/Dropbox/WBK-primary-analysis/Data/Untouched/3_Endline/02. WASHB_Endline_Arm_Identification.dta"
drop _m

ren cluster clusterid
ren arm tr

label drop lblarm 
label define arml 1 "Sanitation" 2 "Handwashing" 3 "Water" 4 "Nutrition" 5 "WSH" 6 "Nutrition + WSH" 7 "Control" 
label values tr arml


* dropped out at midline or endline
gen dropout=0
replace dropout=1 if status_midline==2 | status_endline==2 | status_endline==.
replace dropout=0 if status_endline==1

* manually cleaning while we wait for Kishor
replace status_midline=1 if dataid=="02604" 
replace status_midline=1 if dataid=="03704"
replace status_endline=0 if dataid=="02604" 
replace status_endline=0 if dataid=="03704"

replace reason_midline="" if dataid=="02604"
replace reason_midline="" if dataid=="03704"
replace reason_endline="ABSENT" if dataid=="02604"
replace reason_endline="ABSENT" if dataid=="03704"

* consolidate reasons for drop out and midline and endline
gen str reason = ""
replace reason = reason_midline if dropout==1 & (reason_endline=="" | status_endline==.)
replace reason = reason_endline if dropout==1 & (reason_endline!="" & status_endline!=.)

* loss to FU --------------

* no live birth
gen nolb=0
#delimit;
replace nolb=1 if reason=="ABORTION" |
	reason=="FALSE PREGNANCY" | reason=="MISCARRIAGE" |
	reason=="STILL BIRTH";
#delimit cr
replace nolb=. if reason==""
replace nolb=0 if dropout==0

* withdrew
gen withdrew=0
replace withdrew=1 if reason=="REFUSE" 
replace withdrew=. if reason==""
replace withdrew=0 if dropout==0

* moved
gen moved=0
replace moved=1 if reason=="MIGRATION OUT"
replace moved=. if reason==""
replace moved=0 if dropout==0

* moved
gen absent=0
replace absent=1 if reason=="ABSENT"
replace absent=. if reason==""
replace absent=0 if dropout==0

* deaths
gen cdeath=0 
replace cdeath=1 if reason=="CHILD DEATH"
replace cdeath=. if reason==""
replace cdeath=0 if dropout==0

* primary analysis hhs ------------


keep block dataid tr nolb withdrew moved cdeath dropout reason absent

outsheet using "~/dropbox/wash benefits/kenya/data/temp/endline_withdraw.csv", replace comma

*/
log close
