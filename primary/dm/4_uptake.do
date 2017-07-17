capture log close
log using "~/documents/crg/wash-benefits/Kenya/src/primary/dm/4_uptake.log", replace

*********************************************
* WASH Benefits Kenya
* Primary outcome analysis

* Make uptake dataset

* Reads in raw data
* Outputs 3 datasets with data for baseline, midline
* and endline  

* by Jade Benjamin-Chung (jadebc@berkeley.edu)
*********************************************
clear all
set more off
cd "~/Dropbox/WBK-primary-analysis/Data/Untouched/"


*********************************************
* Baseline
*********************************************

use "Baseline/msP_bl_append_ID_clean_20161010.dta", clear
tostring hhid clusterid, replace

* Visited by promoter in past month
gen promoter=.

* stored drinking water has detectable free chlorine
gen chlorine = 1 if c1015a>0 & c1015a!=99.9
#delimit;
replace chlorine = 0 if c1015a==0 | c1005==2 | (c1005==1 & c100601!=1 &
	c100601!=2 & c100601!=11 & c100601!=12 & c100601!=13 &  c100602!=1 & 
	c100602!=2 & c100602!=11 & c100602!=12 & c100602!=13);
replace chlorine = . if  c1003_1==2 | c1014!=1 | c1015a==99.9 | c1023==11 | c1023==12; 
#delimit cr


* access to improved latrine
gen imp_lat=0
replace imp_lat=1 if c806_7==1 | c806_5==1 
replace imp_lat=. if c806_7==. & c806_5==. | (to1b==2 | to1b==3)
replace imp_lat=0 if c805==2

* child feces safely disposed
* merging in indicator for having child in parasite cohort
preserve
use "~/Dropbox/WBK-primary-analysis/Data/Untouched/msP_household_tracking_20161006.dta", clear
keep hhid parasite_bl
tostring hhid, replace
tempfile para
save `para'
restore

merge 1:1 hhid using `para'
drop _m

gen feces_disp=0
replace feces_disp= 1 if c903_intoilet==1 | c903_diaper==1 | ((c903_potty_incourt==1 | c903_potty_inhouse==1) & (c904==2 | c904==7)) 
replace feces_disp = . if parasite_bl ==1
replace feces_disp = . if c903_intoilet==. & c903_potty_incourt==. & c903_potty_inhouse==. & (c904==. | c904==99)

* handwashing location has water and soap
#delimit; 
gen hws=0;
replace hws=1 if (c702c_water==1 & (c702c_scentbar==1 | c702c_unscentbar==1 | c702c_powder==1 | 
	c702c_soapywat==1)) | ( c703c_water==1 & (c703c_scentbar==1 | c703c_unscentbar==1 | 
	c703c_powder==1 | c703c_soapywat==1));
	
replace hws=. if (c702c_water==. & c702c_scentbar==. & c702c_unscentbar==. & c702c_powder==. & 
	c702c_soapywat==.) & ( c703c_water==. & c703c_scentbar==. & c703c_unscentbar==. &
	c703c_powder==. & c703c_soapywat==.);
#delimit cr

* LNS sachets consumed
gen lns=0

* ---------------------------------------------
* merge in tr
* ---------------------------------------------
tostring clusterid, replace
merge m:1 clusterid using "~/Dropbox/WBK-primary-analysis/Data/Untouched/tr/washb-kenya-tr.dta"
drop _m

keep hhid clusterid tr promoter chlorine imp_lat feces_disp hws lns

outsheet using "~/Dropbox/WBK-primary-analysis/Data/final/jade/uptake_baseline.csv", replace comma 

*********************************************
* Midline
*********************************************
use "Midline/msP_ml_append_ID_clean_uptake_20161015.dta", clear
tostring hhid clusterid, replace

* Visited by promoter in past month
gen promoter=0
replace promoter =1 if f2==1
replace promoter=. if f2==. | f2==99

* stored drinking water has detectable free chlorine
gen chlorine = 1 if c1015a>0 & c1015a!=99.9 & c1015a!=999
#delimit; 
replace chlorine = 0 if c1015a==0 | c1005==2 | (c1005==1 & c1006_cldisp!=1 & 
	c1006_botcl!=1 & c1006_pur!=1 & c1006_aqua!=1 & c1006_cl!=1);
replace chlorine = . if  c1003_1==2 | c1014!=1 | c1015a==99.9 | c1015a==999;
#delimit cr

* access to improved latrine
gen imp_lat=0
replace imp_lat=1 if c806_7==1 | c806_5==1 
replace imp_lat=. if c806_7==. & c806_5==. | (c8052==2 | c8052==3)
replace imp_lat=0 if c8052==4
replace imp_lat=. if c8052==2 | c8052==3

* child feces safely disposed
* merging in indicator for having child in parasite cohort
preserve
use "~/Dropbox/WBK-primary-analysis/Data/Untouched/msP_household_tracking_20161006.dta", clear
keep hhid parasite_bl
tostring hhid, replace
tempfile para
save `para'
restore

merge 1:1 hhid using `para'
drop if _m==2
drop _m

gen feces_disp=0
replace feces_disp= 1 if c903_intoilet==1 | ( c903_diaper==1 & c904==2) | ((c903_pottyincourt==1 | c903_pottyinhouse==1) & (c904==2 | c904==7)) 
replace feces_disp = . if parasite_bl ==1
replace feces_disp = . if c903_intoilet==. & c903_pottyincourt==. & c903_pottyinhouse==. & c904==. 

* handwashing location has water and soap
#delimit;
gen ws = 0;
replace ws = 1 if (c702c_water==1 & (c702c_scentbar==1 | c702c_unscentbar==1 | 
	c702c_powder==1 | c702c_soapywat==1)) | (c703c_water==1 & (c703c_scentbar==1 | 
	c703c_unscentbar==1 | c703c_powder==1 | c703c_soapywat==1));
replace ws = . if (c702c_water==. & c702c_scentbar==. & c702c_unscentbar==. & c702c_powder==. & 
	c702c_soapywat==.) & ( c703c_water==. & c703c_scentbar==. & c703c_unscentbar==. &
	c703c_powder==. & c703c_soapywat==.);
#delimit cr
	
gen tt = 0
replace tt=1 if (tt702b1==1 & tt702d==1 & tt702f==1) | (tt703b1==1 & tt703d==1 & tt703f==1)
replace tt=. if tt702b1==. & tt702d==. & tt702f==. & tt703b1==. & tt703d==. & tt703f==.

gen hws=0
replace hws=1 if ws ==1 | tt==1
replace hws=. if ws==. & tt==.


* LNS sachets consumed

* merge in age to restrict LNS to ages 6-24 months
preserve
use "msP_child_IDchar_20161006.dta", clear
tostring childid hhid, replace
* keeping first index child - not including twin data because LNS was
* only recorded once in the survey
keep if childtype==1

* calculate age
ren DOB dob

keep hhid childid dob

tempfile dob
save `dob'
restore

merge 1:1 hhid using `dob'
drop if _m==2
drop _m

format ms_ml_up_date %d
gen double agem=(ms_ml_up_date-dob)/30.4167

* mean self-reported sachets of LNS fed in prior week
gen lns_cons=c1408*c1409

gen lns=lns_cons/14

replace lns=. if agem <6 | agem >24

* ---------------------------------------------
* merge in tr
* ---------------------------------------------
tostring clusterid, replace
merge m:1 clusterid using "~/Dropbox/WBK-primary-analysis/Data/Untouched/tr/washb-kenya-tr.dta"

drop if _m==2
drop _m

keep hhid clusterid tr promoter chlorine imp_lat feces_disp hws lns

outsheet using "~/Dropbox/WBK-primary-analysis/Data/final/jade/uptake_midline.csv", replace comma 

*********************************************
* Endline
*********************************************
use "Endline/msP_el_append_ID_clean_uptake_20161015.dta", clear
tostring hhid clusterid, replace

* Visited by promoter in past month
gen promoter=0
replace promoter =1 if f2==1
replace promoter=. if f2==. | f2==99

* stored drinking water has detectable free chlorine
gen chlorine = 1 if c1015a>0 & c1015a!=99.9 & c1015a!=999
#delimit; 
replace chlorine = 0 if c1015a==0 | c1005==2 | (c1005==1 & c1006_cldisp!=1 & 
	c1006_botcl!=1 & c1006_pur!=1 & c1006_aqua!=1 & c1006_cl!=1);
replace chlorine = . if  c1003_1==2 | c1014!=1 | c1015a==99.9 | c1015a==999;
#delimit cr

* access to improved latrine
gen imp_lat=0
replace imp_lat=1 if c806_7==1 | c806_5==1 
replace imp_lat=. if c806_7==. & c806_5==. 
replace imp_lat=0 if c8052==4
replace imp_lat=. if c8052==2 | c8052==3

* child feces safely disposed
* merging in indicator for having child in parasite cohort
preserve
use "~/Dropbox/WBK-primary-analysis/Data/Untouched/msP_household_tracking_20161006.dta", clear
keep hhid parasite_bl
tostring hhid, replace
tempfile para
save `para'
restore

merge 1:1 hhid using `para'
drop if _m==2
drop _m

* child feces safely disposed 
gen feces_disp=0
replace feces_disp= 1 if c903==7 | (c903==8 & c904==2) | ((c903 ==1 | c903 ==2) & (c904==2 | c904==7))
replace feces_disp = . if parasite_bl ==1
replace feces_disp = . if (c903==. | c903==99) & (c904==. | c904==99)


* handwashing location has water and soap
#delimit;
gen ws = 0;
replace ws = 1 if (c702c_water==1 & (c702c_scentbar==1 | c702c_unscentbar==1 | 
	c702c_powder==1 | c702c_soapywat==1)) | (c703c_water==1 & (c703c_scentbar==1 | 
	c703c_unscentbar==1 | c703c_powder==1 | c703c_soapywat==1));
replace ws = . if (c702c_water==. & c702c_scentbar==. & c702c_unscentbar==. & c702c_powder==. & 
	c702c_soapywat==.) & ( c703c_water==. & c703c_scentbar==. & c703c_unscentbar==. &
	c703c_powder==. & c703c_soapywat==.);
#delimit cr
	
gen tt = 0
replace tt=1 if (tt702b1==1 & tt702d==1 & tt702f==1) | (tt703b1==1 & tt703d==1 & tt703f==1)
replace tt=. if tt702b1==. & tt702d==. & tt702f==. & tt703b1==. & tt703d==. & tt703f==.

gen hws=0
replace hws=1 if ws ==1 | tt==1
replace hws=. if ws==. & tt==.




* LNS sachets consumed

* merge in age to restrict LNS to ages 6-24 months
preserve
use "msP_child_IDchar_20161006.dta", clear
tostring childid hhid, replace
* keeping first index child - not including twin data because LNS was
* only recorded once in the survey
keep if childtype==1

* calculate age
ren DOB dob

keep hhid childid dob

tempfile dob
save `dob'
restore

merge 1:1 hhid using `dob'
drop if _m==2
drop _m

format ms_el_up_date %d
gen double agem=(ms_el_up_date-dob)/30.4167

* mean self-reported sachets of LNS fed in prior week

gen lns_cons=c1408*c1409

gen lns=lns_cons/14

replace lns=. if agem <6 | agem >24

* ---------------------------------------------
* merge in tr
* ---------------------------------------------
tostring clusterid, replace
merge m:1 clusterid using "~/Dropbox/WBK-primary-analysis/Data/Untouched/tr/washb-kenya-tr.dta"

drop if _m==2
drop _m

keep hhid clusterid tr promoter chlorine imp_lat feces_disp hws lns

* there are 66 observations in the passive control arm, but my understanding
* was that measurements were not taken in that arm. 
drop if tr==8

outsheet using "~/Dropbox/WBK-primary-analysis/Data/final/jade/uptake_endline.csv", replace comma 


log close
