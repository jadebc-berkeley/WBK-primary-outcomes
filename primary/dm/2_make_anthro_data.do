capture log close
log using "~/documents/crg/wash-benefits/Kenya/src/primary/dm/2_make_anthro_data.log", replace

*********************************************
* WASH Benefits Kenya
* Primary outcome analysis

* Make analysis dataset for anthropometry variables

* Reads in raw data
* Outputs 2 datasets with individual level data
* for midline and endline including variables 
* for covariate adjusted analyses 

* by Jade Benjamin-Chung (jadebc@berkeley.edu)
*********************************************
clear all
set maxvar 10000

set more off
cd "~/Dropbox/WASHB-Kenya-Data/0-Untouched-data/1-Main-survey/"


*********************************************
* Prep compound-level variables
*********************************************

use "1_Baseline/msP_bl_append_ID_clean_20161010.dta"
tostring hhid clusterid, replace

ren ms_bl_date date_baseline

gen month = month(date_baseline)

* imputing missing day of birth as first day of the month 
replace b6_day=1 if b6_day==99 & b6_month!=99
replace b6_month=month(date_baseline) if b6_month==99 & b6_day!=99

gen dm_miss = 1 if b6_day==99 & b6_month==99
replace b6_day=day(date_baseline) if dm_miss == 1
replace b6_month=month(date_baseline) if dm_miss == 1
drop dm_miss

gen mother_dob = .
replace mother_dob = mdy(b6_month, b6_day, b6_year)
replace mother_dob=. if b6_year==9999

format date_baseline mother_dob %d

gen mother_age = (date_baseline - mother_dob) / 365.25
sum mother_age

gen mother_edu = 0 if b13_school==1 | b13_school==2
replace mother_edu = 1 if b13_school==3
replace mother_edu = 2 if b13_school>=4 & b13_school<=8
replace mother_edu = 9 if mother_edu ==. 
label define mother_edul 0 "Incomplete primary" 1 "Complete primary" 2 "Any secondary" 9 "Don't know"
label values mother_edu mother_edul 

* primary water source, regardless of whether it is for drinking
replace d7_hrs=0 if d7_hrs==. & d7_min!=. 
gen water_time= (d7_hrs*60) + d7_min
replace water_time=d7_hrs*60 if d7_hrs!=. & d7_min==.
sum water_time

gen roof=0
replace roof=1 if b2_roof>=2
replace roof=9 if b2_roof==.
label define roofl 0 "Thatch/leaf" 1 "Iron/other" 9 "Missing/DK"
label values roof roofl

gen walls=0
replace walls=1 if b3_wall>=3
replace walls=9 if b3_wall==.

replace walls=1 if b3_other=="BRICK"
replace walls=1 if b3_other=="BRICK AND MUD"
replace walls=1 if b3_other=="BRICK WITH SAND"
replace walls=1 if b3_other=="BRICKS"
replace walls=1 if b3_other=="BRICKS AND MUD"
replace walls=1 if b3_other=="BRICKS WITH MUD"
replace walls=1 if b3_other=="CEMENT ON MUD"
replace walls=1 if b3_other=="CONCRETE AND MUD"
replace walls=0 if b3_other=="MIXTURE OF STONES & MUD"
replace walls=1 if b3_other=="MUD AND BAKED BRICKS"
replace walls=1 if b3_other=="MUD AND BRICKS"
replace walls=1 if b3_other=="MUD AND CONCRETE"
replace walls=0 if b3_other=="MUD AND STONE"
replace walls=0 if b3_other=="MUD AND STONES"
replace walls=1 if b3_other=="MUD BUT CEMENTED"
replace walls=1 if b3_other=="MUD CEMENTED"
replace walls=1 if b3_other=="MUD PLASTERED WITH CEMENT"
replace walls=1 if b3_other=="MUD WITH ROUGH CAST"
replace walls=0 if b3_other=="STILL NO WALL"
replace walls=0 if b3_other=="STONE WITH MUD"
replace walls=1 if b3_other=="TILES"

label define wallsl 0 "Bamboo/mud/cane" 1 "Concrete" 9 "Missing/DK"
label values walls wallsl

gen floor = 0
replace floor = 1 if b1_floor>=3
replace floor = 9 if b1_floor==.
label define floorl 0 "Earth/dung" 1 "Cement/concrete" 9 "Missing/DK"
label values floor floorl

gen elec=0
replace elec=1 if b22a==1 
replace elec=9 if b22a==. 
label define elecl 0 "No electricity" 1 "Has electricity" 9 "Missing/DK"
label values elec elecl

gen radio=0
replace radio=1 if b22b==1 
replace radio=9 if b22b==. 
label define radiol 0 "No radio" 1 "Has radio" 9 "Missing/DK"
label values radio radiol

gen tv=0
replace tv=1 if b22c==1 
replace tv=9 if b22c==. 
label define tvl 0 "No TV" 1 "Owns TV" 9 "Missing/DK"
label values tv tvl

gen mobilephone=0
replace mobilephone=1 if b22d==1
replace mobilephone=9 if b22d==.
label define mobilephonel 0 "No mobile phone" 1 "Any mobile phones" 9 "Missing/DK"
label values mobilephone mobilephonel

gen clock=0
replace clock=1 if b22e==1
replace clock=9 if b22e==.
label define clockl 0 "No clock" 1 "Has clock" 9 "Missing/DK"
label values clock clockl

gen bicycle=0
replace bicycle=1 if b22f==1
replace bicycle=9 if b22f==.
label define bicyclel 0 "No bicycle" 1 "Has bicycle" 9 "Missing/DK"
label values bicycle bicyclel

gen motorcycle=0
replace motorcycle=1 if b22g==1
replace motorcycle=9 if b22g==.
label define motorcyclel 0 "No motorcycle" 1 "Has motorcycle" 9 "Missing/DK"
label values motorcycle motorcyclel

gen stove=0
replace stove=1 if b22h==1
replace stove=9 if b22h==.
label define stovel 0 "No stove" 1 "Has stove" 9 "Missing/DK"
label values stove stovel

gen cooker=0
replace cooker=1 if b22i==1
replace cooker=9 if b22i==.
label define cookerl 0 "No gas cooker" 1 "Has gas cooker" 9 "Missing/DK"
label values cooker cookerl

gen car=0
replace car=1 if b22j==1
replace car=9 if b22j==.
label define carl 0 "No car" 1 "Has car" 9 "Missing/DK"
label values car carl

ren b24a_hh cow
ren b24c_hh goat
ren b24g_hh dog
ren b24e_hh chicken 

foreach var of varlist cow goat dog chicken{
	replace `var' = . if `var'==999
}

ren a6_gross Ncomp

gen u18=a4_4t18  + a5_0t3  
replace u18 =. if u18==99

* food insecurity
replace c1907_fr = 0 if c1907==2
replace c1908_fr = 0 if c1908==2
replace c1909_fr = 0 if c1909==2

gen hhs_7b=.
gen hhs_8b=.
gen hhs_9b=.

replace	hhs_7b = 0 if c1907_fr == 0
replace hhs_7b = 1 if c1907_fr == 1
replace hhs_7b = 1 if c1907_fr == 2 
replace hhs_7b = 2 if c1907_fr == 3
replace hhs_8b = 0 if c1908_fr == 0
replace hhs_8b = 1 if c1908_fr == 1
replace hhs_8b = 1 if c1908_fr == 2 
replace hhs_8b = 2 if c1908_fr == 3 
replace hhs_9b = 0 if c1909_fr == 0 
replace hhs_9b = 1 if c1909_fr == 1 
replace hhs_9b = 1 if c1909_fr == 2
replace hhs_9b = 2 if c1909_fr == 3 
gen HHS_total = hhs_7b + hhs_8b + hhs_9b

gen HHS=.
replace HHS=1 if HHS_total == 0
replace HHS=1 if HHS_total == 1
replace HHS=2 if HHS_total == 2
replace HHS=2 if HHS_total == 3 
replace HHS=3 if HHS_total == 4 
replace HHS=3 if HHS_total == 5
replace HHS=3 if HHS_total == 6 

replace HHS=9 if HHS==.

gen HHS_bi=.
replace HHS_bi = 0 if HHS_total == 0 
replace HHS_bi = 0 if HHS_total == 1 
replace HHS_bi = 1 if HHS_total == 2 
replace HHS_bi = 1 if HHS_total == 3
replace HHS_bi = 1 if HHS_total == 4
replace HHS_bi = 1 if HHS_total == 5
replace HHS_bi = 1 if HHS_total == 6

label variable HHS_total "Sum of hunger score responses"
label variable HHS "Hunger score"
label variable HHS_bi "Hunger dichotimized (little none (HHS=1) vs. moderate(HHS=2) and severe (HHS=3)"

label define HHSl 1 "Little to none" 2 "Moderate" 3 "Severe" 9 "Missing"
label values HHS HHSl 
	
#delimit;
keep hhid clusterid compoundid date_baseline date_baseline mother_age mother_edu
roof walls floor water_time roof walls floor elec radio tv mobilephone clock
bicycle motorcycle stove cooker car cow goat dog chicken Ncomp u18 HHS;
#delimit cr


* ---------------------------------------------
* Save compound-level dataset
* ---------------------------------------------
tempfile compounddata
save `compounddata'

*********************************************
* Child age and sex
*********************************************
use "msP_child_IDchar_20161103.dta", clear
tostring childid hhid, replace

* indicators for child type
gen sibling = (childtype>2)
gen targetchild= (childtype<=2)
gen newbirth = (childtype==6)

* rename date vars
ren ms_bl_date date_base
ren ms_ml_am_date date_mid
ren ms_el_am_date date_end

* calculate age
ren DOB dob

recode sex (1=0) (2=1) 
label define sexl 0 "Male" 1 "Female"
label values sex sexl

keep hhid childid date* sibling targetchild newbirth dob sex

tempfile childagesex
save `childagesex'


*********************************************
* Prep midline child-level variables
*********************************************


* ---------------------------------------------
* Mother height
* ---------------------------------------------
* midline
use "2_Midline/msP_ml_append_ID_clean_anthropometry_20160909.dta", clear
tostring hhid, replace
ren c422 c422m
ren c423 c423m
ren c424 c424m
ren resp_hair resp_hairm
ren resp_hair_height resp_hair_heightm
keep c422m-c424m hhid resp_hair*
* endline
preserve
use "3_Endline/msP_el_append_ID_clean_anthropometry_20160909.dta", clear
tostring hhid, replace
ren c422 c422e
ren c423 c423e
ren c424 c424e
ren resp_hair resp_haire
ren resp_hair_height resp_hair_heighte
keep c422e-c424e hhid resp_hair*

tempfile endlinemomht
save `endlinemomht'
restore

merge 1:1 hhid using `endlinemomht'
drop _m

foreach var of varlist c422m-c424e{
	sum `var'
}

replace c422m = c422m - resp_hair_heightm if resp_hairm==2
replace c423m = c423m - resp_hair_heightm if resp_hairm==2
replace c424m = c424m - resp_hair_heightm if resp_hairm==2

replace c422e = c422e - resp_hair_heighte if resp_haire==2
replace c423e = c423e - resp_hair_heighte if resp_haire==2
replace c424e = c424e - resp_hair_heighte if resp_haire==2

egen float med_momht_6 = rowmedian(c422m c423m c424m c422e c423e c424e)
egen float med_momht_m = rowmedian(c422m c423m c424m)
egen float med_momht_e = rowmedian(c422e c423e c424e)

gen float motherht = med_momht_6
replace motherht = med_momht_m if abs(med_momht_m - med_momht_e) > 3 & abs(med_momht_m - med_momht_e)<.

keep hhid motherht

tempfile motherht
save `motherht'

* ---------------------------------------------
* Child length
* ---------------------------------------------
use "2_Midline/msP_ml_child_anthro_20160909.dta", clear

* merge in mother anthro data
merge m:1 hhid using "2_Midline/msP_ml_append_ID_clean_anthropometry_20160909.dta"
* drop if there is no mother anthro data
drop if _m==2
drop _m

tostring hhid childid, replace

* check values for child anthro
foreach var of varlist weight1 weight2 weight3 length1 length2 length3 headc1 headc2 headc3{
	sum `var'
}

* check values for mother antrho
foreach var of varlist c404-c406 c422-c424{
	sum `var'
}

* round to 0.01
foreach var of varlist headc1 headc2 headc3 muac1 muac2 muac3 weight1 weight2 weight3 length1 length2 length3 c422 c423 c424{
	replace `var' = round(`var',0.01)
}

* take median of height measurements
egen double childht = rowmedian(length1 length2 length3)

* clean length measures
replace childht = . if childht==0

egen double childwt = rowmedian(weight1 weight2 weight3)

* take median of head circ measurements
replace headc1=. if headc1==0
replace headc2=. if headc2==0
replace headc3=. if headc3==0

egen headc = rowmedian(headc1 headc2 headc3)

* check for duplicates
duplicates tag hhid childid, gen(tag)

drop tag

duplicates report hhid childid

keep hhid childid childht childwt headc length_m staffid

* ---------------------------------------------
* merge in mother height info
* ---------------------------------------------
merge m:1 hhid using `motherht'

drop if _m==2
drop _m

* ---------------------------------------------
* merge anthro information with  age and sex
* ---------------------------------------------
merge 1:1 hhid childid using `childagesex'

* drop children with missing age and sex information
drop if _m==1

* tab childid for children that are in the childinfo
* dataset but do not have anthro measurements
tab childid if _m==2
* drop children who missing anthro measurements
drop if _m==2

drop _m

* survey date
gen date = .
replace date=date_mid

format date %d
drop date_*

gen month=month(date)

* create age 
gen double aged=(date-dob)
gen double agem=(date-dob)/30.4167
gen double agey=(date-dob)/365.25

* recoding sex to meet zscore06 requirements
recode sex (0=1) (1=2)

zscore06, a(agem) s(sex) h(childht) w(childwt) measure(length_m)

* make flags for outlier z-scores
gen haz_flag = (haz06<-6 | haz06>6)
replace haz_flag=. if haz06==.
gen waz_flag = (waz06<-6 | waz06>5)
replace waz_flag=. if waz06==.
gen whz_flag = (whz06<-5 | whz06>5)
replace whz_flag=. if whz06==.
gen bmi_flag = (bmiz06<-5 | bmiz06>5)
replace bmi_flag=. if bmiz06==.

* list children with z-scores that aren't plausible
list childid agem childht haz06 if haz_flag==1
list childid agem childwt waz06 if waz_flag==1
list childid agem childht whz06 if whz_flag==1
list childid agem childwt bmiz06 if bmi_flag==1

* drop z-score values that are not plausible
replace haz06 = . if haz_flag==1
replace waz06 = . if waz_flag==1
replace whz06 = . if whz_flag==1
replace bmiz06 = . if bmi_flag==1

* rename variables for convenience
ren haz06 haz
ren waz06 waz
ren whz06 whz

* generate indicator for missing anthro data
gen misshaz=0
replace misshaz=1 if haz==.

gen misswaz=0
replace misswaz=1 if waz==.

gen misswhz=0
replace misswhz=1 if whz==.

* anthro proportions
gen stunted=0
replace stunted=1 if haz<-2
replace stunted=. if haz==.

gen sstunted=0
replace sstunted=1 if haz<-3
replace sstunted=. if haz==.

gen wasted=0
replace wasted=1 if whz<-2
replace wasted=. if whz==.

gen underwt=0
replace underwt=1 if waz<-2
replace underwt=. if waz==.



* ---------------------------------------------
* WHO anthro z-scores
* ---------------------------------------------
do "~/Documents/CRG/wash-benefits/kenya/src/primary/dm/2b_who_anthro.do"

replace _zlen = . if _flen==1
replace _zwei = . if _fwei==1
replace _zwfl = . if _fwfl==1
replace _zhc = . if _fhc==1

ren _zlen haz_who
ren _zwei waz_who
ren _zwfl whz_who
*ren _zhc hcz

gen double hcz = _zhc

gen hcz_flag = (hcz<-5 | hcz>5)
replace hcz_flag=. if hcz==.
replace hcz = . if hcz_flag==1

gen underhc=0
replace underhc=1 if hcz<-2
replace underhc=. if hcz==.

recode sex (1=0) (2=1) 

drop _*

drop reflib-measure childwt whosex muac-sw  *_flag length_method miss*

tempfile midline_anthro
save `midline_anthro'



*********************************************
* Prep endline child-level variables
*********************************************


* ---------------------------------------------
* Child length
* ---------------------------------------------
use "3_Endline/msP_el_child_anthro_20160909.dta", clear

* merge in mother anthro data
merge m:1 hhid using "3_Endline/msP_el_append_ID_clean_anthropometry_20160909.dta"
* drop if there is no mother anthro data
drop if _m==2
drop _m

tostring hhid childid, replace

* check values 
foreach var of varlist weight1 weight2 weight3 length1 length2 length3 headc1 headc2 headc3{
	sum `var'
}

* round to 0.01
foreach var of varlist headc1 headc2 headc3 muac1 muac2 muac3 weight1 weight2 weight3 length1 length2 length3 c422 c423 c424{
	replace `var' = round(`var',0.01)
}


* take median of height measurements
egen double childht = rowmedian(length1 length2 length3)

* clean length measures
replace childht = . if childht==0

replace childht = round(childht,0.01)

* take median of child weight measurements
egen double childwt = rowmedian(weight1 weight2 weight3)

* take median of head circ measurements
replace headc1=. if headc1==0
replace headc2=. if headc2==0
replace headc3=. if headc3==0
egen headc = rowmedian(headc1 headc2 headc3)

* check for duplicates
duplicates tag hhid childid, gen(tag)

drop tag

duplicates report hhid childid
keep hhid childid childht childwt headc length_m staffid

* ---------------------------------------------
* merge in mother height info
* ---------------------------------------------
merge m:1 hhid using `motherht'
drop if _m==2
drop _m



* ---------------------------------------------
* merge anthro information with  age and sex
* ---------------------------------------------
merge 1:1 hhid childid using `childagesex'

* drop children with missing age and sex information
drop if _m==1

* tab childid for children that are in the childinfo
* dataset but do not have anthro measurements
tab childid if _m==2
* drop children who missing anthro measurements
drop if _m==2

drop _m

* survey date
gen date = .
replace date=date_end

format date %d
drop date_*

gen month=month(date)

* create age 
gen double aged=(date-dob)
gen double agem=(date-dob)/30.4167
gen double agey=(date-dob)/365.25

* recoding sex to meet zscore06 requirements
recode sex (0=1) (1=2)


zscore06, a(agem) s(sex) h(childht) w(childwt) measure(length_m)

* make flags for outlier z-scores
gen haz_flag = (haz06<-6 | haz06>6)
replace haz_flag=. if haz06==.
gen waz_flag = (waz06<-6 | waz06>5)
replace waz_flag=. if waz06==.
gen whz_flag = (whz06<-5 | whz06>5)
replace whz_flag=. if whz06==.
gen bmi_flag = (bmiz06<-5 | bmiz06>5)
replace bmi_flag=. if bmiz06==.

* list children with z-scores that aren't plausible
list childid agem childht haz06 if haz_flag==1
list childid agem childwt waz06 if waz_flag==1
list childid agem childht whz06 if whz_flag==1
list childid agem childwt bmiz06 if bmi_flag==1

* drop z-score values that are not plausible
replace haz06 = . if haz_flag==1
replace waz06 = . if waz_flag==1
replace whz06 = . if whz_flag==1
replace bmiz06 = . if bmi_flag==1

* rename variables for convenience
ren haz06 haz
ren waz06 waz
ren whz06 whz

* generate indicator for missing anthro data
gen misshaz=0
replace misshaz=1 if haz==.

gen misswaz=0
replace misswaz=1 if waz==.

gen misswhz=0
replace misswhz=1 if whz==.

* anthro proportions
gen stunted=0
replace stunted=1 if haz<-2
replace stunted=. if haz==.

gen sstunted=0
replace sstunted=1 if haz<-3
replace sstunted=. if haz==.

gen wasted=0
replace wasted=1 if whz<-2
replace wasted=. if whz==.

gen underwt=0
replace underwt=1 if waz<-2
replace underwt=. if waz==.


* ---------------------------------------------
* WHO anthro z-scores
* ---------------------------------------------
do "~/Documents/CRG/wash-benefits/kenya/src/primary/dm/2b_who_anthro.do"

replace _zlen = . if _flen==1
replace _zwei = . if _fwei==1
replace _zwfl = . if _fwfl==1
replace _zhc = . if _fhc==1

ren _zlen haz_who
ren _zwei waz_who
ren _zwfl whz_who

gen double hcz = _zhc

gen hcz_flag = (hcz<-5 | hcz>5)
replace hcz_flag=. if hcz==.
replace hcz = . if hcz_flag==1

gen underhc=0
replace underhc=1 if hcz<-2
replace underhc=. if hcz==.

recode sex (1=0) (2=1)

drop _*
drop reflib-measure childwt whosex muac-sw *_flag length_method miss*


tempfile endline_anthro
save `endline_anthro'


*********************************************
* Merge midline individual- and compound-level variables
*********************************************
clear
use `midline_anthro'
merge m:1 hhid using `compounddata'

* dropping main survey rows that don't have anthro data
drop if _m==2
drop _m

* ---------------------------------------------
* merge in block
* ---------------------------------------------
merge m:1 clusterid using "~/Dropbox/WASHB-Kenya-Data/0-Untouched-data/0-Treatment-assignments/washb-kenya-tr.dta"
* drop if there are no children in that compound
drop if _m==2
drop _m

gen time=1

order block clusterid compoundid hhid childid tr time date targetchild
outsheet using "~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/midline-anthro.csv", replace comma 
save "~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/midline-anthro.dta", replace 


*********************************************
* Merge individual- and compound-level variables
*********************************************
clear
use `endline_anthro'
merge m:1 hhid using `compounddata'

* dropping main survey rows that don't have anthro data
drop if _m==2

drop _m

* ---------------------------------------------
* merge in block
* ---------------------------------------------
merge m:1 clusterid using "~/Dropbox/WASHB-Kenya-Data/0-Untouched-data/0-Treatment-assignments/washb-kenya-tr.dta"
* drop if there are no children in that compound
drop if _m==2
drop _m

gen time=2

order block clusterid compoundid hhid childid tr time date targetchild

outsheet using "~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/endline-anthro.csv", replace comma 
save "~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/endline-anthro.dta", replace 


log close
