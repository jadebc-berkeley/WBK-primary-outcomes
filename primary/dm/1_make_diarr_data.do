capture log close
log using "~/documents/crg/wash-benefits/Kenya/src/primary/dm/1_make_diarr_data.log", replace

*********************************************
* WASH Benefits Kenya
* Primary outcome analysis

* Make analysis dataset: diarrhea

* Reads in raw data
* Outputs 1 dataset with individual level data
* for baseline, midline, and endline 
* including variables for covariate adjusted 
* analyses 

* by Jade Benjamin-Chung (jadebc@berkeley.edu)
*********************************************
clear all
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
label define mother_edul 0 "Incomplete primary" 1 "Complete primary" 2 "Any secondary" 9 "Dont know"
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
label define floorl 0 "Earth/dung" 1 "Concrete" 9 "Missing/DK"
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

* food insecurity variable - based on Anne's SAS code
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



*********************************************
* Prep baseline child-level variables
*********************************************

* ---------------------------------------------
* Child diarrhea
* ---------------------------------------------	
use "1_Baseline/msP_bl_child_diarr_20161010.dta", clear
tostring childid hhid, replace

* At baseline, some children had diarrhea recorded twice
* Using the first measurement
tab threedefa_*, mis
tab softa_*, mis
tab blooda_*, mis
tab threedefd_*, mis
tab softd_*, mis
tab bloodd_*, mis

foreach var of varlist threedefa_1 threedefb_1 threedefc_1 threedefd_1 softa_1 softb_1 softc_1 softd blooda_1 bloodb_1 bloodc_1 bloodd_1{
	replace `var' = . if `var'==9
	replace `var' = . if `var'==99
	tab `var'
}

* WHO diarrhea definition: 3+ loose or watery stools in 24 hours 
* OR one or more stools with blood

* 3+ stools in last 2 days
gen plus3stool=0
replace plus3stool=1 if threedefa_1 ==1 | threedefb_1==1 | threedefc_1==1
replace plus3stool=. if threedefa_1 ==. & threedefb_1==. & threedefc_1==.

* water stool in last 2 days
gen watery=0
replace watery=1 if softa_1==1 | softb_1==1 | softc_1==1
replace watery=. if softa_1==. & softb_1==. & softc_1==.

* blood stool in last 2 days
gen bloody=0
replace bloody=1 if blooda_1==1 | bloodb_1==1 | bloodc_1==1
replace bloody=. if blooda_1==. & bloodb_1==. & bloodc_1==.

* 2-day diarrhea
gen diarr2 = 0
replace diarr2 = 1 if (plus3stool==1 & watery==1) | bloody==1
replace diarr2 = . if (plus3stool==. | watery==.) & bloody!=1
replace diarr2 = . if (plus3stool!=1 & watery!=1) & bloody==.
replace diarr2 = . if plus3stool==. & watery==. & bloody==. 

codebook diarr2

* 7-day diarrhea
gen diarr7 = 0
replace diarr7 = 1 if diarr2==1 | ((threedefd_1==1 & softd_1==1) | bloodd_1==1)
replace diarr7 = . if ((threedefd_1==. | softd_1==.) & bloodd_1!=1)
replace diarr7 = . if ((threedefd_1!=1 | softd_1!=1) & bloodd_1==.)
replace diarr7 = . if (threedefd_1==. & softd_1==. & bloodd_1==.)
replace diarr7 = 1 if diarr2==1

codebook diarr7

keep hhid childid diarr2 diarr7 staffid

*---------------------------------------------
* mother height
*---------------------------------------------

merge m:1 hhid using `motherht'

* drop heights that don't correspond to <36 child information 
drop if _m==2
drop _m

gen time =0

tempfile baseline
save `baseline'

*********************************************
* Prep midline child-level variables
*********************************************

* ---------------------------------------------
* Child diarrhea
* ---------------------------------------------
use "2_Midline/msP_ml_child_diarr_20161010.dta", clear
tostring hhid childid, replace

foreach var of varlist threedefa threedefb threedefc threedefd softa softb softc softd blooda bloodb bloodc bloodd{
	replace `var' = . if `var'==9
	replace `var' = . if `var'==99
	tab `var'
}

* WHO diarrhea definition: 3+ loose or watery stools in 24 hours 
* OR one or more stools with blood

* 3+ stools in last 2 days
gen plus3stool=0
replace plus3stool=1 if threedefa ==1 | threedefb==1 | threedefc==1
replace plus3stool=. if threedefa ==. & threedefb==. & threedefc==.

* water stool in last 2 days
gen watery=0
replace watery=1 if softa==1 | softb==1 | softc==1
replace watery=. if softa==. & softb==. & softc==.

* blood stool in last 2 days
gen bloody=0
replace bloody=1 if blooda==1 | bloodb==1 | bloodc==1
replace bloody=. if blooda==. & bloodb==. & bloodc==.

* 2-day diarrhea
gen diarr2 = 0
replace diarr2 = 1 if (plus3stool==1 & watery==1) | bloody==1
replace diarr2 = . if (plus3stool==. | watery==.) & bloody!=1
replace diarr2 = . if (plus3stool!=1 & watery!=1) & bloody==.
replace diarr2 = . if plus3stool==. & watery==. & bloody==. 

codebook diarr2

* 7-day diarrhea
gen diarr7 = 0
replace diarr7 = 1 if diarr2==1 | ((threedefd==1 & softd==1) | bloodd==1)
replace diarr7 = . if ((threedefd==. | softd==.) & bloodd!=1)
replace diarr7 = . if ((threedefd!=1 | softd!=1) & bloodd==.)
replace diarr7 = . if (threedefd==. & softd==. & bloodd==.)
replace diarr7 = 1 if diarr2==1


codebook diarr7

keep hhid childid diarr2 diarr7 staffid


*---------------------------------------------
* mother height
*---------------------------------------------

merge m:1 hhid using `motherht'

* drop heights that don't correspond to <36 child information 
drop if _m==2
drop _m

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

foreach var of varlist threedefa_ad threedefb_ad threedefc_ad threedefd_ad softa_ad softb_ad softc_ad softd_ad blooda_ad bloodb_ad bloodc_ad bloodd_ad{
	replace `var' = . if `var'==9
	replace `var' = . if `var'==99
	tab `var'
}

* At endline diarrhea was measured at the anthro day and 
* at the uptake measurement.
* Using the anthro day measure 

* WHO diarrhea definition: 3+ loose or watery stools in 24 hours 
* OR one or more stools with blood

* 3+ stools in last 2 days
gen plus3stool=0
replace plus3stool=1 if threedefa_ad ==1 | threedefb_ad==1 | threedefc_ad==1
replace plus3stool=. if threedefa_ad ==. & threedefb_ad==. & threedefc_ad==.

* water stool in last 2 days
gen watery=0
replace watery=1 if softa_ad==1 | softb_ad==1 | softc_ad==1
replace watery=. if softa_ad==. & softb_ad==. & softc_ad==.

* blood stool in last 2 days
gen bloody=0
replace bloody=1 if blooda_ad==1 | bloodb_ad==1 | bloodc_ad==1
replace bloody=. if blooda_ad==. & bloodb_ad==. & bloodc_ad==.


* 2-day diarrhea
gen diarr2 = 0
replace diarr2 = 1 if (plus3stool==1 & watery==1) | bloody==1
replace diarr2 = . if (plus3stool==. | watery==.) & bloody!=1
replace diarr2 = . if (plus3stool!=1 & watery!=1) & bloody==.
replace diarr2 = . if plus3stool==. & watery==. & bloody==. 

codebook diarr2

* 7-day diarrhea
gen diarr7 = 0
replace diarr7 = 1 if diarr2==1 | ((threedefd_ad==1 & softd_ad==1) | bloodd_ad==1)
replace diarr7 = . if ((threedefd_ad==. | softd_ad==.) & bloodd_ad!=1)
replace diarr7 = . if ((threedefd_ad!=1 | softd_ad!=1) & bloodd_ad==.)
replace diarr7 = . if (threedefd_ad==. & softd_ad==. & bloodd_ad==.)
replace diarr7 = 1 if diarr2==1

keep hhid childid diarr2 diarr7 staffid

* ---------------------------------------------
* Mother's height
* ---------------------------------------------
merge m:1 hhid using `motherht'

* drop heights that don't correspond to <36 child information 
drop if _m==2
drop _m

gen time =2

tempfile endline
save `endline'

*********************************************
* Combine individual-level variables from all 3 time points
*********************************************
use `baseline', clear
append using `midline'
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

keep hhid childid date* sibling targetchild newbirth date* dob sex

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

drop if diarr2==. & diarr7==. & date==.

gen month=month(date)

* create age at each time point
gen double aged=(date-dob)
gen double agem=(date-dob)/30.4167
gen double agey=(date-dob)/365.25

gen dcohort=0
replace dcohort=1 if agey<3 & targetchild==0 & diarr7!=. & time==0
replace dcohort=1 if targetchild==1 & diarr7!=. & time==1
replace dcohort=1 if targetchild==1 & diarr7!=. & time==2
replace dcohort=0 if agem==.

*********************************************
* Merge endline individual- and compound-level variables
*********************************************
merge m:1 hhid using `compounddata'

* drop if no child info for that compound
drop if _m==2
drop _m

* ---------------------------------------------
* merge in block
* ---------------------------------------------
merge m:1 clusterid using "~/Dropbox/WASHB-Kenya-Data/0-Untouched-data/0-Treatment-assignments/washb-kenya-tr.dta"

* drop if there are no children in that compound
* no data for clusterid 11027
drop if _m==2
drop _m

sort hhid childid time
order block clusterid compoundid hhid childid time targetchild tr date 

outsheet using "~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/diarrhea.csv", replace comma 
save  "~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/diarrhea.dta", replace 

log close
