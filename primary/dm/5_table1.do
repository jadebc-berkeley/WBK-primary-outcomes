capture log close
log using "~/documents/crg/wash-benefits/Kenya/src/primary/dm/5_table1.log", replace

*********************************************
* WASH Benefits Kenya
* Primary outcome analysis

* Make table 1 dataset (baseline balance table)

* Reads in raw data
* Outputs 1 dataset with input for baseline
* balance table

* by Jade Benjamin-Chung (jadebc@berkeley.edu)
*********************************************
clear all
set more off
cd "~/Dropbox/WASHB-Kenya-Data/0-Untouched-data/1-Main-survey/"

use "1_Baseline/msP_bl_append_ID_clean_20161010.dta", clear

* -----------------------------------
* mother characteristics
* -----------------------------------
ren ms_bl_date date_baseline

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

gen mother_edu = 0
replace mother_edu = 1 if b13_school >2 
replace mother_edu = . if b13_school ==. 

* -----------------------------------
* father characteristics
* -----------------------------------
gen father_edu = (b14_spouse>2 & b14_spouse<99)
replace father_edu =. if b14_spouse==. 
replace father_edu =. if b14_spouse==99

gen father_agri = (b18_occ==1| b18_occ==2 | b18_occ==3)
replace father_agri =. if b18_occ==. 
replace father_agri =. if b18_occ==99 

* -----------------------------------
* compound info
* -----------------------------------
gen Nhh = a01_hh 
gen Nppl = a6_gross

* -----------------------------------
* household info
* -----------------------------------
gen u18 = a4_4t18 + a5_0t3

gen elec = (b22a==1)
replace elec = . if b22a==. 

gen cement = (b1_floor==3)
replace cement = . if b1_floor ==.

gen roof = (b2_roof==2)
replace roof = . if b2_roof ==. 

* -----------------------------------
* water
* -----------------------------------
label variable d8c_drink "Drink water from primary water source"
label variable d8c2_drink "Drink water from secondary water source"

ren d2 pws
ren d8c_drink drink_pws
ren d8c7 youdrink_pws

ren d3 sws
ren d8c2_drink drink_sws
ren d8c14 youdrink_sws

* if they drink from primary water source, is it improved
capture drop prim_water_imp
gen prim_water_imp = 0
replace prim_water_imp = 1 if (pws==1 | pws==4 | pws==6 | pws==7 | pws==11 | pws==12) & (drink_pws==1 | youdrink_pws==1)
replace prim_water_imp = . if drink_pws==2 & youdrink_pws==2
replace prim_water_imp = . if drink_pws==. & youdrink_pws==.

* if they drink from secondary water source, is it improved
gen sec_water_imp = 0
replace sec_water_imp = 1 if (sws==1 | sws==4 | sws==6 | sws==7 | sws==11 | sws==12) & (drink_sws==1 | youdrink_sws==1)
replace sec_water_imp = . if drink_sws==2 & youdrink_sws==2
replace sec_water_imp = . if drink_sws==. & youdrink_sws==.

* if they drink from neither water source, is source for stored water improved
gen store_water_imp = 0
replace store_water_imp = 1 if (c1023==1 | c1023==4 | c1023==6 | c1023==7 | c1023==11 | c1023==12)
replace store_water_imp = . if c1023==. 

* primary drinking water source is improved
gen prim_drink_ws = 0
replace prim_drink_ws = 1 if prim_water_imp==1 
replace prim_drink_ws = 1 if prim_water_imp==. & sec_water_imp==1 
replace prim_drink_ws = 1 if prim_water_imp==. & sec_water_imp==. & store_water_imp==1
replace prim_drink_ws = . if prim_water_imp==. & sec_water_imp==. & store_water_imp==.

drop prim_water_imp sec_water_imp store_water_imp

* one-way walking time to primary water source (in minutes)
gen wat_time = d7_min
replace wat_time = d7_hrs * 60 if d7_min==. 
replace wat_time = . if d7_min==. & d7_hrs==.

* reported treating currently stored water
gen tr_storedwt = 0
replace tr_storedwt = 1 if c1005==1 & (c100601!=4 & c100601!=7 & c100601!=.) |  (c100602!=4 & c100602!=7 & c100602!=.) |(c100603!=4 & c100603!=7 & c100603!=.)
replace tr_storedwt = . if c100601==. & c100602==. & c100603==. 
replace tr_storedwt = 0 if c1005==2

* -----------------------------------
* sanitation
* -----------------------------------
* always or usually use primary toilet for defecation

* adult men 
gen toilet_men = 0
replace toilet_men = 1 if c809d==1 | c809d==2 
replace toilet_men = . if c809d==88
replace toilet_men = 0 if c805==2
replace toilet_men = . if c809d==. & c805==. 

* adult women
gen toilet_women = 0
replace toilet_women = 1 if c809e==1 | c809e==2 
replace toilet_women = . if c809e==88
replace toilet_women = 0 if c805==2
replace toilet_women = . if c809e==. & c805==.

* open defecation 
 
* child 3-<8
gen od_child38 = (c801b==1)
replace od_child38= . if c801b==. | c801b==88

* child 0-<3
gen od_child03 = (c801a==1)
replace od_child03= . if c801a==. | c801a==88 | c801a==99

* Latrine ownership at the compound level 
gen ownlat = 0
replace ownlat = 1 if c813!=4 & c813!=5 & c813!=99 & c813!=.
replace ownlat = . if c813==. | c813==99
replace ownlat = 0 if c805==2

* access to improved latrine 
gen implat = 0 
replace implat = 1 if c806_7==1 | c806_5==1 
replace implat = . if (c806_7==. & c806_5==.) | (c806_7==99 & c806_5==.)
replace implat = . if to1b==2 | to1b==3
replace implat = 0 if c805==2

* human feces observed in the compound
gen feces = 0
replace feces = 1 if (c827> 0 & c827<99) | (c828> 0 & c828<99) | (c830> 0 & c830<99 )
replace feces = . if (c827==. | c827==99) & (c828==. | c828==99)  * (c830==. | c830==99)

* -----------------------------------
* handwashing
* -----------------------------------
* handwashing location has water and soap
gen water2m = 0
replace water2m =1 if c702c_water==1 | c703c_water==1
replace water2m = . if c702c_water==. & c703c_water==. 

#delimit; 
gen soap2m = 0;
replace soap2m =1 if (c702c_scentbar==1 | c702c_unscentbar==1 | c702c_powder==1 | c702c_soapywat==1) | 
					 (c703c_scentbar==1 | c703c_unscentbar==1 | c703c_powder==1 | c703c_soapywat==1);	
replace soap2m=. if  (c702c_scentbar==. & c702c_unscentbar==. & c702c_powder==. & c702c_soapywat==.) &
					 (c703c_scentbar==. &c703c_unscentbar==. & c703c_powder==. & c703c_soapywat==.);

#delimit cr

* -----------------------------------
* handwashing
* -----------------------------------
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

* ---------------------------------------------
* merge in tr
* ---------------------------------------------
tostring clusterid, replace
merge m:1 clusterid using "~/Dropbox/WASHB-Kenya-Data/0-Untouched-data/0-Treatment-assignments/washb-kenya-tr.dta"
drop _m


#delimit; 
keep hhid compoundid clusterid tr mother_age mother_edu father_edu father_agri Nhh Nppl u18 elec cement roof 
prim_drink_ws wat_time tr_storedwt toilet_men toilet_women od_child38 od_child03
ownlat implat feces water2m soap2m HHS_bi;
#delimit cr

order clusterid hhid tr

outsheet using "~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/table1.csv", replace comma 

log close 
