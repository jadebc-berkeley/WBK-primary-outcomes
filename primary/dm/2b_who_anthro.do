*********************************************
* WASH Benefits Kenya

* Primary outcome analysis
* WHO z-scores

* This file loads data needed to create head 
* circumference z-scores and runs the Stata
* ado file to generate z-scores

* NOTE the Stata file igrowup_standard.ado
* would not run as is. I had to replace 
* each line similar to:	"local string "xxx\lenanthro.dta"
* with a forward slash instead of backward slash

* by Jade
*********************************************

*clear all
*set more 1

/* Higher memory might be necessary for larger datasets */
*set memory 20m
*set maxvar 10000

/* Indicate to the Stata compiler where the igrowup_standard.ado file is stored*/
adopath + "~/documents/crg/wash-benefits/kenya/src/primary/dm/WHO igrowup STATA/"

/* Load the data file */
*use "~/documents/crg/wash-benefits/kenya/src/primary/dm/WHO igrowup workdata/midline-headc-prep.dta"

/*generate the first three parameters reflib, datalib & datalab */
gen str200 reflib="~/documents/crg/wash-benefits/kenya/src/primary/dm/WHO igrowup STATA"
lab var reflib "Directory of reference tables"

gen str200 datalib="~/documents/crg/wash-benefits/kenya/src/primary/dm/WHO igrowup workdata"
lab var datalib "Directory for datafiles"

gen str30 datalab="mysurvey"
lab var datalab "Working file"

/* define your ageunit */
gen str6 ageunit="days"
lab var ageunit "=days"

* recumbent length measured
gen str measure="L" if length_m==1
replace measure="H" if length_m==2
replace measure=" " if measure == ""
label var measure "Height measured lying -L- or standing -H-"

gen whosex = sex
replace whosex = 2 if whosex==0

gen muac = .
gen tri = .
gen sub =.
gen oedema = .
gen sw = .

save "~/dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/temp-anthro.dta", replace 

* calculate z-scores
igrowup_standard reflib datalib datalab whosex aged ageunit childwt childht measure headc muac tri sub oedema sw



*---------------------------------------------
* Retrieve WHO calculated output
* "_f" variables identify variables outside of
* reasonable bounds
*
* merge back to the main anthro dataset
*---------------------------------------------
use "~/dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/temp-anthro.dta", clear

merge 1:1 hhid childid using "/Users/jadederong/Documents/CRG/wash-benefits/kenya/src/primary/dm/WHO igrowup workdata/mysurvey_z_st.dta"
assert _m==3
drop _m

*keep dataid childid _zwei _zlen _zbmi _zwfl _zhc _fwei _flen _fbmi _fwfl _fhc
sort hhid childid


