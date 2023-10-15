*Settings ----
clear all
set more off
set varabbrev off
version 17


*Imports ----
cd "D:\OneDrive\Documentos\Bristol\Economics (Bsc)\Applied Economics Dissertation\lippman-replication"
use "data\raw\gsoep.dta", clear

*keep if cpf_hid == 607401

*Cleaning ----
*codebook
/* Codebook shows pretty well the variables and help us decide what to clean.
It seems like everything up to edu4 is fine, but edu4 itself contains some -1
values, which I believe would be missing. Not sure if remove for whole sample
or just when we actually use it.

whweek has some negative values, so I will remove them. Same with the incomes.

errand, hwork, ccare, repairs and hobbies all have missing variables. Not sure
if we should remove them, either option gives wrong sample size.

Importantly, it seems that the parstat6 identifies with a person is married,
but it does not mean that the partner is in the sample
 */

keep if whweek >= 0 & incjob1_mg > 0 & incjob1_mn > 0 //only dual earner couples here
drop if missing(errand, hwork, repairs, hobbies)
drop if edu4 == -1

*Marriage dummies [married, straight]
sort wave cpf_hid female
by wave cpf_hid: gen married = (_N == 2)

bysort wave cpf_hid (female): gen straight = (_N == 2 & female[1] != female[2])

*Origin dummies [east, west, mixed_origin]
keep if married == 1 & straight == 1

bysort wave cpf_hid: gen east = (loc89[1] == 1 & loc89[2] == 1)
bysort wave cpf_hid: gen west = (loc89[1] == 2 & loc89[2] == 2)
bysort wave cpf_hid: gen mixed_origin = (loc89[1] != loc89[2])

*Income vars [max_inc, wife_earns_more]
drop if mixed_origin == 1

bysort wave cpf_hid: egen max_inc = max(incjob1_mn)

gen wife_earns_more = (female == 1 & incjob1_mn == max_inc)
replace wife_earns_more = 1 if female == 0 & incjob1_mn != max_inc

*Income shares/ratio [total_incjob1_mn, income_share, female_income_share]
bysort wave cpf_hid: egen total_incjob1_mn = total(incjob1_mn)

*this is probably not the correct "relative income"
gen income_ratio = incjob1_mn / (total_incjob1_mn - incjob1_mn)

gen income_share = incjob1_mn / total_incjob1_mn
gen female_income_share = incjob1_mn / total_incjob1_mn if female == 1
bysort wave cpf_hid (female): replace female_income_share = female_income_share[2] if missing(female_income_share)


*Figure 2 ----
kdensity female_income_share if female == 1 & east == 1
kdensity female_income_share if female == 1 & west == 1


*Figure 3 ----
*TODO: this could possibly be simplified if I share the egen between all graphs and use the ifs inside the fns.
preserve
keep if west == 1 & female == 1
egen fem_share_cat = cut(female_income_share), at(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
egen mean = mean(hwork), by(fem_share_cat)
graph twoway line mean fem_share_cat, sort yscale(r(0 4))
restore

preserve
keep if east == 1 & female == 1
egen fem_share_cat = cut(female_income_share), at(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
egen mean = mean(hwork), by(fem_share_cat)
graph twoway line mean fem_share_cat, sort yscale(r(0 4))
restore

preserve
keep if west == 1 & female == 0
egen fem_share_cat = cut(female_income_share), at(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
egen mean = mean(hwork), by(fem_share_cat)
graph twoway line mean fem_share_cat, sort yscale(r(0 4))
restore

preserve
keep if east == 1 & female == 0
egen fem_share_cat = cut(female_income_share), at(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
egen mean = mean(hwork), by(fem_share_cat)
graph twoway line mean fem_share_cat, sort yscale(r(0 4))
restore

*Regression ----
*NOTE: this two next bits should be a function, but I cant do that because stata sucks

*Age dummies
bysort wave cpf_hid (female): gen male_age = age if female == 0
bysort wave cpf_hid (female): gen female_age = age if female == 1

bysort wave cpf_hid (female): replace male_age = male_age[1] if missing(male_age)
bysort wave cpf_hid (female): replace female_age = female_age[2] if missing(female_age)

gen p_age = male_age if female == 1
replace p_age = female_age if female == 0

drop male_age female_age

*Education dummies
bysort wave cpf_hid (female): gen male_ed = edu4 if female == 0
bysort wave cpf_hid (female): gen female_ed = edu4 if female == 1

bysort wave cpf_hid (female): replace male_ed = male_ed[1] if missing(male_ed)
bysort wave cpf_hid (female): replace female_ed = female_ed[2] if missing(female_ed)

gen p_edu4 = male_ed if female == 1
replace p_edu4 = female_ed if female == 0

drop male_ed female_ed

*Income variables
bysort wave cpf_hid (female): gen p_income = incjob1_mn[1] if female == 1
bysort wave cpf_hid (female): replace p_income = incjob1_mn[2] if female == 0

gen linc = log(incjob1_mn)
gen plinc = log(p_income)
gen lhhd_inc = log(total_incjob1_mn)

*Other dummies
*note missing values with log income
gen kids = (kidsn_hh17 != 0)

*Housework gap
bysort wave cpf_hid: egen couple_hwork = total(hwork)
gen hwork_gap = 2*hwork - couple_hwork if female == 1
bysort wave cpf_hid (female): replace hwork_gap = hwork_gap[2] if missing(hwork_gap)

*TODO: Check s.e. estimator. should be cluster(varlist); cpf_hid?
*TODO: figure out the relative income variable thing, column 3 and 6
*TODO: figure out if relative income is income ratio or share

*define a string with all controls (this should all instead be in a function, but stata sucks)
local cross_sec_controls income_share lhhd_inc linc plinc c.age##c.age c.p_age##c.p_age kids i.edu4 i.p_edu4
local longitudinal_controls c.income_share##c.east lhhd_inc linc plinc c.age##c.age c.p_age##c.p_age kids i.edu4 i.p_edu4

*Panel A
reghdfe hwork wife_earns_more `cross_sec_controls' if female == 1 & west == 1, absorb(wavey state) vce(cluster pid) //(1)
reghdfe hwork wife_earns_more `cross_sec_controls' if female == 1 & east == 1, absorb(wavey state) vce(cluster pid) //(2)
reghdfe hwork wife_earns_more c.wife_earns_more#c.east `longitudinal_controls' if female == 1, absorb(wavey state) vce(cluster pid) //(3)

reghdfe hwork wife_earns_more `cross_sec_controls' if female == 1 & west == 1, absorb(wavey state pid) vce(cluster pid) //(4)
reghdfe hwork wife_earns_more `cross_sec_controls' if female == 1 & east == 1, absorb(wavey state pid) vce(cluster pid) //(5)
reghdfe hwork wife_earns_more c.wife_earns_more#c.east `longitudinal_controls' if female == 1, absorb(wavey state pid) vce(cluster pid) //(6)


*Panel B
*(1)
reghdfe hwork wife_earns_more income_share lhhd_inc linc plinc c.age##c.age c.p_age##c.p_age kids i.edu4 i.p_edu4 ///
if female == 0 & west == 1, absorb(wavey state) vce(cluster pid)

*(2)
reghdfe hwork wife_earns_more income_share lhhd_inc linc plinc c.age##c.age c.p_age##c.p_age kids i.edu4 i.p_edu4 ///
if female == 0 & east == 1, absorb(wavey state) vce(cluster pid)

*(3)
reghdfe hwork wife_earns_more c.wife_earns_more#c.east c.income_share##c.east lhhd_inc linc plinc c.age##c.age c.p_age##c.p_age kids i.edu4 i.p_edu4 ///
if female == 0, absorb(wavey state) vce(cluster pid)

*(4)
reghdfe hwork wife_earns_more income_share lhhd_inc linc plinc c.age##c.age c.p_age##c.p_age kids i.edu4 i.p_edu4 ///
if female == 0 & west == 1, absorb(wavey state pid) vce(cluster pid)

*(5)
reghdfe hwork wife_earns_more income_share lhhd_inc linc plinc c.age##c.age c.p_age##c.p_age kids i.edu4 i.p_edu4 ///
if female == 0 & east == 1, absorb(wavey state pid) vce(cluster pid)

*(6)
reghdfe hwork wife_earns_more c.wife_earns_more#c.east c.income_share##c.east lhhd_inc linc plinc c.age##c.age c.p_age##c.p_age kids i.edu4 i.p_edu4 ///
if female == 0, absorb(wavey state pid) vce(cluster pid)


*Panel C
*TODO: create the correct dep. var.

*(1)
reghdfe hwork_gap wife_earns_more income_share lhhd_inc linc plinc c.age##c.age c.p_age##c.p_age kids i.edu4 i.p_edu4 ///
if female == 1 & west == 1, absorb(wavey state) vce(cluster pid)

*(2)
reghdfe hwork_gap wife_earns_more income_share lhhd_inc linc plinc c.age##c.age c.p_age##c.p_age kids i.edu4 i.p_edu4 ///
if east == 1, absorb(wavey state) vce(cluster pid)

*(3)
reghdfe hwork_gap wife_earns_more c.wife_earns_more#c.east c.income_share##c.east lhhd_inc linc plinc c.age##c.age c.p_age##c.p_age kids i.edu4 i.p_edu4 ///
, absorb(wavey state) vce(cluster pid)

*(4)
reghdfe hwork_gap wife_earns_more income_share lhhd_inc linc plinc c.age##c.age c.p_age##c.p_age kids i.edu4 i.p_edu4 ///
if west == 1, absorb(wavey state pid) vce(cluster pid)

*(5)
reghdfe hwork_gap wife_earns_more income_share lhhd_inc linc plinc c.age##c.age c.p_age##c.p_age kids i.edu4 i.p_edu4 ///
if east == 1, absorb(wavey state pid) vce(cluster pid)

*(6)
reghdfe hwork_gap wife_earns_more c.wife_earns_more#c.east c.income_share##c.east lhhd_inc linc plinc c.age##c.age c.p_age##c.p_age kids i.edu4 i.p_edu4 ///
, absorb(wavey state pid) vce(cluster pid)