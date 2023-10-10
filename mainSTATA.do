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
 
 *only dual earner couples here
keep if whweek >= 0 & incjob1_mg > 0 & incjob1_mn > 0
drop if missing(errand, hwork, repairs, hobbies)
drop if edu4 == -1

*Marriage dummies
gen married = 0
sort wave cpf_hid female
by wave cpf_hid: replace married = 1 if _N == 2

gen straight = .
bysort wave cpf_hid (female): replace straight = 1 if _N == 2 & female[1] != female[2]
bysort wave cpf_hid (female): replace straight = 0 if _N == 2 & female[1] == female[2]

*Origin dummies
keep if married == 1 & straight == 1

gen east = 0
gen west = 0
gen mixed_origin = 0

bysort wave cpf_hid: replace east = 1 if loc89[1] == 1 & loc89[2] == 1
bysort wave cpf_hid: replace west = 1 if loc89[1] == 2 & loc89[2] == 2
bysort wave cpf_hid: replace mixed_origin = 1 if loc89[1] != loc89[2]

*Income dummies
drop if mixed_origin == 1

bysort wave cpf_hid: egen max_inc = max(incjob1_mn)

gen wife_earns_more = 0
replace wife_earns_more = 1 if female == 1 & incjob1_mn == max_inc
replace wife_earns_more = 1 if female == 0 & incjob1_mn != max_inc

*Income shares
bysort wave cpf_hid: egen total_incjob1_mn = total(incjob1_mn)

gen income_share = .
gen female_income_share = .

replace income_share = incjob1_mn / total_incjob1_mn
replace female_income_share = incjob1_mn / total_incjob1_mn if female == 1
bysort wave cpf_hid (female): replace female_income_share = female_income_share[2] if missing(female_income_share)


*Figure 2 ----
preserve
keep if female == 1 & east == 1
kdensity female_income_share
restore

preserve
keep if female == 1 & west == 1
kdensity female_income_share
restore


* Figure 3 ----
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

*Age dummies
gen male_age = .
gen female_age = .

bysort wave cpf_hid (female): replace male_age = age if female == 0
bysort wave cpf_hid (female): replace female_age = age if female == 1

bysort wave cpf_hid (female): replace male_age = male_age[1] if missing(male_age)
bysort wave cpf_hid (female): replace female_age = female_age[2] if missing(female_age)

gen p_age = male_age if female == 1
replace p_age = female_age if female == 0

drop male_age female_age

*Education dummies
gen male_ed = .
gen female_ed = .

bysort wave cpf_hid (female): replace male_ed = edu4 if female == 0
bysort wave cpf_hid (female): replace female_ed = edu4 if female == 1

bysort wave cpf_hid (female): replace male_ed = male_ed[1] if missing(male_ed)
bysort wave cpf_hid (female): replace female_ed = female_ed[2] if missing(female_ed)

gen p_edu4 = male_ed if female == 1
replace p_edu4 = female_ed if female == 0

drop male_ed female_ed

*Other dummies
*note missing values with log income
gen lhhd_inc = log(total_incjob1_mn)
gen age2 = age^2
gen p_age2 = p_age^2
gen kids = (kidsn_hh17 != 0)
gen wife_east = wife_earns_more * east

*Panel A (1)
preserve
keep if female == 1 & west == 1
reghdfe hwork wife_earns_more lhhd_inc age p_age age2 p_age2 kids i.edu4 i.p_edu4, absorb(wavey state)
restore

*Panel A (2)
preserve
keep if female == 1 & east == 1
reghdfe hwork wife_earns_more lhhd_inc age p_age age2 p_age2 kids i.edu4 i.p_edu4, absorb(wavey state)
restore

*Panel A (3)
preserve
keep if female == 1
reghdfe hwork wife_earns_more wife_east east lhhd_inc age p_age age2 p_age2 kids i.edu4 i.p_edu4, absorb(wavey state)
restore
