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
keep if whweek >= 0 & incjob1_mg >= 0 & incjob1_mn >= 0
drop if missing(errand, hwork, repairs, hobbies)
*drop if edu4 == -1

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