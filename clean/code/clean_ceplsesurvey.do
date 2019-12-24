************************************
/* clean_ceplsesurvey.do

Description: This cleans the cep lse survey data.

*/
************************************

clear
cap log close
set more off

*** 0. Load data

use "$rawdatdir/Surveys/Survey_UK/Data_dta/clean_data.dta", clear

*** 1. Sample restrictions

keep if screener1 == 4

*** 2. Clean individual variables

gen hours = self_hour
gen fulltime = self_hour > 30 & self_hour != .
gen male = gender == 1 & gender != 0

// education
label define edu 1 "No qualification" 2 "Some GSCE/O level" ///
3 "5+ GSCE/O level" 4 "Vocational training" 5 "A levels" ///
6 "Bachelor" 7 "Masters" ///
8 "Doctorate"
label values education edu
gen education_tmp = ""
replace education_tmp = "Below secondary" if education < 3
replace education_tmp = "Secondary" if education >= 3 & education <= 5
replace education_tmp = "Tertiary" if education > 5
drop education
rename education_tmp education

// industry
drop self_industry_other
gen industry = ""
replace industry = "Agriculture, forestry and fishing" if self_ind == 1
replace industry = "Energy and Water" if self_ind == 2 | self_ind == 4 | self_ind == 5 
replace industry = "Manufacturing" if self_ind == 3
replace industry = "Construction" if self_ind == 6
replace industry = "Distribution, hotels and restaurants" if self_ind == 7 | self_ind == 9
replace industry = "Transport and communication" if self_ind == 8 | self_ind == 10
replace industry = "Banking and finance" if self_ind >= 11 & self_ind <= 14
replace industry = "Public admin, education and health" if self_ind >= 15 & self_ind <= 17
replace industry = "Other services" if self_ind >= 18 & self_ind <= 21

// reason for being self-employed
gen reason_selfemp = ""
replace reason_selfemp = "Only option" if self_main_reason == 1
replace reason_selfemp = "Can work from home" if self_main_reason == 2
replace reason_selfemp = "Prefer work from home" if self_main_reason == 3
replace reason_selfemp = "Better pay" if self_main_reason == 4
replace reason_selfemp = "Complement pay" if self_main_reason == 5
replace reason_selfemp = "Earn while studying" if self_main_reason == 6
replace reason_selfemp = "Flexibility" if self_main_reason == 7
replace reason_selfemp = "Other" if self_main_reason == 8
	
// more or fewer hours
gen more_fewer_hours = ""
replace more_fewer_hours = "More hours" if desired_hour == 1
replace more_fewer_hours = "Fewer hours" if desired_hour == 2
replace more_fewer_hours = "Satisfied" if desired_hour == 3

// tax benefit
replace tax_benefit = 0 if tax_benefit == 2

// would they prefer employment?
replace prefer_empl = 0 if prefer_empl == 2

// which benefit would they prefer
gen pref_ben = .
foreach i of numlist 1/8 {
replace pref_ben = `i' if benefit`i' == 1
}
label define bens 1 "Retirement savings" 2 "UI" ///
	3 "Paid sick leave" 4 "Health insurance" 5 "Life insurance" ///
	6 "Workers' comp insurance" 7 "Paid family leave" ///
	8 "Disability insurance"
label values pref_ben bens

// employee (do they also work as a traditional employee)
replace employee = 0 if employee == 2

keep age hours fulltime education male industry ///
reason_selfemp more_fewer_hours satisfaction ///
tax_benefit prefer_empl pref_ben employee

*** 3. Final cleaning and export

// sort out missings, drop if any missing
missings tag, gen(missing_vars)
drop if missing_vars > 0
des

save "$cleandatdir/ceplsesurveyclean.dta", replace
export delimited "$cleandatdir/ceplsesurveyclean.csv", replace


