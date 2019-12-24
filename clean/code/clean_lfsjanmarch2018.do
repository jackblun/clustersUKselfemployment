************************************
/* clean_lfsjjanmarch2018.do

Description: This cleans the raw lfs data.

*/
************************************

clear
cap log close
set more off

*** 0. Load raw LFS data

use "$rawdatdir/LFS/Jan-March 2018/UKDA-8343-stata/stata/stata11/lfsp_jm18_eul.dta", clear

*** 1. Sample restrictions

// self employed
keep if STAT == 2

// people in work
keep if ILO == 1

*** 2. Select vars of interest

// these are all variables that could potentially use in analysis

keep AGE SEX MARSTA HOURPAY NET99 GROSS99 SC10MMJ SC10MMN SOC10M LEVQUL15 ///
GOVTOF2 QULNOW PWT18  PAIDHRU  PAIDHRA NSECM10 JOBTYP INECAC05 INDD07M  ///
INDC07M ILODEFR HOME BUSHR BENFTS BACTHR AGWRK ADDJOB SELF* SOLOR ///
FUTUR13 HIQUL15D INDE07M ETHUKEUL HDPCH19 FTPT HEALTH HOME ///
TEN1 UNDEMP CRYOX7_EUL_Main PWT18 INDG07M STAT2

*** 3. Clean variables

gen male = SEX == 1
label var male "Male"

gen looking_newjob = ADDJOB == 1
label var looking_newjob "Looking for a new job?"
replace looking_newjob = . if ADDJOB == -8

gen age = AGE
label var age "Age"

gen benefits = BENFTS == 1
label var benefits "Receives benefits"
replace benefits = . if BENFTS < 0

decode GOVTOF2, gen(region)
label var region "Region"

gen hours = PAIDHRA
label var hours "Hours worked"
replace hours = . if hours < 0

decode HIQUL15D, gen(education)
replace education = "Tertiary" if education == "Degree or equivalent" | education == "Higher education"
replace education = "Secondary" if education == "GCE A level or equivalent" | education == "GCSE grades A*-C or equivalent"
replace education = "Below secondary" if education == "No qualification" | education == "Other qualification" 
replace education = "" if education == "Don't know" | education == "No answer"
label var education "Education"

decode MARSTA, gen(marital_stat)
replace marital_stat = "Married" if marital_stat == "Married, living with husbandwife" | ///
marital_stat == "Civil Partner"
replace marital_stat = "Never married" if marital_stat == "Single, never married"
replace marital_stat = "Widowed/separated" if marital_stat == "Married, separated from husbandwife" | ///
marital_stat == "Divorced" | marital_stat == "Widowed" | marital_stat == "Separated Civil Partner"
replace marital_stat = "Married/separated" if marital_stat == "Married" | marital_stat == "Widowed/separated"

decode SC10MMJ, gen(occupation)
replace occ = "" if occ == "No answer"
replace occ = substr(occ, 3,length(occ))
replace occ = subinstr(occ,"'","",2)
label var occ "Occupation"
replace occ = substr(occ,2,length(occ)) if strpos(occ, " ") == 1 // needed to drop off first space in string
replace occ = subinstr(occ," Occupations","",1) 
replace occ = subinstr(occ," Operatives"," Ops",1) 
replace occ = subinstr(occ," And Senior Officials","",1) 
replace occ = subinstr(occ,"Associate","Ass.",1) 
replace occ = subinstr(occ," And",",",1) 

gen has_employees = SOLOR == 2
label var has_employees "Has employees?"
replace has_employees = . if SOLOR < 0

decode INDE07M, gen(industry)
replace ind = "" if ind == "No answer"
replace ind = substr(ind, 4,length(ind))
replace ind = subinstr(ind,"-","",2)
replace ind = "Energy and Water" if ind == ",E  Energy and water"
replace ind = "Banking and finance" if ind == ",M,N  Banking and finance"
replace ind = "Public admin, education and health" if ind == ",Q  Public admin, education and health"
replace ind = "Other services" if ind == ",T,U  Other services"
replace ind = "Transport and communication" if ind == " Transport and communication"
replace ind = "Construction" if ind == " Construction"
replace ind = "Manufacturing" if ind == " Manufacturing"
replace ind = "Agriculture, forestry and fishing" if ind == " Agriculture, forestry and fishing"
replace ind = "Distribution, hotels and restaurants" if ind == " Distribution, hotels and restaurants"

decode SELF1, gen(emp_type)
replace emp_type = "" if emp_type == "No answer"
replace emp_type = "Partner / running business" if emp_type == "Running a business or prof practice" ///
| emp_type == "Partner in business or prof practice"

decode CRYOX7_EUL_Main, gen(birth_country)
replace birth_country = "" if birth_country == "No answer"
replace birth_country = "Other Europe" if birth_country == "European Union (excluding UK)"

decode ETHUKEUL, gen(ethnicity)
replace ethnicity = "" if ethnicity == "No answer"
replace eth = "Asian" if eth == "Bangladeshi" | eth == "Pakistani" ///
| eth == "Indian" | eth == "Any other Asian background" | eth == "Chinese"
replace eth = "Mixed/other" if eth == "Mixed/Multiple ethnic groups" | eth == "Other ethnic group"

gen dep_children = HDPCH19
replace dep_children = . if dep_children < 0
label var dep_children "Number of dependent children"

gen fulltime = FTPT == 1
replace fulltime = . if FTPT == -8
label var fulltime "Works full time"

gen health_problem = HEALTH > 0
replace health_problem = . if HEALTH == -8
label var health_problem "Health problem"

decode HOME, gen(workfromhome)
replace workfromhome = "" if HOME < 0
label var workfromhome "Whether works from home"

decode TEN, gen(tenure)
replace tenure = "" if TEN < 0
label var tenure "Tenure"
replace tenure = "Rented" if tenure == "Part rent"

decode INDG07M, gen(industry_det)

decode SC10MMN, gen(occupation_det)

// whether has a standard employment job as well
gen also_emp = 0
replace also_emp = 1 if STAT2 == 1

// whether has multiple jobs
gen mult_jobs = 0
replace mult_jobs = 1  if STAT2 > 0

//gen wantmorehours = UNDEMP == 1 // too many missings for this one
//replace wantmorehours = . if UNDEMP < 0

rename PWT18 weight

*** 4. Keep cleaned variables that we want in analysis and drop if missing any data

keep age region hours education marital ///
occ* has_employees ind* emp_type fulltime eth birth work tenure male ///
 weight also_emp mult_jobs

// sort out missings, drop if any missing
missings tag, gen(missing_vars)
drop if missing_vars > 0
des

*** 5. Save cleaned dataset as stata and csv

sort age
order male age region education occupation industry hours emp_t also_emp
save "$cleandatdir/lfsjanmarch2018clean.dta", replace
export delimited "$cleandatdir/lfsjanmarch2018clean.csv", replace


