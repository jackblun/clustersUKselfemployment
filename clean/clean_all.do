************************************
/* clean_all.do

Description: This cleans all data ready for analysis.

*/
************************************

clear
cap log close
set more off

do "clean/code/clean_ceplsesurvey.do"
do "clean/code/clean_lfsjanmarch2018.do"
