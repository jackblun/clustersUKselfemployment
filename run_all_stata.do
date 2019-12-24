************************************
/* run_all_stata.do

Description: This runs all stata code for gig economy project

*/
************************************

clear
cap log close
set more off

// Set directory 
cd "/Users/jack/Dropbox/Documents/Projects/Gig Economy/Code" // Jack

do "set_globals.do"
do "clean/clean_all.do"
