************************************
/* set_globals.do

Description: This sets up globals for the gig economy paper

*/
************************************

clear
cap log close
set more off

// Set directory 
cd "/Users/jack/git_repos/clustersUKselfemployment" // Jack

// set globals
global cleandatdir "/Users/jack/Dropbox/Documents/Projects/Gig Economy/Code/clean/output" // where to put clean data
global rawdatdir "/Users/jack/Dropbox/Documents/Projects/Gig Economy/Raw Data" // where raw data is found 
