### Run all R code
# Jack Blundell

### 0. Setup ------

setwd("/Users/jack/git_repos/clustersUKselfemployment") # Jack
cleaneddatadir <-  c("/Users/jack/Dropbox/Documents/Projects/Gig Economy/Code/clean/output") # directory where clean data goes
paperfigdir <- c("/Users/jack/Dropbox/Apps/Overleaf/Clusters in UK self-employment/figs") # directory for figure output for paper
presfigdir <- c("/Users/jack/Dropbox/Apps/Overleaf/Clusters in UK self-employment slides/figs") # directory for figure output for slides

### 1. Run all R code ------

source("analysis/code/analysis_fitclusters") # apply clustering algorithms
source("analysis/code/analysis_post") # explore results of clustering
source("analysis/code/analysis_robustness") # test robustness of clustering