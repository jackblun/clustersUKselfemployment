### Robustness of clustering
# Jack Blundell

### 0. Setup ------

rm(list = ls()) 	
options("scipen"=100, "digits"=4)
set.seed(123)
setwd("/Users/jack/Dropbox/Documents/Projects/Gig Economy/Code") # main output directory
paperfigdir <- c("/Users/jack/Dropbox/Apps/Overleaf/Clusters in UK self-employment/figs") # directory for output for paper

# Load packages

library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)
library(factoextra)
library(plotly)
library(ggalluvial)
library(reshape2)
library(NbClust)

### 1. Test how much clustering affected by setting different seeds

df.full <- read.csv("clean/output/lfsjanmarch2018clean.csv")

# need to recode some variables
df.full$education <- ordered(df.full$education)

# restrict to set of variables used in clustering
df <- df.full %>% select(-weight, - missing_vars, - hours,
                          - ethnicity, # tenure
                         -birth_country, # region
                          - emp_type, - industry_det, #- marital_stat,
                         - occupation_det, - ethnicity, - has_employees,
                         - workfromhome, -mult_jobs, -tenure, -also_emp, 
                         - marital_stat, - region)

gower_dist <- daisy(df, metric = "gower")
gower_mat <- as.matrix(gower_dist) # Note that diagonal entries are zero, as they represent the same individual

seedvec <- c(1,2,3,4,5,6,7,8,9,10)
sillist <- list()
k <- 0
for (seed in seedvec) {
  set.seed(seed)
  k <- k+1
# make silhouette figure to explore different numbers of clusters
sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
sillist[[k]] <- sil_width
}

for (i in 1:10) {
  # ggplot silwidth plot
  sw.df <- as.data.frame(sillist[[i]])
  sw.df <- sw.df %>% mutate(numclus = row_number())
  names(sw.df)[1] = "sil_width"
  ggplot(data = sw.df) + geom_line(aes(y = sil_width, x = numclus)) + 
    scale_x_continuous(breaks = seq(1,8,1)) +
    scale_y_continuous(breaks = seq(0.2,0.3,0.01)) +
    xlab("Number of clusters") +
    ylab("Average silhouette width")
  ggsave(filename = paste("analysis/output/robustness/silseed",i,".png", sep = ""))
  ggsave(filename = paste("analysis/output/robustness/silseed",i,".png", sep = ""))
}

### 2. Try dropping features one at a time and inspect silwidths

df.full <- read.csv("clean/output/lfsjanmarch2018clean.csv")

# need to recode some variables
df.full$education <- ordered(df.full$education)

# restrict to set of variables used in clustering
df <- df.full %>% select(-weight, - missing_vars, - hours,
                         - ethnicity, # tenure
                         -birth_country, # region
                         - emp_type, - industry_det, #- marital_stat,
                         - occupation_det, - ethnicity, - has_employees,
                         - workfromhome, -mult_jobs, -tenure, -also_emp, 
                         - marital_stat, - region)


sillist <- list()
k <- 0
for (dropped_var in 1:dim(df)[2]) {
  k <- k+1
  
  df.res <- df[-dropped_var]
  gower_dist <- daisy(df.res, metric = "gower")
  gower_mat <- as.matrix(gower_dist) # Note that diagonal entries are zero, as they represent the same individual
  
    # make silhouette figure to explore different numbers of clusters
  sil_width <- c(NA)
  for(i in 2:8){  
    pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
    sil_width[i] <- pam_fit$silinfo$avg.width  
  }
  sillist[[k]] <- sil_width
}

saveRDS(object = sillist, file = "analysis/intermediate/droppingests.rds")

sillist <- readRDS(file = "analysis/intermediate/droppingests.rds")

sw.df <- as.data.frame(sillist)
names(sw.df) <- names(df)
sw.df <- sw.df %>% mutate(numclus = row_number())
names(sw.df) <- c("Sex","Age","Education","Occupation","Industry","Full-time","numclus")
sw.df <- melt(sw.df, id.vars = "numclus", variable.name = "dropvar", value.name = "silwidth")

ggplot(data = sw.df, aes(numclus, silwidth)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color="steelblue") + 
  labs(y = "Average silhouette width", x = "Number of clusters") + 
  facet_wrap(~ dropvar) + 
  theme(strip.text.x = element_text(
    color = "black", family="serif"),
    axis.title = element_text(family="serif"),
    text = element_text(family="serif"))
ggsave(filename = paste("analysis/output/robustness/sildrop.png", sep = ""), width = 15, height = 10, units = "cm")
ggsave(filename = paste(paperfigdir,"/robustness/sildrop.png", sep = ""), width = 15, height = 10, units = "cm")

### 3. Heirachical clustering 
# see this blog https://towardsdatascience.com/hierarchical-clustering-on-categorical-data-in-r-a27e578f2995

df.full <- read.csv("clean/output/lfsjanmarch2018clean.csv")

# need to recode some variables
df.full$education <- ordered(df.full$education)

# restrict to set of variables used in clustering
df <- df.full %>% select(-weight, - missing_vars, - hours,
                         - tenure, - ethnicity,
                         -birth_country,  - region,
                         - marital_stat, - emp_type, - industry_det,
                         - occupation_det, - ethnicity, - has_employees,
                         - workfromhome)


gower_dist <- daisy(df, metric = "gower")
gower_mat <- as.matrix(gower_dist) # Note that diagonal entries are zero, as they represent the same individual

divisive.clust <- diana(gower_mat, diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive")


### 4. Different variable weighting regimes

df.full <- read.csv("clean/output/lfsjanmarch2018clean.csv")

# need to recode some variables
df.full$education <- ordered(df.full$education)

# restrict to set of variables used in clustering
df <- df.full %>% select(-weight, - missing_vars, - hours,
                         - ethnicity, - tenure, 
                         -birth_country,  - region,
                         - emp_type, - industry_det, - marital_stat, 
                         - occupation_det, - ethnicity, - has_employees,
                         - workfromhome, -mult_jobs, - also_emp)

sillist <- list()
numruns = 20
for (k in 1:numruns) {
  weights <- runif(6)
  #weights <- c(1,2,1,1,1,1)
  gower_dist <- daisy(df, metric = "gower", weights = weights)
  gower_mat <- as.matrix(gower_dist) # Note that diagonal entries are zero, as they represent the same individual
  
  # make silhouette figure to explore different numbers of clusters
  sil_width <- c(NA)
  for(i in 2:8){  
    pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
    sil_width[i] <- pam_fit$silinfo$avg.width  
  }
  sillist[[k]] <- sil_width
}
saveRDS(object = sillist, file = "analysis/intermediate/weightingests.rds")

sillist <- readRDS(file = "analysis/intermediate/weightingests.rds")

df.full <- read.csv("clean/output/lfsjanmarch2018clean.csv")

rescale <- function(x) {  
  ave <- mean(x, na.rm = TRUE)
  var <- var(x, na.rm = TRUE)
  return((x - ave)/(var)^0.5)
}

names(sillist) = seq(1,numruns,1)
sillist_rescaled <- lapply(sillist, rescale)

sw.df <- as.data.frame(sillist_rescaled)
sw.df <- sw.df %>% mutate(numclus = row_number())
df.long<-melt(sw.df,id.vars="numclus")
ggplot(data = df.long) + geom_line(aes(y = value, x = numclus, color=variable)) + 
  scale_x_continuous(breaks = seq(1,8,1)) +
  xlab("Number of clusters") +
  ylab("Average silhouette width") +
  theme(legend.position = "none",
        axis.title = element_text(family="serif"),
        text = element_text(family="serif"))
ggsave(filename = paste("analysis/output/robustness/silweights.png", sep = ""), width = 15, height = 10, units = "cm")
ggsave(filename = paste(paperfigdir,"/robustness/silweights.png", sep = ""), width = 15, height = 10, units = "cm")

#ggsave(filename = paste("analysis/output/robustness/silrunifweight",i,".png", sep = ""))

### 5. Alternative ways of choosing the number of clusters
# see this blog: https://towardsdatascience.com/10-tips-for-choosing-the-optimal-number-of-clusters-277e93d72d92
# these are not working very well, scaling possibly an issue.

# Elbow method
df <- df.full %>% select(-weight, - missing_vars, - hours,
                         - tenure, - ethnicity,
                         -birth_country,  - region,
                         - marital_stat, - emp_type, - industry_det,
                         - occupation_det, - ethnicity, - has_employees,
                         - workfromhome, -mult_jobs, - also_emp)

gower_dist <- daisy(df, metric = "gower")
pam1 <- function(df, k) {
  pam_fit <- pam(gower_dist, diss = TRUE, k = k)
  return(pam_fit)
}

dfmat <- as.matrix(df)
# function to compute total within-cluster sum of squares (takes too long)
fviz_nbclust(df, FUNcluster = pam1, method = "wss", k.max = 10) + theme_minimal() + ggtitle("the Elbow Method")

# Gap statistic
gap_stat <- clusGap(mammals_scaled, FUN = kmeans, nstart = 30, K.max = 24, B = 50)
fviz_gap_stat(gap_stat) + theme_minimal() + ggtitle("fviz_gap_stat: Gap Statistic")

# NB clust compares lots of different ones
df.full <- read.csv("clean/output/lfsjanmarch2018clean.csv")

# need to recode some variables
df.full$education <- ordered(df.full$education)

# restrict to set of variables used in clustering
df <- df.full %>% select(-weight, - missing_vars, - hours,
                         - tenure, - ethnicity,
                         -birth_country,  - region,
                         - marital_stat, - emp_type, - industry_det,
                         - occupation_det, - ethnicity, - has_employees,
                         - workfromhome, -mult_jobs, - also_emp)

gower_dist <- daisy(df, metric = "gower")
res.nbclust <- NbClust(df, diss = gower_dist,
                       min.nc = 2, max.nc = 9, 
                       method = "median")
factoextra::fviz_nbclust(res.nbclust) + theme_minimal() + ggtitle("NbClust's optimal number of clusters")

### 6. Test cluster assignment under different weighting regimes

df.full <- read.csv("clean/output/lfsjanmarch2018clean.csv")

# restrict to set of variables used in clustering
df <- df.full %>% select(-weight, - missing_vars, - hours,
                         - ethnicity, # tenure
                         -birth_country, # region
                         - emp_type, - industry_det, #- marital_stat,
                         - occupation_det, - ethnicity, - has_employees,
                         - workfromhome, -mult_jobs, -tenure, -also_emp, 
                         - marital_stat, - region)
# need to recode some variables
df.full$education <- ordered(df.full$education)

# numruns 
numruns = 20
clusmat = matrix(nrow = dim(df)[1],ncol = numruns)

for (k in 1:numruns) {
  weights <- runif(6)
  gower_dist <- daisy(df, metric = "gower", weights = weights)
  gower_mat <- as.matrix(gower_dist) # Note that diagonal entries are zero, as they represent the same individual
  pam_fit <- pam(gower_dist, diss = TRUE, k = 2)
  clusmat[,k] <- pam_fit$clustering
}

# need to identify analogous clusters for different weights
malevec <- df.full$male
for (k in 1:numruns) {
  vecsing <- clusmat[,k]
  maleclus <- cbind(malevec,vecsing)
  maleclusdf <- as.data.frame(maleclus)
  names(maleclusdf) <- c("male","group")
  maleclusdf %>% group_by("group") %>% 
    count(male) %>% 
    mutate(percent = n/sum(n))
}



