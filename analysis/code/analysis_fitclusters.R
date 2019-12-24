### Fit clusters to LFS data
# Jack Blundell

### 0. Setup ------

rm(list = ls()) 	
options("scipen"=100, "digits"=4)
set.seed(123)
setwd("/Users/jack/Dropbox/Documents/Projects/Gig Economy/Code") # Jack
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

### 1. Load cleaned LFS data (cleaned in stata)

df.full <- read.csv("clean/output/lfsjanmarch2018clean.csv")

# need to recode some variables
df.full$education <- ordered(df.full$education)

# restrict to set of variables used in clustering
df <- df.full %>% select(-weight, - missing_vars, - hours,
                         - ethnicity, - tenure, 
                         -birth_country,  - region,
                          - industry_det, - marital_stat, - emp_type,
                         - occupation_det, - ethnicity, - has_employees,
                         - workfromhome, -mult_jobs, - also_emp)

df.grouped <- df.full %>% group_by(male, education, occupation, industry, fulltime)

### 2. calculate Gower dissimilarity between each observation

gower_dist <- daisy(df, metric = "gower")
gower_mat <- as.matrix(gower_dist) # Note that diagonal entries are zero, as they represent the same individual

# inspect most similar individuals
df[which(gower_mat == min(gower_mat[gower_mat != 0]), arr.ind = TRUE)[1, ], ]

# inspect least similar individuals
df[which(gower_mat == max(gower_mat), arr.ind = TRUE)[1, ], ]

### 3. Determine number of types via average silhouette

# make silhouette figure to explore different numbers of clusters
sil_width <- c(NA)
for(i in 2:10){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}

# ggplot silwidth plot
sw.df <- as.data.frame(sil_width)
sw.df <- sw.df %>% mutate(numclus = row_number())
ggplot(data = sw.df) + geom_line(aes(y = sil_width, x = numclus), color='steelblue', alpha = 0.8, size = 1.5) + 
  scale_x_continuous(breaks = seq(1,10,1)) +
  scale_y_continuous(breaks = seq(0.2,0.3,0.02)) +
  xlab("Number of clusters") +
  ylab("Average silhouette width")  +
  theme(text = element_text(size=30))
ggsave(filename = "analysis/output/sil.png", 
      height = 4, width = 7)
ggsave(file = paste(paperfigdir,"/sil.png", sep = ""),
       width = 13, height = 8)

# alternative quick but ugly silwidth plot
# png("analysis/output/sil.png")
#plot(1:10, sil_width,
#     xlab = "Number of clusters",
#     ylab = "Silhouette Width", 
#     ylim = c(0.2, 0.3))
#lines(1:10, sil_width)
#dev.off()

### 4. Fit chosen clusters

numclusters <- c(2,6) # choose how many clusters we want to fit based on silhouette plot

cluster.results <- list()
k <- 0
for (i in numclusters) {
  k <- k+1
  # summary of each cluster
  pamcluster_fit <- pam(gower_dist, diss = TRUE, k = i)
  cluster.results[[k]] <- pamcluster_fit
}

# export clusters
saveRDS(object = cluster.results, file = "analysis/intermediate/clusterresults.rds")

### 5. cluster diagnostics        

### 5.1 silhouettes

k <- 0
for (i in numclusters) {
  k <- k+1
  fviz_silhouette(silhouette(cluster.results[[k]])) + 
    scale_y_continuous(breaks = seq(0,0.6,0.1)) +
    #scale_fill_discrete(labels=c("a","b"), name = "") +
    guides(col=FALSE) +
    ggtitle("") + 
    ylab("Silhouette width") +
    theme(legend.position = "bottom", legend.direction = "horizontal")
  ggsave(filename = paste("analysis/output/sil",i,"cluster.png", sep = ""))  
}

### 5.2 median types

k <- 0
for (i in numclusters) {
  k <- k+1
  print(df.full[cluster.results[[k]]$medoids,])
}

### 5.3 visualize in lower-dim space

# visualize clusters in lower-dim space
k <- 0
for (i in numclusters) {
  k <- k+1
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(cluster.results[[k]]$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
geom_point(aes(color = cluster))
ggsave(filename = paste("analysis/output/lowerdim",i,".png"))
}

### 5.4 check which individuals are poorly assigned
# note that this is not working - mismatch in silwidths and row numbers

# associate sil widths with row number
silwidths <- as.data.frame(rownames(cluster.results[[1]]$silinfo$widths))
silwidths$silwidth <- cluster.results[[1]]$silinfo$widths[,3]
names(silwidths)[1] <- "index"
silwidths$index <- as.numeric(silwidths$index)
silwidths <- arrange(silwidths, index)

# assign along with cluster
df$cluster2 <- factor(cluster.results[[1]]$clustering)
df$silwidth <- silwidths$silwidth
ordering <- df %>% group_by(cluster2) %>% 
  mutate(male = as.numeric(male)) %>%
  summarise(male_share = mean(male)) %>%
  arrange(-male_share)
cluster2 <- as.vector(ordering$cluster2)
labels2 <- c("A","B")
clusterorder2 <- as_tibble(cbind(cluster2,labels2))
df <- inner_join(df,clusterorder2, by = "cluster2")
df$labels2 <- as.factor(df$labels2)

# inspect those with negative silwidths
df %>% filter(silwidth < 0)

