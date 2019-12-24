### Post cluster analysis
# Jack Blundell

### 0. Setup ------

rm(list = ls()) 	
options("scipen"=100, "digits"=4)
set.seed(123)
setwd("/Users/jack/Dropbox/Documents/Projects/Gig Economy/Code") # Jack
paperfigdir <- c("/Users/jack/Dropbox/Apps/Overleaf/Clusters in UK self-employment/figs") # directory for output for paper
presfigdir <- c("/Users/jack/Dropbox/Apps/Overleaf/Clusters in UK self-employment slides/figs") # directory for output for paper

# Load packages

library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)
library(factoextra)
library(plotly)
library(ggalluvial)
library(tidyr)

### 1. Load cleaned LFS and survey data (cleaned in stata)

cep.full <- read.csv("clean/output/ceplsesurveyclean.csv")
cep.full <- cep.full %>% mutate(dataset = "CEP",
                                male = factor(male),
                                fulltime = factor(fulltime,
                                                  labels = c("Part time",
                                                             "Full time")),
                                male = factor(male, labels = c("Female","Male")),
                                id = row_number(),
                                tax_benefit = factor(tax_benefit),
                                prefer_empl = factor(prefer_empl,
                                                     labels = c("No",
                                                                "Yes")),
                                employee = factor(employee,
                                                  labels = c("No",
                                                             "Yes")),
                                reason_selfemp = replace(reason_selfemp, reason_selfemp == "Prefer work from home", "Can work from home")) 
cep.full$education <- ordered(cep.full$education) # make education ordinal
cep.full$type <- "cepdat"

# recode satisfaction variable into 3 groups
cep.full <- cep.full %>% mutate(satisfaction = replace(satisfaction, satisfaction == 1, 2),
                                satisfaction = replace(satisfaction, satisfaction == 5, 4),
                                satisfaction = satisfaction - 1,
                                satisfac = factor(satisfaction, labels = c("Satisfied", "Neutral", "Not Satisfied")))


lfs.full <- read.csv("clean/output/lfsjanmarch2018clean.csv")
lfs.full <- lfs.full %>% mutate(dataset = "LFS",
                                workfromhome = factor(workfromhome),
                                male = factor(male),
                                mult_jobs = factor(mult_jobs),
                                fulltime = factor(fulltime,
                                                  labels = c("Part time",
                                                  "Full time")),
                                male = factor(male, labels = c("Female","Male")))



### 2. Assign and label clusters in LFS, explore patterns

# load clusters
cluster.results <- readRDS(file = "analysis/intermediate/clusterresults.rds")

# assign cluster labels
# rank in order of share male
lfs.full$cluster2 <- factor(cluster.results[[1]]$clustering)
ordering <- lfs.full %>% group_by(cluster2) %>% 
  mutate(male = as.numeric(male)) %>%
  summarise(male_share = mean(male)) %>%
  arrange(-male_share)
cluster2 <- as.vector(ordering$cluster2)
labels2 <- c("MaLE","FeDe")
clusterorder2 <- as_tibble(cbind(cluster2,labels2))
lfs.full <- inner_join(lfs.full,clusterorder2, by = "cluster2")
lfs.full$labels2 <- as.factor(lfs.full$labels2)

lfs.full$cluster6 <- factor(cluster.results[[2]]$clustering)
ordering <- lfs.full %>% group_by(cluster6) %>% 
  mutate(male = as.numeric(male)) %>%
  summarise(male_share = mean(male)) %>%
  arrange(-male_share)
cluster6 <- as.vector(ordering$cluster6)
labels6 <- c("Construction workers","Low-educated young men","Managers",
             "London professionals","Older health/educ workers",
             "Female service workers") # name groups from most male to least
clusterorder6 <- as_tibble(cbind(cluster6,labels6))
lfs.full <- inner_join(lfs.full,clusterorder6, by = "cluster6")
lfs.full$labels6 <- as.factor(lfs.full$labels6)

# Get some summary stats
lfs.full %>% group_by(cluster2) %>% 
  summarise(mean(age))

lfs.full %>% group_by(cluster2) %>% 
  count(male) %>% 
  mutate(percent = n/sum(n))

lfs.full %>% group_by(cluster2) %>% 
  count(occupation) %>% 
  mutate(percent = n/sum(n))

lfs.full %>% group_by(cluster2) %>% 
  count(education) %>% 
  mutate(percent = n/sum(n))

lfs.full %>% group_by(cluster2) %>% 
  count(industry) %>% 
  mutate(percent = n/sum(n))

lfs.full %>% group_by(cluster2) %>% 
  count(fulltime) %>% 
  mutate(percent = n/sum(n))

# produce graphs of categorical variables by cluster
col <- c("education","occupation", "male", "region", "fulltime", "ethnicity",
         "emp_type","workfromhome", "industry", "mult_jobs")
legend.labs <- c("Education","Occupation","Sex","Region","PT/FT",
                 "Ethnicity","Emp type","Work from home","Industry","Holds multiple jobs")
clust <- c("labels2","labels6")
k <- 0
for (i in col) {
  k <- k+1
  for (j in clust) {
  print(i)
    print(j)
    print(legend.labs[k])
  g <- lfs.full %>% group_by(!!ensym(j), !!ensym(i))  %>%
    summarise (n = n()) %>%
    mutate(share = 100*n / sum(n))
  ggplot(data = g) + 
    geom_bar(mapping = aes(x = !!ensym(j), y = share, fill = !!ensym(i), group = !!ensym(i)), 
           position = "stack", stat = "identity", alpha = 0.7) + 
    scale_fill_brewer(type = "qual", palette = "Set1") +
    theme_minimal(base_size = 25) + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1),
                                     text = element_text(size=25), #, family="serif"
          legend.position = 'right', 
          legend.spacing.x = unit(0.5, 'cm'),
          legend.direction = 'vertical',
          legend.key.size = unit(1.2,'cm'),
          legend.title=element_text(size=25),
          plot.margin=unit(c(2,1,1,1),"cm")) +
  labs(fill = paste(legend.labs[k]), x = "", y = "% share")
  ggsave(file = paste("analysis/output/lfsdescriptives/",i,"lfs",j,".png", sep = ""),
         width = 13, height = 8)
  ggsave(file = paste(paperfigdir,"/LFSgraphs/",i,"lfs",j,".png", sep = ""),
         width = 13, height = 8)
  ggsave(file = paste(presfigdir,"/LFSgraphs/",i,"lfs",j,".png", sep = ""),
         width = 13, height = 8)
 
}
}

# produce graphs of continuous variables by cluster
col <- c("hours","age")
legend.labs <- c("Hours","Age")
k <- 0
for (i in col) {
  k <- k+1
  for (j in clust) {
    print(i)
    print(j)
    print(legend.labs[k])
    ggplot(data = lfs.full, aes(x = !!ensym(i), color = !!ensym(j))) + 
      geom_density(alpha = 0.8, size = 1.2) + 
      scale_fill_brewer(type = "qual", palette = "Set1") + 
      theme_minimal(base_size = 25) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  text = element_text(size=28), #, family="serif"
                  legend.position = 'right', 
                  legend.spacing.x = unit(0.5, 'cm'),
                  legend.direction = 'vertical',
                  legend.key.size = unit(1.2,'cm'),
            plot.margin=unit(c(2,1,1,1),"cm")) +
      labs(x = paste(legend.labs[k]), color = "Cluster", y = "Density")
    ggsave(file = paste("analysis/output/lfsdescriptives/",i,"lfs",j,".png", sep = ""),
           width = 13, height = 8)
    ggsave(file = paste(paperfigdir,"/LFSgraphs/",i,"lfs",j,".png", sep = ""),
           width = 13, height = 8)
    ggsave(file = paste(presfigdir,"/LFSgraphs/",i,"lfs",j,".png", sep = ""),
           width = 13, height = 8)
  }
}

# produce graphs of population shares using lfs weights
for (j in clust) {
    print(j)
  df.share <- lfs.full %>% group_by(!!ensym(j)) %>%
       summarise(n = sum(weight)) %>%
       mutate(share = 100*n/sum(n))
  ggplot(data = df.share) +
  geom_bar(mapping = aes(x = !!ensym(j), y = share, fill = !!ensym(j)),
              stat = "identity", position = "dodge") + 
     theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggsave(file = paste("analysis/output/lfsdescriptives/sharelfs",j,".png", sep = ""))
}

# produce alluvial linking 2 and 6 cluster case
ggplot(data = lfs.full %>% group_by(labels6, labels2) %>%
          summarise (n = n()) %>%
         mutate(share = 100*n/sum(n)),
        aes(y = share, axis1 = labels2, axis2 = labels6)) +
   geom_alluvium(aes(fill = labels2), width = 1/12) +
   geom_stratum(width = 1/12, fill = "black", color = "grey") +
   geom_label(stat = "stratum", label.strata = TRUE, size = 6) + # family = "serif",
   scale_x_discrete(limits = c("Broad cluster", "Narrow cluster"), expand = c(.05, .05)) +
   scale_fill_brewer(type = "qual", palette = "Set1", direction=-1) +
   guides(fill = FALSE) + theme_void() +
  theme(text = element_text(size=16, family="serif")) 
   ggsave("analysis/output/lfsdescriptives/alluvial.png",
          height = 8, width = 13)
   ggsave(file = paste(paperfigdir,"/LFSgraphs/alluvial.png", sep = ""),
          width = 13, height = 8)
   ggsave(file = paste(presfigdir,"/LFSgraphs/alluvial.png", sep = ""),
          width = 13, height = 8)
   
# alluvial linking 2, 6 and 9 case
# ggplot(data = lfs.full %>% group_by(cluster6, cluster2, cluster9) %>%
#          summarise (n = n()),
#        aes(y = n, axis1 = cluster2, axis2 = cluster6, axis3 = cluster9)) +
#   geom_alluvium(aes(fill = cluster2), width = 1/12) +
#   geom_stratum(width = 1/12, fill = "black", color = "grey") +
#   geom_label(stat = "stratum", label.strata = TRUE) +
#   scale_x_discrete(limits = c("Broad cluster", "Narrow cluster","vnarrow cluster"), expand = c(.05, .05)) +
#   scale_fill_brewer(type = "qual", palette = "Set1") +
#   guides(fill = FALSE) +
#   theme(text = element_text(size=16, family="serif"))
#   ggsave("analysis/output/lfsdescriptives/alluvia269l.png")

# look at detailed occupations within each group
lfs.full %>% filter(labels6 == "Low-educated young men") %>% 
  group_by(occupation_det) %>%
  summarise(n = n()) %>%
  mutate(share = n/sum(n)) %>%
  arrange(-n)

lfs.full %>% filter(labels6 == "Older health/educ workers") %>% 
  group_by(occupation_det) %>%
  summarise(n = n()) %>%
  mutate(share = n/sum(n)) %>%
  arrange(-n)


lfs.full %>% filter(labels6 == "Female service workers") %>% 
  group_by(occupation_det) %>%
  summarise(n = n()) %>%
  mutate(share = n/sum(n)) %>%
  arrange(-n)

lfs.full %>% filter(labels6 == "London professionals") %>% 
  group_by(occupation_det) %>%
  summarise(n = n()) %>%
  mutate(share = n/sum(n)) %>%
  arrange(-n)

lfs.full %>% filter(labels6 == "Managers") %>% 
  group_by(occupation_det) %>%
  summarise(n = n()) %>%
  mutate(share = n/sum(n)) %>%
  arrange(-n)

### 3. Assign clusters in CEP-LSE survey

### 3.1 Find closest clusters

numclusters <- c(2,6) # which clusters were fit?
closest.clusters <- matrix(nrow = dim(cep.full)[1], ncol = length(numclusters)) # matrix to store clusters
k <- 0

for (i in numclusters) {
  k <- k+1
  # extract median types
  medtypes <- lfs.full[cluster.results[[k]]$medoids,]
  medtypes$type <- "medtype"
  
  # bind to end of CEP data
  cep.full.medtypes <- bind_rows(medtypes, cep.full)
  cep.full.medtypes <- cep.full.medtypes %>% arrange(type) %>%
    select(age, fulltime, male, education, industry, id)
  
  # calculate distance
  gower_dist <- daisy(cep.full.medtypes %>% select(-id), metric = "gower")
  gower_mat <- as.matrix(gower_dist) # Note that diagonal entries are zero, as they represent the same individual
  
  # find closest cluster
  n <- dim(gower_mat)[2]
  clust.dist <- gower_mat[(n-(i-1)):n,] # distance to medoids (final few rows)
  closest.medoid <- apply(clust.dist, 2, FUN=which.min) # minimize across distances
  closest.medoid <- 
  closest.clusters[,k] <- closest.medoid[1:(n-i)]
}

# assign cluster labels
cep.full$cluster2 <- factor(closest.clusters[,1],
                            levels = c(1,2))
cep.full <- inner_join(cep.full,clusterorder2, by = "cluster2")
cep.full$labels2 <- as.factor(cep.full$labels2)

cep.full$cluster6 <- factor(closest.clusters[,2],
                            levels = c(1,2,3,4,5,6))
cep.full <- inner_join(cep.full,clusterorder6, by = "cluster6")
cep.full$labels6 <- as.factor(cep.full$labels6)

cep.full %>% group_by(cluster2) %>% 
  count(more_fewer_hours) %>% 
  mutate(percent = n/sum(n))

cep.full %>% group_by(cluster2) %>% 
  count(prefer_empl) %>% 
  mutate(percent = n/sum(n))

cep.full %>% group_by(cluster6) %>% 
  count(prefer_empl) %>% 
  mutate(percent = n/sum(n))

cep.full %>% group_by(cluster2) %>% 
  count(pref_ben) %>% 
  mutate(percent = n/sum(n))

cep.full %>% group_by(cluster2) %>% 
  count(reason_selfemp) %>% 
  mutate(percent = n/sum(n))


View(cep.full %>% group_by(cluster6) %>% 
  count(reason_selfemp) %>% 
  mutate(percent = n/sum(n)))

# check clusters make sense
# produce graphs of categorical variables by cluster
col <- c("education", "industry", "male","satisfac","fulltime","reason_selfemp",
         "more_fewer_hours", "pref_ben", "tax_benefit",
         "prefer_empl","employee")
legend.labs <- c("Education","Industry","Sex","Satisfaction","PT/FT",
                 "Reason for SE","More/Fewer hours?","Preferred benefit","Tax benefits",
                 "Prefer employment?","Also employee?")
clust <- c("labels2","labels6")
k <- 0
for (i in col) {
  k <- k+1
  for (j in clust) {
    print(i)
    print(j)
    print(legend.labs[k])
    g <- cep.full %>% group_by(!!ensym(j), !!ensym(i))  %>%
      summarise (n = n()) %>%
      mutate(share = 100*n / sum(n))
    ggplot(data = g) + 
      geom_bar(mapping = aes(x = !!ensym(j), y = share, fill = !!ensym(i), group = !!ensym(i)), 
               position = "stack", stat = "identity", alpha = 0.7) + 
      scale_fill_brewer(type = "qual", palette = "Set1") +
      theme_minimal(base_size = 25) + 
      theme(axis.text.x = element_text(angle = 60, hjust = 1),
            text = element_text(size=25), #, family="serif"
            legend.position = 'right', 
            legend.spacing.x = unit(0.5, 'cm'),
            legend.direction = 'vertical',
            legend.key.size = unit(1.2,'cm'),
            legend.title=element_text(size=25),
            plot.margin=unit(c(2,1,1,1),"cm")) +
      labs(fill = paste(legend.labs[k]), x = "", y = "% share")
    
    ggsave(file = paste("analysis/output/cepdescriptives/",i,"cep",j,".png", sep = ""),
           width = 13, height = 8)
    ggsave(file = paste(paperfigdir,"/CEPgraphs/",i,"cep",j,".png", sep = ""),
           width = 13, height = 8)
    ggsave(file = paste(presfigdir,"/CEPgraphs/",i,"cep",j,".png", sep = ""),
           width = 13, height = 8)
  }
}

# produce graphs of continuous variables by cluster
col <- c("age")
clust <- c("labels2","labels6")
for (i in col) {
  for (j in clust) {
    print(i)
    print(j)
    ggplot(data = cep.full, aes(x = !!ensym(i), color = !!ensym(j))) + 
      geom_density() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.margin=unit(c(2,1,1,1),"cm"))
    ggsave(file = paste("analysis/output/cepdescriptives/",i,"cep",j,".png", sep = ""))
    ggsave(file = paste(paperfigdir,"/CEPgraphs/",i,"cep",j,".png", sep = ""),
           width = 13, height = 8)
  }
}