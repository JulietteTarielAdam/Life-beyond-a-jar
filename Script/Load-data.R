# Packages
## for data transformation
library(tidyverse) 
## for plots
library(ggplot2) 
library(ggpubr) # assemble multiple plots
theme_set(theme_classic())
library(viridis)
library(ggforce) # facet_wrap_paginate
## for PCA analysis
library(FactoMineR) 
## for linear model analysis
library(lme4)
library(lmerTest)
library(emmeans)
library(easystats)
library(fitdistrplus)
library(glmmTMB)
library(car)
library(DHARMa)
library(rptR) # calculate CI of repeatability

# The loaded file is the Master.csv and not the excel Master.xlsx
master <- read.table("Script/Master.csv", sep = ",", dec = ".", header = TRUE, stringsAsFactors = TRUE)

# change order or name of factor levels
master$Tank <- factor(master$Tank, levels = c("Jar", "Small", "Medium", "Large", "Barren")) 
master$Time <- factor(master$Time, levels = c("7:00 AM", "10:00 AM", "2:00 PM", "6:00 PM"))
levels(master$Filter)
levels(master$Filter) <- c("No-filter", "Filter")

# vectors indicating which columns are the behavioural variables
beh.cols <- c("Resting", "Swimming", "Hovering", "Sinking.Floating", "Stereotypic.swimming", "Nest.building", "Foraging",  "Interation.with.surface")
beh.cols.full <- c(beh.cols, "Out.of.view")

# transforming the raw dataset into the dataset
# each row of the raw dataset is a time spent by a fish doing a certain behavioural type 
# for a same trial, there could be several scored times of the same behaviour
# the following function is then summing all the times corresponding to a similar behaviour (or applying any function providing in the "fct" attribute)
# this can be done by different grouping columns ("grouping" attribute), for instance by Tank, or by Tank and Fish
# the "beh.cols" attribute indicates which behavioural types to consider
## transforming function
tf <- function(data, grouping, beh.cols, fct){
  list <- group_split(data, data[,grouping]) # split the data.frame into a list where each list item is a sub data.frame splitted by the grouping factors
  list2 <- list[sapply(list, nrow)!=0] # filter empty list items
  result <- lapply(list2, # apply to each sub dataframe the function
                   function(sub.data){
                     temp <- fct(sub.data[, beh.cols])
                     if(length(grouping)==1){
                       output <- data.frame(unique(sub.data[,grouping]), data.frame(t(temp)))
                       names(output)[1] <- grouping
                     } else {
                       output <- data.frame(unique(sub.data[,grouping]), data.frame(t(temp)))
                     }
                     output
                   })
  result2 <- do.call(rbind, result)
  row.names(result2) <- NULL
  result2
}

## create the main file used for the analysis
grouping <- c("Fish","Tank","Order","Time", "Filter")
fct <- function(x) colSums(x, na.rm = TRUE)
data <- tf(master, grouping, beh.cols.full, fct) 

## main file with up and down
grouping <- c("Fish","Tank","Order","Time", "Up.Down", "Filter")
dataUpDown <- tf(master, grouping, beh.cols.full, fct) 
dataUpDown$Total <- rowSums(dataUpDown[,beh.cols.full])
dataUpDown <- dataUpDown %>% 
 dplyr::select(-beh.cols.full) %>% 
 pivot_wider(values_from = Total, names_from = Up.Down) %>% 
 mutate(perc.up = up / (up + down))

# PCA
pca <- PCA(data[,beh.cols], graph = FALSE, ncp=3, scale.unit = TRUE)
data <- cbind(data, pca$ind$coord)
names(data)[match(c("Dim.1", "Dim.2","Dim.3"), names(data))] <- c("pc1","pc2","pc3")

# package renv to have a reproducible code by storing packages versions
library(renv)
renv::snapshot()

# Transform some behavioural types into binary = presence/absence of this behaviour during a trial
data$Foraging.bin <- factor(ifelse(data$Foraging !=0 , "Yes", "No"))
data$SS.bin <- factor(ifelse(data$Stereotypic.swimming != 0, "Yes", "No"))
data$Hovering.bin <- factor(ifelse(data$Hovering != 0, "Yes", "No"))
data$Interacting.bin <- factor(ifelse(data$Interation.with.surface != 0, "Yes", "No"))
data$Nest.bin <- factor(ifelse(data$Nest.building != 0, "Yes", "No"))
data$SF.bin <- factor(ifelse(data$Sinking.Floating != 0, "Yes", "No"))