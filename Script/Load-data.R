# Packages
## for data transformation
library(tidyverse) 
## for plots
library(ggplot2) 
library(ggpubr) # assemble multiple plots
library(ggrepel)
library(ggforce) # facet_wrap_paginate
theme_set(theme_classic())
library(RColorBrewer) # nice color palette
## for PCA analysis
library(FactoMineR)  
## for linear model analysis
library(lme4) # fit LMMs
library(lmerTest) # type II F-tests with Kenward Roger estimation in LMMs
library(emmeans) # estimate posthoc contrasts
# library(easystats)
library(fitdistrplus) # find potential distributions other than normal distribution
library(glmmTMB) # fit the GLMMs
library(DHARMa) # estimate residuals for the GLMMs to do the diagnosis of the model
library(rptR) # estimate CI of repeatability
library(performance) # for the function check_model
library(nlme) # for the linear model of Hovering with weighted least squares

# The loaded file is the Master.csv and not the excel Master.xlsx
master <- read.table("Script/Master.csv", sep = ",", dec = ".", header = TRUE, stringsAsFactors = TRUE)

# change order or name of factor levels
master$Order <- factor(master$Order)
master$Tank <- factor(master$Tank, levels = c("Jar", "Small", "Medium", "Large", "Barren")) 
master$Time <- factor(master$Time, levels = c("7:00 AM", "10:00 AM", "2:00 PM", "6:00 PM"))
levels(master$Filter)
levels(master$Filter) <- c("No-filter", "Filter")

# vectors indicating which columns are the behavioural variables
## Final behaviours studied
beh.cols <- c("Swimming","Resting", "Hovering", "Stereotypic.swimming", "Nest.building", "Foraging",  "Interation.with.surface")
## With Sinking.Floating
beh.cols2 <- c(beh.cols, "Sinking.Floating")
## With Out.Of.View
beh.cols3 <- c(beh.cols2, "Out.of.view")

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
data3 <- tf(master, grouping, beh.cols3, fct) 
data2 <- subset(data3, select = -Out.of.view) # Without Out.Of.View
data <-  subset(data2, select = -Sinking.Floating) # Without Sinking.Floating

## main file with up and down
grouping <- c("Fish","Tank","Order","Time", "Up.Down", "Filter")
dataUpDown <- tf(master, grouping, beh.cols, fct) 
dataUpDown$Total <- rowSums(dataUpDown[,beh.cols])
dataUpDown <- dataUpDown %>% 
 dplyr::select(-beh.cols) %>% 
 pivot_wider(values_from = Total, names_from = Up.Down) 
dataUpDown[is.na(dataUpDown$down), "down"] <- 0 # NA for down column if the fish spent all the trial up. Replace NA by 0
dataUpDown <- dataUpDown %>% 
  mutate(total = up + down, 
         up = round(up,0),
         down = round(down, 0),
         perc = up / (up + down),
         adj.up = round((up * 600) / total,0), # adjusted time up by the fact that the trial was not perfectly 600 s
         adj.down = round((down * 600) / total,0)) # adjusted time down by the fact that the trial was not perfectly 600 s

# PCA
pca <- PCA(data2[,beh.cols2], graph = FALSE, ncp=4, scale.unit = TRUE)

# package renv to have a reproducible code by storing packages versions
library(renv)
renv::snapshot()

# Transform some behavioural types into binary = presence/absence of this behaviour during a trial
data$Foraging.bin <- factor(ifelse(data$Foraging !=0 , "Yes", "No"))
data$SS.bin <- factor(ifelse(data$Stereotypic.swimming != 0, "Yes", "No"))
data$Hovering.bin <- factor(ifelse(data$Hovering != 0, "Yes", "No"))
data$Interacting.bin <- factor(ifelse(data$Interation.with.surface != 0, "Yes", "No"))
data$Nest.bin <- factor(ifelse(data$Nest.building != 0, "Yes", "No"))

# Resting place
## Analysis only for Small, Medium and Large
## Concatenate "Plant leaves" and "Under or against plant" together
## Concatenate "Inside barrel" and "On or against barrel" together
## temp 1 is the data.frame with the resting time for each resting place for each trial
## including 0 is not resting at this place for a trial
temp1<- master %>% filter(Tank!= "Barren", Tank != "Jar") %>% 
  filter(Resting !=0, !is.na(Resting)) %>% 
  dplyr::select(Fish, Tank, Order, Time, Filter, Resting, Resting.place) %>% 
  mutate(Resting.place = factor(Resting.place,
                                levels = c("Floor","Surface","Under or against plant","Plant leaves", "On or against barrel", "Inside barrel","Filter"),
                                labels = c("Floor","Surface", "Plant",  "Plant", "Barrel", "Barrel", "Filter")),
         Tank = fct_drop(Tank)) %>% 
  # some scoring with NA for the resting place
  filter(!is.na(Resting.place)) %>% 
  # add similar scores for up and down
  group_by(Fish, Tank, Order, Time, Filter, Resting.place) %>%
  summarize(Resting = sum(Resting), .groups = "drop") %>% 
  # add zeros when the fish has not rested in a specific place for this trial
  complete(nesting(Fish, Tank, Order, Time, Filter), Resting.place, fill = list(Resting = 0)) %>% 
  # remove filter if it is a trial without filter
  filter(!(Filter == "No-filter" & Resting.place== "Filter")) %>% 
  # remove Barrel for Small and Medium
  filter(!(Tank == "Small" & Resting.place == "Barrel"),
         !(Tank == "Medium" & Resting.place == "Barrel"))

## temp2 is the total resting time for each trial
temp2 <- data %>% dplyr::select(Fish, Tank, Order, Time, Filter, Resting) %>% 
  filter(Tank!= "Barren", Tank != "Jar") %>% 
  rename(total.resting = Resting) %>% 
  mutate(Tank = fct_drop(Tank))

## data_RP is the final concatenated data frame with the percentage of time resting at each place
data_RP <- merge(temp1, temp2, by = c("Fish", "Tank", "Order", "Time", "Filter")) %>% 
  mutate(percentage = Resting / total.resting)

## new column as binary variable, 0 if the fish did not rest at all to this place during a trial, 1 otherwise
data_RP$binary <- ifelse(data_RP$percentage >0, 1, 0)

## data without zeros (and without Filter because it is only zeros besides one trial)
data_RPwz <- data_RP %>% filter(Resting.place!="Filter", Resting > 0)

# Interaction.with.surface types
data_IT <- tf(master[master$Interation.with.surface !=0 & !is.na(master$Interation.with.surface),], 
              c("Tank","Interaction.with.surface.type","Fish","Filter","Time"), 
              c("Interation.with.surface","Swimming"), # I have to specify two columns otherwise my function tf is not working
              function(x) colSums(x, na.rm = TRUE)) %>% 
  mutate(Interaction.with.surface.type = factor(Interaction.with.surface.type))
levels(data_IT$Interaction.with.surface.type)
## Group levels together
new.levels <- c("Bite", "Swim into ground", "Swim into ground", "Swim into wall/object", "Swim into wall/object",
                "Swim into wall/object", "Trash at the water surface", "Swim into wall/object", "Bite", "Bite",
                "Swim into ground", rep("Swim into wall/object", 9), "Bite", "Swim into wall/object", "Swim into wall/object")
### Check
unique(data.frame(ori.levels = levels(data_IT$Interaction.with.surface.type), new.levels))
data_IT$IT <- factor(data_IT$Interaction.with.surface.type, 
labels = c("Bite", "Swim into ground", "Swim into ground", "Swim into wall/object", "Swim into wall/object",
           "Swim into wall/object", "Trash at the water surface", "Swim into wall/object", "Bite", "Bite",
           "Swim into ground", rep("Swim into wall/object", 9), "Bite", "Swim into wall/object", "Swim into wall/object"))

# Hovering place
data_HP <- tf(master[master$Hovering !=0 & !is.na(master$Hovering),], 
              c("Tank","Hovering.place","Fish","Filter","Time"), 
              c("Hovering","Swimming"), # I have to specify two columns otherwise my function tf is not working
              function(x) colSums(x, na.rm = TRUE)) %>% 
  mutate(Hovering.place = fct_drop(Hovering.place),
         Hovering.place = factor(Hovering.place, c("Above ground","Mid water column", "Just under surface", "Just under surface (under bubble nest)", "Inside barrel")))