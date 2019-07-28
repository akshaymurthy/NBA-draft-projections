library(data.table)
library(corrplot)
library(GGally)
library(tidyverse)
library(PerformanceAnalytics)
library(Hmisc)
library(rvest)
library(stringr)
library(magrittr)
library(leaps)
library(pander)
library(dplyr)
library(rvest)
library(RCurl)


# Import data-------------------------------------------------------------------------
# data import from csv files produced by Data_scraping.R

all_draft_data_multiple_seasons <- read.csv("alldraftdata_multiple_seasons.csv", header=TRUE)
all_draft_data_multiple_seasons <- all_draft_data_multiple_seasons[,c(-1)]

#Import 2017-18 data set to compare to 2018-219 values
prediction_draft_data_2018 <- read.csv("draft_2018_info.csv", header = TRUE)
prediction_draft_data_2018$Player <- as.character(prediction_draft_data_2018$Player)

#Import 2018-19 data set to predict 2019-20 values
prediction_draft_data_2019 <- read.csv("draft_2019_info.csv", header = TRUE)
prediction_draft_data_2019$Player <- as.character(prediction_draft_data_2019$Player)
glimpse(prediction_draft_data_2019)

#analysis_stats does not contain player names
#names_analysis_stats contains player names
analysis_all_draft_data_multiple_seasons <- all_draft_data_multiple_seasons[,c(-1,-2,-21, -23)]

names_analysis_all_draft_data_multiple_seasons <- all_draft_data_multiple_seasons[,c(-2,-21, -23)]
glimpse(names_analysis_all_draft_data_multiple_seasons)



# Regression BPM  ---------------------------------------------------------

# Regression models constructed for NBA years 1, 2 and 3 using CBB stats
# Year 1
# Compare models by Mallows Cp and adjR2
prediction_dataset_filter <-as.matrix(analysis_all_draft_data_multiple_seasons[, c(1:19)])
BPM_leap <- leaps(prediction_dataset_filter, analysis_all_draft_data_multiple_seasons$BPM_NBA_1, nbest = 2, method = "adjr2")
cbind(BPM_leap$size,BPM_leap$matrix, BPM_leap$adjr2)
BPM_leap <- leaps(prediction_dataset_filter, analysis_all_draft_data_multiple_seasons$BPM_NBA_1, nbest = 2)
cbind(BPM_leap$size,BPM_leap$matrix, BPM_leap$Cp)

# Select best model
selected_BPM_model  <-7
coefficients_BPM <- (1:ncol(prediction_dataset_filter))[BPM_leap$which[selected_BPM_model,]]
new_BPM_data=as_tibble(prediction_dataset_filter[,coefficients_BPM])
BPM_model_year1 <-lm(analysis_all_draft_data_multiple_seasons$BPM_NBA_1~., data = new_BPM_data)
BPM_model_year1



# Year 2
# Compare models by Mallows Cp and adjR2prediction_dataset_filter <-as.matrix(analysis_all_draft_data_multiple_seasons[, c(1:19)])
BPM_leap <- leaps(prediction_dataset_filter, analysis_all_draft_data_multiple_seasons$BPM_NBA_2, nbest = 2, method = "adjr2")
cbind(BPM_leap$size,BPM_leap$matrix, BPM_leap$adjr2)
BPM_leap <- leaps(prediction_dataset_filter, analysis_all_draft_data_multiple_seasons$BPM_NBA_2, nbest = 2)
cbind(BPM_leap$size,BPM_leap$matrix, BPM_leap$Cp)

# Select best model
selected_BPM_model  <-7
coefficients_BPM <- (1:ncol(prediction_dataset_filter))[BPM_leap$which[selected_BPM_model,]]
new_BPM_data=as_tibble(prediction_dataset_filter[,coefficients_BPM])
BPM_model_year2 <-lm(analysis_all_draft_data_multiple_seasons$BPM_NBA_2~., data = new_BPM_data)
BPM_model_year2


# Year 3
# Compare models by Mallows Cp and adjR2prediction_dataset_filter <-as.matrix(analysis_all_draft_data_multiple_seasons[, c(1:19)])
prediction_dataset_filter <-as.matrix(analysis_all_draft_data_multiple_seasons[, c(1:19)])
BPM_leap <- leaps(prediction_dataset_filter, analysis_all_draft_data_multiple_seasons$BPM_NBA_3, nbest = 2, method = "adjr2")
cbind(BPM_leap$size,BPM_leap$matrix, BPM_leap$adjr2)
BPM_leap <- leaps(prediction_dataset_filter, analysis_all_draft_data_multiple_seasons$BPM_NBA_3, nbest = 2)
cbind(BPM_leap$size,BPM_leap$matrix, BPM_leap$Cp)

#Select best model
selected_BPM_model  <-7
coefficients_BPM <- (1:ncol(prediction_dataset_filter))[BPM_leap$which[selected_BPM_model,]]
new_BPM_data=as_tibble(prediction_dataset_filter[,coefficients_BPM])
BPM_model_year3 <-lm(analysis_all_draft_data_multiple_seasons$BPM_NBA_3~., data = new_BPM_data)
BPM_model_year3

