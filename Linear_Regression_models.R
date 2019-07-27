#install.packages("data.table")
#install.packages("corrplot")
#install.packages("GGally")
#install.packages("tidyverse")
#install.packages("PerformanceAnalytics")
#install.packages("plotly")
#install.packages("Hmisc")
#install.packages("rvest")
#install.packages("stringr")
#install.packages("magrittr")
#install.packages("leaps")
#install.packages("pander")
#install.packages("dplyr")
#install.packages("rvest")
#install.packages("XML")
#install.packages("RCurl")
#install.packages("cowplot")


library(data.table)
library(corrplot)
library(GGally)
library(tidyverse)
library(PerformanceAnalytics)
library(plotly)
library(Hmisc)
library(rvest)
library(stringr)
library(magrittr)
library(leaps)
library(pander)
library(dplyr)
library(rvest)
library(XML)
library(RCurl)
library(cowplot)


all_draft_data_multiple_seasons <- read.csv("alldraftdata_multiple_seasons.csv", header=TRUE)
all_draft_data_multiple_seasons <- all_draft_data_multiple_seasons[,c(-1)]

#store each filtered dataset into new dataframes
#analysis_stats does not contain player names
#names_analysis_stats contains player names
analysis_all_draft_data_multiple_seasons <- all_draft_data_multiple_seasons[,c(-1,-2,-21, -23)]

names_analysis_all_draft_data_multiple_seasons <- all_draft_data_multiple_seasons[,c(-2,-21, -23)]
glimpse(analysis_all_draft_data_multiple_seasons)



# regression BPM  ---------------------------------------------------------


#Compare models by Mallows Cp and adjR2
prediction_dataset_filter <-as.matrix(analysis_all_draft_data_multiple_seasons[, c(1:19)])
BPM_leap <- leaps(prediction_dataset_filter, analysis_all_draft_data_multiple_seasons$BPM_NBA_1, nbest = 2, method = "adjr2")
cbind(BPM_leap$size,BPM_leap$matrix, BPM_leap$adjr2)
BPM_leap <- leaps(prediction_dataset_filter, analysis_all_draft_data_multiple_seasons$BPM_NBA_1, nbest = 2)
cbind(BPM_leap$size,BPM_leap$matrix, BPM_leap$Cp)

#Select best model
selected_BPM_model  <-7
coefficients_BPM <- (1:ncol(prediction_dataset_filter))[BPM_leap$which[selected_BPM_model,]]
new_BPM_data=as_tibble(prediction_dataset_filter[,coefficients_BPM])
BPM_model_year1 <-lm(analysis_all_draft_data_multiple_seasons$BPM_NBA_1~., data = new_BPM_data)
BPM_model_year1

#Compare models by Mallows Cp and adjR2
prediction_dataset_filter <-as.matrix(analysis_all_draft_data_multiple_seasons[, c(1:19)])
BPM_leap <- leaps(prediction_dataset_filter, analysis_all_draft_data_multiple_seasons$BPM_NBA_2, nbest = 2, method = "adjr2")
cbind(BPM_leap$size,BPM_leap$matrix, BPM_leap$adjr2)
BPM_leap <- leaps(prediction_dataset_filter, analysis_all_draft_data_multiple_seasons$BPM_NBA_2, nbest = 2)
cbind(BPM_leap$size,BPM_leap$matrix, BPM_leap$Cp)

#Select best model
selected_BPM_model  <-7
coefficients_BPM <- (1:ncol(prediction_dataset_filter))[BPM_leap$which[selected_BPM_model,]]
new_BPM_data=as_tibble(prediction_dataset_filter[,coefficients_BPM])
BPM_model_year2 <-lm(analysis_all_draft_data_multiple_seasons$BPM_NBA_2~., data = new_BPM_data)
BPM_model_year2


#Compare models by Mallows Cp and adjR2
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


#Bring in predictors from 2018-19 data set to predict 2019-20 values
prediction_draft_data <- read.csv("CBBstats2019.csv", header = TRUE)
prediction_draft_data <- prediction_draft_data[,c(-1,-2)]
prediction_draft_data$Player <- as.character(prediction_draft_data$Player)


glimpse(prediction_draft_data)
BPM_predictor_values_year1<-prediction_draft_data[,c(7,13,14,20)]
glimpse(BPM_predictor_values_year1)

BPM_predictor_values_year2<-prediction_draft_data[,c(4,9,13,20)]
glimpse(BPM_predictor_values_year2)

BPM_predictor_values_year3<-prediction_draft_data[,c(7,13,14,20)]
glimpse(BPM_predictor_values_year3)

glimpse(prediction_draft_data)

draft_data_2018 <- names_analysis_all_draft_data[658:719,]
glimpse(draft_data_2018[,c(10,12,19,20)])






BPM_predictions_2018<- predict(BPM_model, draft_data_2018[,c(10,12,19,20)], se.fit = TRUE, interval = "prediction", level = 0.95)
draft_data_2018[,c(43:45)] <-as_tibble(BPM_predictions_2018$fit) [,c(1:3)]
colnames(draft_data_2018) [43:45] <-c("mean_BPM_prediction", "lower_limit_BPM_prediction",
                                            "upper_limit_BPM_prediction")
glimpse(draft_data_2018)