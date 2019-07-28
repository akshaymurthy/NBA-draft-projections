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
library(RCurl)

# Prediction segmentation stage -----------------------------------------------------------------------

# Predictor values using 2018 CBB data is segmented
BPM_predictor_values_2018 <- prediction_draft_data_2018[,c(4,11,13,20)]
glimpse(BPM_predictor_values_2018)

# Predictor values for Years 1, 2, and 3 for 2019 CBB data are segmented
glimpse(prediction_draft_data_2019)
BPM_predictor_values_year1<-prediction_draft_data_2019[,c(4,11,13,20)]
glimpse(BPM_predictor_values_year1)

BPM_predictor_values_year2<-prediction_draft_data_2019[,c(4,9,13,20)]
glimpse(BPM_predictor_values_year2)

BPM_predictor_values_year3<-prediction_draft_data_2019[,c(7,13,14,20)]
glimpse(BPM_predictor_values_year3)

# Output Finished Data ----------------------------------------------------
# Requires variables and data output from Linear_Regression_models.R

# Predictions using 2018 CBB data and 2019 CBB data
# Predictions stored in columns 31 (mean), 32 (lower limit of 95% confidence interval), 
# 33 (upper limit of 95% confidence interval)

BPM_predictions_2018<- predict(BPM_model_year1, BPM_predictor_values_2018, se.fit = TRUE, interval = "prediction", level = 0.95)
prediction_draft_data_2018[,c(31:33)] <-as_tibble(BPM_predictions_2018$fit) [,c(1:3)]
colnames(prediction_draft_data_2018) [31:33] <-c("mean_BPM_prediction", "lower_limit_BPM_prediction",
                                      "upper_limit_BPM_prediction")

#Merge with NBA stats for 2018-19 season
draft_data_2018 <- merge(prediction_draft_data_2018, nba_list[[14]], by.x = "Player", by.y = "Player")
draft_data_2018$BPM <- as.numeric(draft_data_2018$BPM)
draft_data_2018$minutes_played <- as.numeric(draft_data_2018$minutes_played)
glimpse(draft_data_2018)


BPM_predictions_2019<- predict(BPM_model_year1, BPM_predictor_values_year1, se.fit = TRUE, interval = "prediction", level = 0.95)
prediction_draft_data_2019[,c(31:33)] <-as_tibble(BPM_predictions_2019$fit) [,c(1:3)]
colnames(prediction_draft_data_2019) [31:33] <-c("mean_BPM_prediction", "lower_limit_BPM_prediction",
                                                 "upper_limit_BPM_prediction")
glimpse(prediction_draft_data_2019)



# Plots -------------------------------------------------------------------
#plotly projections for 2018-19 season using 2018 college basketball data
proj_2018 <- draft_data_2018 %>%
  plot_ly(
    type = 'scatter',
    mode = 'markers',
    x = ~BPM,
    y = ~mean_BPM_prediction,
    marker = list(size = ~((upper_limit_BPM_prediction-lower_limit_BPM_prediction)),
                  sizeref = 1, sizemode = 'diameter'),
    color = ~(minutes_played),
    text = ~Player,
    hovertemplate = paste(
      "<b>%{text}</b><br><br>",
      "BPM 2019 Prediction: %{y:.2f}<br>",
      "2019 Actual BPM: %{x:.000}<br>",
      "Spread in 99% CI: %{marker.size:.2f,}<br>",
      "<extra></extra>"
    )
  ) %>%
  layout(title =  "BPM Projection vs Actual", legend = list(orientation = 'h', y = -0.3, title = "Minutes Played 2018-19"), yaxis = list(title = "2018-19 Projected BPM"),
         xaxis = list(title = "2018-19 Actual BPM"))
proj_2018

fit <- lm(mean_BPM_prediction ~ BPM, data = draft_data_2018)
summary(fit)




#plotly projections for 2019-20 season using 2019 college basketball data
proj_2019 <- prediction_draft_data_2019 %>% 
  plot_ly(
    type = 'scatter',
    mode = 'markers',
    x = ~PER_college,
    y = ~mean_BPM_prediction,
    marker = list(size = ~((upper_limit_BPM_prediction-lower_limit_BPM_prediction)),
                  sizeref = 1, sizemode = 'diameter'),
    color = ~(usage_rate_college),
    text = ~Player,
    hovertemplate = paste(
      "<b>%{text}</b><br><br>",
      "BPM 2019-20 Prediction: %{y:.2f}<br>",
      "2018-19 College PER: %{x:.000}<br>",
      "Spread in 99% CI: %{marker.size:.2f,}<br>",
      "<extra></extra>"
    )
  ) %>%
  layout(title =  "BPM Projection", legend = list(orientation = 'h', y = -0.3, title = "Usage Rate 2018-19"), yaxis = list(title = "2018-19 Projected BPM"),
         xaxis = list(title = "2018-19 College PER"))

proj_2019

