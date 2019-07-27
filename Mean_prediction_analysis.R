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



# Output Finished Data ----------------------------------------------------
#Finished data

#plotly projections for 2018-19 season using 2018 college basketball data
proj_2018 <- draft_data_2018 %>%
  plot_ly(
    type = 'scatter',
    mode = 'markers',
    x = ~BPM_NBA,
    y = ~mean_BPM_prediction,
    marker = list(size = ~((upper_limit_BPM_prediction-lower_limit_BPM_prediction)),
                  sizeref = 1, sizemode = 'diameter'),
    color = ~(minutes_played_NBA),
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


fit <- lm(mean_BPM_prediction ~ BPM_NBA, data = draft_data_2018)
summary(fit)

proj_2018


#plotly projections for 2019-20 season using 2019 college basketball data
proj_2019 <- prediction_draft_data %>% 
  plot_ly(
    type = 'scatter',
    mode = 'markers',
    x = ~PER_college,
    y = ~mean_BPM_prediction,
    marker = list(size = ~((upper_limit_BPM_prediction-lower_limit_BPM_prediction)),
                  sizeref = 0.5, sizemode = 'diameter'),
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


#plotly projections for 2019-20 season using 2019 college basketball data to predict likelihood
# of a player reaching +2 BPM
r <- prediction_draft_data %>% 
  plot_ly(
    type = 'scatter',
    mode = 'markers',
    x = ~PER_college,
    y = ~star_probability,
    marker = list(size = ~((points_per_shot_college)),
                  sizeref = 0.2, sizemode = 'diameter'),
    color = ~(usage_rate_college),
    text = ~Player,
    hovertemplate = paste(
      "<b>%{text}</b><br><br>",
      "Likelihood of BPM > 2: %{y:.4f}<br>",
      "2018-19 College PER: %{x:.000}<br>",
      "<extra></extra>"
    )
  ) %>%
  layout(title =  "Probability of BPM > 2 in 2020", legend = list(orientation = 'h', y = -0.3, title = "Usage Rate 2018-19"), yaxis = list(title = "Probability of BPM > 2.0"),
         xaxis = list(title = "2018-19 College PER"))

r





