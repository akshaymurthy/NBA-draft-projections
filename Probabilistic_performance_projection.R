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



# Probability Predictions --------------------------------------------------------------

# Predictions for 2019 draft prospects for years 1, 2, and 3 of potential NBA careers in terms of BPM
prediction_draft_probability_year1<-predict.lm(BPM_model_year1, BPM_predictor_values_year1, type = 'response', se.fit = TRUE)
prediction_draft_probability_year2 <-predict.lm(BPM_model_year2, BPM_predictor_values_year2, type = 'response', se.fit = TRUE)
prediction_draft_probability_year3 <-predict.lm(BPM_model_year3, BPM_predictor_values_year3, type = 'response', se.fit = TRUE)

# Create lists for storing probability plots
myplots <- vector('list', nrow(BPM_predictor_values_year1))
myplots_1 <- vector('list', nrow(BPM_predictor_values_year1))
myplots_2 <- vector('list', nrow(BPM_predictor_values_year2))
myplots_3 <- vector('list', nrow(BPM_predictor_values_year3))



# Plot Creation -----------------------------------------------------------------

# Year 1 Plot creation for every element in data set
# Different portion st devs away from mean are plotted with different colors using vars: 
# x1, x2_1, x2_2, x3_1, x3_2, and y1, y2_1, y2_2, y3_1, y3_2
for (i in seq_along(prediction_draft_probability_year1$fit)) {
  myplots_1[[i]] <- local({
    i <- i
    x1 <- seq(prediction_draft_probability_year1$fit[i] -
                1 * prediction_draft_probability_year1$se.fit[i],prediction_draft_probability_year1$fit[i] +
                1 * prediction_draft_probability_year1$se.fit[i], by=0.005)
    x2_1 <- seq(prediction_draft_probability_year1$fit[i] -
                  2 * prediction_draft_probability_year1$se.fit[i],prediction_draft_probability_year1$fit[i] -
                  1 * prediction_draft_probability_year1$se.fit[i], by=0.005)
    x2_2 <- seq(prediction_draft_probability_year1$fit[i] +
                  1 * prediction_draft_probability_year1$se.fit[i],prediction_draft_probability_year1$fit[i] +
                  2 * prediction_draft_probability_year1$se.fit[i], by=0.005)
    x3_1 <- seq(prediction_draft_probability_year1$fit[i] -
                  5 * prediction_draft_probability_year1$se.fit[i],prediction_draft_probability_year1$fit[i] -
                  2 * prediction_draft_probability_year1$se.fit[i], by=0.005)
    x3_2 <- seq(prediction_draft_probability_year1$fit[i] +
                  2 * prediction_draft_probability_year1$se.fit[i],prediction_draft_probability_year1$fit[i] +
                  5 * prediction_draft_probability_year1$se.fit[i], by=0.005)
    y1<-dnorm(x1, mean =prediction_draft_probability_year1$fit[i], sd =prediction_draft_probability_year1$se.fit[i])
    y2_1<-dnorm(x2_1, mean =prediction_draft_probability_year1$fit[i], sd =prediction_draft_probability_year1$se.fit[i])
    y2_2<-dnorm(x2_2, mean =prediction_draft_probability_year1$fit[i], sd =prediction_draft_probability_year1$se.fit[i])
    y3_1<-dnorm(x3_1, mean =prediction_draft_probability_year1$fit[i], sd =prediction_draft_probability_year1$se.fit[i])
    y3_2<-dnorm(x3_2, mean =prediction_draft_probability_year1$fit[i], sd =prediction_draft_probability_year1$se.fit[i])
    plot_ly(
      type = 'scatter',
      mode = 'lines',
      fill = 'tozeroy',
      x = ~x1,
      y = ~y1,
      hovertemplate = paste(
        "Probability distribution function: %{y:.3f}<br>",
        "2020 BPM: %{x:.3f}<br>",
        "<extra></extra>"
      )) %>%
      add_trace(x = ~x2_1, y = ~y2_1, fill = 'tozeroy') %>%
      add_trace(x = ~x2_2, y = ~y2_2, fill = 'tozeroy') %>%
      add_trace(x = ~x3_1, y = ~y3_1, fill = 'tozeroy') %>%
      add_trace(x = ~x3_2, y = ~y3_2, fill = 'tozeroy') %>%
      layout(showlegend = FALSE, title =  paste(prediction_draft_data$Player[i], '2020 BPM Prediction'),  yaxis = list(title = "Probability distribution function"),
             xaxis = list(title = "BPM", range = c(prediction_draft_probability_year1$fit[i] -
                                                     5 * prediction_draft_probability_year1$se.fit[i],
                                                   prediction_draft_probability_year1$fit[i] +
                                                     5 * prediction_draft_probability_year1$se.fit[i])
             ))   })
}

# Year 2 Plot creation for every element in data set
# Different portion st devs away from mean are plotted with different colors using vars: 
# x1, x2_1, x2_2, x3_1, x3_2, and y1, y2_1, y2_2, y3_1, y3_2
for (i in seq_along(prediction_draft_probability_year2$fit)) {
  myplots_2[[i]] <- local({
    i <- i
    x1 <- seq(prediction_draft_probability_year2$fit[i] -
                1 * prediction_draft_probability_year2$se.fit[i],prediction_draft_probability_year2$fit[i] +
                1 * prediction_draft_probability_year2$se.fit[i], by=0.005)
    x2_1 <- seq(prediction_draft_probability_year2$fit[i] -
                  2 * prediction_draft_probability_year2$se.fit[i],prediction_draft_probability_year2$fit[i] -
                  1 * prediction_draft_probability_year2$se.fit[i], by=0.005)
    x2_2 <- seq(prediction_draft_probability_year2$fit[i] +
                  1 * prediction_draft_probability_year2$se.fit[i],prediction_draft_probability_year2$fit[i] +
                  2 * prediction_draft_probability_year2$se.fit[i], by=0.005)
    x3_1 <- seq(prediction_draft_probability_year2$fit[i] -
                  5 * prediction_draft_probability_year2$se.fit[i],prediction_draft_probability_year2$fit[i] -
                  2 * prediction_draft_probability_year2$se.fit[i], by=0.005)
    x3_2 <- seq(prediction_draft_probability_year2$fit[i] +
                  2 * prediction_draft_probability_year2$se.fit[i],prediction_draft_probability_year2$fit[i] +
                  5 * prediction_draft_probability_year2$se.fit[i], by=0.005)
    y1<-dnorm(x1, mean =prediction_draft_probability_year2$fit[i], sd =prediction_draft_probability_year2$se.fit[i])
    y2_1<-dnorm(x2_1, mean =prediction_draft_probability_year2$fit[i], sd =prediction_draft_probability_year2$se.fit[i])
    y2_2<-dnorm(x2_2, mean =prediction_draft_probability_year2$fit[i], sd =prediction_draft_probability_year2$se.fit[i])
    y3_1<-dnorm(x3_1, mean =prediction_draft_probability_year2$fit[i], sd =prediction_draft_probability_year2$se.fit[i])
    y3_2<-dnorm(x3_2, mean =prediction_draft_probability_year2$fit[i], sd =prediction_draft_probability_year2$se.fit[i])
    plot_ly(
      type = 'scatter',
      mode = 'lines',
      fill = 'tozeroy',
      x = ~x1,
      y = ~y1,
      hovertemplate = paste(
        "Probability distribution function: %{y:.3f}<br>",
        "2021 BPM: %{x:.3f}<br>",
        "<extra></extra>"
      )) %>%
      add_trace(x = ~x2_1, y = ~y2_1, fill = 'tozeroy') %>%
      add_trace(x = ~x2_2, y = ~y2_2, fill = 'tozeroy') %>%
      add_trace(x = ~x3_1, y = ~y3_1, fill = 'tozeroy') %>%
      add_trace(x = ~x3_2, y = ~y3_2, fill = 'tozeroy') %>%
      layout(showlegend = FALSE, title =  paste(prediction_draft_data$Player[i], '2021 BPM Prediction'),  yaxis = list(title = "Probability distribution function"),
             xaxis = list(title = "BPM", range = c(prediction_draft_probability_year2$fit[i] -
                                                     5 * prediction_draft_probability_year2$se.fit[i],
                                                   prediction_draft_probability_year2$fit[i] +
                                                     5 * prediction_draft_probability_year2$se.fit[i])
             ))   })
}

# Year 3 Plot creation for every element in data set
# Different portion st devs away from mean are plotted with different colors using vars: 
# x1, x2_1, x2_2, x3_1, x3_2, and y1, y2_1, y2_2, y3_1, y3_2
for (i in seq_along(prediction_draft_probability_year3$fit)) {
  myplots_3[[i]] <- local({
    i <- i
    x1 <- seq(prediction_draft_probability_year3$fit[i] -
                1 * prediction_draft_probability_year3$se.fit[i],prediction_draft_probability_year3$fit[i] +
                1 * prediction_draft_probability_year3$se.fit[i], by=0.005)
    x2_1 <- seq(prediction_draft_probability_year3$fit[i] -
                  2 * prediction_draft_probability_year3$se.fit[i],prediction_draft_probability_year3$fit[i] -
                  1 * prediction_draft_probability_year3$se.fit[i], by=0.005)
    x2_2 <- seq(prediction_draft_probability_year3$fit[i] +
                  1 * prediction_draft_probability_year3$se.fit[i],prediction_draft_probability_year3$fit[i] +
                  2 * prediction_draft_probability_year3$se.fit[i], by=0.005)
    x3_1 <- seq(prediction_draft_probability_year3$fit[i] -
                  5 * prediction_draft_probability_year3$se.fit[i],prediction_draft_probability_year3$fit[i] -
                  2 * prediction_draft_probability_year3$se.fit[i], by=0.005)
    x3_2 <- seq(prediction_draft_probability_year3$fit[i] +
                  2 * prediction_draft_probability_year3$se.fit[i],prediction_draft_probability_year3$fit[i] +
                  5 * prediction_draft_probability_year3$se.fit[i], by=0.005)
    y1<-dnorm(x1, mean =prediction_draft_probability_year3$fit[i], sd =prediction_draft_probability_year3$se.fit[i])
    y2_1<-dnorm(x2_1, mean =prediction_draft_probability_year3$fit[i], sd =prediction_draft_probability_year3$se.fit[i])
    y2_2<-dnorm(x2_2, mean =prediction_draft_probability_year3$fit[i], sd =prediction_draft_probability_year3$se.fit[i])
    y3_1<-dnorm(x3_1, mean =prediction_draft_probability_year3$fit[i], sd =prediction_draft_probability_year3$se.fit[i])
    y3_2<-dnorm(x3_2, mean =prediction_draft_probability_year3$fit[i], sd =prediction_draft_probability_year3$se.fit[i])
    plot_ly(
      type = 'scatter',
      mode = 'lines',
      fill = 'tozeroy',
      x = ~x1,
      y = ~y1,
      hovertemplate = paste(
        "Probability distribution function: %{y:.3f}<br>",
        "2022 BPM: %{x:.3f}<br>",
        "<extra></extra>"
      )) %>%
      add_trace(x = ~x2_1, y = ~y2_1, fill = 'tozeroy') %>%
      add_trace(x = ~x2_2, y = ~y2_2, fill = 'tozeroy') %>%
      add_trace(x = ~x3_1, y = ~y3_1, fill = 'tozeroy') %>%
      add_trace(x = ~x3_2, y = ~y3_2, fill = 'tozeroy') %>%
      layout(showlegend = FALSE, title =  paste(prediction_draft_data$Player[i], 'BPM Prediction'),  yaxis = list(title = "Probability distribution function"),
             xaxis = list(title = "BPM", range = c(prediction_draft_probability_year3$fit[i] -
                                                     5 * prediction_draft_probability_year3$se.fit[i],
                                                   prediction_draft_probability_year3$fit[i] +
                                                     5 * prediction_draft_probability_year3$se.fit[i])
             ))   })
}

#Example of Year 3 plot for Coby White
myplots_3[[449]]


# Interactive function that outputs 3 year projection for player of interest
# Takes user input
readName <- function()
{ 
  n <- readline(prompt="Enter a draft prospect: ")
  n <- as.character(n)
  k <- which(prediction_draft_data$Player == n, arr.ind = TRUE)
  print(k)
  if(length(k)>0){
    print(prediction_draft_data$Player[k])
    p1 <- myplots_1[k]
    p2 <- myplots_2[k]
    p3 <- myplots_3[k]
    all_plots <- subplot(p1[[1]],p2[[1]], p3[[1]])
    htmlwidgets::saveWidget(as_widget(all_plots), paste0(n,"_BPM_Predictions.html"))
    return(all_plots)
  }
  n <- readName()
}

# User Input Entry Function
print(readName())
