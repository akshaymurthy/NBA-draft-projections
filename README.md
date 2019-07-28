# NBA-projections
NCAA college basketball statistics are used to predict player performance in years 1, 2, and 3 in the NBA.
NCAA advanced statistics are taken from RealGM (https://basketball.realgm.com/ncaa/stats) and used to predict BPM 
(box plus/minus) as calculated by Basketball Reference. Full definition of BPM and its overall utility is provided here: 
https://www.basketball-reference.com/about/bpm.html

R Files in Repository:

Data_scraping.R
Linear_Regression_models.R
Mean_prediction_analysis.R
Probabilistic_performance_projection.R

Data_scraping.R provides a method for scraping stats from the aforementioned webpages. The user can ignore this script and use
the provided .csv files instead.

Linear_Regression_models.R uses the data taken from the webpages to construct and select the best linear regression models that correlate advanced metrics to BPM in years 1, 2, and 3 in the NBA.

Mean_prediction_analysis.R uses the defined models to provide predictions on the mean BPM for players drafted in 2018 and 2019.

Probabilistic_performance_projection.R takes the constructed models to construct probabilistic regression projections that illustrate the likelihood of a player achieving a particular level of performance in a given season following being drafted.

HTML files provide examples of the plots created using these scripts with the help of plotly.

Enjoy!
