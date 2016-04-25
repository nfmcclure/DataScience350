##--------------------------------------------
##
## Chicago Diabetes Homework (Lecture 4)
##
## Class: PCE Data Science Methods Class
##
##--------------------------------------------


setwd('E:/Work/Teaching/PCE_Data_Science/4_HypothesisTesting_CentralLimit/')

data = read.csv('ChicagoDiabetesData.csv', stringsAsFactors = FALSE)

data_sums = apply(data[-1],2,sum)

hospitalizations = data_sums[grepl('Hospitalizations', names(data_sums))]
admit_rate = data_sums[grepl('Crude.Rate.[0-9]+$', names(data_sums), perl = TRUE)]

plot(hospitalizations, admit_rate)

hospitalizations_diff = diff(hospitalizations)
admit_rate_diff = diff(admit_rate)

plot(hospitalizations_diff, admit_rate_diff)
