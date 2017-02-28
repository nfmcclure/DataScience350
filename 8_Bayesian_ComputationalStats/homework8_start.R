# Homework 8 start
#
#  Objective:  Use bootstrapping to estimate the error. (NOT on the residuals)
#  Dependent variable: Predict whether or not indivual will buy ('Buy')
#
# Don't forget to add logging, a unit test, and functions for everything
# You will return a 95% confidence interval for every coefficient, including the intercept
#   (But not the residual error)
#-------------------------------------------------------

library(logging)

ad_data = read.csv('AdvertisingPrediction.csv')
ad_data$Obs.No. = NULL

# Bootstrapping the residuals.  First we have to get the residuals from
#  the logistic regression

logistic_fit = glm(Buy ~ ., family=binomial, data = ad_data)


logistic_boot = function(data, indices){
  data_new = data[indices, ]
  fit_new = glm(Buy ~ ., family=binomial, data = data_new)
  return(coef(fit_new))
}

N = 100 # What to increase this to?
boot_estimates = boot(data=ad_data, statistic=logistic_boot, R=N)


# Calculate Confidence intervals for everything with 'boot.ci()'

#Intercept
boot.ci(boot_estimates, type="norm", index=1)

# Can also use 'type="bca"', for tighter approximations to the Conf. Interval.

# You may get some warnings about the CI using extreme endpoint in the calculation
#  If that bothers you, you can (1) increase N, or (2) decrease the confidence interval
#  (2) can be done if you read the documentation of the function, type: '?boot.ci'
