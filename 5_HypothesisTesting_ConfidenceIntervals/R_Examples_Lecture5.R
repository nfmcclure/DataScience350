##--------------------------------------------
##
## Counting/Probability R code (lecture 5)
##
## Class: PCE Data Science Methods Class
##
## Contains examples of:
##
## -More on Regression (and graph algorithms)
##
##--------------------------------------------

setwd('E:/Work/Teaching/PCE_Data_Science/5_HypothesisTesting_ConfidenceIntervals')

library(MASS)

##-----Look at transformations-----

data(attenu)
summary(attenu)
pairs(attenu, pch=16) # Might be some non-normal data in here

hist(attenu$dist)
hist(log(attenu$dist)) # Possible
hist(attenu$dist^(1/4)) # Also Possible

hist(attenu$accel)
hist(log(attenu$accel)) # Meh
hist(attenu$accel^(1/4), n=25) # Slightly Better

# Plot the combination:
#   we will plot:
#  x = acceleration
#  y = log(distance)

plot(attenu$accel^(1/4), log(attenu$dist), pch=16) # Not bad!
grid()
best_line = lm(log(attenu$dist) ~ I(attenu$accel^(1/4)))
abline(best_line, lwd=2, col='red')
# Note the I() function. R's formula format treats '^' as a special character otherwise.

# Summary of the line
summary(best_line)

# Summary Plots
plot(best_line)

# First plot: Residuals vs. Fitted values
#    - Look for a trend here.  Red line is a smoothed fit to the points (lowess).
#
# Second plot: Normal Q-Q Plot
#    - Check for normality!
#
# Third plot: Scale-Location Plot of Fitted Values
#   - Measurement of total error across fitted values. Look for a flat trend.
#
# Fourth plot: Residuals vs. Leverage
#   - Measurement of Points impact on fit.  Look for zero-flat trend.

##----Plot the Residuals-----
# Residuals vs. Fitted
plot(best_line$residuals, best_line$fitted.values, pch=16,
     main="Residual Plot", xlab="Residuals", ylab="Fitted Values")
residual_trend_line = lm(best_line$fitted.values ~ best_line$residuals)
abline(residual_trend_line, col='red', lwd=2)
summary(residual_trend_line)

# Residuals vs. Y:
plot(best_line$residuals, log(attenu$dist), pch=16,
     main="Residual Plot", xlab="Residuals", ylab="Y Values")
residual_trend_line = lm(log(attenu$dist) ~ best_line$residuals)
abline(residual_trend_line, col='red', lwd=2)
summary(residual_trend_line)

##-----Multiple Regression Example------
data = read.table('imports-85.data', sep=",", header=FALSE, na.strings = c("NA","?"))
columns = c('symboling','normalized_losses','make','fuel_type','aspiration',
            'num_of_doors','body_style','drive_wheels','engine_location','wheel_base',
            'length','width','height','curb_weight','engine_type','num_of_cylinders',
            'engine_size','fuel_system','bore','stroke','compression_ratio','horsepower',
            'peak_rpm','city_mpg','highway_mpg','price')
names(data) = columns

# Missing data:
# fill in normalized_losses:
mean_norm_losses = mean(data$normalized_losses, na.rm=TRUE)
data$normalized_losses[is.na(data$normalized_losses)] = mean_norm_losses

# Deal with missing values
summary(data)
numeric_data_cols = c('symboling','normalized_losses','wheel_base',
                      'length','width','height','curb_weight',
                      'engine_size','bore','stroke','compression_ratio','horsepower',
                      'peak_rpm','city_mpg','highway_mpg','price')
# Fill in with mean:
for(i in numeric_data_cols){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

# Plot price vs. curb weight
plot(data$curb_weight, data$price, pch=16, main="Price vs. Weight",
     xlab="Weight", ylab="Price")

# When dealing with financial data, it usually is better to take the LOG of the $$
plot(data$curb_weight, log(data$price), pch=16, main="Log-Price vs. Weight",
     xlab="Weight", ylab="log(Price)")
log_price_weight_model = lm(log(data$price) ~ data$curb_weight)
abline(log_price_weight_model, lwd=2, col="red")

log_price_weight_summary = summary(log_price_weight_model)

# We can easily create confidence intervals with confint() function:
confint(log_price_weight_model, level = 0.95)

##----Plot a band or 95% confidence!----
alpha = 0.95
num_sds = qt((1+0.95)/2, df = nrow(data)-2) # 2 because there are 2 parameters (slope and intercept)

# First, plot the data
plot(data$curb_weight, log(data$price), pch=16, main="Log-Price vs. Weight",
     xlab="Weight", ylab="log(Price)")
log_price_weight_model = lm(log(data$price) ~ data$curb_weight)
abline(log_price_weight_model, lwd=2, col="red")

# Create confidence bands
CI = predict(log_price_weight_model, se.fit=TRUE)
CI_band = cbind(CI$fit - num_sds * CI$se.fit, CI$fit + num_sds * CI$se.fit)

# Plot Confidence bands
if (log_price_weight_summary$coefficients[2] > 0){ # Positive Slope
  lines(sort(data$curb_weight), sort(CI_band[,1]), lty=2)
  lines(sort(data$curb_weight), sort(CI_band[,2]), lty=2)
}else{ # Negative Slope
  lines(sort(data$curb_weight), sort(CI_band[,1], decreasing = TRUE), lty=2)
  lines(sort(data$curb_weight), sort(CI_band[,2], decreasing = TRUE), lty=2)
}

# This can all easily be done via 'predict()' function
plot(data$curb_weight, log(data$price), pch=16, main="Log-Price vs. Weight",
     xlab="Weight", ylab="log(Price)")
log_price_weight_model = lm(log(data$price) ~ data$curb_weight)
abline(log_price_weight_model, lwd=2, col="red")

conf_bands = predict(log_price_weight_model,newdata=data, interval="confidence")
conf_bands = conf_bands[order(conf_bands[,1]),]
lines(sort(data$curb_weight), conf_bands[,2], col='green', lwd=3)
lines(sort(data$curb_weight), conf_bands[,3], col='green', lwd=3)

# And prediction bands:
pred_bands = predict(log_price_weight_model,newdata=data, interval="prediction")
pred_bands = pred_bands[order(pred_bands[,1]),]
lines(sort(data$curb_weight), pred_bands[,2], col='green', lwd=3, lty=2)
lines(sort(data$curb_weight), pred_bands[,3], col='green', lwd=3, lty=2)

# Transform back out to Price
x_weight = seq(min(data$curb_weight),max(data$curb_weight),len=1000)
y_price_fit = exp(log_price_weight_summary$coefficients[1] + log_price_weight_summary$coefficients[2] * x_weight)

# Plot the results
plot(x_weight, y_price_fit, type='l', lty=1, col='red', lwd=2)
points(data$curb_weight, data$price)
# Add transformed confidence and prediction bands:
lines(sort(data$curb_weight), exp(conf_bands[,2]), col='green', lwd=3)
lines(sort(data$curb_weight), exp(conf_bands[,3]), col='green', lwd=3)
lines(sort(data$curb_weight), exp(pred_bands[,2]), col='green', lwd=3, lty=2)
lines(sort(data$curb_weight), exp(pred_bands[,3]), col='green', lwd=3, lty=2)

# But are our error normally distributed?
# Test normality of errors!
shapiro.test(log_price_weight_model$residuals)
# Remember that the null hypothesis is that our data is normal (we want high p-values)

# No-  Our model's error is not normally distributed

# Look at residuals vs fitted
plot(log_price_weight_model$residuals, log_price_weight_model$fitted.values, pch=16,
     main="Residuals vs Fitted", xlab="Fitted Values", ylab="Residuals")

# Could be other explanatory factors!
# Add fuel - type
unique(data$fuel_type)

log_price_weight_fuel_model = lm(log(price) ~ curb_weight + fuel_type, data = data)
summary(log_price_weight_fuel_model)
shapiro.test(log_price_weight_fuel_model$residuals)

# Not much help. Let's add in everything except engine_location (not enough variability)
log_price_all_model = lm(log(price) ~ . - engine_location, data = data) # All variables but engine location

summary(log_price_all_model)
shapiro.test(log_price_all_model$residuals)

plot(log_price_all_model$residuals, log_price_all_model$fitted.values, pch=16,
     main="Residuals vs Fitted", xlab="Fitted Values", ylab="Residuals")

# Let's create an ordered factor out of 'Num_of_cylinders'
data$num_of_cylinders = ordered(data$num_of_cylinders,
                                levels = c('three','four','five','six','eight','twelve' ))

log_price_all_model2 = lm(log(price) ~ . - engine_location, data = data)

summary(log_price_all_model2)
shapiro.test(log_price_all_model2$residuals)

# Note the num_of_cylinders now has 5 versions:
#   (L)inear, (Q)uadratic, (C)ubic, Quartic, and Quintic
# or x, x^2, x^3, x^4, and x^5

# What variables should I pick?  Note that a few are NA estimates.  This implies that
#  they are multiples of another variable (dependent)
#
# Also, a lot of the variables are NOT significantly different than zero.  We need to
#  get rid of them.  Preferably one at a time, because they have related significances.
#  I.e., removing one variable could make another variable significant.

# We use the function stepAIC to do this.
?step
step_lm = step(log_price_all_model2)
summary(step_lm)
anova(step_lm)

# Why do all this?  Let's try to predict price with random variables:
num_vars = 100
y = data$price
x_names = paste0('x',1:num_vars)
rand_data = lapply(1:num_vars, function(x) rnorm(length(y)))
rand_data = as.data.frame(rand_data)
names(rand_data) = x_names
rand_data$y = y
rm(y)

random_fit = lm(as.formula(paste('y ~',paste(names(rand_data)[1:num_vars], collapse='+'))), data = rand_data)

summary(random_fit)
shapiro.test(random_fit$residuals)
plot(random_fit$residuals, random_fit$fitted.values,pch=16,
     main="residuals vs fitted", xlab="residuals", ylab="fitted values")

##------Graph Examples------

# Load wikipedia edge list
wiki_edges = read.csv("wikipedia_edge_list.csv", stringsAsFactors=FALSE)
wiki_degree_list = table(c(wiki_edges$Source))
wiki_mean_degree = mean(wiki_degree_list)

wiki_hist = hist(wiki_degree_list, breaks=50, freq=FALSE)

# Test Power law distribution
library(igraph)

# Fit power law
pow_fit = power.law.fit(wiki_degree_list)
