##--------------------------------------------
##
## Counting/Probability R code (lecture 4)
##
## Class: PCE Data Science Methods Class
##
## Contains examples of:
##
## -More on Hypothesis Testing, CLT, and Regression
##
##--------------------------------------------


##-----K-S Test-----

norm1 = rnorm(100,mean=0,sd=1)
norm2 = rnorm(100,mean=0,sd=1)

plot(ecdf(norm1), col='blue')
lines(ecdf(norm2), col='red')

# Have to standardize the x-values
x_seq = seq(-3,3,len=100)
y_cdf1 = sapply(x_seq, function(x){
  sum(norm1<x)/length(norm1)
})
y_cdf2 = sapply(x_seq, function(x){
  sum(norm2<x)/length(norm1)
})

plot(x_seq,y_cdf1, col='blue', pch=16)
points(x_seq,y_cdf2,col='red', pch=16)

k_s_stat = max(abs(y_cdf1-y_cdf2))
# where does it occur?
k_index = which.max(abs(y_cdf1-y_cdf2))
k_s_x = x_seq[k_index]
# Add to plot
lines(c(k_s_x,k_s_x), c(y_cdf1[k_index],y_cdf2[k_index]),
      col='black', lwd=2)

# Create k-s statistic function
ks_stat = function(x_min,x_max, dist_a, dist_b){
  x_seq = seq(x_min,x_max,len=1000)
  y_cdf1 = sapply(x_seq, function(x){
    sum(dist_a<x)/length(dist_a)
  })
  y_cdf2 = sapply(x_seq, function(x){
    sum(dist_b<x)/length(dist_b)
  })
  k_s_stat = max(abs(y_cdf1-y_cdf2))
  return(k_s_stat)
}

# Check to make sure
ks_stat(-3, 3, norm1, norm2)

# What should we expect? Is this an ok value?...

##----Repeat N Times-----
N = 10000
k_s_rep = sapply(1:100, function(i){
  dist_a = rnorm(100,mean=0,sd=1)
  dist_b = rnorm(100,mean=0,sd=1)
  return(ks_stat(-3, 3, dist_a, dist_b))
})

hist(k_s_rep, breaks=30, freq=FALSE)
lines(density(k_s_rep))

##----Empirical One Tailed KS test-----
# Alternative hypothesis is that the k-s- statistic
#  is greater than the "expected value".

dist1 = rnorm(1000, mean=0.05, sd = 1)
dist2 = rnorm(1000, mean=0, sd = 1)

# We hypothesize that dist2 is normal(0,1), like we know dist1 is.

k_s_stat = ks_stat(-5,5, dist1, dist2)

# What should the distribution be?
# i.e. what is the expected value?
k_s_hypothesis = sapply(1:500, function(i){
  dist_a = rnorm(1000,mean=0,sd=1)
  dist_b = rnorm(1000,mean=0,sd=1)
  return(ks_stat(-3, 3, dist_a, dist_b))
})

# Histogram of the NULL hypothesis
hist(k_s_hypothesis, breaks=30)
# Add a line of the observation
abline(v=k_s_stat, col='red', lwd=2)


empirical_p_value = sum(k_s_hypothesis>k_s_stat)/500


##-----Shapiro-Wilk's Test----
dist_a = rnorm(100,mean=0,sd=1)
shapiro.test(dist_a)
# Can NOT reject null that dist_a is from a normal population.

dist_b = runif(100)
shapiro.test(dist_b)
# Why is this rejected? Because the uniform distribution is *flat* (kurtosis)

##----Look at Normal Quantile-Quantile Plot----
qqnorm(dist_a, pch=16)
qqline(dist_a)

qqnorm(dist_b, pch=16)
qqline(dist_b)

##-----ANOVA Example-----
df = data.frame('group'=c(rep(1,50),
                          rep(2,50),
                          rep(3,60),
                          rep(4,40)),
                'val' = c(rnorm(50, mean=0, sd=1),
                          rnorm(50, mean=0, sd=1),
                          rnorm(60, mean=0.5, sd=1), # Note different mean here
                          rnorm(40, mean=0, sd=1)))
df$group = factor(df$group) # Make sure your groups are a factor (for further analysis below)

boxplot(df$val ~ df$group)
df_aov = aov(val ~ group, data = df)
summary(df_aov)

# we get statistics on the groups and total residuals:
# DF = degrees of freedom
# Sum Sq = sum of squares
# Mean Sq = Mean Squared Error
# F -value = our statistic based on a ratio of Mean Square Errors
# Pr(>F) = p-value for the NULL hypothesis that all groups are the same.

# BUT WHICH GROUP IS DIFFERENT? ANOVA does not tell us which.  We need 
#  to further test to find out.

tukey_anova = TukeyHSD(df_aov)  # Tukey's Range test:
tukey_anova
# This test is built on the distribution of testing two means (say mean1 and mean2),
#   and the statistic is given by (mean1 - mean2)/sd(both samples)
#   Know that this statistic has a known (albeit uncommon) distribution and this allows us
#   (or R) to create multiple hypotheses (pairwise tests).
plot(tukey_anova)

##----CLT and Confidence Intervals----
# Let's create a NOT normal distribution
x = c(rnorm(1000),rnorm(1000,mean=3,sd=0.5))
plot(density(x)) # Definitely not normal

# generate 100 samples
x_samples = lapply(1:500, function(i) sample(x, size=50, replace=TRUE))
x_means = lapply(x_samples, mean)
hist(unlist(x_means))
qqnorm(unlist(x_means)) # Yay normality!
qqline(unlist(x_means))

pop_mean_estimate = mean(unlist(x_means))
pop_mean_sd = sd(unlist(x_means))

actual_mean = mean(x)

# Create an alpha-level confidence interval
#    it will be two tailes, so we need (1+alpha)/2
alpha = 0.95
half_width = qnorm((1+alpha)/2, mean=pop_mean_estimate, sd = pop_mean_sd) - pop_mean_estimate

ci_low = pop_mean_estimate - half_width
ci_high = pop_mean_estimate + half_width

print(paste('The actual mean is',round(actual_mean,3)))
print(paste('The',alpha,'level CI is (',round(ci_low,3),',',round(ci_high,3),').'))

# What happens to the width of the CI
#   when we increase or decrease the alpha level?

##---Confidence interval of monty hall switching-----

resample <- function(x, ...) x[sample.int(length(x), ...)]

n=1000
switch_outcomes = sapply(1:n, function(x){
  prize = resample(1:3,1)           # select a prize
  non_prizes = setdiff(1:3,prize)   # set the non-prizes
  first_pick = resample(1:3,1)      # select a door
  revealed_non_prize = resample(setdiff(non_prizes,first_pick),1) # reveal a non-prize/non-selected door
  switch_pick = setdiff(1:3,c(first_pick,revealed_non_prize))     # select the other door
  return(switch_pick==prize)
})

sample_mean = mean(switch_outcomes)
sample_var = var(switch_outcomes)

alpha_level = 0.95
scaled_var = sample_var/sqrt(n)

# How many sd's away from the mean is our cutoff?
n_sds = qnorm((1+alpha_level)/2)
# or
n_sds = -qnorm((1-alpha_level)/2)

c_i_upper = sample_mean + n_sds*sqrt(scaled_var)
c_i_lower = sample_mean - n_sds*sqrt(scaled_var)

print('Population Mean Estaimate:')
print(paste(round(sample_mean,3),'with a',100*alpha_level,'% CI: (',
            round(c_i_lower,3),',',round(c_i_upper,3),').'))

# Try changing the alpha-level:
# A lower alpha level, say 0.9 (90% C.I.) will be smaller because
#   we are going to be LESS sure of where the population mean is.
#
# A higher alpha level, say 0.99 (99% C.I.) will be larger because
#   we are going to be MORE confident of where the population mean is.

##-----Interpret Slope Example-----
lv_high_temps = c(100,91,101,102,112,89,103, 96, 105)
lv_flamingo_hc = c(5971, 5980, 6366, 6771, 6699, 5888, 6119, 6341, 6550)

lv_flamingo_hc = lv_flamingo_hc[order(lv_high_temps)]
lv_high_temps = lv_high_temps[order(lv_high_temps)]

plot(lv_high_temps, lv_flamingo_hc)
slope_guess = (lv_flamingo_hc[1] - lv_flamingo_hc[9])/(lv_high_temps[1] - lv_high_temps[9])

# Pick a point, and calculate the y-intercept:
# y = mx + b,   or   b = y - mx
y_int_guess = lv_flamingo_hc[1] - slope_guess * lv_high_temps[1]

abline(y_int_guess, slope_guess)

# What are the units of the slope?
#   remember that all the terms in y = mx + b have the same units,
#   so the units of m must be in terms of (units of y)/(units of x)

# How do I interpret the slope?
#  For every 1 increase in x, y must increase my 'm'!

##-----Linear Regression Example-----

x = 1:20
y = x + rnorm(20, mean=0, sd=3)

plot(x,y,pch=16, main="Sample Data")
true_y = 1:20
lines(x,true_y, col='blue')

SSE = sum((y - true_y)^2)

# Pick a random line and see SSE
y_rand1 = 0.75 * x
sse_rand1 = sum((y - y_rand1)^2)

y_rand2 = 0.85 * x
sse_rand2 = sum((y - y_rand2)^2)

# Calculate SSE for a sequence of slopes to see min
slopes_test = seq(0.75,1.25, len=1000)
sse_slopes = sapply(slopes_test, function(m) sum((y - (m * x))^2))
plot(slopes_test, sse_slopes, pch=16, type='l')
grid()

# find min
slope_min = slopes_test[which.min(sse_slopes)]

# What if I just predicted the average y value (horizontal line?
y_avg = rep(mean(true_y), length(x))
plot(x,y,pch=16)
lines(x,y_avg)

SST = sum((y - y_avg)^2)  # Sum of Squares Total

# Proportion of variablity explained by predicting y from x
1 - SSE/SST  # This is called R-squared.  It is interpreted as
             #  the % of variance explained in the model (more than the average)


##-----Least Squares Regression in R-----
best_fit_line = lm(y~x)
plot(x,y,pch=16)
abline(best_fit_line)

summary(best_fit_line)

# Note the better r-squared.
#
# Also, the 'Adjusted R-squared' tries to take into account
#   the fact that adding random predictors will eventually
#   make your prediction better.  So it adjusts the R-squared
#   by the ratio:
#
#    (N-1)
#   -------     ,  N = # of obs.,   k = # of variables
#   (N-k-1)
#


##----Negative R Squared-----

x = 1:50
y = 2 * x + 5 * rnorm(length(x), mean=5, sd=3)
plot(x,y, pch=16, ylim=c(0,150), xlim=c(0,50))
grid()

# add the mean line
y_mean = rep(mean(y), length(x))
lines(x, y_mean, col="black")

# add best fit line
best_line = lm(y~x)
abline(best_line, col="gray23", lty=2)

# add a horrible line
horrid_line = -2 * x + 150
lines(x, horrid_line, col="red")

# Best fit R^2:
summary(best_line)

# Horrid fit R^2:
SSE = sum(( horrid_line - y )^2)
SST = sum(( y_mean - y )^2)
1 - SSE/SST

# Plot the errors
plot(x, (horrid_line - y)) # Ugh! A trend in the errors exists!
plot(x, (best_line$fitted.values - y)) # Yay!  No apparent trend!

# Mean of the best fit line errors
mean(horrid_line - y)
mean(best_line$fitted.values - y) # Bias is essentially zero!

# Look at mean of the absolute values of the errors
mean(abs(best_line$fitted.values - y))  # What does this mean?

# Look at summary of fit
summary(best_line)

# Residuals: Tells us about the residuals. (Better to look at a plot)
#
# Coefficients:
#  -Intercept
#  -Slope for each X variable
#    -Estimates, Std. Error
#    -t-value:  This is the statistic for a test
#               if the parameter is different than 0 (two tailed)
#    - Pr(>|t|):  This the p-value for the above
#                 hypothesis test (with sig. codes)

# Residual Standard Error: Standard deviation of the residuals
# Multiple R Squared: Measure of variation explained
#     -Adjusted R Squared: Controls for # of independent variables
# F Statistic: Hypothesis test comparing model fit against random noise

# Confidence Interval for parameters
slope_est = summary(best_line)$coefficients[2]
y_int_est = summary(best_line)$coefficients[1]

slope_st_err = summary(best_line)$coefficients[4]
y_int_st_err = summary(best_line)$coefficients[3]

# Here we use the t-distribution for alpha-confidence cutoffs
#   because we do not know the inherent population variance!

alpha_level = 0.95

n_sds = qt((1+alpha_level)/2, df = length(x)-2) # t distribution!
c_i_lower_slope = slope_est - n_sds * slope_st_err
c_i_upper_slope = slope_est + n_sds * slope_st_err

print('Slope Estimate:')
print(paste(round(slope_est,3),'with a',100*alpha_level,'% CI: (',
            round(c_i_lower_slope,3),',',round(c_i_upper_slope,3),').'))

c_i_lower_yint = y_int_est - n_sds * y_int_st_err
c_i_upper_yint = y_int_est + n_sds * y_int_st_err

print('Y-Intercept Estimate:')
print(paste(round(y_int_est,3),'with a',100*alpha_level,'% CI: (',
            round(c_i_lower_yint,3),',',round(c_i_upper_yint,3),').'))


##-------Homework 4 Hint-------
# You will need to sum across zip codes.
# 
# Note that zip code units will be proportions.
# Here it is ok to average them because the proportions have the
#   same denominator (per X admissions)

test_data = data.frame('Label'=letters[1:10],
                       'x_val1'=1*rnorm(10), 'y_val1' = 1*rnorm(10),
                       'x_val2'=2*rnorm(10), 'y_val2' = 2*rnorm(10),
                       'x_val3'=3*rnorm(10), 'y_val3' = 3*rnorm(10),
                       'x_val4'=4*rnorm(10), 'y_val4' = 4*rnorm(10),
                       'x_val5'=5*rnorm(10), 'y_val5' = 5*rnorm(10),
                       'x_val6'=6*rnorm(10), 'y_val6' = 6*rnorm(10),
                       'x_val7'=7*rnorm(10), 'y_val7' = 7*rnorm(10),
                       'x_val8'=8*rnorm(10), 'y_val8' = 8*rnorm(10),
                       'x_val9'=9*rnorm(10), 'y_val9' = 9*rnorm(10),
                       'x_val10'=10*rnorm(10), 'y_val10' = 10*rnorm(10))

# Get the averages of all columns
test_data_avgs = apply(test_data[-1],2,mean)

x_vals = test_data_avgs[grepl('^x_val[0-9]+$', names(test_data_avgs), perl = TRUE)]
y_vals = test_data_avgs[grepl('^y_val[0-9]+$', names(test_data_avgs), perl = TRUE)]

x_diffs = diff(x_vals)
y_diffs = diff(y_vals)
