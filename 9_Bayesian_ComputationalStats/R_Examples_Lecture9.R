##--------------------------------------------
##
## Lecture 8 R methods
##
## Class: PCE Data Science Methods Class
##
##--------------------------------------------

setwd('E:/Work/Teaching/PCE_Data_Science/9_Bayesian_ComputationalStats')

library(boot)
library(ggplot2)
library(MASS)
library(caret)
library(e1071)
library(ROCR)

##----Explore the Beta Distribution-----

# Beta depends on two parameters, alpha and beta

alpha = c(0.5,1,2,3,4)
beta = alpha

x = seq(0.001,0.999,length=100)

par(mfrow = c(5,5), mar=c(2,1,2,1)) # mar = c(bottom, left, top, right)
sapply(alpha, function(a){
  sapply(beta, function(b){
    plot_title = paste("(a,b)=(",a,",",b,")")
    plot(x,dbeta(x,a,b),xlab="",ylab="",
         main=plot_title, type="l", lwd=2)
  })
})

# Set plot options back to normal
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))

##------Bayesian Inference for a coin flip-----
# One parameter estimation:
# coin flip p(H)

# Create Coin Flips
flips = sample(c(0,1), 10, replace=TRUE, prob = c(0.5, 0.5))
num_heads = sum(flips==1)
num_tails = sum(flips==0)

# Create x-axis values (p(h) = theta)
theta_vals = seq(0.01, 0.99, length=100)

# Create a prior:
# Arbitrarily chose to be a shifted/reflected absolute value shape = /\
prior = pmin(theta_vals,1-theta_vals)
prior = prior / sum(prior)
# look at prior belief:
plot(theta_vals, prior)

# Calclulate the probability of each theta value (theta_vals) for our given data (num_heads, num_tails)
likelihood = theta_vals**num_heads * (1-theta_vals)**num_tails

# Look at likelihood:
plot(theta_vals, likelihood)
abline(v=num_heads/10) # Most likely estimator

# Calculate the P(Data) which is just the sum of all probabilities: P(D|theta)*P(Theta) for all theta
# P(Data|0.01)*0.01 + P(Data|0.02)*0.02 + P(Data|0.03)*0.03 + ...
# This is can be interpreted as the 'Probability of observing the data, under all possible weighted coins'
prob_data = sum(likelihood * theta_vals)

# Finally, calculate the posterior according to Bayes Law
posterior = likelihood * (theta_vals / prob_data)

# Plot the output
par(mfrow=c(3,1))
plot(theta_vals, prior, type="l")
plot(theta_vals, likelihood, type="l")
plot(theta_vals, posterior, type="l")

# Stick this in a function
prior_likelihood_posterior_coin = function(theta_vals, prior, data){
  num_heads = sum(data==1)
  num_tails = sum(data==0)
  
  # Calculate likelihood
  likelihood = theta_vals**num_heads * (1-theta_vals)**num_tails
  
  # Calculate P(data)
  prob_data = sum(likelihood * prior)
  
  # Calculate Posterior
  posterior = likelihood * (prior / prob_data)
  
  # Plot the output
  par(mfrow = c(3,1), mar=c(3,3,1,0))
  plot(theta_vals, prior, xlab="", ylab="", main="Prior", type="l")
  plot(theta_vals, likelihood, xlab="", ylab="", main="Likelihood", type="l")
  plot(theta_vals, posterior, xlab="", ylab="", main="Posterior", type="l")
  
}

prior_likelihood_posterior_coin(theta_vals, dbeta(theta_vals,2,5), flips)

# Crazy prior:

unscaled_prior = pmax((-theta_vals + (1/8)),0) + pmax(sqrt(1-(theta_vals-0.5)^2)-0.95,0) +
  pmax((theta_vals - (7/8)),0)
crazy_prior = unscaled_prior / sum(unscaled_prior)
plot(theta_vals, crazy_prior)

prior_likelihood_posterior_coin(theta_vals, crazy_prior, flips)


##-----Monte-Carlo Markov-Chain for a Coin Flip------

likelihood_fun = function(theta, data){
  num_heads = sum(data==1)
  num_tails = sum(data==0)
  # Calculate likelihood
  likelihood = theta**num_heads * (1-theta)**num_tails
  # Because we are sampling theta at random, if theta is out-of-bounds, likelihood is zero:
  likelihood[theta > 1 | theta < 0] = 0
  return(likelihood)
}

prior_fun = function(theta){
  # prior = dbeta(pmin(2*theta,2*(1-theta)) ,2,2 ) # bimodal
  prior = rep(1/length(theta), length(theta)) # Even
  
  # Again, if theta is out-of-bounds, probability = 0
  prior[theta > 1 | theta < 0] = 0
  
  return(prior)
}

relative_probability_fun = function(theta, data){
  target_prob = likelihood_fun(theta, data) * prior_fun(theta)
  return(target_prob)
}

# Observe 25 fair coin flips
observed_data = sample(c(0,1), 25, replace=TRUE)

# MCMC needs to know how many iterations to do:
chain_length = 1000

# MCMC needs a starting position
chain_start = 0.5

# 'Burn in' is how long it takes to converge to the distribution
burn_in = 100

# Need to keep track of how many points were rejected or accepted
accepted_num = 0
rejected_num = 0

# Initialize the chain
chain = rep(0, chain_length)
chain[1] = chain_start

for (t in 1:(chain_length-1)){
  current_position = chain[t]
  
  # Propose new spot
  proposal = rnorm(1, mean=0, sd=0.1)
  
  # Calculate the new acceptance probability
  # ----side note: the min() function here is not really needed.
  accept_prob = min(1, relative_probability_fun(current_position+proposal, observed_data) / 
                      relative_probability_fun(current_position, observed_data))
  
  if (runif(1) < accept_prob){
    chain[t+1] = current_position + proposal
    # If we are past our burn in period, accept
    if (t > burn_in){accepted_num = accepted_num + 1}
  } else{
    chain[t+1] = current_position
    # If we are past our burn in period, reject
    if (t > burn_in){rejected_num = rejected_num + 1}
  }
}

par(mfrow = c(1,1))
hist(chain, breaks = 30, xlim = c(0,1))

print(paste("Accepted Proportion: ", round(accepted_num/(chain_length-burn_in),3)))
print(paste("Rejected Proportion: ", round(rejected_num/(chain_length-burn_in),3)))


##-----Multiple Parameter Estimation------
# Create a 'truth' distribution and plot some samples from it.
random_points = mvrnorm(10000, mu=c(0.5,0.5), Sigma=matrix(c(1,0.6,0.6,1), nrow=2))
plot(random_points[,1], random_points[,2], xlim=c(-4,4), ylim=c(-4,4), col=rgb(0,0,0,0.25))

# Now let's try to recreate that distribution via MCMC...

# Given a point, our value at that point(x,y) will be the value of the distribution at x,y:
likelihood = function(x,y){
  sigma = matrix(c(1,0.6,0.6,1), nrow=2)
  mu = c(0.5,0.5)
  dist = c(x,y) - mu
  value = (1/sqrt(4*pi^2**det(sigma))) * exp((-1/2) * t(dist) %*% ginv(sigma) %*% t(t(dist)) )
  return(value)
}

# Where to start:
x_chain = 4
y_chain = -4
# Chain length:
chain_length = 10000

#Evaluate current position:
current_val = likelihood(x_chain,y_chain)

# Standard deviation of how far out to propose:
proposal_sd = 1

# Keep track of things:
accept_count = 0
reject_count = 0


for (n in 1:(chain_length-1)){ # chain length minus 1 because we already have a point (the starting point)
  proposed_x = x_chain[n] + rnorm(1, mean=0, sd=proposal_sd)
  proposed_y = y_chain[n] + rnorm(1, mean=0, sd=proposal_sd)
  proposed_val = likelihood(proposed_x, proposed_y)
  
  # Accept according to probability:
  if (runif(1) < (proposed_val/current_val)){
    x_chain = c(x_chain, proposed_x)
    y_chain = c(y_chain, proposed_y)
    current_val = proposed_val
    accept_count = accept_count + 1
  }else{
    x_chain = c(x_chain, x_chain[n])
    y_chain = c(y_chain, y_chain[n])
    reject_count = reject_count + 1
  }
  
}

plot(x_chain, y_chain, col=rgb(0,0,0,0.25), xlim=c(-4,4), ylim=c(-4,4),
     main="MCMC values for a Bivariate Normal", xlab="x", ylab="y")

# Burn in problem.  Solution?  Throw away first part of chain.
num_burnin = round(0.1*chain_length)

plot(x_chain[num_burnin:chain_length], y_chain[num_burnin:chain_length],
     col=rgb(0,0,0,0.25), xlim=c(-4,4), ylim=c(-4,4),
     main="MCMC values for a Bivariate Normal", xlab="x", ylab="y")

# Estimate bivariate mean from chain:
mcmc_mean = c(mean(x_chain), mean(y_chain))

# Acceptance/Reject rate:
accept_count/chain_length
reject_count/chain_length

# Always look at the chain, we would like random noise centered around means
plot(x_chain, type="l")
plot(y_chain, type="l")

# What happens to the chains when we decrease the s.d. of the proposal?

# Summary:
#   Proposal s.d. is too small when the chain does not look like random noise.
#   Proposal s.d. is too large when the acceptance rate is too low.

##-----Bayesian Linear Regression-----
hosp_data = read.csv("ChicagoDiabetesData.csv", stringsAsFactors = FALSE)
data_sums = apply(hosp_data[-1],2,sum)

hospitalizations = data_sums[grepl('Hospitalizations', names(hosp_data))]
admit_rate = data_sums[grepl('Crude.Rate.[0-9]+$', names(hosp_data), perl = TRUE)]

plot(hospitalizations, admit_rate, main="Admit Rate vs. Hospitalizations for Diabetes (Chicago)",
     pch=16, xlab="Hospitalizations", ylab="Admit Rate")
# Know that model chosen will be a 'y=mx+b' model and that the
#   error is distributed via a Normal with zero mean and some standare deviation, 'sd'

# Define a likelihood function.  This will tell us the probability of the data given
#   a set of parameters (m,b,sd)

likelihood_linear = function(m, b, sd){
  # Create predictions based on parameters
  predictions = m * hospitalizations + b
  
  # Actual Probabilities depend on the data's distance from the predictions and 'sd'
  individual_likelihoods = dnorm(admit_rate, mean=predictions, sd = sd, log = T) # Log likelihood
  
  # Sum them all up (logs of products = sum of logs)
  sum_likelihood = sum(individual_likelihoods)
  return(sum_likelihood)
}

# Need to defind the prior probability
prior_linear = function(m, b, sd){
  m_prior = dunif(m, min=0, max=5, log=TRUE)      # 0 < m < 10 is a good prior
  b_prior = dunif(b, min=0, max=200, log=TRUE)    # Random guess
  sd_prior = dunif(sd, min=0,  max=500, log=TRUE) # 0 < sd < 500 Just picked a random range.
  return(max(m_prior,0) + max(b_prior,0) + max(sd_prior,0)) # Remember, logs of a product = sum of logs
}

# Need to define a posterior:
posterior_linear = function(m, b, sd){
  return(likelihood_linear(m, b, sd) + prior_linear(m, b, sd)) # sum because of logs again
}

# Perform MCMC to get parameter distributions:
chain_length = 10000
burn_in = round(0.1 * chain_length)
chain_vals = array(dim=c(chain_length, 3))
chain_vals[1,] = c(1, 75, 2.5) # Guess start value
current_val = posterior_linear(chain_vals[1,1], chain_vals[1,2], chain_vals[1,3])

accept_count = 0
reject_count = 0

m_sd = 0.02
b_sd = 30
sigma_sd = 1

for (i in 1:(chain_length-1)){
  new_proposal = rnorm(3, mean = chain_vals[i,], sd = c(m_sd,b_sd,sigma_sd))
  # Note: you might have to tweak the standard deviation of the proposal region
  
  # Also, the sd parameter cannot be proposed to be negative:
  new_proposal[3] = abs(new_proposal[3])
  
  # Now calculate the value at new proposal:
  proposal_prob = posterior_linear(new_proposal[1], new_proposal[2], new_proposal[3])
  probability = exp(proposal_prob - current_val) # A math trick to prevent rounding errors
  if (runif(1) < probability){
    chain_vals[i+1,] = new_proposal
    current_val = proposal_prob
    accept_count = accept_count + 1
  }else{
    chain_vals[i+1,] = chain_vals[i,]
    reject_count = reject_count + 1
  }
  
  # Update Proposal distribution:
  if ((i%%1000==0) & (i > 999)){
    m_sd = sd(chain_vals[(i-999):i,1])
    b_sd = sd(chain_vals[(i-999):i,2])
    sigma_sd = sd(chain_vals[(i-999):i,3])
  }
  
}

par(mar=c(2,4.1,2,2.1), mfrow = c(3,1))
hist(chain_vals[,1], breaks=30)
hist(chain_vals[,2], breaks=30)
hist(chain_vals[,3], breaks=30)

plot(chain_vals[,1], main="m (slope)")
plot(chain_vals[,2], main="b (intercept)")
plot(chain_vals[,3], main="sd (sigma)")


par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))
plot(hospitalizations, admit_rate, main="Hospitalizations vs. Admit Rate for Diabetes (Chicago)",
     pch=16, xlab="Hospitalizations", ylab="Admit Rate")
abline(mean(chain_vals[burn_in:chain_length,2]), mean(chain_vals[burn_in:chain_length,1]))

# We can also plot sample lines from our distribution of m's and b's:
N = 100 # # of lines to plot
sample_line_indices = sample(1:chain_length, N)
for (line in sample_line_indices){
  m_temp = chain_vals[line,1]
  b_temp = chain_vals[line,2]
  abline(b_temp, m_temp, col=rgb(0.9,0.9,0,0.25))
}
abline(mean(chain_vals[burn_in:chain_length,2]), mean(chain_vals[burn_in:chain_length,1]))
points(hospitalizations, admit_rate, pch=16)


##-----Bootstrapping-----

ages = c(21,25,27,28,28,29,29,30,30,31,33,33,33,35,40,95)
mean(ages)
sd(ages)
num_samples = length(ages)

# Super bad approximation, probably not even
#   statistically valid with a small sample:
mean(ages) + 1.98*sd(ages) / sqrt(num_samples)
mean(ages) - 1.98*sd(ages) / sqrt(num_samples)

N = 5000

bootstrap_samples = sapply(1:N, function(x){
  sample(ages, num_samples, replace = TRUE)
})

boot_means = apply(bootstrap_samples, 2, mean)

hist(boot_means, breaks = 50)
# Q: Why the multi-modal distribution?

# Look at some outliers:
bootstrap_samples[,boot_means<28]
bootstrap_samples[,boot_means>50]

# Estimate the S.D.:
boot_sds = apply(bootstrap_samples, 2, sd)
mean(boot_sds)

# New CI
mean(boot_means) + 1.98 * mean(boot_sds) / sqrt(num_samples)
mean(boot_means) - 1.98 * mean(boot_sds) / sqrt(num_samples)

# Use the boot function
mean_fun = function(data, ix) mean(data[ix], na.rm = TRUE)
# Boot function requires
#  a function that takes the data and indices (ix)

boot_results = boot(ages, mean_fun, R=1000, sim="ordinary")

# Create a trim function to remove outliers:
trim = function(data, n=1){
  return(sort(data)[(1+n):(length(data)-n)])
}

boot_trim_means = apply(bootstrap_samples, 2, function(x) mean(trim(x, 2)))
hist(boot_trim_means, breaks = 50)
mean(boot_trim_means)

# use boot function:
mean_trim_fun = function(data, ix) mean(trim(data[ix]), na.rm = TRUE)
boot_trim_results = boot(ages, mean_trim_fun, R=1000, sim="ordinary")

##-----Bootstrapping for Linear Regression-----
N = 1000

# Get samples of which points to fit a line to (with replacement)
point_indices = lapply(1:N, function(x){
  sample(1:length(hospitalizations), length(hospitalizations), replace=TRUE)
})

# Get m and b for each fit line
boot_coef = lapply(point_indices, function(x){
  temp_lm = lm(hospitalizations[x] ~ admit_rate[x])
  return(c(summary(temp_lm)$coefficients[1], summary(temp_lm)$coefficients[2]))
})

# Convert to matrix
boot_coef = matrix(unlist(boot_coef), ncol=2, byrow = TRUE)
head(boot_coef)

hist(boot_coef[,1], breaks = 30)
hist(boot_coef[,2], breaks = 30)

# Bootstrapping the residuals
admit_rate_hosp_model = lm(hospitalizations ~ admit_rate)
residuals = as.numeric(admit_rate_hosp_model$residuals)
fit_vals = as.numeric(admit_rate_hosp_model$fitted.values)

hospitalizations_boots = lapply(1:N, function(x){
  ix = sample(1:length(residuals), length(residuals), replace=TRUE)
  return(fit_vals + residuals[ix])
})

# Look a a few plots:
plot(hospitalizations_boots[[1]], admit_rate, pch=16, col=rgb(0,0,0,0.05))
lapply(1:50, function(x){
  points(hospitalizations_boots[[x]], admit_rate, pch=16, col=rgb(0,0,0,0.05))
})

# Calculate linear models:
boot_coef_resid = lapply(1:N, function(x){
  temp_lm = lm(hospitalizations_boots[[x]] ~ admit_rate)
  return(c(summary(temp_lm)$coefficients[1], summary(temp_lm)$coefficients[2]))
})

boot_coef_resid = matrix(unlist(boot_coef_resid), ncol=2, byrow = TRUE)
head(boot_coef_resid)

hist(boot_coef_resid[,1], breaks = 30)
hist(boot_coef_resid[,2], breaks = 30)

# See if SDs are close enough
sd(boot_coef[,1])
sd(boot_coef_resid[,1])

sd(boot_coef[,2])
sd(boot_coef_resid[,2])

# Note, this is a well behaved, nice linear relationship.
#  In general, the residual bootstrap is more robust, and is more often used.

##-----Computational P-value----

# Consider the facebook edges:
fb_data = read.csv("facebook_edge_list.csv", stringsAsFactors = FALSE)

# Define our K-S function:
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

# Mean degree:
degree_list = table(fb_data$Source)
mean_degree = mean(degree_list)
num_vertices = length(unique(fb_data$Source))

# Create similar distributions
N = 1000
similar_degree_dists = lapply(1:N, function(x){
  sample(degree_list, num_vertices, replace=TRUE) # replace MUST be true
})

# Find expected distribution of k-s-stats of similar distributions
k_s_stat_similar = lapply(similar_degree_dists, function(d){
  xmin = 1
  xmax = max( max(d), max(degree_list) )
  return(ks_stat(xmin, xmax, d, degree_list))
}) # This might take a few seconds to compute

hist(unlist(k_s_stat_similar), breaks=50)
# Who knows what distribution this might be.

# Let's compute the k_s_stat with many poissons
sample_pois = lapply(1:N, function(x){
  rpois(num_vertices, lambda = mean_degree)
})

k_s_stat_poisson = lapply(sample_pois, function(s){
  xmin = 1
  xmax = max( max(s), max(degree_list) )
  ks_statistic = ks_stat(xmin, xmax, s, degree_list)
})

# plot two distributions on top of each other:
plotting_data_frame = data.frame("k_s_stat"=c(unlist(k_s_stat_similar), unlist(k_s_stat_poisson)),
                                 "category"=c(rep("similar",N),rep("poisson",N)))
ggplot(plotting_data_frame, aes(k_s_stat, fill=category)) + geom_density(alpha=0.3)
# Not even worth computing a p-value, or a hypothesis test that these are similar distributions.

# For a better example, what is the p-value of observing a k-s statistic of 0.05 (or worse)?
# one-tailed or two tailed?
similar_ks = unlist(k_s_stat_similar)
sum(similar_ks>0.05)/N

##----Hypothesis testing via repeated calculations-----
# Mandatory coin flip example:

observed_heads = c(1,1,1,0,1,0,0,1,1,0,1) # 7/11 heads
7/11
# Hypothesize that this is not a fair coin:
#   Null hypothesis:         H0: p(H) = 0.5
#   Alternative hypothesis:  Ha: p(H) != 0.5

# Two tailed test, (but we can also be smart and make a one tailed test, since 7/11 > 0.5)
N = 1000
sim_outcomes = lapply(1:N, function(x) sample(c(0,1), length(observed_heads), replace=TRUE))
sim_means = unlist(lapply(sim_outcomes, mean))
hist(sim_means, breaks=10)

# Perform hypothesis test:
alpha = 0.95
one_tailed_p = quantile(sim_means, 0.95)  # Here, an 8/11 or 3/11 would reject Null
two_tailed_p = quantile(sim_means, 0.975) # Here a 9/11 would reject the Null


##-------Confusion Matrix------

# Load the Advertising Prediction Dataset:
#  Here we will try to predict wether or not
#  a customer will buy a magazine based on
#  several factors

magazine = read.csv("AdvertisingPrediction.csv", stringsAsFactors = FALSE)

# Create logistic model:
log_mag_model = glm(Buy ~ ., data = magazine, family = "binomial")
cutoff = 0.5
log_predictions = as.numeric(log_mag_model$fitted.values>cutoff)

actuals = magazine$Buy

confusionMatrix(log_predictions, actuals)


##------AUC/ROC------

# Change the cutoffs
cutoff_seq = seq(0.001, 0.999, length = 100)

true_pos_rate = sapply(cutoff_seq, function(x){
  log_predictions = as.numeric(log_mag_model$fitted.values>x)
  true_pos = sum((log_predictions==1) & (actuals==1))/sum(actuals==1)
})

false_pos_rate = sapply(cutoff_seq, function(x){
  log_predictions = as.numeric(log_mag_model$fitted.values>x)
  false_pos = sum((log_predictions==1) & (actuals==0))/sum(actuals==1)
})

plot(false_pos_rate, true_pos_rate, type="l", xlim=c(0,1), ylim=c(0,1))
abline(0,1)

# Do this with the ROCR package:
# First we create an ROCR 'prediction' object:
logistic_prediction = prediction(log_mag_model$fitted.values, actuals)

performance(logistic_prediction, measure = 'auc')

# If we are only concerned with accuracy, maybe a different cutoff would help:
acc_logistic = performance(logistic_prediction, measure = "acc")
plot(acc_logistic)
# What does this show us?

# Find the best cutoff for maximal accuracy:
ind = which.max( slot(acc_logistic, "y.values")[[1]] )
acc = slot(acc_logistic, "y.values")[[1]][ind]
max_cutoff = slot(acc_logistic, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = max_cutoff))


##----Logistic Model Evaluation-----

train_ix = sample(1:nrow(magazine), round(0.8*nrow(magazine)))

train_set = magazine[train_ix,]
test_set = magazine[-train_ix,]

# Create logistic model:
log_mag_model2 = glm(Buy ~ ., data = train_set, family = "binomial")

# Evaluate model on test set:
log_predict_test = predict(log_mag_model2, newdata = test_set, type="response")
test_predictions = as.numeric(log_predict_test>0.5)
test_actuals = test_set$Buy

# Confusion Matrix:
confusionMatrix(test_predictions, test_actuals)


##----Cross Validation-----

# Approximate way:

# Sample from 1 to k, nrow times (the number of observations in the data)
k = 4
magazine$cv_group = sample(1:k, nrow(magazine), replace = TRUE)
cv_list = 1:k

cv_acc = c()
for (i in 1:k){
  # remove rows with id i from dataframe to create training set
  # select rows with id i to create test set
  train_set = subset(magazine, cv_group %in% cv_list[-i])
  test_set = subset(magazine, cv_group %in% cv_list[i])
  
  # Fit the logistic model:
  log_model_cv = glm(Buy ~ . - cv_group, data = train_set, family = "binomial")
  
  # Calculate predictions on test set:
  test_probs = predict(log_model_cv, newdata = test_set, type="response")
  test_preds = as.numeric(test_probs > 0.5)
  
  # Calculate accuracy:
  test_acc = sum(test_preds == test_set$Buy)/nrow(test_set)
  
  # Store accuracy:
  cv_acc = c(cv_acc, test_acc)
}

mean(cv_acc)
sd(cv_acc)

# Exact way for LOOCV:
acc_loocv = c()
for (r in 1:nrow(magazine)){
  # Define train/test sets
  train_ix = setdiff((1:nrow(magazine)), r)
  train_set = magazine[train_ix,]
  test_set = magazine[-train_ix,]
  
  # Fit the Model:
  log_model_cv = glm(Buy ~ . - cv_group, data = train_set, family = "binomial")
  
  # Calclulate predictions on the test set:
  test_probs = predict(log_model_cv, newdata = test_set, type="response")
  test_preds = as.numeric(test_probs > 0.5)
  
  # Calculate accuracy:
  test_acc = sum(test_preds == test_set$Buy)/nrow(test_set)
  
  # Store accuracy:
  acc_loocv = c(acc_loocv, test_acc)
}

mean(acc_loocv)
sd(acc_loocv)


##----Bias Variance Trade off for k-fold CV------

k_seq = 2:75

bias_var = function(k){
  magazine$cv_group = sample(1:k, nrow(magazine), replace = TRUE)
  cv_list = 1:k
  cv_acc = sapply(1:k, function(group){
    train_set = subset(magazine, cv_group %in% cv_list[-group])
    test_set = subset(magazine, cv_group %in% cv_list[group])
    log_model_cv = glm(Buy ~ . - cv_group, data = train_set, family = "binomial")
    test_probs = predict(log_model_cv, newdata = test_set, type="response")
    test_preds = as.numeric(test_probs > 0.5)
    test_acc = sum(test_preds == test_set$Buy)/nrow(test_set)
    return(test_acc)
  })
  return(sd(cv_acc))
}

sd_acc = c()

for (ki in k_seq){
  sd_acc = c(sd_acc, bias_var(ki))
}

plot(k_seq, sd_acc, type="l", lwd=2, col="blue",
     main="St.Dev of Group Accuracies for K", ylab="",
     xlab = "k", ylim = c(0,0.125))
