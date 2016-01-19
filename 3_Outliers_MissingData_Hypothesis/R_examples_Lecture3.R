##--------------------------------------------
##
## Counting/Probability R code (lecture 3)
##
## Class: PCE Data Science Methods Class
##
## Contains examples of:
##
## -Sampling, hypothesis testing and outliers
##
##--------------------------------------------

data = data.frame('index' = 1:100,
                  'var1' = rnorm(100),
                  'group' = sample(1:3, 100, replace=TRUE, prob=c(0.1,0.45,0.45)))

##----Bernoulli Sampling-----
p = 0.1
bernoulli_sample = data[runif(100)<p,] # Yay for R vectorization

##----Cluster Sampling-----
num_clusters = 10
cluster_samples = lapply(1:num_clusters, function(x){
  stopifnot((nrow(data) %% num_clusters)==0)
  cluster_labels = sample(rep(1:num_clusters,each=num_clusters))
  return(data[cluster_labels == x,])
})

##----Simple Random Sample-----
size = 15
simple_random_sample = data[sample(1:nrow(data), size),]

##-----Stratified Sampling-----
split_data = split(data, list(data$group))
strat_samples_list <- lapply(split_data, function(x) x[sample(1:nrow(x), 5, FALSE),])
strat_sample <- do.call(rbind, strat_samples_list)

##------Systematic Sampling-----
k = 5 # 100/5 = 20 observations
sys_sample_even = data[seq(1,nrow(data), by = k),]

k = 6 # 100/6 = 16.67 obs? hrm... do this:
# Pick a random start point between 1 and k:
start = sample(1:k,1)
#Sample every k:
sys_sample = data[seq(start,nrow(data), by=k),]


##-----Law of Large Numbers----
p_six = 1/6

# roll a dice 60 times, find p(x=10)
dbinom(x=10, size=60, prob=p_six)
# roll a dice 600 times, find p(x=100)
dbinom(x=100, size=600, prob=p_six)

# Probability of within 5%?
# 1) p(7<x<13|60 trails)
pbinom(12, size=60, prob=p_six) - pbinom(7, size=60, prob=p_six)
# alternatively
sum(sapply(8:12, function(x) dbinom(x, size=60, prob=p_six)))

# 2) p(70<x<130|600 trails)
pbinom(129, size=600, prob=p_six) - pbinom(70, size=600, prob=p_six)
# alternatively
sum(sapply(71:129, function(x) dbinom(x, size=600, prob=p_six)))

# View Distributions:
x_60 = 1:60
y_60 = dbinom(x_60, size=60, prob=p_six)

x_600 = 1:150
y_600 = dbinom(x_600, size=600, prob=p_six)

plot(x_60, y_60, type='l', main='Roll a Die 60 or 600 Times', xlab="# of Successes",
     ylab="Probability", lwd=2, col="green", xlim=c(1,150))
lines(x_600, y_600, lwd=2, col="blue")
legend("topright", c("Roll 60 Times", "Roll 600 Times"), col=c("green", "blue"),
      lty=c(1,1), lwd=c(2,2))

##----Coin Flips-----

# Calculate a running average of N-trials of flipping a fair coin
n = 10000

outcomes = round(runif(n))

running_average = sapply(1:n, function(x) mean(outcomes[1:x]))

plot(running_average, type='l')
grid()

outcomes_sd = sd(outcomes)
outcomes_sd_theo = sqrt( 0.5 * (1 - 0.5) )

# You can imagine that there is a inherent variation in the population,
#  this is the standard deviation we have found above.

# But also, there is some variation in finding the mean of the outcomes.
#  The more coin flips, the closer the mean will be to 0.5... see the plot of
#  the running average above.  This plot scales with 1/sqrt(n).

##----Prob between two points on a Normal-----

prob_normal = function(a, b, mean=0, sd=1){
  stopifnot(a<=b) # Similar to an assert
  return(pnorm(b,mean,sd) - pnorm(a,mean,sd))
}

#One tailed
prob_normal(20.1262055, Inf, 15, 4) # 10% of the area lies to the left of 20.1262055 on N(15,4)

prob_normal(-Inf,Inf)
prob_normal(-1,1)
prob_normal(-2,2)
prob_normal(-3,3)


##----Cutoff function on a Normal----

# If we want a cutoff area percentage on a normal distribution,
#  we need the total areal to the left (and right if 2-tailed),
#  what x-value gives us that area?

cutoff_stat = function(alpha, mean=0, sd=1, one_tailed=TRUE){
  stopifnot((alpha>0) & (alpha<1))
  if (one_tailed){
    return(qnorm(1-alpha, mean, sd))
  }else{
    return(qnorm(1-(alpha/2), mean, sd))
  }
}

cutoff_stat(0.1, 15, 4)

##----St. Dev. vs. St. Error-----
n = seq(10,10000,len=1000)

sample_means = sapply(n, function(x) mean(rnorm(x)))
sample_sds = sapply(n, function(x) sd(rnorm(x)))

plot(n, sample_means) # Plot means
lines(n, 1/sqrt(n))   # Plot means +- st. error
lines(n, -1/sqrt(n))

plot(n, sample_sds)   # Plot sd's
lines(n, 1/sqrt(n)+1) # plot sd's +- st. error
lines(n, -1/sqrt(n)+1)


##----Chi-squared Test----

ab_data = data.frame('occurrence'=c(55,43,22),
                     'expected_per'=c(0.6,0.3,0.1))
chisq.test(ab_data$occurrence, p = ab_data$expected_per)
1-pchisq(13.708, df=2) # 13.708 from slides



##-----Fisher's Exact in R-----

mat_test = matrix(c(2,3,3,4), nrow=2, byrow=TRUE)
fisher.test(mat_test, alternative = "less")

# Manual
fisher_outcome_p = function(a,b,c,d){
  n = choose(a+c,a)*choose(b+d,b)
  d = choose(a+b+c+d, a+b)
  return(n/d)
}

# One exact outcome:
do.call(fisher_outcome_p, as.list(c(mat_test)))

# or
all_outcomes = list(c(2,3,3,4),c(1,4,4,3),c(0,5,5,2))
outcome_probs = lapply(all_outcomes, function(x) do.call(fisher_outcome_p, as.list(c(x))))
p = sum(unlist(outcome_probs))

