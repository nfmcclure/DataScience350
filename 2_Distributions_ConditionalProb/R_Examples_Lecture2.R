##--------------------------------------------
##
## R code (lecture 2)
##
## Class: PCE Data Science Methods Class
##
##--------------------------------------------

# Bernoulli (Binomial with n = 1)
p = 0.75
n = 1000
bern_samples = rbinom(n, 1, p)
bern_sample_mean = sum(bern_samples)/length(bern_samples)
bern_sample_var = bern_sample_mean * (1-bern_sample_mean)
bern_var = p*(1-p)

# Binomial
N = c(5, 25, 75)
binom_samples = lapply(N, function(x) rbinom(n, x, p))
binom_sample_means = lapply(binom_samples, mean)
binom_means = N*p
binom_sample_vars = lapply(binom_samples, var)
binom_vars = N*p*(1-p)

# Compare Normal Approximation to binomial
par(mfrow=c(2,2))
for (i in 1:3){
  hist(binom_samples[[i]], main=paste(N[i],'Experiments'), freq=FALSE)
  x_norm = seq(0,N[i], by = 0.025)
  y_norm = dnorm(x_norm, mean=binom_means[i], sd=sqrt(binom_vars[i]))
  lines(x_norm, y_norm)
}

#Normal approximation to p(x=4)
pnorm(4.5, mean=binom_means[[1]], sd=sqrt(binom_vars[[1]])) -
  pnorm(3.5, mean=binom_means[[1]], sd=sqrt(binom_vars[[1]]))
#Empirical p(x=4)
sum(binom_samples[[1]]==4)/1000

#Normal approximation to p(x=18)
pnorm(18.5, mean=binom_means[[2]], sd=sqrt(binom_vars[[2]])) -
  pnorm(17.5, mean=binom_means[[2]], sd=sqrt(binom_vars[[2]]))
#Empirical p(x=18)
sum(binom_samples[[2]]==18)/1000

#Normal approximation to p(x=55)
pnorm(55.5, mean=binom_means[[3]], sd=sqrt(binom_vars[[3]])) -
  pnorm(54.5, mean=binom_means[[3]], sd=sqrt(binom_vars[[3]]))
#Empirical p(x=18)
sum(binom_samples[[3]]==55)/1000

# Poisson Distribution
lambda = c(1,5,25,100)
poisson_samples = lapply(lambda, function(x) rpois(n, x))
poisson_sample_means = lapply(poisson_samples, mean)
poisson_sample_vars = lapply(poisson_samples, var)

#Normal approximation
par(mfrow=c(2,2))
for (i in 1:4){
  hist(poisson_samples[[i]], main=paste('Lambda=',lambda[i]), freq=FALSE)
  x_norm = seq(0,10*lambda[i], by = 0.025)
  y_norm = dnorm(x_norm, mean=lambda[i], sd=sqrt(lambda[i]))
  lines(x_norm, y_norm)
}

# The Uniform Distribution
uniform_samples = runif(n)
par(mfrow=c(1,1))
plot(density(uniform_samples))
lines(c(0,0),c(0,1))
lines(c(0,1),c(1,1))
lines(c(1,1),c(0,1))

# The Normal Distribution
n_params = list(c(0,1),c(5,1),c(0,0.1),c(4,4))
norm_samples = lapply(n_params,function(x) rnorm(n,mean=x[1], sd=sqrt(x[2])))

par(mfrow=c(1,1))
colors = c('black', 'red', 'blue', 'green')
plot(density(norm_samples[[1]]), xlim=c(-3,9),ylim=c(0,1.25),
     col=colors[1], lwd=2, xlab='x', ylab='y', main='Plot of Normals')
for (i in 2:4){
  lines(density(norm_samples[[i]]), col=colors[i], lwd=2)
}
grid()
legend('topright',c('N(0,1)','N(5,1)','N(0,0.1)','N(4,4)'),
       lwd=2,lty=1,col=colors)

##------Use of system.time------
# system.time() can be used to estimate time costs

# VERY helpful for running simulations

system.time(sapply(1:1000, function(x){
  five_cards = sample(1:nrow(deck),5)
  return(length(unique(deck$suits[five_cards]))==1)
}))
# 0.08 on my system for 1000, so 1 million would take ~ 1000*0.08 = 80 seconds,
#    but 100K would take 8 seconds.

# For better system times, use the package 'microbenchmark':
# Careful! this essentially does system.time(rep(1000, f() ))
library(microbenchmark)
microbenchmark(sapply(1:1, function(x){
  five_cards = sample(1:nrow(deck),5)
  return(length(unique(deck$suits[five_cards]))==1)
}))

# ~ 100 microseconds on avg. = 100E-6 = 1E-4.  1 Million = 100 seconds.


##----Missing Data Demo with Amelia-----
# NA vs. NaN vs. NULL
#
# NA: Not Available, results from missing data or an
#       out of bounds reference (Logical, niether T or F)
#
# NaN:  Not A Number: results from performing an illegal
#         mathematical action. (Numeric placeholder)
#
# NULL: This is operational.  R returns this when referencing
#         non-existent columns.  R also uses a NULL assignment
#         to remove objects. (NULL Class).
#       If you've had set theory before, think of this as the
#         'empty-set'
#

# NaNs:
0/0
sqrt(-1)
log(-1)
asin(2)
NaN > 1 # NA because NaN is an unknown number
sum(c(1,2,NaN))
sum(c(1,2,NaN), na.rm=TRUE)  # !!! NaN isn't a type of NA, but
                             #     na.rm is very general

#NAs:
c(1,2)[3]          # third argument missing
as.numeric('data') # R is 'missing' the number available
NA > 1
sum(c(1,2,NA))
sum(c(1,2,NA), na.rm=TRUE)

# NULLs:
t = data.frame('a'=1:4, 'b'=runif(4))
t$c
t$a = NULL
t
s = c(1,2,NULL)
s

# NA vs. NaN vs. NULL
class(NA)
class(NaN)
class(NULL)

library(Amelia)

# To illustrate the helpfulness of Multiple Imputation,
#   we will test this out on a contrived data set
n = 1000

full_data = data.frame('A'=runif(n),
                       'B'=rnorm(n),
                       'C'=rpois(n, 5))
# Note:
# true mean of A = 0.5
# true mean of B = 0
# true mean of C = 5
sample_means = apply(full_data, 2, mean)
sample_sds = apply(full_data, 2, sd)

# Remove some data:
data = full_data
data$A[sample(1:n, round(n*0.05))] = NA
data$B[sample(1:n, round(n*0.15))] = NA
data$C[sample(1:n, round(n*0.5))] = NA

# Removal of missing data (by entry only)
#   Note: This is only really applicable because our statistic is calculated
#          on each row separately.
means_rem_entry = apply(data, 2, function(x) mean(x, na.rm=TRUE))
sd_rem_entry = apply(data, 2, function(x) sd(x, na.rm=TRUE))

# Removal of missing data (by row)
means_rem_rows = apply(data[complete.cases(data),], 2, mean)
sd_rem_rows = apply(data[complete.cases(data),], 2, sd)

amelia_data = amelia(data)[1]$imputations # Amelia spits out WAY too much information.

# Calculate samples means
imp_means = lapply(amelia_data, function(x) apply(x,2,function(y) mean(y, na.rm=TRUE)))
avg_imp_means = apply( do.call(rbind, imp_means), 2, function(y) mean(y, na.rm=TRUE))

# Calculate samples sds
imp_sds = lapply(amelia_data, function(x) apply(x,2,function(y) sd(y, na.rm=TRUE)))
avg_imp_sds = apply(do.call(rbind, imp_means), 2,function(y) sd(y, na.rm=TRUE))


##-----Getting/Storing Data-----

# csv files
?read.csv # Note the option stringsAsFactors = FALSE

# txt files
?read.table

# web/html
# See previous class weather_retrieval.R from previous class
?readLines


# API
# Twitter Example
##-----Twitter Text Mining-----
setwd('E:/Work/Teaching/PCE_Data_Science/2_Distributions_ConditionalProb')

##-----Oath Setup-----
twit_cred = read.csv('twitter_cred.csv', stringsAsFactors=FALSE)

TWITTER_CONSUMER_KEY = twit_cred$TWITTER_CONSUMER_KEY
TWITTER_CONSUMER_SECRET = twit_cred$TWITTER_CONSUMER_SECRET
TWITTER_ACCESS_TOKEN = twit_cred$TWITTER_ACCESS_TOKEN
TWITTER_ACCESS_SECRET = twit_cred$TWITTER_ACCESS_SECRET

library(twitteR)
library(httpuv)
setup_twitter_oauth(TWITTER_CONSUMER_KEY, TWITTER_CONSUMER_SECRET)


##----Retrieve Info From Twitter----
s <- searchTwitter('#datascience')
?searchTwitter

##-----Data Output Examples-----

# Write/Create RSQLite DB
library(sqldf)
db_file = "test.db"

conn = dbConnect(RSQLite::SQLite(), dbname=db_file)
dbSendQuery(conn = conn,paste("CREATE TABLE test (indexer INTEGER,",
                            "entry1 TEXT,entry2 TEXT)"))
dbSendQuery(conn = conn,"INSERT INTO test VALUES (1, 'joseph', 'fourier')")
dbSendQuery(conn = conn,"INSERT INTO test VALUES (2, 'leonhard', 'euler')")

dbListTables(conn)            # The tables in the database
dbListFields(conn, "test")    # The columns in a table
dbReadTable(conn, "test")     # The data in a table


# Enter data from csv:
# dbWriteTable(conn = conn, name = "test", value = "mathematicians.csv")

# Enter data from dataframe:
mathematicians = data.frame('indexer'=3:6,
                            'entry1'=c('william', 'emmy', 'srinivasa', 'alfred'),
                            'entry2'=c('hamilton', 'noether', 'ramanujan', 'lotka'))
dbWriteTable(conn = conn, name = "test", value = mathematicians, append=TRUE, overwrite=FALSE)
