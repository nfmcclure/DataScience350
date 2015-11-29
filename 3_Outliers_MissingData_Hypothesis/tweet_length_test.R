##--------------------------------------------
##
## Twitter Word Count Test (lecture 3)
##
## Class: PCE Data Science Methods Class
##
##--------------------------------------------

##----Twitter word count hypothesis test-----
#
# Alternative Hypothesis:
#    The hastag #datascience contains longer tweets
#      than #statistics
#
##-----Twitter Text Mining-----
library(twitteR)
library(stringr)
library(devtools)
devtools::install_version("httr", version="0.6.0", repos="http://cran.us.r-project.org")
library(httr)

setwd('E:/Work/Teaching/PCE_Data_Science/3_Outliers_MissingData_Hypothesis')

##-----Oath Setup-----
twit_cred = read.csv('twitter_cred.csv', stringsAsFactors=FALSE)

TWITTER_CONSUMER_KEY = twit_cred$TWITTER_CONSUMER_KEY
TWITTER_CONSUMER_SECRET = twit_cred$TWITTER_CONSUMER_SECRET
TWITTER_ACCESS_TOKEN = twit_cred$TWITTER_ACCESS_TOKEN
TWITTER_ACCESS_SECRET = twit_cred$TWITTER_ACCESS_SECRET

setup_twitter_oauth(TWITTER_CONSUMER_KEY, TWITTER_CONSUMER_SECRET,
                    TWITTER_ACCESS_TOKEN, TWITTER_ACCESS_SECRET)

##----Retrieve Info From Twitter----
ds <- searchTwitter('#datascience', locale=NULL, geocode=NULL,n=500)
s <- searchTwitter('#statistics', locale=NULL, geocode=NULL,n=500)

# remove non-ascii, convert to lowercase, and stick in a dataframe
s = do.call("rbind", lapply(s, function(x) tolower(iconv(x$text, "latin1", "ASCII", sub=""))))
s = data.frame(s)

ds = do.call("rbind", lapply(ds, function(x) tolower(iconv(x$text, "latin1", "ASCII", sub=""))))
ds = data.frame(ds)

# Count occurences of "and"
ds$length = nchar(as.character(ds$ds))
s$length = nchar(as.character(s$s))

# Count of statistics per tweet:
s_mean = mean(s$length)
s_sd = sd(s$length)

ds_mean = mean(ds$length)
ds_sd = sd(ds$length)

# Simple and wrong test:
#  is a mean of s_mean large enough to be considered different?

# Create test statistic:
cutoff_stat = function(alpha, mean=0, sd=1, one_tailed=TRUE){
  stopifnot((alpha>0) & (alpha<1))
  if (one_tailed){
    return(qnorm(1-alpha, mean, sd))
  }else{
    return(qnorm(1-(alpha/2), mean, sd))
  }
}

cutoff_stat(0.05, ds_mean, ds_sd)

# what is the probability of s_mean or more?

prob_normal = function(a, b, mean=0, sd=1){
  stopifnot(a<=b) # Similar to an assert
  return(pnorm(b,mean,sd) - pnorm(a,mean,sd))
}
prob_normal(s_mean, Inf, mean=ds_mean, sd=ds_sd)

##----Create a hypothetical plot----
x = seq(ds_mean-4*ds_sd, ds_mean+4*ds_sd, len=100)
y_ds = dnorm(x, mean=ds_mean, sd=ds_sd)
plot(x, y_ds, type="l")
abline(v=s_mean)


##-----T-test-----
t.test(ds$length, s$length, var.equal=TRUE) # Student's T-Test (of non equality)
t.test(ds$length, s$length) # Welch's T-Test (of non equality)

t.test(ds$length, s$length, var.equal=TRUE, alternative="less") # Student's T-Test (one sided)
t.test(ds$length, s$length, alternative="less") # Welch's T-Test (one sided)

# Another formatting type:
test_vals = c(ds$length, s$length)
test_groups = c(rep(0,nrow(ds)), rep(1,nrow(s))) # Must contain exactly 2 different levels
t.test(test_vals ~ test_groups)
