##--------------------------------------------
##
## Regular Expressions in R
##
## Class: PCE Data Science Methods Class
##
##--------------------------------------------

setwd('E:/Work/Teaching/PCE_Data_Science/')

# For a regex in R guide, type:
?"regular expression"

# R's pattern matching and replacement tools:
?grep

# Create a sample string vector
string_vec = c('i like beer', 'i don\'t like bears', 'double match beerbeer', 'not a match')

# grep - returns the index of matches
string_pattern = 'be'
grep(string_pattern, string_vec, perl=TRUE)

#grepl - returns a vector of logicals indicating a match
grepl(string_pattern, string_vec, perl=TRUE)

#sub - substitute expression for first occurence
sub('i', 'you', string_vec[1], perl=TRUE)

# gsub - global substitute
gsub('i', 'you', string_vec[1], perl=TRUE)

# regexpr - returns a list of information about the first match
regexpr(string_pattern, string_vec, perl=TRUE)

# gregexpr - returns a list of information about all matches
gregexpr(string_pattern, string_vec, perl=TRUE)

# R studio also has regular expression searching in the find/replace bar
#  To see this hit 'Ctrl-F' on windows or select Find under the Edit menu.


##------Twitter Example-----
# Note: The Twitter rate limit is 180 or 300 requests every 15 minutes.
# Use functions like Sys.sleep() to pause your requests
library(stringi)
library(devtools)
devtools::install_version("httr", version="0.6.0", repos="http://cran.us.r-project.org")
library(httr)
library(twitteR)
library(stringr)

##-----Oath Setup-----
twit_cred = read.csv('twitter_cred.csv', stringsAsFactors=FALSE)

TWITTER_CONSUMER_KEY = twit_cred$TWITTER_CONSUMER_KEY
TWITTER_CONSUMER_SECRET = twit_cred$TWITTER_CONSUMER_SECRET
TWITTER_ACCESS_TOKEN = twit_cred$TWITTER_ACCESS_TOKEN
TWITTER_ACCESS_SECRET = twit_cred$TWITTER_ACCESS_SECRET

setup_twitter_oauth(TWITTER_CONSUMER_KEY, TWITTER_CONSUMER_SECRET,
                    TWITTER_ACCESS_TOKEN, TWITTER_ACCESS_SECRET)

# Get up to 100 latest tweets from the whitehouse

wh_tweets <- userTimeline('barackobama', n=100)

pattern = '[Aa]merican?'

wh_tweets = sapply(wh_tweets, function(x) x$text)

grep(pattern, wh_tweets, perl=TRUE)

