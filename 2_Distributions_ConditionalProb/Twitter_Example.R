##-----Twitter Text Mining-----
library(twitteR)

setwd('E:/Work/Teaching/PCE_Data_Science/2_Distributions_ConditionalProb')

##-----Oath Setup-----
twit_cred = read.csv('twitter_cred.csv', stringsAsFactors=FALSE)

TWITTER_CONSUMER_KEY = twit_cred$TWITTER_CONSUMER_KEY
TWITTER_CONSUMER_SECRET = twit_cred$TWITTER_CONSUMER_SECRET

TWITTER_ACCESS_TOKEN = twit_cred$TWITTER_ACCESS_TOKEN
TWITTER_ACCESS_SECRET = twit_cred$TWITTER_ACCESS_SECRET

setup_twitter_oauth(TWITTER_CONSUMER_KEY, TWITTER_CONSUMER_SECRET,
                    TWITTER_ACCESS_TOKEN, TWITTER_ACCESS_SECRET)

##----Retrieve Info From Twitter----

s <- searchTwitter('#datascience', locale=NULL, geocode=NULL,)