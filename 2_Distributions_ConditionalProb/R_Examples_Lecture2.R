##--------------------------------------------
##
## Counting/Probability R code (lecture 2)
##
## Class: PCE Data Science Methods Class
##
## Contains examples of:
##
## -Counting, Probability
##
## -More on Distributions
##
##--------------------------------------------

library(MASS) # has the function 'fractions()', which is useful.

##-----Sandwich Count----
breads = c('white', 'wheat', 'italian', 'sevengrain')
meats = c('ham', 'turkey', 'chicken', 'pastrami', 'meatballs')
toppings = c('mustard', 'mayo', 'salt_pepper', 'oil_vinegar')

sandwiches = expand.grid(breads,
                         meats,
                         toppings)

##-----Two Dice------
two_dice = expand.grid(1:6,1:6)
two_dice$sum = two_dice$Var1 + two_dice$Var2
two_dice$isdouble = two_dice$Var1 == two_dice$Var2

# Count different sums
sum_counts = table(two_dice$sum)

# Count doubles
doubles = sum(two_dice$isdouble)

# Probabilities of sums:
sum_prob = fractions(table(two_dice$sum)/nrow(two_dice)) # type ?fractions for more detail
barplot(sum_prob)

# Probability of a double:
fractions(doubles/nrow(two_dice))

##-------Simulations in R------
# Define deck
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
ranks <- c("Ace", "Deuce", "Three", "Four","Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(ranks=ranks, suits=suits)

# Find probability that 5 cards make up a flush from simulations
n = 100000 # stay under 1 million
hands = sapply(1:n, function(x){
  five_cards = sample(1:nrow(deck),5)
  return(length(unique(deck$suits[five_cards]))==1)
})

emp_prob = sum(hands)/n
emp_var = var(hands)

##------Use of system.time------
# system.time() can be used to estimate time costs

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
#         to remove objects. (NULL Class)
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
