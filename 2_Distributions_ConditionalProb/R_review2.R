##--------------------------------------------
##
## R Coding Review
##
## Class: PCE Data Science Methods Class
##
## Creator: Nick McClure (nickmc@uw.edu)
##
##--------------------------------------------
getwd()
setwd('E:/Work/Teaching/PCE_Data_Science/1_Intro_Lecture')

##-----Functions------
headcount = read.csv('JitteredHeadCount.csv', stringsAsFactors=FALSE)  # Read in headcount file
str(headcount) # Look at structure of data

# Convert dates:
headcount$DateFormat = as.Date(headcount$DateFormat, format = "%m/%d/%Y")

# See
?strptime # for representations of dates/datetimes

# The purpose of functions are to:
#    1.  Make code readable.
#    2.  Make code reusable.  Do NOT copy and paste code! (evil stare from Linus Torvalds)

# Let's make a season computation:
#
# 1 = Winter: months 1-3
# 2 = Spring: months 4-6
# 3 = Summer: months 7-9
# 4 = Fall:   months 10-12
seasons = c('winter', 'spring', 'summer', 'fall') # we will use this later

# Ugly way:
season_vector = c() # Initialization
for (row in 1:nrow(headcount)){ # Loop through rows
  month_temp_string = format(headcount$DateFormat[row], format="%m")
  month_temp = as.numeric(month_temp_string)
  
  if ( month_temp %in% (1:3) ){                # if month is in 1,2,3 then winter
    season_vector = c(season_vector,'winter')  #    then add 'winter' to vector
  }else if ( month_temp %in% (4:6) ){          # if month is in 4,5,6 then spring
    season_vector = c(season_vector,'spring')  #    then add 'spring' to vector...
  }else if ( month_temp %in% (4:6) ){
    season_vector = c(season_vector,'summer')
  }else{
    season_vector = c(season_vector,'fall')
  }
  
}# End row loop (row)
headcount$season_ugly = season_vector # Assign outcome to dataframe

# Put this in a function:
get_season_from_date = function(date_value){
  month_temp = as.numeric(format(date_value, format="%m"))
  if ( month_temp %in% (1:3) ){
    season = 'winter'
  }else if ( month_temp %in% (4:6) ){
    season = 'spring'
  }else if ( month_temp %in% (7:9) ){
    season = 'summer'
  }else{
    season = 'fall'
  }
  return(season)
}

# Our for loop now becomes:
season_vector = c() # Initialization
for (row in 1:nrow(headcount)){ # Loop through rows
  season = get_season_from_date(headcount$DateFormat[row])
  season_vector = c(season_vector, season)
}

# clean up
rm(season_vector)   # Remove stuff we don't need
gc()                # Force R to clean up the memory
invisible(gc())     # Don't really care about memory stats at this level
invisible(gc());invisible(gc());invisible(gc());invisible(gc());invisible(gc());

# Better, but still so slow!  Let's get rid of the for loop entirely
headcount$season_fast = sapply(headcount$DateFormat, function(x) get_season_from_date(x))
# or better yet:
headcount$season_fast = sapply(headcount$DateFormat, get_season_from_date)

# faster way:
headcount$month = as.numeric(format(headcount$DateFormat, format="%m")) # Set a month integer
headcount$season_faster = ''  # Initialize
headcount$season_faster[headcount$month %in% (1:3)] = 'Winter'
headcount$season_faster[headcount$month %in% (4:6)] = 'Spring'
headcount$season_faster[headcount$month %in% (7:9)] = 'Summer'
headcount$season_faster[headcount$month %in% (10:12)] = 'Fall'

# Let's see if we can speed it up
library(data.table)
headcount = as.data.table(headcount) # Make headcount a data.table

# Here is a trick:
#   Note the following:
(1:12)  # A sequence of months, 1 to 12
ceiling((1:12)/3) # Converted to an integer, 1 = Winter, ... 4 = Fall.
seasons[ceiling((1:12)/3)] # Use these integers as indices in our seasons vector

# Now stick the above trick in data.table format:
# headcount = headcount[ ,newcolumn := <formula for new column here>, ]
headcount = headcount[ ,season_fastest := seasons[ceiling(as.numeric(format(DateFormat, format="%m"))/3)], ]

##-----Loading/Reading data-----

?read.table  # read.table is the way R reads all files.

?readLines  # One way to access html connections
stack_html = readLines('http://www.stackoverflow.com')

# Find all the lines that contain the phrase " vote"
lines_with_vote = grep(" vote" ,stack_html)

# Look at top few lines with 'vote'
stack_html[head(lines_with_vote)]

# Look at end lines with 'vote'
stack_html[tail(lines_with_vote)]

# Drop first four
lines_with_vote = lines_with_vote[5:length(lines_with_vote)] # Careful, only run this line once

# Find position where 'vote' starts
vote_vector = c()
for (x in lines_with_vote){
  number_position = gregexpr(' vote',stack_html[x])[[1]][1]  # Find where in the line the word 'vote' appears
  number_string = substring(stack_html[x], number_position-1, number_position) # Find the number before it
  votes = as.numeric(number_string)   # Convert strings to numbers
  vote_vector = c(vote_vector,votes)  # Attach number to vector
}
hist(vote_vector)

##------Apply Functions-----

# That forloop is ugly. Let's write an sapply loop (simplified lapply)
vote_vector = sapply(lines_with_vote, function(x){
  number_position = gregexpr(' vote',stack_html[x])[[1]][1]  # Find where in the line the word 'vote' appears
  number_string = substring(stack_html[x], number_position-1, number_position) # Find the number before it
  return(as.numeric(number_string))   # Convert strings to numbers
})

# Still ugly, let's condense it with functions
get_vote_number = function(line_num){
  number_position = gregexpr(' vote',stack_html[line_num])[[1]][1]  # Find where in the line the word 'vote' appears
  number_string = substring(stack_html[line_num], number_position-1, number_position) # Find the number before it
  return(as.numeric(number_string))   # Convert strings to numbers
}

# Much more readable:
vote_vector = sapply(lines_with_vote, function(x) get_vote_number(x))
# or better yet:
vote_vector = sapply(lines_with_vote, get_vote_number)

##----Logging----
library(logging)
?logging  # This command will show you how to set it up
basicConfig() # Use default logging configuration
addHandler(writeToFile, logger="logger_name", file="test.log") # Add a logging function handler

# When logging, be sure to specify type of log and logger name
loginfo("This is an informative statement.", logger="logger_name")
logwarn("This is a warning!", logger="logger_name")
logerror("ERROR!!", logger="logger_name")

##-----SQLite Access-----
library(RSQLite)
# Name of the db
db_name = 'test.db'

# Create the connection
db_conn = dbConnect(dbDriver("SQLite"), db_name)

# Create a dataframe
stack_frame = data.frame("votes"=vote_vector)

# Write dataframe to a table
dbWriteTable(db_conn,"stack_votes", stack_frame)

# Query database
query = 'SELECT * FROM stack_votes WHERE votes > 0'
non_zero_votes = dbSendQuery(db_conn, query)

# Have to fetch the results for storage in R memory
non_zero_votes = fetch(non_zero_votes)

# Disconnect, because we have clean R code.
dbDisconnect(db_conn)


##----Try/Catch Format----
#
#  The tryCatch function is extremely helpful and vastly underused.
#  It is similar to python's try/except format. It works like the following:
#
#  result = tryCatch({
#                              line1 executes
#                              line2 executes
#                     },
#                     error=function(error_condition){
#                               message('Error line 1 information here')
#                               message(error_condition)
#                     },
#                     warning=function(warning_condition){
#                               message('Warning line 1 information here')
#                               message(warning_condition)
#                     },
#                     finally={
#                               Always execute these commands.
#                     }
#                   )
#
# tryCatch is especially helpful when attempting to read webpages:
#
html_code = function(url){
  html = tryCatch(
    {
      suppressWarnings(readLines(url))
    },
    error=function(error_condition){ # executes only if there is an error in the block above
      message('There was an error reading your url.') # generates a diagnostic looking print statement
      message(paste('Here is the error:',error_condition))
    },
    warning=function(warning_condition){# Executes only if there was a warning in the block above
      message('Warning!')
      message(warning_condition)
    },
    finally={ # Executes always, no matter the outcome
      message('\n All Done!')
    }
  )
  return(html)
}

html_code('http://www.thiswebsitedoesntexist.com/123123123')
html_code('http://www.york.ac.uk/teaching/cws/wws/webpage1.html')

##----interactive()-----
?interactive

if (interactive()){
  # Put commands here
}

##----P/D/Q/R Distribution Functions-----
# R has 20 different distributions:
# http://www.stat.umn.edu/geyer/old/5101/rlook.html
#
# D - (D)ensity of the probability function
# P - (P)robability of being less than X
# Q - (Q)uantile (inverse of P)
# R - (R)andom number
#

# Normal
x = seq(-4,4,length=500)
y_density = dnorm(x, mean=0, sd=1) # Same as dnorm(x)
plot(x, y_density, type='l', ylim=c(0,1))
grid()

y_cdf = pnorm(x, mean=0, sd=1)
lines(x, y_cdf, col='red', lwd=2)

pnorm(0)
qnorm(0.5)

