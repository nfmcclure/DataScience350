##--------------------------------------------
##
## R Review Homework Headstart
##
## Class: PCE Data Science Methods Class
##
##--------------------------------------------

##-----Set working directory-----
setwd('E:/Work/Teaching/PCE_Data_Science/1_Intro_Lecture/')

##-----Load Libraries-----
library(dplyr)
library(data.table)

source('weather_retrieval.R')

# Load jittered Data
headcount = read.csv('JitteredHeadCount.csv', stringsAsFactors = FALSE)
headcount$DateFormat = as.Date(headcount$DateFormat, format="%m/%d/%Y")

weather_file_name = 'las_vegas_hourly_weather.csv'

# Let's test if the file is in the directry using list.files()
#        If it is, load that file instead of running webscraper
# <<INSERT CODE HERE>>>

# <<CHANGE BELOW CODE BASE ON ABOVE CODE>>>
# If it isn't, run webscraper:
# Look at dates:
range(headcount$DateFormat)

airport = 'KLAS'
dates = seq(from=min(headcount$DateFormat),
            to=max(headcount$DateFormat),
            by=1)
weather_data = get_weather_data(airport, dates)
names(weather_data) = c('time','temp','dew_pt','humidity','pressure',
                        'visibility','wind_dir','wind_speed','gust_speed',
                        'precipitation','events','conditions',
                        'wind_dir_deg','date')

# Let's create a datetime in the weather data
weather_data$datetime = paste(weather_data$date,weather_data$time)
weather_data$datetime = strptime(weather_data$datetime, format="%Y-%m-%d %I:%M %p")
weather_data$Hour = as.numeric(format(round(weather_data$datetime, units="hours"), format="%H"))


# Let's merge with different methods.
#   - If we were truly merging to analyze casino data, we don't want to lose
#      headcount data if weather doesn't exist, so we want to do a
#      left merge (keeping all the headcount data)
#
#   - Remember, we want to merge on date AND hour.
#   - Note: the headcount data has multiple rows for these (more than 1 game type)

# Check for duplicates!
anyDuplicated(headcount[c("DateFormat", "Hour","GameCode")])

anyDuplicated(weather_data[c("date", 'Hour')]) # Oh no!  How could this happen?

# Drop for now:
weather_data = weather_data[!duplicated(weather_data[c("date", 'Hour')]),]

# Rename some columns:
intersect(names(headcount), names(weather_data))
weather_data$DateFormat = weather_data$date
weather_data$date = NULL


# Pick one of the below merges, and comment out the other two.
# <<<CHANGE BELOW MERGING CODE>>>

# Merge (base)
headcount_base_all = merge(headcount, weather_data, all.x=TRUE, by=c("DateFormat","Hour"))


# Merge(data.table)
# Note that data.table has a problem.  It canNOT take POSIX values. So we drop it (we are done with that column anyways)
weather_data$datetime = NULL
library(data.table)
headcount = as.data.table(headcount)
weather_data = as.data.table(weather_data)

# Set keys for faster merges
setkeyv(headcount, c("DateFormat", "Hour"))
setkeyv(weather_data, c("DateFormat", "Hour"))

headcount_dt_all = merge(headcount, weather_data, all.x=TRUE, by=c("DateFormat", "Hour"))

# Merge(dplyr)
library(dplyr)
headcount_dplyr_all = left_join(headcount, weather_data, by=c("DateFormat", "Hour"))


##----Find another insight involving weather------

# For now, drop all NA rows:
#     use the command 'complete.cases'
# Use 'complete.cases()' as a row filter on your data frame.

# <<<INSERT NEW CODE HERE>>>
