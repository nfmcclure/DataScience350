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

if (weather_file_name %in% list.files()){
  weather_data = read.csv(weather_file_name, stringsAsFactors = FALSE)
  names(weather_data) = c('time','temp','dew_pt','humidity','pressure',
                          'visibility','wind_dir','wind_speed','gust_speed',
                          'precipitation','events','conditions',
                          'wind_dir_deg','date')
} else {
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
}

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
weather_data$DateFormat = as.Date(weather_data$DateFormat, format="%Y-%m-%d")


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
#     use the command 'complete.cases':
# Use 'complete.cases()' as a row filter on your data frame.
headcount_base_all$datetime = NULL
headcount_base_all = headcount_base_all[complete.cases(headcount_base_all),]

# can also use na.omit()

headcount_dplyr_all = na.omit(headcount_dplyr_all)
headcount_dt_all = na.omit(headcount_dt_all)

# Know that na.omit does not subselect columns though.  It is easier to do:
#  df = df[complete.cases(df[,c(2,4,6)]),]
# The above preserves columns, but removew rows where columns 2, 4, or 6 are empty.

# Look at average headcount for each condition
avg_hc_by_condition = aggregate(HeadCount ~ conditions, data = headcount_base_all, FUN = mean)
barplot(avg_hc_by_condition[,2], names.arg=avg_hc_by_condition[,1], 
        main="Avg HeadCount By Condition", xlab="Weather Conditions", ylab="Mean Guests per Hour")
# Not entirely too helpful, because headcount is seasonal, and so is weather.

# Control for Seasons?
headcount_base_all$month = as.numeric(format(headcount_base_all$DateFormat,format="%m"))
headcount_base_all$season = 1
headcount_base_all$season[headcount_base_all$month%in%4:6]=2
headcount_base_all$season[headcount_base_all$month%in%7:9]=3
headcount_base_all$season[headcount_base_all$month%in%10:12]=4

avg_hc_by_cond_season = aggregate(HeadCount ~ conditions + season, data = headcount_base_all, FUN = mean)
library(ggplot2)

ggplot(avg_hc_by_cond_season, aes(factor(season), HeadCount, fill= conditions)) +
  geom_bar(stat="identity", position = "dodge")
# Not too much going on.
# Might not get rid of all the seasonality though (month-to-month effects)


# Temperature:
# Going to use data.table here, because it's just easier
headcount_dt_all$month = as.numeric(format(headcount_dt_all$DateFormat,format="%m"))
headcount_dt_all$season = 1
headcount_dt_all$season[headcount_dt_all$month%in%4:6]=2
headcount_dt_all$season[headcount_dt_all$month%in%7:9]=3
headcount_dt_all$season[headcount_dt_all$month%in%10:12]=4

daily_maxtemp_hc = headcount_dt_all[,list("max_daily_temp"=max(temp),
                                          "total_hc"=sum(HeadCount)), by=DayNumber]

plot(daily_maxtemp_hc$max_daily_temp, daily_maxtemp_hc$total_hc, pch=16,
     main="Total HC vs Max Temp", xlab="Max Daily Temp", ylab="Total Headcount")
grid()

#Plot with Seasonality
daily_maxtemp_hc_season = headcount_dt_all[,list("max_daily_temp"=max(temp),
                                                 "total_hc"=sum(HeadCount)), by=c("DayNumber","season")]
plot(daily_maxtemp_hc_season$max_daily_temp[daily_maxtemp_hc_season$season==1],
     daily_maxtemp_hc_season$total_hc[daily_maxtemp_hc_season$season==1], pch=16, col="blue",
     main="Total HC vs Max Temp", xlab="Max Daily Temp", ylab="Total Headcount",
     ylim=c(1500,7000), xlim=c(40,120))
points(daily_maxtemp_hc_season$max_daily_temp[daily_maxtemp_hc_season$season==2],
       daily_maxtemp_hc_season$total_hc[daily_maxtemp_hc_season$season==2], pch=16, col="green")
points(daily_maxtemp_hc_season$max_daily_temp[daily_maxtemp_hc_season$season==3],
       daily_maxtemp_hc_season$total_hc[daily_maxtemp_hc_season$season==3], pch=16, col="red")
points(daily_maxtemp_hc_season$max_daily_temp[daily_maxtemp_hc_season$season==4],
       daily_maxtemp_hc_season$total_hc[daily_maxtemp_hc_season$season==4], pch=16, col="brown")
grid()
# Not too much going on here either, Maybe something to the effect of Fall/Winter have more highs and lows
# Very high and very lows might have a small detrimental effect?
# Maybe the max temps above 80 have a more consistent headcount?  or is it just spring/summer?
