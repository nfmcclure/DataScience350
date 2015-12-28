##--------------------------------------------
##
## Sample Production Code For Exploring
##   JitteredHeadCount.csv
##
## Class: PCE Data Science Methods Class
##
## Creator: Nick McClure (nickmc@uw.edu)
##
##--------------------------------------------

##-----Load Libraries-----
require(logging)

# Declare the loading data function
load_data = function(datafile, logger=NA){
  data = read.csv(datafile, stringsAsFactors=FALSE)
  loginfo("Loaded Data.", logger="data_logger")
  
  # Check if any data was loaded
  if(nrow(data)==0){
    logwarn("No Data Loaded", logger="data_logger")
  }
  return(data)
}

# Declare the moving average function
moving_average = function(data_series, num_trailing){
  mov_avg = sapply(1:length(data_series), function(x){
    if(x<num_trailing){ # Special case in begining (when we data doesn't exist far back enough)
      mean(data_series[x:1], na.rm=TRUE)
    }else{ # Else average last n cases
      mean(data_series[x:(x-num_trailing)], na.rm=TRUE)
    }
  })
  return(mov_avg)
}


if (interactive()){
  # Set logging information
  basicConfig()
  addHandler(writeToFile, logger="data_logger", file="file_log.log")
  
  # Set working director and load data
  loginfo("Setting wd and loading data.", logger="data_logger")
  setwd('E:/Work/Teaching/PCE_Data_Science/1_Intro_Lecture')
  datafile = 'JitteredHeadCount.csv'
  data = load_data(datafile)
  
  # Change date to R's Date
  data$DateFormat = as.Date(data$DateFormat, format="%m/%d/%Y")
  
  # Weekends (Fri, Sat, and Sun) are Day # 5,6,7
  weekend = 5:7
  week = 1:4
  
  # Look at total headcount per day
  data_aggregate_day = aggregate(HeadCount ~ DayOfWeek + DayNumber, data, FUN=sum)
  data_aggregate_day = data_aggregate_day[order(data_aggregate_day$DayNumber),]
  # calculate moving average of last N days
  N = 10
  headcount_total_mov_average = moving_average(data_aggregate_day$HeadCount, 21)
  plot(headcount_total_mov_average, type='l', main="Trailing moving average of 10 days",
       xlab="Day of Year", ylab="Total Guests")
  
  # Look at average headcount per day
  data_mean_day = aggregate(HeadCount ~ DayNumber + DayOfWeek, data, FUN=mean)
  
  # Look at average headcount per day of week
  data_mean_dayofweek = aggregate(HeadCount ~ DayOfWeek, data, FUN=mean)
  boxplot(t(data_mean_dayofweek), main="Avg Headcount Per Table on Each Day of Week")
  
  # Calculate average weekend vs. week headcount, but recalculate becuase we
  #   don't want to take averages of averages
  avg_weekend_hc = mean(data$HeadCount[data$DayOfWeek%in%weekend])
  avg_week_hc = mean(data$HeadCount[data$DayOfWeek%in%week])
  
  loginfo((paste('The avg weekend headcount/table is', round(avg_weekend_hc,3),
                      '.  The avg week headcount/table is', round(avg_week_hc,3),'.')),
          logger="data_logger")
  
  # Look at headcount per tables occupied?
  
}