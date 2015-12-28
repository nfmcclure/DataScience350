#-----------------------------
#
#  Weather Scraper Function
#
#  Purpose: Get and store data from wunderground
#
#  Created by: Nikola Tesla (ntesla@uw.edu)
#
#  Created on: 2015-05-07
#
#-----------------------------

##----Import Libraries-----
require(RSQLite)
require(logging)

##----Define the get-weather-data function-----
get_weather_data = function(airport, dates, logger=NA, db_conn=NA){
  
  # Build HTML Link String
  site_prefix = 'http://www.wunderground.com/history/airport/'
  site_suffix = '/DailyHistory.html?format=1'
  weather_links = paste0(site_prefix,airport,'/',gsub('-','/',dates),site_suffix)
  
  # Initialize final data frame
  weather_frame = data.frame()
  
  # Get Data
  for (l in weather_links){
    print(paste('Getting link',l))
    # Log each attempt
    if (is.function(logger)){
      loginfo(paste('Getting link',l),logger=logger)
    }
    
    weather_info = tryCatch(readLines(l)[-1], # Get String Response
                            error = function(e){
                              
                              print(paste('Error getting',l)) # Output Error on Screen/Console
                              
                              if(is.function(logger)){loginfo(paste('Error getting',l)
                                                              ,logger=logger)} # Store Error in Log
                              
                            })
    weather_info = strsplit(weather_info,',') # Parse each line by  a comma
    headers = weather_info[[1]]               # Get Headers
    weather_info = weather_info[-1]           # Drop Headers
    
    # Now transform list into data frame
    weather_info = do.call(rbind.data.frame, weather_info)
    names(weather_info) = headers
    
    # Post Retrieval Data Cleanup
    weather_info <- data.frame(lapply(weather_info, as.character),
                               stringsAsFactors=FALSE)
    # Convert numeric columns to numbers
    numeric_cols = c(2,3,4,5,6,8,9,10,13)
    weather_info[numeric_cols] = lapply(weather_info[numeric_cols],as.numeric)
    # Fill in the 'dashes' to zero
    weather_info[is.na(weather_info)]=0
    # Rename the date column and drop the last html tag
    colnames(weather_info)[14]="Date"
    weather_info$Date = as.Date(substr(weather_info$Date,1,10))
    
    # Concatenate DFs together
    weather_frame = tryCatch(rbind(weather_frame, setNames(weather_info, names(weather_frame))),
                             error=function(e) {print(e);weather_frame})
    
    
  } # End loop through each day's weather csv link (l)
  
  # Log ending time
  if(is.function(logger)){
    loginfo('All done!',logger=logger)
  }
  
  # Write to SQLite DB
  if(isS4(db_conn)){
    dbWriteTable(db_conn, airport, weather_frame, overwrite=TRUE)
  }
  
  names(weather_frame) = c('time','temp','dew_pt','humidity','pressure',
                          'visibility','wind_dir','wind_speed','gust_speed',
                          'precipitation','events','conditions',
                          'wind_dir_deg','date')
  
  return(weather_frame)
  
}

if(interactive()){
  ##----Setup Test Logger-----
  basicConfig()
  addHandler(writeToFile, file="~/testing.log", level='DEBUG')  
  
  ##----Test Parameters----
  airport = 'KSEA'
  dates = seq(from=as.Date('2015-05-01'),
              to=as.Date('2015-05-06'),
              by=1)
  sql_db_name = 'weather.db'
  
  ##----Connect to SQLite DB----
  con = dbConnect(SQLite(), dbname=sql_db_name)
  
  weather_data = get_weather_data(airport, dates)
  
  dbDisconnect(con)
}