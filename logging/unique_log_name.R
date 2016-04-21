#------------------
#
#  DS PCE 350
#  -Logging file names
#
#------------------

library(logging)
# Today's date:
# Sys.Date()

# Today's date-time
# Sys.time()

# Get the log file name that has a date-time in the name
get_log_filename = function(){
  log_file_name = format(Sys.time(), format="HW2_log_%Y_%m_%d_%H%M%S.log")
  return(log_file_name)
}

# Unit test to check that log file name doesn't exist
test_log_file_name_uniqueness = function(log_file_name){
  all_files = list.files()
  stopifnot(!log_file_name%in%all_files)
}


if (interactive()){
  # Get logger file name
  logger_file_name = get_log_filename()
  
  # Test for uniqueness
  test_log_file_name_uniqueness(logger_file_name)
  
}
