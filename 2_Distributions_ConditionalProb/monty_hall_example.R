##--------------------------------------------
##
## Example Monty Hall Homework Solution
##
## Class: PCE Data Science Methods Class
##
## Name: 
##
##--------------------------------------------

library(logging)


# Get the log file name that has a date-time in the name
get_log_filename = function(){
  log_file_name = format(Sys.time(), format="HW2_log_%Y_%m_%d_%H%M%S.log")
  return(log_file_name)
}


# Redefine the sample() function properly (Straight from the help file)
resample <- function(x, ...) x[sample.int(length(x), ...)]


# One game simulation function
one_game = function(switch_logical){
  
  # Perform one game logical steps in here.
  # Remember that if 'switch_logical' is TRUE,
  #   we must switch doors to the non-revealed door.
  
  # This function returns either a 0/1 or a FALSE/TRUE
}


# Unit test
# Test if a simulation returns TRUE or FALSE
test_simulation_return_val = function(){
  one_game_outcome = one_game(switch_logical=TRUE)
  stopifnot(one_game_outcome %in% c(TRUE, FALSE))
}


if (interactive()){
  # Setup Logging
  log_file_name = get_log_filename()
  basicConfig()
  addHandler(writeToFile, file=log_file_name, level='INFO')
  
  # Setup working directory
  setwd('path to my working directory')
  
  # Perform unit test
  test_simulation_return_val()
  
  # Set simulation parameters
  N_sims = 10000 # Number of games to simulate
  
  # Perform Stay Simulations
  # Here we come up with a vector of wins and losses
  #   by running the above function.
  # It should look like a vector of 1's and 0's (0, 0, 1, 1, ...)
  #   OR a vector of TRUE and FALSE values (FALSE, FALSE, ...)
  
  # For example:
  stay_results = sapply(1:N_sims, function(x) one_game(switch_logical=FALSE))
  
  # Perform Switch Simulations
  # Here we come up with a vector of wins and losses
  #   by running the above function.
  # It should look like a vector of 1's and 0's (0, 0, 1, 1, ...)
  #   OR a vector of TRUE and FALSE values (FALSE, FALSE, ...)
  
  
  # Compute Results
  # First we compute the average
  prob_win_switch = mean(switch_results)
  prob_win_stay = mean(stay_results)
  
  # Then we compute the variance of the results
  var_switch = var(switch_results)
  var_stay = var(stay_results)
  
  # Log Results
  # Here is an example of how to log a result
  prob_for_an_experiment = 0.42
  loginfo(paste('Here are my results:', prob_for_an_experiment))
}