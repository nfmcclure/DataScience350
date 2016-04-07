##--------------------------------------------
##
## Examples of logging verbosity
##
## Class: PCE Data Science Methods Class
##
## https://en.wikipedia.org/wiki/Buffon%27s_needle#Estimating_.CF.80
##
##--------------------------------------------

library(logging)

##-----Toss one stick------
toss_stick = function(stick_length){
  # Randomly pick a start point
  start_x = rnorm(1)
  start_y = rnorm(1)
  
  # Ranomly assign an angle
  theta_sample = sample(seq(0,2*pi,len=100), 1)
  # Calculate the end point
  end_x = start_x + stick_length * cos(theta_sample)
  end_y = start_y + stick_length * sin(theta_sample)
  
  return(c(start_x, start_y, end_x, end_y))
}


##-----Check if a stick crosses a vertical line-----
does_stick_cross = function(stick, line_seq){
  # Need to order the x values on the stick
  min_x = min(c(stick[1], stick[3]))
  max_x = max(c(stick[1], stick[3]))
  
  # Check if any of the vertical grid lines overlab the stick
  cross_logical = any( (line_seq > min_x) & (line_seq < max_x) )
  
  return(cross_logical)
}


##-----Main Program------
if(interactive()){
  # Setup logger
  logReset()
  log_level = "INFO"
  basicConfig(level=log_level)
  addHandler(writeToFile, file="pi_sim_log.log", level=log_level)
  
  # Set parameters
  stick_length = 1
  line_distance = 2
  ylim = c(-5,5)
  xlim = c(-5,5)
  num_sticks = 1000
  
  if (stick_length >= line_distance){
    logwarn('Stick Length >= Line Spacing, which means estimate is not accurate.')
  }
  
  loginfo('Parameter Set:')
  loginfo(paste('Stick Length:', stick_length))
  loginfo(paste('Vertical Line Spacing:', line_distance))
  loginfo(paste('Number of Sticks Tossed:', num_sticks))
  
  # Create positions of vertical lines
  line_seq = seq(from=ylim[1], to=ylim[2], by=line_distance)
  logdebug(paste('Grid of lines:',paste(line_seq,collapse=",")))
  
  # Perform simulations
  outcomes = c()
  for (i in 1:num_sticks){
    stick_on_line =  does_stick_cross(toss_stick(stick_length), line_seq)
    logdebug(paste('Outcome',i,':',stick_on_line))
    outcomes = c(outcomes, stick_on_line)
  }
  
  # Pi approximation
  approx_pi_numerator = 2 * stick_length * num_sticks
  approx_pi_denominator = line_distance * sum(outcomes)
  approx_pi = approx_pi_numerator/approx_pi_denominator
  loginfo(paste('Pi Approx:', approx_pi), logger = 'pi_logger')
}