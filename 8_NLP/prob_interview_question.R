##-------------------------------------------
##
##    Data Science PCE Interview Question
##
##-------------------------------------------

# Question: Two trains are slowly going towards each
# other at 4mph and 6mph and are 20 miles apart.
#
# There is a fly that can go 8 mph flying between
# each of them.  When the fly arrives at the first
# train, it instantly turns around and flies back
# to the other train and so on.
#
# What is the total distance the fly goes until the
# two trains meet?

##-------- (1) -------------
# Solve this programmatically

total_distance = 0 # Keep track of distance

# Now we need to keep track of which train the fly
# is headed towards, say 1 = 4mph train, 0 = 6mph train.
logical_train = 1

# Keep track of distance between:
dist_between_trains = 20

while (dist_between_trains>0.0001){
  if (logical_train==1){
    # Fly and 4mph train are headed towards each other.
    # Combined speed of 4+8 = 12 mph
    time_to_intercept = dist_between_trains/12
    dist_fly_traveled = time_to_intercept * 8
    total_distance = total_distance + dist_fly_traveled
    
    # Need to compute distance train traveled:
    dist_train_traveled = time_to_intercept * (4+6)
    dist_between_trains = dist_between_trains - dist_train_traveled
    
  }else{
    # Fly and 6mph train are headed towards each other.
    # Combined speed of 6+8 = 14 mph
    time_to_intercept = dist_between_trains/14
    dist_fly_traveled = time_to_intercept * 8
    total_distance = total_distance + dist_fly_traveled
    
    # Need to compute distance train traveled:
    dist_train_traveled = time_to_intercept * (4+6)
    dist_between_trains = dist_between_trains - dist_train_traveled
  }
}

print(paste('Fly traveled', round(total_distance, 2), 'miles'))


##-------- (2) -------------
# Solve this the short way
time_till_trains_meet = 20/(4+6)
fly_dist = time_till_trains_meet * 8
