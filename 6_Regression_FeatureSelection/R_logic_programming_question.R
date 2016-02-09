##------------------------------------
##
##  Logic Puzzle, solvable via programming
##
##------------------------------------

# Puzzle:  There are 100 lockers, numbered 1-100.
# There are 100 students lined up to pass by the lockers,
#  one by one.  The first student switches any locker that
#  is divisible by 1 (opens all of them).  The second student
#  switches the lockers that are divisible by 2 (closes all
#  even ones).  The third student switches all lockers that
#  are divisible by 3 and so on.  Until the last student (#100),
#  which just switches the position of the last locker (#100).

# How many lockers are open and closed?

lockers_pos = rep(0,100) # Here, '0' will stand for closed, '1' for open
lockers_num = 1:100
students = 1:100

# Loop through the students:
for (student in students){
  # Loop through the lockers
  for (locker in lockers_num){
    if (locker %% student == 0){
      # Then student switches locker positon
      lockers_pos[locker] = 1 - (lockers_pos[locker])
    }
  } # end locker loop (key = locker)
} # end student loop (key = student)

print(paste('There are',sum(lockers_pos),'lockers open.'))

# Notice:
which(lockers_pos==1)

# Generalize this:
count_lockers = function(num_lockers, num_students){
  lockers_pos = rep(0,num_lockers)
  lockers_num = 1:num_lockers
  students = 1:num_students
  # Loop through the students:
  for (student in students){
    # Loop through the lockers
    for (locker in lockers_num){
      if (locker %% student == 0){
        # Then student switches locker positon
        lockers_pos[locker] = 1 - (lockers_pos[locker])
      }
    } # end locker loop (key = locker)
  } # end student loop (key = student)
  return(sum(lockers_pos))
}

# Try all integers to 500:
count_open = sapply(1:100, function(x){
  count_lockers(x,x)
})

# Compare this to the count of square numbers less than x
count_squares_less_than_x = function(x){
  sum(sqrt(1:x)%%1==0) # test if sqrt(#)==integer
}

sapply(1:100, count_squares_less_than_x)
