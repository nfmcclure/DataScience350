##------------------------------------
##
## Probability Interview Question in R
##
##------------------------------------

# Question:
# There are 100 airline passengers and 100 seats.
# If the first person to board is crazy and will ignore
# the seat # and sit wherever, and the subsequent sane
# 99 people will take thier assigned seat unless
# theirs is already taken (then they will take a
# random seat) then what is the probability that
# the last person (100th person) will sit in the
# correct seat?

##-----Parameters------
seats = 1:100
people_count = 1
total_people = length(seats)
seats_taken = c()

##-----First Person-----
first_seat = sample(seats, 1)
seats = setdiff(seats, first_seat)
people_count = people_count + 1
seats_taken = c(seats_taken, first_seat)

##-----Loop through-----

while (people_count <= total_people){
  # First check if the person can sit down in their seat
  if (people_count %in% seats){
    # take that seat
    seats = setdiff(seats, people_count)
    seats_taken = c(seats_taken, people_count)
  } else {
    # take a random seat
    rand_seat = sample(seats, 1)
    # remove that seat from choices
    seats = setdiff(seats, rand_seat)
    seats_taken = c(seats_taken, rand_seat)
  }
  
  # increment people
  people_count = people_count + 1
  
}

##-----Check if last is '100'-----
last_correct = seats_taken[total_people] == total_people


##----Make function, repeat-----
check_last_seat = function(num_seats){
  seats = 1:num_seats
  people_count = 1
  total_people = length(seats)
  seats_taken = c()
  
  # First Person
  first_seat = sample(seats, 1)
  seats = setdiff(seats, first_seat)
  people_count = people_count + 1
  seats_taken = c(seats_taken, first_seat)
  
  while (people_count <= total_people){
    # First check if the person can sit down in their seat
    if (people_count %in% seats){
      # take that seat
      seats = setdiff(seats, people_count)
      seats_taken = c(seats_taken, people_count)
    } else {
      # take a random seat
      rand_seat = sample(seats, 1)
      # remove that seat from choices
      seats = setdiff(seats, rand_seat)
      seats_taken = c(seats_taken, rand_seat)
    }
    
    # increment people
    people_count = people_count + 1
    
  }
  
  # Check if last is '100'
  return(seats_taken[total_people] == total_people)
}

##----Run Simulation-----
n = 1000
sims = sapply(1:n, function(x) check_last_seat(100))

sum(sims)/n


# Run 500 times for seat #'s 5 through 150
seat_seq = seq(from=5, to=150, by=5)
prob_seq = sapply(seat_seq, function(x) sum(sapply(1:500, function(n) check_last_seat(x)))/500)


plot(seat_seq, prob_seq, type="l", ylim=c(0,1))
abline(h=0.5, lty=2)
