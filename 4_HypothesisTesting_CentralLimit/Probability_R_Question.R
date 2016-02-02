##------------------------------
##
## Probability Interview Question
##   "Birthday Problem"
##
##------------------------------

# Answers the questions:
#  1) What's the probability that at least 1 out of
#     N people share a birthday?
#  2) How many people do we need to have >= 50% chance
#     of at least one sharing a birthday?
#
#  Note: to answer this question we consider this "at least"
#  to be:  1-P(none share a birthday)

##----Question 1-----
##----Set parameters-----
n = 10 # number of people

##----Calculate probability-----
total_choices = 365 ^ n # number of ways n people can have birthdays
not_share = choose(365, n) * factorial(n)

probability = 1 - (not_share / total_choices)

##----Question 2-----
x = 0.5   # Probability

n_seq = 1:40 # Loop through sequence of people
prob_seq = sapply(n_seq, function(x){
  return(1 - (choose(365,x)*factorial(x))/(365^x))
})

# Find which one is >= x
n_seq[which(min(prob_seq[prob_seq>x])==prob_seq)]

# Plot outcomes:
plot(n_seq, prob_seq, xlab="Number of People", ylab="Probability",
     main="Prob that at least 1 out of N Share a Birthday")
grid()
