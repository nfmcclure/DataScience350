# Probability Interview Question
#
# Source: Data Scientist Facebook interview in Spring 2014. (Menlo Park)
#
#
#----------------------
# Question: 
#  You're about to visit Seattle.  You call 3 random people in Seattle and ask each one
#  if it's raining there currently.  Each person has a 2/3 change of telling the truth,
#  and 1/3 chance of lying.  All three friends say that, yes it is raining.
#  What is the probability of it actually raining in Seattle?

# (Assume the probability of rain on any given day is 25%)
#

library(MASS)

p_rain = 0.25
p_truth = 2/3
responses = c(1, 1, 1) # '1' will stand for rain in Seattle, '0' will stand for no rain.

# Note:
# We are interested in P(R | YYY):
#
# P(R | YYY) = P( R and YYY) / P(YYY)
#
# P(YYY) = P(YYY and R) + P(YYY and R')

# We need the probability of YYY
#
# Tree:
#            __ YYY given R     =>  P(R)*P(YYY|R)   = P(R and YYY)
#           /
#     __ R <
#    /      \__ (YYY)' given R  =>  P(R)*P(YYY'|R)  = P(R and YYY')
#   /
# <          __ YYY given R'    =>  P(R')*P(YYY|R') = P(R' and YYY)
#   \       /
#    \__ R'<
#           \__ (YYY)' given R' =>  P(R')*P(YYY'|R')= P(R' and YYY')
#

p_yyy_given_r    = (p_truth)^(sum(responses))
p_yyy_given_rnot = (1-p_truth)^(sum(responses))

p_n_given_r      = (1-p_yyy_given_r)
p_n_given_rnot   = (1-p_yyy_given_rnot)

# Then P(YYY) = P(YYY and R) + P(YYY and R'):

p_yyy_and_r    = p_rain * p_yyy_given_r
p_n_and_r      = p_rain * p_n_given_r
p_yyy_and_rnot = (1-p_rain) * p_yyy_given_rnot
p_n_and_rnot   = (1-p_rain) * p_n_given_rnot

p_yyy = p_yyy_and_r + p_yyy_and_rnot

# And finally: P(R | YYY) = P( R and YYY) / P(YYY)
p_r_given_yyy = p_yyy_and_r / p_yyy

fractions(p_r_given_yyy)


# As a side note:
p_n = p_n_and_r + p_n_and_rnot
p_r_given_n = p_n_and_r/p_n  # This is the probability of it raining given
# ... not all of the friends say it is.

##----------Functionalize/Generalize it---------
p_rain = function(n_friends, p_truth, p_rain){
  p_yyy_given_r    = (p_truth)^(n_friends)
  p_yyy_given_rnot = (1-p_truth)^(n_friends)
  
  p_yyy_and_r    = p_rain * p_yyy_given_r
  p_yyy_and_rnot = (1-p_rain) * p_yyy_given_rnot
  
  p_yyy = p_yyy_and_r + p_yyy_and_rnot
  
  p_r_given_yyy = p_yyy_and_r / p_yyy
  
  return(p_r_given_yyy)
}

p_rain(3, 2/3, 0.25)

# Plot n_friends vs outcome:

prob_by_num_friends = sapply(1:30, function(x) p_rain(x, 2/3, 0.25))

plot(1:30, prob_by_num_friends, type="l")
grid()

