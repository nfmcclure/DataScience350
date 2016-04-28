##---------------------------------------------------
##
##  PCE DS 350: Interview question for lecture 5
##
##---------------------------------------------------

# Question from an Amazon Interview (source: Glassdoor.com)
#
# Three assumptions about 1st and 2nd interviews of candidates:
#  - 50% of all people who get a 1st interview get a 2nd interview
#  - 95% of people you know who got a 2nd interview feel they did well
#  - 75% of people you know who didn't get a 2nd interview feel they did well
#
# If you feel you did well, what is the probability you get a second interview?

# Bayes law:
#                           __ (did well)
#                          /
#    _   (2nd Interview)  <
#   /                      \__ (didn't do well)
#  /
# <                         __ (did well)
#  \                       /
#   \_ (no 2nd interview) <
#                          \__ (didn't do well)
#
#
#                               P(did well | 2nd) * P(2nd)
# P(2nd Interview | did well) = ------------------------
#                                    P(did well)

p_2nd = 0.5
p_no2nd = 1 - p_2nd

p_didwell_given_2nd = 0.95
p_notwell_given_2nd = 1 - p_didwell_given_2nd

p_didwell_given_no2nd = 0.75
p_notwell_given_no2nd = 1 - p_didwell_given_no2nd

p_didwell_and_2nd   = p_2nd * p_didwell_given_2nd
p_notwell_and_2nd   = p_2nd * p_notwell_given_2nd
p_didwell_and_no2nd = p_no2nd * p_didwell_given_no2nd
p_notwell_and_no2nd = p_no2nd * p_notwell_given_no2nd

p_didwell = p_didwell_and_2nd + p_didwell_and_no2nd
p_notwell = p_notwell_and_2nd + p_notwell_and_no2nd

# What we are interested in:
# P(2nd interview | did well)
p_2nd_given_didwell = p_didwell_given_2nd * p_2nd / p_didwell

# Which is only slightly better than the 50% overall.
# but:
p_2nd_given_not_well = p_notwell_given_2nd * p_2nd / p_notwell
