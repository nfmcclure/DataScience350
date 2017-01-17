##--------------------------------------------
##
## Exploring Data in R (lecture 1)
##
## Class: PCE Data Science Methods Class
##
## Contains examples of:
##
## -Working with Distributions
##
## -Visually/Numerically Exploring data
##
##--------------------------------------------

library(MASS) # has the function 'fractions()', which is useful.

##-----Sandwich Count----
breads = c('white', 'wheat', 'italian', 'sevengrain')
meats = c('ham', 'turkey', 'chicken', 'pastrami', 'meatballs')
toppings = c('mustard', 'mayo', 'salt_pepper', 'oil_vinegar')

sandwiches = expand.grid(breads,
                         meats,
                         toppings)

##-----Two Dice------
two_dice = expand.grid(1:6,1:6)
two_dice$sum = two_dice$Var1 + two_dice$Var2
two_dice$isdouble = two_dice$Var1 == two_dice$Var2

# Count different sums
sum_counts = table(two_dice$sum)

# Count doubles
doubles = sum(two_dice$isdouble)

# Probabilities of sums:
sum_prob = fractions(table(two_dice$sum)/nrow(two_dice)) # type ?fractions for more detail
barplot(sum_prob)

# Probability of a double:
fractions(doubles/nrow(two_dice))

##-------Simulations in R------
# Define deck
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
ranks <- c("Ace", "Deuce", "Three", "Four","Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(ranks=ranks, suits=suits)

# Find probability that 5 cards make up a flush from simulations
n = 100000 # stay under 1 million
hands = sapply(1:n, function(x){
  five_cards = sample(1:nrow(deck),5)
  return(length(unique(deck$suits[five_cards]))==1)
})

emp_prob = sum(hands)/n
emp_var = var(hands)


##-------Exploring Data------
# The 'iris' data set is in the base package
str(iris)

summary(iris)

head(iris)

tail(iris, n=10)

table(iris$Species)

IQR(iris$Sepal.Length)

# Covariance won't use factors or text features
# First we compute covariances for everything
iris_num = iris
iris_num$Species = NULL
cov(iris_num)

# Then we compute covariances for each group
group_cov = lapply(unique(iris$Species), function(x){
  cov(iris[iris$Species==x,-5], use="na.or.complete")
})

# same for correlations (cor)
cor(iris_num)
group_cor = lapply(unique(iris$Species), function(x){
  cor(iris[iris$Species==x,-5], use="na.or.complete")
})

# Classic Simpsons Paradox Example
UC_admittance = data.frame('dept'=c('a','b'),
                           'applied_men'=c(1695,996),
                           'applied_women'=c(404,1431),
                           'admitted_men'=c(1058,255),
                           'admitted_women'=c(322,379))

#Boxplots
boxplot(iris_num)

# Scatterplots
pairs(iris_num)
pairs(iris)    # Factors too!