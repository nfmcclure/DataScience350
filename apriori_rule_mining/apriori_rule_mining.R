##--------------------------------------------
##
## Extra topic: Basket Analysis (Apriori Rule Mining)
##
## Class: PCE Data Science Methods Class
##
##--------------------------------------------

setwd('E:/Work/Teaching/PCE_Data_Science/apriori_rule_mining')


# Load Libraries
library(arules)
library(arulesViz)

##-----Sample Data-----

trans_sample = list(c('bread', 'milk', 'eggs', 'beer'),
                    c('beer', 'pingpongballs', 'cups'),
                    c('eggs', 'cups', 'bread'),
                    c('beer', 'pingpongballs', 'wine'))

n_trans = length(trans_sample)

# Create a support function
support_fun = function(items, my_list){
  num_in_set = sum(sapply(my_list, function(x){
    all(items%in%x)
  }))
  return(num_in_set/length(my_list))
}

# Test support
items = c('bread', 'milk')
support_fun(items, trans_sample)

# Create confidence function
confidence_fun = function(items1, items2, my_list){
  numerator = support_fun(c(items1, items2), my_list)
  denominator = support_fun(items1, my_list)
  return(numerator/denominator)
}

# Test confidence function
confidence_fun(c('bread'),c('milk'),trans_sample)

# Create lift function
lift_fun = function(items1, items2, my_list){
  conf = confidence_fun(items1, items2, my_list)
  return(conf/support_fun(items2, my_list))
}

# Test lift
lift_fun(c('bread'), c('milk'), trans_sample)

# Go through all 2-combinations:
items = c('bread', 'milk', 'eggs', 'beer',
          'pingpongballs', 'cups', 'wine')

two_combinations = t(combn(unique(as.character(items)), 2))

results_df = data.frame('rule'=apply(two_combinations, 1, function(x){paste(x[1],x[2])}))

# Supports
supports2 = sapply(1:(dim(two_combinations)[1]), function(item_row){
  item1 = two_combinations[item_row,1]
  item2 = two_combinations[item_row,2]
  support_fun(c(item1, item2), trans_sample)
})
results_df$support = supports2

# Confidences
confidences2 = sapply(1:(dim(two_combinations)[1]), function(item_row){
  item1 = two_combinations[item_row,1]
  item2 = two_combinations[item_row,2]
  confidence_fun(item1, item2, trans_sample)
})
results_df$confidence = confidences2

# Lifts
lifts2 = sapply(1:(dim(two_combinations)[1]), function(item_row){
  item1 = two_combinations[item_row,1]
  item2 = two_combinations[item_row,2]
  lift_fun(item1, item2, trans_sample)
})
results_df$lift = lifts2

##----Randomly Created Data----
n = 1000

random_trans = lapply(1:n, function(x){
  sample(items,sample(1:length(items),1))
})

rand_results = data.frame('rule'=apply(two_combinations, 1, function(x){paste(x[1],x[2])}))

# Supports
supports2 = sapply(1:(dim(two_combinations)[1]), function(item_row){
  item1 = two_combinations[item_row,1]
  item2 = two_combinations[item_row,2]
  support_fun(c(item1, item2), random_trans)
})
rand_results$support = supports2

# Confidences
confidences2 = sapply(1:(dim(two_combinations)[1]), function(item_row){
  item1 = two_combinations[item_row,1]
  item2 = two_combinations[item_row,2]
  confidence_fun(item1, item2, random_trans)
})
rand_results$confidence = confidences2

# Lifts
lifts2 = sapply(1:(dim(two_combinations)[1]), function(item_row){
  item1 = two_combinations[item_row,1]
  item2 = two_combinations[item_row,2]
  lift_fun(item1, item2, random_trans)
})
rand_results$lift = lifts2
# Should get slightly above 1 for lift, because we only have 7 items.

##----Kaggle Data Set-----
# Information about the data set is here:
# https://www.kaggle.com/c/walmart-recruiting-trip-type-classification

walmart_data = read.csv('walmart_train.csv', stringsAsFactors = FALSE)

##-----Data Description-----
#
# Trip Type: Walmart's trip type
# VisitNumber: Transaction ID
# Weekday: The weekday
# UPC:  UPC code
# ScanCount: # of item purchased
# DepartmentDescription: Department
# FineLineNumber: Refined Category of product

length(unique(walmart_data$VisitNumber))
length(unique(walmart_data$Upc))

#100K X 100K matrix = 100 Billion entries...

# really need to read it in as transactions.
rm(walmart_data)
gc()
walmart_transactions = read.transactions("walmart_train.csv", format="single",
                                         sep=",", cols = c("VisitNumber", "Upc"),
                                         rm.duplicates = TRUE)

# Only run the following on a faster computer
# to see how transactions look: (might take a while to show up)

#----------------
# image(walmart_transactions)
#----------------
#
# Plots Transactions in rows vs items in columns
# and a point where a '1' occurs
#
# should look like a bunch of black dots.

# Perform Apriori search for rules:
rules = apriori(walmart_transactions, parameter = list(support=0.001,
                                                       confidence=0.1,
                                                       maxlen=10))
# plot confidence vs support
plot(rules)

# Inspect rules:
inspect(rules)

# Not sure what UPCs stand for, but I do know that
# 4011 = banana
# 4087 = tomato
# 4046 = avocado
# Not sure if product codes == upc codes...

# View LHS items ordered vs RHS items ordered, color = lift
plot(rules, method="matrix", meaure="lift")

# Look at top 30 rules:
subrules = head(sort(rules, by="lift"), 30)
plot(subrules, method="graph")
# Incoming arrows = RHS
# Outgoing arrows = LHS

# Look at top rule:
top_rule = subrules[1]
inspect(top_rule)
