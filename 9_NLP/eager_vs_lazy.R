##--------------------------------------------
##
## R example of eager vs lazy evaluation
##
##--------------------------------------------

setwd('/home/nick/Documents/teaching/DataScience350/8_NLP/')

# Going to show the difference between eager and lazy evaluation
#    for short circuiting logical statements.
# See:
#  https://en.wikipedia.org/wiki/Short-circuit_evaluation

# Short circuiting is the lazy program evaluation of a compound logical statement:
#----------------------------------------------------------------
# Consider:
#    "if (condition 1) AND (condition 2) AND (condition 3) then <do something>"
# If (condition 1) is false, then there's no need to evaluate more conditions.
#----------------------------------------------------------------
# Consider:
#    "if (condition 1) OR (condition 2) OR (condition 3) then <do something>"
# If (condition 1) is true, then there's no need to evaluate more conditions.
#----------------------------------------------------------------

# You can imagine R has both ways to do this.

##-----Eager evaluation-------
if (1==2 & 5==NULL){
  print("True!")
} else{
  print("False!")
}

if ((1==1) | (5==NULL)){
  print("True!")
} else{
  print("False!")
}

##-----Lazy Evaluation-------
if ((1==2) && (5==NULL)){
  print("True!")
} else{
  print("False!")
}

if ((1==1) || (5==NULL)){
  print("True!")
} else{
  print("False!")
}


# What are the implications of this?
test_matrix = matrix(rnorm(1000000), nrow = 1000)

# eager:
system.time(apply(test_matrix,1,function(r) sapply(r,function(c) {
  if(c>3 & c<3.1){c}else{0}
  })))

# lazy:
system.time(apply(test_matrix,1,function(r) sapply(r,function(c) {
  if(c>3 && c<3.1){c}else{0}
})))

# Even harder:
# eager:
system.time(apply(test_matrix,1,function(r) sapply(r,function(c) {
  if(c>3 & abs(c**2+3*c-2)<0.999 & sin(tan(abs(pi*c)))>0.5){c}else{0}
  })))

# lazy:
system.time(apply(test_matrix,1,function(r) sapply(r,function(c) {
  if(c>3 && abs(c**2+3*c-2)<0.999 && sin(tan(abs(pi*c)))>0.5){c}else{0}
})))