##--------------------------------------------
##
## Extra topic: Neural Networks
##
## Class: PCE Data Science Methods Class
##    *** Many ideas have been taken from:
##    http://karpathy.github.io/neuralnets/  ***
##
##--------------------------------------------

setwd('E:/Work/Teaching/PCE_Data_Science/neural_networks')

library(neuralnet)

##-------Circuits------
# Single Circuit, one multiply gate:
# x ---- ____
#       |  * |==> (x * y)
# y ----|____|
#

forward_multiply = function(x1, x2){
  return(x1 * x2)
}

forward_multiply(3, 4)

# Now how can we change the input to decrease the output?

forward_multiply(2.9, 4)

# But not all 'gates' are that simple. To answer this question,
#  we need to use the 'derivative'.  The derivative tells us
#  which direction to change the inputs to change the outputs.
#
# In general, the derivative of a function f(x,y,z,...) with
#  respect to x is:
#  df(x,...)   f(x+h,...) - f(x, ...)
#  --------- = ---------------------
#     dx                h
#
# Above, our multiply function was:
#
#  f(x,y) = x * y
#
#
# Our derivative is:
#
#  df(x,y)    (x+h)*y - xy
#  ------- = -------------- = y
#    dx           h

# Don't worry too much about this, we can always evaluate
# derivatives numerically:
x = 3; y = 4

h = 0.0001

# df/dx:
( forward_multiply(x+h,y) - forward_multiply(x,y) ) / h
# This means df/dx = y

# df/dy:
( forward_multiply(x,y+h) - forward_multiply(x,y) ) / h
# Similarly, df/dy = x

# So now there's a shortcut:
mult_gradient_x = function(x,y) { return(y) }
mult_gradient_y = function(x,y) { return(x) }

##-------Implement a one-gate network, decrease output------
num_loops = 100
step_size = 0.001
best_x = x
best_y = y
best_result = x*y

for (i in 1:num_loops){
  x = x - (step_size * mult_gradient_x(x,y))
  y = y - (step_size * mult_gradient_y(x,y))
  result = x * y
  if (result < best_result){
    best_x = x
    best_y = y
    best_result = result
  }
}

# Is x* y lower?
best_x * best_y

##------Multiple Gates-------

# Consider: f(x,y,z) = (x + y) * z
#
#  or:  g(x,y) = x + y
#       f(x,y,z) = g(x,y) * z  OR f = g * z
#
#
# x ---- ____
#       |  + |
# y ----|____|---____
#               |  * |===> (x+y)*z
# z ------------|____|
#

forward_add = function(x1, x2){
  return(x1 + x2)
}

# Declare our two-gate example
forward_two_gate = function(x,y,z){
  out1 = forward_add(x, y)
  out2 = forward_multiply(out1, z)
  return(out2)
}

# Test it out:
forward_two_gate(2,3,4) # (2+3)*4 = 20

# Derivatives:
#  Turns out they are the same for both variables
#   so just need one function.
add_gradient = function(x,y){ return(1) }

# But these operations are 'chained together'.  So we
#  use something called the chain rule for derivatives:
#
#
# df(x,y,z)      dg          df
# --------- = --------  *  -------    (I hate to think of these as fractions.... but oh well)
#    dx          dx          dg

# Loop through again:
num_loops = 100
step_size = 0.001
x = 2; y = 3; z = 4
best_x = x
best_y = y
best_z = z
best_result = forward_two_gate(x, y, z)

for (i in 1:num_loops){
  out1 = forward_add(x, y)
  x = x - (step_size * add_gradient(x,y) * mult_gradient_x(out1, z))
  y = y - (step_size * add_gradient(x,y) * mult_gradient_x(out1, z))
  z = z - (step_size * mult_gradient_y(out1, z))
  result = (x + y) * z
  if (result < best_result){
    best_x = x
    best_y = y
    best_z = z
    best_result = result
  }
}

# Is it less than original? (< 20?)
best_result


##----Simple Neural Network-----
# We add a 'neuron' to our network by composing the output with
# our 'sigmoid' function from logistical regression.
#
# f(x, a, b) = sigmoid(a*x + b)
#
# Where                  1
#     sigmoid(x) = ------------
#                   1 + exp(-x)

sigmoid = function(x){
  return(1/(1 + exp(-x)))
}

# This sigmoid is special because it's derivative is related to itself

d_sigmoid = function(x){
  return( sigmoid(x) * (1 - sigmoid(x)) )
}

# test if neuron works:
forward_neuron = function(x, a, b){
  out1 = forward_multiply(a, x)
  out2 = forward_add(out1, b)
  return(sigmoid(out2))
}

forward_neuron(2, 2, 3) # sigmoid(2 * 2 + 3)
# Should be the same as:
1/(1 + exp(-7))

# Loop this many times.  But let's consider only changing a and b (fixed x value)
num_loops = 100
step_size = 0.01
a = 2; b = 3; x = 2
best_a = a
best_b = b
best_result = forward_neuron(x, a, b)

# There's a print statement in the following loop, so we can see the output
for (i in 1:num_loops){
  # Use chain rule:
  a_gradient = d_sigmoid(a * x + b) * mult_gradient_x(x, a)
  b_gradient = d_sigmoid(a * x + b) * add_gradient(a * x, b)
  
  a = a - (step_size * a_gradient)
  b = b - (step_size * b_gradient)
  result = forward_neuron(x, a, b)
  if (result < best_result){
    print(best_result)
    best_a = a
    best_b = b
    best_result = result
  }
}

# Is it less than original?
best_result < forward_neuron(2, 2, 3)


##-----Compare NN Gates-----

# Sigmoid
sigmoid = function(x){
  return(1/(1 + exp(-x)))
}

# Rectified Linear Unit (ReLU)
relu = function(x){
  return(pmax(x, 0))
}

# Softplus
softplus = function(x){
  return(log(1+exp(x)))
}

# Leaky ReLU
leaky_relu = function(x, a){
  stopifnot(a<1)
  return(pmax(x, a*x))
}

x = seq(-4, 4, length=1000)

plot(x, sigmoid(x), type="l", lwd=2, col="black", ylim=c(-0.5,3), xlim=c(-3,3))
lines(x, relu(x), lwd=2, col="red")
lines(x, softplus(x), lwd=2, col="green")
lines(x, leaky_relu(x, 0.15), col="blue")
grid()
legend('topleft', c('Sigmoid', 'ReLU', 'Softplus', 'Leaky ReLU'),
       lty=c(1,1,1,1), lwd=c(2,2,2,2), col=c('black', 'red', 'green', 'blue'))

##-----Training a Neural Network-----
evacuated = c(0,0,0,1,1,0,0,0,0,0,0,0,1,1,1,1,0,0,0)
pets = c(1,1,1,1,0,0,0,1,1,0,0,1,1,1,1,1,1,0,0)
mobile_home = c(0,0,1,1,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0)
tenure = c(16,26,11,1,5,34,3,3,10,2,2,25,20,11,15,21,3,5,7)
years_educ = c(16,12,13,10,12,12,14,16,12,18,12,16,12,10,8,15,22,18,18)

evac_df = data.frame("evacuated" = evacuated,
                     "pets" = pets,
                     "mobile_home" = mobile_home,
                     "tenure" = tenure,
                     "years_educ" = years_educ)

# Normalize data
normalize = function(x){
  (x - min(x, na.rm=TRUE))/(max(x, na.rm=TRUE) - min(x, na.rm=TRUE))
}
evac_df = data.frame(evac_df$evacuated, apply(evac_df[2:5],2,normalize))
names(evac_df)[1] = "evacuated"

# Here we want to predict evacuated or not (0 or 1)
# from four different inputs.
#
# Our Neural network will look like:
#  F(p,m,t,y,a,b,c,d,f) = sigmoid(ap + bm + ct + dy + f)

# Declare the feed forward network output
forward_network1 = function(p,m,t,y,a,b,c,d,f){
  out1 = a*p + b*m + c*t + d*y + f
  return(sigmoid(out1))
}

# Given an output, what should we predict?
#  Here we will just do: if val > 0.5 => evacuated (1)
predicted_label = function(sigmoid_output){
  round(sigmoid_output)
}

# Evaluate a prediction point
evaluate_binary_classifier = function(p,m,t,y,a,b,c,d,f, actual){
  prediction = predicted_label(forward_network1(p,m,t,y,a,b,c,d,f))
  return(prediction==actual)
}

# Loop this many times.
# Remember, we will only change a, b, c, d, f (not p, m, t, or y)
num_loops = 5000
step_size = 0.005
a = 1; b = 1; c = 1; d = 1; f = 1
best_a = a
best_b = b
best_c = c
best_d = d
best_f = f

# There's a print statement in the following loop, so we can see the output
for (i in 1:num_loops){
  # Select a random point
  rand_row = evac_df[sample(1:nrow(evac_df), 1),]
  p = rand_row$pets
  m = rand_row$mobile_home
  t = rand_row$tenure
  y = rand_row$years_educ
  network_out = forward_network1(p,m,t,y,a,b,c,d,f)
  
  # Determine if we need to make it greater or less
  actual = rand_row$evacuated
  if (((actual==1) & (network_out>0.5)) | ((actual==0) & (network_out<=0.5))){
    pull = 0 # Correctly identified, no need to change
  } else if ((actual==1) & (network_out<=0.5)){
    pull = +1 # False negative, pull in positive direction
  } else if ((actual==0) & (network_out>0.5)){
    pull = -1 # False positive, pull in negative direction
  } else {
    stop("You have a logical error here. Please check your if statements.")
  }
  
  # Use chain rule for gradients:
  a_gradient = d_sigmoid(a*p + b*m + c*t + d*y + f) *
    mult_gradient_x(a, p)
  b_gradient = d_sigmoid(a*p + b*m + c*t + d*y + f) *
    mult_gradient_x(b, m)
  c_gradient = d_sigmoid(a*p + b*m + c*t + d*y + f) *
    mult_gradient_x(c, p)
  d_gradient = d_sigmoid(a*p + b*m + c*t + d*y + f) *
    mult_gradient_x(d, m)
  f_gradient = d_sigmoid(a*p + b*m + c*t + d*y + f) *
    add_gradient(a*p + b*m + c*t + d*y, f)
  
  a = a + pull * (step_size * a_gradient)
  b = b + pull * (step_size * b_gradient)
  c = c + pull * (step_size * c_gradient)
  d = d + pull * (step_size * d_gradient)
  f = f + pull * (step_size * f_gradient)
  
  # Get accuracy of network
  classification_correctness = apply(evac_df, 1, function(x){
    p = x['pets']
    m = x['mobile_home']
    t = x['tenure']
    y = x['years_educ']
    actual = x['evacuated']
    return(evaluate_binary_classifier(p,m,t,y,a,b,c,d,f,actual))
  })
  
  # Percent classified correctly:
  classification_per = sum(classification_correctness)/nrow(evac_df)
  
  #Print out every 50 loops:
  if (i%%50==0){
    print(paste('Loop',i,'. Accuracy=',round(classification_per,3)))
  }
}

# Print model:
print('Prediction = sigmoid(a * pets + b * mobile_home + c * tenure + d * years_educ + f')
print(paste('a =',round(a, 4)))
print(paste('b =',round(b, 4)))
print(paste('c =',round(c, 4)))
print(paste('d =',round(d, 4)))
print(paste('f =',round(f, 4)))

# Original logistic:
evac_logit = glm(evacuated ~ pets + mobile_home + tenure + years_educ, data = evac_df, family = binomial)

prediction = as.numeric(evac_logit$fitted.values>0.5)

pred_evac_actual_evac = sum( (prediction == 1) & (evacuated == 1) )
pred_stay_actual_stay = sum( (prediction == 0) & (evacuated == 0) )
accuracy = (pred_evac_actual_evac + pred_stay_actual_stay)/(nrow(evac_df))

# Same?
classification_correctness
( ((prediction == 1) & (evacuated == 1)) | ((prediction == 0) & (evacuated == 0)) )


# Why one different?  Probably normalization.  Do we have to normalize? No, but it really really really
#   helps with Neural Network convergence!!!

##----Larger Neural Networks-----

# Close representation of what we did above:
neural_network = neuralnet(evacuated ~ pets + mobile_home + tenure + years_educ,
                           data = evac_df, hidden=c(0), act.fct = 'logistic')
neural_network$result.matrix
plot(neural_network)

# Deeper Neural Network:
neural_network2 = neuralnet(evacuated ~ pets + mobile_home + tenure + years_educ,
                            data = evac_df, hidden=c(4,2), act.fct = 'logistic')
# '1layhid.3.to.2layhid2' : Weight from 3rd node, 1st hidden layer to 2nd node 2nd hidden layer
neural_network2$result.matrix
plot(neural_network2)


