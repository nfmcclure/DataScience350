##--------------------------------------------
##
## Lecture 6 R methods
##
## Class: PCE Data Science Methods Class
##
##--------------------------------------------

setwd('E:/Work/Teaching/PCE_Data_Science/6_Regression_FeatureSelection')
library(png)
library(raster)
library(glmnet)
library(pls)
library(zoo)
library(MASS)

##-----Linear Algebra Examples-----

A = matrix(runif(12), nrow=4)

B = matrix(runif(12), nrow=3)

C = matrix(runif(12), nrow=4)

# Create a vector
a_vec = matrix(1:15)
a_array = 1:15

# Not the same as an array
a_vec == a_array
dim(a_vec)
dim(a_array)

c(a_vec) # This is how to convert them

# Careful with a full matrix though:
c(A)

# Identity Matrix:
eye4 = diag(4)

# Algebraic Properties of Matrices:
# Addition:
A + C

A %*% C # Error

A %*% B

# Transpose:
t(C)
A %*% t(C)

# Multiply by identity matrix:
square_matrix = matrix(runif(16), nrow=4)

square_matrix %*% eye4

# Mulitiply by inverse:
round(square_matrix %*% ginv(square_matrix))

# SVD of square matrix:

square_svd = svd(square_matrix)

X = square_svd$u
Y = diag(square_svd$d)
Z = t(square_svd$v)

X %*% Y %*% Z # Should be equal to square_matrix

round(X %*% t(X)) # Nice properties of X,Z is that
round(Z %*% t(Z)) #    the transpose is equal to the inverse!

##----- SVD in R-----

# Create a dataset with patterns in R:

sigma1 = 1
sigma2 = 1
corr = 0.75 * sigma1 * sigma2


correlated_data = mvrnorm(n=500, mu=c(5,5), Sigma = matrix(c(sigma1**2,corr,corr,sigma2**2),2,2))

# If we plot the two columns, it appears that there is a correlation between the two
plot(correlated_data[,1],correlated_data[,2],pch=16,main='Dependent Data')

cor(correlated_data[,1],correlated_data[,2])

# Let's find the svd axes for the data:
data_svd = svd(correlated_data)
data_svd$v
svd_component1_x = c(0,data_svd$v[1])*5 + mean(correlated_data[,1])
svd_component1_y = c(0,data_svd$v[2])*5 + mean(correlated_data[,2])
svd_component2_x = c(0,data_svd$v[3])*5 + mean(correlated_data[,1])
svd_component2_y = c(0,data_svd$v[4])*5 + mean(correlated_data[,2])
plot(correlated_data[,1], correlated_data[,2], pch=16, ylim=c(0,10), xlim=c(0,10))
lines(svd_component1_x,svd_component1_y, lwd=2, col='blue')
lines(svd_component2_x,svd_component2_y, lwd=2, col='red')

# plot the transformed data!
plot(data_svd$u[,1], data_svd$u[,2], pch=16, main='Transformed Data')
grid(lwd=2, col='blue')

# This can also be done with R's prcomp() function:
pr_comp1 = prcomp(correlated_data, scale. = TRUE)
plot(pr_comp1$x, pch=16)


# New correlation!
cor(data_svd$u[,1], data_svd$u[,2])


#----SVD in linear regression----
# Load the automobile data from the prior class:
data = read.table('imports-85.data', sep=",", header=FALSE, na.strings = c("NA","?"))
columns = c('symboling','normalized_losses','make','fuel_type','aspiration',
            'num_of_doors','body_style','drive_wheels','engine_location','wheel_base',
            'length','width','height','curb_weight','engine_type','num_of_cylinders',
            'engine_size','fuel_system','bore','stroke','compression_ratio','horsepower',
            'peak_rpm','city_mpg','highway_mpg','price')
names(data) = columns

# Missing data:
# fill in normalized_losses:
mean_norm_losses = mean(data$normalized_losses, na.rm=TRUE)
data$normalized_losses[is.na(data$normalized_losses)] = mean_norm_losses

# Now remove missing rows
data = data[complete.cases(data),]

# Full linear regression:
log_price_all_model = lm(log(price) ~ . - engine_location, data = data)

summary(log_price_all_model)

# Create our data matrix!
data_matrix = model.matrix(log(price) ~ . - engine_location, data = data)
head(data_matrix)

# Compute the principle components of the data
pc_data_matrix = prcomp(data_matrix)

# Plot the magnitude of the principal components:
plot(pc_data_matrix$sdev)
# Here you can see we don't need all the components.  The magnitudes of the
#   associated eigenvalues go to zero quite fast.

# Perform linear regression on components:
pc_all_components = lm(log(data$price) ~ pc_data_matrix$x[,1:10])
summary(pc_all_components)

# Compare AIC
AIC(log_price_all_model)
AIC(pc_all_components)

# How many components?  Let's see the AIC by # of components
aic_by_num_pc = sapply(2:65, function(x){
  formula_rhs_temp = paste(paste0('pc_data_matrix$x[,',1:x,']'), collapse = ' + ')
  formula_temp = paste('log(data$price) ~',formula_rhs_temp)
  pc_all_components_temp = lm(eval(parse(text=formula_temp)))
  return(AIC(pc_all_components_temp))
})
plot(aic_by_num_pc, type='l', lwd=2,
     main='AIC of P.C. Linear Reg with X components',
     xlab="# of components", ylab='AIC')
# add a horizontal line of where the all variable AIC is at
abline(h=AIC(log_price_all_model), lwd=2, col='red')
which.min(aic_by_num_pc) # 59 principal components!

# Why do this? If we can't reformulate our linear regression model
#  in a way to get rid of dependence in the independent variables.
# With SVD, we are guarenteed to have every feature independent!!

##-----SVD as a type of Regression: Total Least Squares------
# Remember the first SVD axis? That looked like a good fit.
#  And it actually is.  Just a different way of solving linear regression.
x = 1:25
y = x + 6*runif(length(x))

plot(x,y, main="X vs. Y", xlab = "X", ylab = "Y", pch = 16)

best_fit_2d = lm(y ~ x)
abline(best_fit_2d, lwd = 2, col = "red")

# Add vertical lines
for (i in 1:length(x)){
  x_temp = c(x[i], x[i])
  y_temp = c(best_fit_2d$fitted.values[i], y[i])
  lines(x_temp, y_temp, lty=2, col="red")
}

# Calculate the total least squares (SVD)
calculate_svd_reg = function(x,y){
  v = prcomp(cbind(x,y))$rotation
  x_mean = mean(x)
  y_mean = mean(y)
  slope = -v[1,2]/v[2,2]
  intercept = y_mean - x_mean * slope
  pred = slope * x + intercept
  return(list(coefs=c(slope, intercept), pred=pred))
}

y_tls = calculate_svd_reg(x,y)
svd_slope = y_tls$coef[1]
svd_intercept = y_tls$coefs[2]

abline(svd_intercept, svd_slope, col="green", lwd=2)

# Plot orthogonal lines
for (i in 1:length(x)){
  m = svd_slope
  b = svd_intercept
  x_temp = c(((m*y[i]+x[i]) - m*b)/(m**2 + 1),x[i])
  y_temp = c((m*(x[i] + m*y[i])+b)/(m**2 + 1),y[i])
  lines(x_temp, y_temp, lty=2, col="green")
}

##-----Compress Information via SVD-----

yoda = readPNG("yoda.png")
# Change 3d array to 1d grayscale
yoda = yoda[,,1]+yoda[,,2]+yoda[,,3]
yoda = yoda/max(yoda)

# Plot image
plot(c(0,1),c(0,1),type='n')
rasterImage(yoda, 0, 0, 1, 1)

# Compute SVD
yoda_svd = svd(yoda) # 768 X 1024 matrix of values between 0 an 1
# What is this doing?
# If we consider each row a 1024 dimenional point, then the image is
# a set of 768 points.

# We use SVD to decompose these points down to 1024 principal components (Really only 768),
#  Where the first component explains the direction of highest variance.
#

# Extract the decomposition
d = yoda_svd$d
u = yoda_svd$u
v = yoda_svd$v

# Check to make sure decomposition worked:
yoda_reconstructed = u %*% diag(d) %*% t(v)
mean(yoda - yoda_reconstructed) # close to zero.
# Plot reconstruction:
#  - note that the reconstruction has some rounding errors, so we re-normalize it:
yoda_reconstructed = yoda_reconstructed - min(yoda_reconstructed)
yoda_reconstructed = yoda_reconstructed/max(yoda_reconstructed)
plot(c(0,1),c(0,1),type='n')
rasterImage(yoda_reconstructed, 0, 0, 1, 1)

# Try representation by 1 SVD:
yoda_compressed <- (matrix(u[,1]) * d[1]) %*% t(matrix(v[,1]))

# Rescale because of numerical rounding issues
yoda_compressed = yoda_compressed - min(yoda_compressed) + 1E-16
yoda_compressed = yoda_compressed/max(yoda_compressed)

# Plot reconstruction
rasterImage(yoda_compressed, 0, 0, 1, 1) # Not very helpful!
# Note that all of these rows are the exact same, just are multiples of each other

# Now do multiple SVD components:

# Convert each row to a scalar multiple of the first n SVD componentd
i <- 2 # n SVD components
yoda_compressed <- u[,1:i] %*% diag(d[1:i]) %*% t(v[,1:i])

# Rescale because of numerical rounding issues
yoda_compressed = yoda_compressed - min(yoda_compressed) + 1E-16
yoda_compressed = yoda_compressed/max(yoda_compressed)

# Plot reconstruction
rasterImage(yoda_compressed, 0, 0, 1, 1)

# Saving size?
orig_size = object.size(yoda)
object.size(yoda_compressed)

# Actually we really want to calculate:
stored_size = object.size(u[,1:i]) + object.size(d[1:i]) + object.size(v[,1:i])

# For a grand total size savings of:
orig_size - stored_size
# Or
as.numeric((orig_size - stored_size)/orig_size) # percentage

# Clean up
rm(list = ls())
gc()

##-----Lasso Regression-----
predictor = log(data$price)
xfactors = model.matrix(log(price) ~ . - engine_location, data = data)[,-1]

log_price_lasso =glmnet(xfactors,predictor,alpha=1,family='gaussian')
# Lasso regression penalty formula form is given by using alpha = 1

# See how the variables vary with the different regularization factor, lambda
plot(log_price_lasso, xvar="lambda")

log_price_lasso # See all the possible choices

coef(log_price_lasso)[,20][coef(log_price_lasso)[,20]>1e-10]

# How do we know which to select?
# we will cover this more next class, but we can use cross validation,
#   which is a method of fitting subsets of the data to get a handle on the error
#   which in turn lets us find the best lambda to pick, which gives us the best fit:
log_price_lasso_cv = cv.glmnet(xfactors,predictor,alpha=1,family='gaussian')
plot(log_price_lasso_cv) # There is a minimum in MSE!
best_lambda = log_price_lasso_cv$lambda.min

best_coef = coef(log_price_lasso)[,log_price_lasso$lambda == best_lambda]
best_coef = best_coef[best_coef > 1e-10]

##-----Hurricane Evacuation Prediction-----

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

evac_lm = lm(evacuated ~ pets + mobile_home + tenure + years_educ, data = evac_df)
summary(evac_lm)

# What's the problem here?
range(evac_lm$fitted.values)


##------Logistic Regression------

evac_logit = glm(evacuated ~ pets + mobile_home + tenure + years_educ, data = evac_df, family = binomial)
# Remember that if each error is  bernoulli distributed, then
#  the distribution of many errors is binomial (multiple trials).
summary(evac_logit)
# Not a very good model because no variable is significantly different from zero!

# See what we predicted:
cutoff = 0.5
prediction = as.numeric(evac_logit$fitted.values>cutoff)

pred_evac_actual_evac = sum( (prediction == 1) & (evacuated == 1) )
pred_stay_actual_stay = sum( (prediction == 0) & (evacuated == 0) )

# Maybe say our accuracy is total right out of total:
accuracy = (pred_evac_actual_evac + pred_stay_actual_stay)/(nrow(evac_df))

# False positives
pred_evac_actual_stay = sum( (prediction == 1) & (evacuated == 0) )

# False negatives
pred_stay_actual_evac = sum( (prediction == 1) & (evacuated == 0) ) 


# How do we predict?
new_data = data.frame("pets" = 1,
                      "mobile_home" = 0,
                      "tenure" = 15,
                      "years_educ" = 18)
predict(evac_logit, newdata=new_data, type="response")


# How does probability change the more education a person has?
varied_education = seq(0,40,len=100)
prob_stay = sapply(varied_education, function(x){
  new_data = data.frame("pets" = 1,
                        "mobile_home" = 0,
                        "tenure" = 15,
                        "years_educ" = x)
  return(predict(evac_logit, newdata=new_data, type="response"))
})

plot(varied_education, prob_stay, type='l', lwd=2)
