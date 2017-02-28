##--------------------------------------------
##
## Lecture 7 R methods
##
## Class: PCE Data Science Methods Class
##
##--------------------------------------------

setwd('E:/Work/Teaching/PCE_Data_Science/7_TimeSeries_SpatialStats_Bayes')

# Load Libraries
library(TSA)
library(forecast)
library(gstat)
library(akima)
library(spatstat)
library(deldir)
library(lattice)
library(geoR)
library(sp)
library(zoo)

##-------Time Series Introduction------

# Moving Window Averages

# Create some data
x = seq(1,10,len=200)
y = 5*sin(x) + sin(10*x) + 0.25*rnorm(100)

plot(x,y,main="Time Series Data", xlab="x", ylab="y", pch=16, cex = 0.25)

# Past moving window:
mov_avg_past = function(x,window_size){
  stopifnot(length(x)>=window_size)
  mov_avg = c()
  for (i in 1:length(x)){
    if (i<window_size){
      temp_avg = mean(x[1:i])
    }else{
      temp_avg = mean(x[(i-window_size):(i)])
    }
    mov_avg = c(mov_avg, temp_avg)
  }
  return(mov_avg)
}

mov_avg_past5 = mov_avg_past(y,window_size = 5)
mov_avg_past10 = mov_avg_past(y,window_size = 10)
mov_avg_past25 = mov_avg_past(y,window_size = 25)
mov_avg_past50 = mov_avg_past(y,window_size = 50)

lines(x,mov_avg_past5, col="red", lwd=2)
lines(x,mov_avg_past10, col="blue", lwd=2)
lines(x,mov_avg_past25, col="green", lwd=2)
lines(x,mov_avg_past50, col="purple", lwd=2)
legend('bottomright', c('5 units', '10 units', '25 units', '50 units'),
       lty=c(1,1,1,1), lwd=c(2,2,2,2), col=c('red','blue','green','purple'))

# Let's look at the residuals!
time_resid_5 = y - mov_avg_past5
time_resid_10 = y - mov_avg_past10
time_resid_25 = y - mov_avg_past25
time_resid_50 = y - mov_avg_past50

# Plot of the residuals vs points(mov avg)
plot(time_resid_50, mov_avg_past5, pch = 16, cex = 0.5, col="red")
# This plot isn't too helpful, because the result isn't linear...

# Look at residuals over time:
plot(x, time_resid_5, pch = 16, cex = 0.5, col="red")
plot(x, time_resid_10, pch = 16, cex = 0.5, col="blue")
plot(x, time_resid_25, pch = 16, cex = 0.5, col="green")
plot(x, time_resid_50, pch = 16, cex = 0.5, col="purple")
# Notice the wider the window, the more of a trend we see.

# Note that there is a function in the 'zoo' package that does this for you:
#library(zoo)
?rollapply
zoo_mov_avg5 = rollapply(y, width = 5, by = 1, FUN= mean, align = "left")
plot(x[5:length(x)],zoo_mov_avg5)
lines(x,mov_avg_past5, col="red", lwd=2) # They are the same!
points(x,y,pch=16, cex=0.5)

# Note that rollapply does not deal with the ends of moving average at all.
# One way to deal with this is to 'pad' with NAs
zoo_mov_avg5 = rollapply(c(rep(NA, 4),y), width = 5, by = 1, FUN = mean, na.rm = TRUE, align = "left")
plot(x,zoo_mov_avg5)
lines(x,mov_avg_past5, col="red", lwd=2)
points(x,y,pch=16, cex=0.25)

##-----Fourier Transform-----
# Create some data
period1 = 3
period2 = 0.5
x = seq(0,15,len=5000)
y = sin((1/period1)*(2*pi)*x) + 0.5*sin((1/period2)*(2*pi)*x) + 0.05*rnorm(100)

plot(x,y, pch = 16, cex=0.25)

# Compute Fourier Transform (FFT) and plot the spectrum of frequencies
y_spectrum = spectrum(y)

plot(y_spectrum$freq[y_spectrum$freq<0.1], y_spectrum$spec[y_spectrum$freq<0.1], type = 'l')

# Find maximum frequencies
potential_frequencies = y_spectrum$freq[y_spectrum$freq<0.1 & y_spectrum$spec > 25]

# Find Periods
potential_periods = (1/potential_frequencies) * mean(diff(x))

# With the TSA package:
p = periodogram(y)
periods_data = data.frame(freq=p$freq, spec=p$spec)
periods_data = periods_data[order(-periods_data$spec),]
head(periods_data)
x_width = x[2]-x[1]

periods_data$seasonality = x_width / periods_data$freq

##------Simple Exponential Smoothing-----

dj_data = read.csv("dow_jones_data.csv")
dj_data$Date = as.Date(dj_data$Date, format="%Y-%m-%d")

plot(dj_data$Date, dj_data$DJIA, type="l")

# Let's consider 2014 onwards
dj_data = dj_data[dj_data$Date>=as.Date("2014-01-01"),]
plot(dj_data$Date, dj_data$DJIA, type="l",
     main="Dow Jones Value Daily", xlab="Date", ylab="Value")

# use forecast's ses() function:
exp_smooth1 = ses(dj_data$DJIA, alpha=0.05, h=31) # h is how many t-units to forecast out

names(exp_smooth1)
exp_smooth1["model"]
# Model returns the alpha, initial state, and the standard error
exp_smooth1["mean"]
# Mean returns the mean. May change with trend or seasonality
exp_smooth1["upper"]
exp_smooth1["lower"]
# Upper and lower return 2 series by default the:
exp_smooth1["level"]
# 80th and 95th percentile based on the standard error.

exp_smooth1["fitted"]
# Returns the fitted values.
exp_smooth1["residuals"]
# Returns the residuals

exp_smooth1["method"]


exp_smooth2 = ses(dj_data$DJIA, alpha=0.15, h=31)
exp_smooth3 = ses(dj_data$DJIA, alpha=0.95, h=31)

plot(exp_smooth1)
lines(exp_smooth1$fitted, col="blue")
lines(exp_smooth2$fitted, col="green")
lines(exp_smooth3$fitted, col="red")
legend('topleft', c('Original Data','alpha=0.05', 'alpha=0.15', 'alpha=0.95'),
       col=c('black','blue', 'green', 'red'), lty=c(1,1,1))

##----Double Exponential Smoothing----
# For future slide reference:
# Remember this is equivalent to ARIMA(0,1,1)
# P - Auto Regression
# D - Degree Integrated (1) # One level differencing
# Q - Moving Average (1) # Based only on the previous one

double_exp_smooth = Arima(dj_data$DJIA, order = c(0,1,1), seasonal=c(0,1,0))
double_exp_fit = dj_data$DJIA - double_exp_smooth$residuals # fitted values
plot(dj_data$Date, dj_data$DJIA,type="l", lwd=2,
     xlim=c(as.Date('2014-01-01'), as.Date('2015-06-01')), ylim=c(15000, 19000))
lines(dj_data$Date, double_exp_fit, col="red", lwd=2, lty=2)

# prediction
double_exp_pred = predict(double_exp_smooth, n.ahead = 30)

lines(seq(from=dj_data$Date[333], to=dj_data$Date[333]+30, by=1)[-1],
      double_exp_pred$pred, lwd=2, col='green')
# Add in standard error lines
lines(seq(from=dj_data$Date[333], to=dj_data$Date[333]+30, by=1)[-1],
      double_exp_pred$pred + double_exp_pred$se, lwd=2, col='green')
lines(seq(from=dj_data$Date[333], to=dj_data$Date[333]+30, by=1)[-1],
      double_exp_pred$pred - double_exp_pred$se, lwd=2, col='green')


##----Auto regressive Model----
lh   # Built in dataset
?lh
plot(lh)

# First Order Auto regressive
ar1 = ar(lh, order.max = 1)
ar1_fitted = lh - ar1$resid

# Plot outcome
plot(lh, lwd=2, xlim=c(0,60)) # extend xlim for prediction later
lines(ar1_fitted, lwd=2, col="red")

# Predict ahead n
ar1_pred = predict(ar1, n.ahead=10, se.fit = TRUE)
x_pred = length(ar1$resid) : (length(ar1$resid) + 10)
lines(x_pred,c(ar1_fitted[length(ar1_fitted)],ar1_pred$pred),
      lwd=2, col="red", lty=2)
lines(x_pred,c(ar1_fitted[length(ar1_fitted)],ar1_pred$pred + ar1_pred$se),
      lwd=2, col="red", lty=3)
lines(x_pred,c(ar1_fitted[length(ar1_fitted)],ar1_pred$pred - ar1_pred$se),
      lwd=2, col="red", lty=3)


##-----Auto regressive Moving average Model----
# First Order Auto regressive and Moving average
arma1 = arma(lh, order=c(1,1)) # Order is a length 2 vector, [1]=AR, [2]=MA
arma1_fitted = lh - arma1$resid

# Plot outcome
plot(lh, lwd=2, xlim=c(0,60))
lines(arma1_fitted, lwd=2, col="red")

# Hold off on predictions until later.


##----ARIMA(0,N,0)=Random Walk Model-----

# Generate ARIMA(0,1,0), random walk
y_I1 = arima.sim(list(order=c(0,1,0)),n=500)
plot(y_I1)

# 2nd order random walk
y_I2 = arima.sim(list(order=c(0,2,0)), n=500)
plot(y_I2)

##----ARIMA(N,0,0)= Autoregressive Model-----
# Careful with the AR coefficients, R will throw an error when
#   D = 0 (Integrated) and series is non-stationary
# Generate ARIMA(1,0,0)
y_AR1 = arima.sim(list(order=c(1,0,0), ar=0.8),n=500)
plot(y_AR1)

# 2nd order random walk
y_AR2 = arima.sim(list(order=c(2,0,0), ar=c(0.69, 0.3)), n=500)
plot(y_AR2)

# 5th order random walk
y_AR5 = arima.sim(list(order=c(5,0,0), ar=c(0.45, 0.2, 0.1, 0.1, 0.1)), n=500)
plot(y_AR5)

##----ARIMA(0,0,N) = Moving Average Model-----

# Generate ARIMA(0,0,1)
y_MA1 = arima.sim(list(ma=c(0.7)),n=500)
plot(y_MA1)

# 3rd order Moving Average
y_MA3 = arima.sim(list(ma=c(0.9,0.8,0.2)), n=500)
plot(y_MA3)

# 10th order Moving Average
ma_orders = sample(c(0.5,0.6,0.7,0.8), 10, replace=TRUE)
y_MA10 = arima.sim(list(ma=ma_orders), n=500)
plot(y_MA10)

##-----ARIMA(P,D,Q) X (P,D,Q) seasonality-----
# Load las vegas headcounts:
headcount = read.csv('JitteredHeadCount.csv', stringsAsFactors = FALSE)
# Aggregate headcounts by week:
headcount$DateFormat = as.Date(headcount$DateFormat, format="%m/%d/%Y")
headcount$week_count = floor(headcount$DayNumber/7.0)

headcount_weekly = aggregate(HeadCount ~ week_count, data = headcount, sum)

# Not a full last week, drop the last data point
headcount_weekly = headcount_weekly[1:52,]

# Fit time series:
headcount_arima = arima(headcount_weekly$HeadCount, order = c(1,1,1), seasonal = list(order=c(1,0,1)))
# What are the fitted coefficients?
headcount_arima$coef

# Get the predictions. Strange, but arima gives the resids, not the predictions:
arima_fitted = headcount_weekly$HeadCount - headcount_arima$residuals

arima_predictions = predict(headcount_arima, n.ahead=25)
x_pred = nrow(headcount_weekly) : (nrow(headcount_weekly) + 25)

plot(headcount_weekly$HeadCount, type="l", lwd = 2, col="black", xlim = c(0,80))
lines(arima_fitted, lwd = 2, col="red")
lines(x_pred, c(arima_fitted[length(arima_fitted)],arima_predictions$pred),
      lwd=2, col="red", lty=8)


##-----Time Series by Factors----
dj = read.csv("dow_jones_data.csv", stringsAsFactors = FALSE)
dj$Date = as.Date(dj$Date, format = "%Y-%m-%d")

# Create many different time factors
dj$day_count = as.numeric(dj$Date - min(dj$Date))
dj$week_count = floor(dj$day_count/7.0)
dj$month_count = floor(dj$day_count/30.5)
dj$year_count = as.numeric(format(dj$Date, format="%Y")) - 1990

dj$month = as.numeric(format(dj$Date, format="%m"))
dj$season = floor((dj$month-1) / 3)
dj$weekday = as.numeric(format(dj$Date, format = "%w"))
dj$week = as.numeric(format(dj$Date, format = "%W"))

# Market Bull Dates
bull_dates = seq(from = as.Date("2007-10-11"),
                 to = as.Date("2009-03-09"),
                 by = 1)

# Create Bull dates logical 0 - 1
dj$bull_market = as.numeric(dj$Date %in% bull_dates) # Is this cheating? Maybe.

# Create linear model:

dj_model = lm(DJIA ~ . - Date, data = dj)
summary(dj_model)

# Look at plot
plot(dj$Date, dj$DJIA, type="l", lwd=2, main="DJIA",
     xlab="Time", ylab="DJIA")
lines(dj$Date, dj_model$fitted.values, lwd=2, lty=8, col="red")

# Closer view:
plot(dj$Date, dj$DJIA, type="l", lwd=2, main="DJIA",
     xlab="Time", ylab="DJIA",
     xlim=c(as.Date("2009-01-01"), as.Date("2009-12-31")),
     ylim=c(6500,11000))
lines(dj$Date, dj_model$fitted.values, lwd=2, lty=8, col="red")
# Slightly lagging by a bit?
# Makes sense because of the "most important" variable.

# More autoregressive than 1 day?  Let's check:
DJIA_2_periods_ago = sapply(1:nrow(dj), function(x){
  if(x <= 2){
    return(dj$DJIA[1])
  }else{
    return(dj$DJIA[x-2])
  }
})
dj$two_days_ago = DJIA_2_periods_ago

dj_model_AR2 = lm(DJIA ~ . - Date, data = dj)
summary(dj_model_AR2)

# Can this predict well? Why not?
#
# - Turns out just slightly shifting the time series is
#   a good fit, as the stock market is a random walk and
#   is highly dependent on where it just was.

# - Also, can we predict the bull market variable? Probably not.

##-------Spatial Statistics------
# Clean up
rm(list = ls())
gc()

?coalash
data(coalash) # Coal ash is a pollutant generated from coal powered mining
head(coalash)


# Plot with labels
plot(coalash$x, coalash$y, type="n", lab=c(16,23,7), xlab="X",ylab="Y")
text(coalash$x, coalash$y, format(round(coalash$coalash,1)))
title("Coal Ash Percentages",cex=1.5)

# Median Polish
?medpolish

# Have to create a matrix of coalash values according to thier x-y positions:
coalash_mat = tapply(coalash$coalash, list(factor(coalash$x), factor(coalash$y)),
                     function(x) x)
coalash_med_pol = medpolish(coalash_mat, na.rm=TRUE) # Returns residuals....

coal_no_trend = coalash_mat - coalash_med_pol$residuals

# Look at a greyscale plot:
par(mfrow=c(1,2))
# We need the max's and mins for scaling the colors
zmin = min(coalash_mat[!is.na(coalash_mat)],coal_no_trend[!is.na(coal_no_trend)])
zmax = max(coalash_mat[!is.na(coalash_mat)],coal_no_trend[!is.na(coal_no_trend)])

image(x=1:max(coalash$x), y=1:max(coalash$y), coalash_mat,zlim=c(zmin,zmax),cex.axis=1.5,
      xlab="Columns",ylab="Rows",cex.lab=1.6,col=gray.colors(12))
title("Original Coal Ash",cex.main=1.5)
image(x=1:max(coalash$x),y=1:max(coalash$y),coal_no_trend,zlim=c(zmin,zmax),cex.axis=1.5,
      xlab="Columns",ylab="Rows",cex.lab=1.6,col=gray.colors(12))
title("Med Polish Coal Ash",cex.main=1.5)


##------Voronoi Diagrams-----
par(mfrow=c(1,1))
x = runif(10)
y = runif(10)

plot(x,y,pch=16, ylim=c(0,1),xlim=c(0,1))

?ppp
xy_ppp = ppp(x,y,c(0,1),c(0,1))
?deldir
tv = deldir(x,y,rw=c(0,1,0,1),plot=TRUE)
# Dashed lines are Voronoi Polygons (Dirichlet)
# Delaunay Triangulation are the solid lines

# get weights:
xy_weights_obj = dirichlet(xy_ppp)
tile.areas(xy_weights_obj)

##----Variogram----

# First, load and depict the 'walker lake data'.
walker_lake = read.table("walk470.txt",header=T)

plot(walker_lake$x,walker_lake$y,pch="",xlab="",ylab="",   # Plots the (x,y) locations with a "+" sign, no
     xlim=range(walker_lake$x),ylim=range(walker_lake$y),axes=F)# axis labels, specified axis limits, & no axes.
box()                                                       # Places a box around the plot.
for (i in 1:nrow(walker_lake)){
  text(walker_lake$x[i],walker_lake$y[i]+.30,               # Iteratively writes the V-values over the "+"
       paste(walker_lake$v)[i],cex=0.9)                     #   signs of character size 0.9 (default=1).
}

# Contour plot:
int.v = interp(walker_lake$x,walker_lake$y,walker_lake$v)    # Linear interpolation across region
contour(int.v,levels=seq(0,1500,100),xlab="",                # Creates a contour plot with the interpolated
        ylab="",xlim=range(walker_lake$x),ylim=range(walker_lake$y),labcex=1,axes=F)
box()
# you can see the effect that clusters of points have on the countour plot by adding the points:
points(walker_lake$x, walker_lake$y, pch=16)

# Compute Variograms

# First, change dataframe to coordinate object, from the 'sp' package
coordinates(walker_lake) = ~x+y
# Compute and plot the no-angle variogram (all points within a distance at any angle)
walker_variogram = variogram(v~x+y, data = walker_lake, width=10, cutoff=100)
plot(walker_variogram, type="l")

# Make u & v easier to reference
v = walker_lake$v
u = walker_lake$u
# Compute variogram at different angles
variogram_angles = seq(0,165,15)
# A side note:  R computes anges as angle clockwise from north!!!
#  This means our angle of zero we know is represented by angle=90 deg.
walker_variogram_angled = variogram(v~x+y, data=walker_lake,
                                    width=10, alpha=variogram_angles,
                                    tol.hor=15, cutoff=100)
plot(walker_variogram_angled, type="l")

# In order to use the variogram to predict, we have to fit the variogram.
# We will fit the variogram with a Spherical model (pretty standard)

# In order to fit the variogram, we have to make some parameter estimates:
#   -ratio: this is the ratio of the largest range over the shortest
#   -major angle: angle of the largest range
#   -sill: where everything levels out
#   -nugget: where the curves start
#   -range: how long it takes to level out
# We just have to guess (it most likely will converge with bad guesses)
#
#   Ratio guess:  largest = 60, smallest = 20, ratio = 60/20 = 3
#   Major angle: 165
#   Sill: 80,000
#   Nugget: 30,000
#   Range:  50

variogram_model = fit.variogram(walker_variogram_angled,
                                vgm(80000,"Sph",50,30000,
                                    anis = c(165,1/3)), fit.method = 2)
# Note that the angle parameters fall under the 'anis' parameter.
#   This stands for anisotropy, the term for a non-circular rose plot.

plot(walker_variogram_angled, variogram_model, main="Variogram by Angles")


##----Kriging-----
# To start doing kriging computationally, we need to define a bounded region around
#   our points to limit the computations.  We do this by computing a 'convex hull',
#   which is just the region defined by putting a rubber band around our points in space.

poly_bounds = chull(coordinates(walker_lake))

# Create a series of closely spaced gridded points in our region to do kriging predictions:
x = seq(2.5, 247.5, 5)
y = seq(2.5, 247.5, 5)
poly_grid = polygrid(x, y, coordinates(walker_lake)[poly_bounds,])

# Perform Kriging:
coordinates(poly_grid) = ~x+y
krige_results = krige(v ~ 1, walker_lake, newdata = poly_grid, model = variogram_model)

head(krige_results)


# Plot the interpolation of kriging results:
xpred = seq(min(x),max(x),length=100)
ypred = seq(min(y),max(y),length=100)
int_obs = interp(walker_lake$x,walker_lake$y,walker_lake$v,xo=xpred,yo=ypred)   # Interpolates the observed V-values.

# Interpolates the kriging prediction values over the coordinates in xo and yo.
int_pred = interp(coordinates(poly_grid)[,1],coordinates(poly_grid)[,2],krige_results$var1.pred,xo=xpred,yo=ypred)

# Interpolates the kriging standard errors over the coordinates in xo and yo.
int_se = interp(coordinates(poly_grid)[,1], coordinates(poly_grid)[,2],sqrt(krige_results$var1.var),xo=xpred,yo=ypred)

# Plot original, kriging results, and kriging error
par(mfrow=c(2,2))

# Computes the minimum V/predicted value
zmin = min(int_obs$z[!is.na(int_obs$z)],int_pred$z[!is.na(int_pred$z)])

# Computes the maximum V/predicted value
zmax = max(int_obs$z[!is.na(int_obs$z)], int_pred$z[!is.na(int_pred$z)])

image(int_obs,xlab="X",ylab="Y",cex.lab=1.6,main="Observed Concentrations",
      cex.main=1.6,zlim=c(zmin,zmax),col=rev(heat.colors(24)),cex=1)

# Creates a greyscale plot of the interpolated kriged V-values
image(int_pred,xlab="X",ylab="Y",cex.lab=1.6,main="Kriging Predicted Values",cex.main=1.6, 
      zlim=c(zmin,zmax),col=rev(heat.colors(24)),cex=1)

# Creates a greyscale plot of the interpolated kriging SE's
image(int_se,xlab="X",ylab="Y",cex.lab=1.6,main="Kriging Standard Errors",cex.main=1.6,
      col=rev(heat.colors(24)),cex=1)

# Why are there vertical bands in the standard error graph?
#  This has to do with our range ratio (varies with angle)
#  In fact, you can see the slight angle around 165 degrees clockwise from North.

##-----Clustering-----
par(mfrow=c(1,1))

# Complete Randomness
x_rand = runif(200)
y_rand = runif(200)
plot(x_rand, y_rand, xlim=c(0,1),ylim=c(0,1),
     main="Random X Y Points on Unit Square", pch=16)

# Clustering
x_cluster_centers = runif(5)
y_cluster_centers = runif(5)

x_cluster = c()
y_cluster = c()

while (length(x_cluster)<200){
  x_temp = runif(1)
  y_temp = runif(1)
  
  # Find the min distance from our random point to all the clusters
  min_dist = min(sqrt((x_cluster_centers - x_temp)**2 + (y_cluster_centers-y_temp)**2))
  
  if (min_dist < rnorm(1,mean=0,sd=0.05)){
    x_cluster = c(x_cluster, x_temp)
    y_cluster = c(y_cluster, y_temp)
  }
}

plot(c(0,1), c(0,1), type="n",main="Clustered X Y Points on Unit Square")
points(x_cluster, y_cluster, pch=16)

# Over-Regular (homogeneity)

x_reg = runif(1)
y_reg = runif(1)

while (length(x_reg)<200){
  x_temp = runif(1)
  y_temp = runif(1)
  
  # Find the min distance from our random point to all the clusters
  min_dist = min(sqrt((x_reg - x_temp)**2 + (y_reg-y_temp)**2))
  expected_even_dist = ( 4 * ( 1/sqrt(200) ) + 4 * ( sqrt(2)/sqrt(200) ) ) / 8
  
  if (min_dist > (expected_even_dist*0.65)){ # Allow for wiggle room
    x_reg = c(x_reg, x_temp)
    y_reg = c(y_reg, y_temp)
  }
}

plot(c(0,1), c(0,1), type="n",main="Homogeneous X Y Points on Unit Square")
points(x_reg, y_reg, pch=16)

# Ripley's K clustering coefficient!

h = 0.1 # Radius of sample
n = 100 # sample n circles

num_points_circle = function(x_points, y_points, x_temp, y_temp, h){
  distances = sqrt((x_points - x_temp)**2 + (y_points - y_temp)**2)
  return(sum(distances <= h))
}

ripley_k_at_h = function(x_points, y_points, h, n){
  stopifnot(length(x_points) == length(y_points))
  
  ripley_k_dist = c()
  
  ripley_k_dist = sapply(1:n, function(n_i){
    sample_index = sample(1:length(x_points), 1)
    x_center = x_points[sample_index]
    y_center = y_points[sample_index]
    num_points = num_points_circle(x_points, y_points, x_center, y_center, h)
  })
  
  return(ripley_k_dist)
}

# Random points:
ripley_rand = ripley_k_at_h(x_rand, y_rand, 0.2, 100)
mean(ripley_rand)

# Clustered Points:
ripley_cluster = ripley_k_at_h(x_cluster, y_cluster, 0.2, 100)
mean(ripley_cluster)

# Regular Points:
ripley_reg = ripley_k_at_h(x_reg, y_reg, 0.2, 100)
mean(ripley_reg)


##-----Ripley's K as a function of distance!----
h = seq(0.01, 0.5, length=100)

ripley_rand_line = sapply(h, function(h_i){
  mean(ripley_k_at_h(x_rand, y_rand, h_i, 100))
})

ripley_cluster_line = sapply(h, function(h_i){
  mean(ripley_k_at_h(x_cluster, y_cluster, h_i, 100))
})

ripley_reg_line = sapply(h, function(h_i){
  mean(ripley_k_at_h(x_reg, y_reg, h_i, 100))
})

# Plot results
plot(h, ripley_rand_line, type="l", lwd = 2,
     main="Ripley's K at varying h", ylab="h", xlab="Ripley's K")
lines(h, ripley_cluster_line, col="red", lwd=2)
lines(h, ripley_reg_line, col="blue", lwd=2)
grid()
legend('topleft', c('Random', 'Clustered', 'Over Regular'),
       lwd=c(2,2,2), col=c('black','red','blue'), lty=c(1,1,1))

# Notice the lines 'straighten out' at larger h values. And they even
#   get slightly closer together as h increases.
# This is because of the edge effects!  The larger h, the larger the circles,
#   and we are missing points outside of our region.


# To save us time, let's do this the easy way:
cluster_ppp = ppp(x_cluster, y_cluster)
cluster_ripleyK = Kest(cluster_ppp)

rand_ppp = ppp(x_rand, y_rand)
rand_ripleyK = Kest(rand_ppp)

reg_ppp = ppp(x_reg, y_reg)
reg_ripleyK = Kest(reg_ppp)

plot(c(0,0.25), c(0,0.25), type="n", xlab='h', ylab="Ripley's K",
     main="Ripley's K Function")
lines(rand_ripleyK$r, rand_ripleyK$iso, lwd=2, lty=1)
lines(cluster_ripleyK$r, cluster_ripleyK$iso, lwd=2, col="red")
lines(reg_ripleyK$r, reg_ripleyK$iso, lwd=2, col="blue")

lines(rand_ripleyK$r, rand_ripleyK$theo, lwd=2, lty=8, col="green")


# Since the theoretical value of Ripley's K is a square function (think A =  pi * r^2)
# We usually transform the data by taking the square root of it.  This is called
# Ripley's L function.
cluster_ripleyL = Lest(cluster_ppp)
rand_ripleyL = Lest(rand_ppp)
reg_ripleyL = Lest(reg_ppp)

plot(c(0,0.25), c(-0.08,0.15), type="n", xlab='h', ylab="Ripley's K",
     main="Ripley's K Function")
lines(rand_ripleyL$r, rand_ripleyL$iso - rand_ripleyL$r, lwd=2, lty=1)
lines(cluster_ripleyL$r, cluster_ripleyL$iso - cluster_ripleyL$r, lwd=2, col="red")
lines(reg_ripleyL$r, reg_ripleyL$iso - reg_ripleyL$r, lwd=2, col="blue")

lines(rand_ripleyL$r, rand_ripleyL$theo - rand_ripleyL$r, lwd=2, lty=8, col="green")

legend('bottomright',c('Theoretical', 'Random', 'Clustered', 'Over Regular'),
       lty=c(8,1,1,1), lwd=c(2,2,2,2), col=c('green','black', 'red', 'blue'))