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

##-----Exploring data Visually----
# Bernoulli (Binomial with n = 1)
p = 0.75
n = 1000
bern_samples = rbinom(n, 1, p)
bern_sample_mean = sum(bern_samples)/length(bern_samples)
bern_sample_var = bern_sample_mean * (1-bern_sample_mean)
bern_var = p*(1-p)

# Binomial
N = c(5, 25, 75)
binom_samples = lapply(N, function(x) rbinom(n, x, p))
binom_sample_means = lapply(binom_samples, mean)
binom_means = N*p
binom_sample_vars = lapply(binom_samples, var)
binom_vars = N*p*(1-p)

# Compare Normal Approximation to binomial
par(mfrow=c(2,2))
for (i in 1:3){
  hist(binom_samples[[i]], main=paste(N[i],'Experiments'), freq=FALSE)
  x_norm = seq(0,N[i], by = 0.025)
  y_norm = dnorm(x_norm, mean=binom_means[i], sd=sqrt(binom_vars[i]))
  lines(x_norm, y_norm)
}

#Normal approximation to p(x=4)
pnorm(4.5, mean=binom_means[[1]], sd=sqrt(binom_vars[[1]])) -
  pnorm(3.5, mean=binom_means[[1]], sd=sqrt(binom_vars[[1]]))
#Empirical p(x=4)
sum(binom_samples[[1]]==4)/1000

#Normal approximation to p(x=18)
pnorm(18.5, mean=binom_means[[2]], sd=sqrt(binom_vars[[2]])) -
  pnorm(17.5, mean=binom_means[[2]], sd=sqrt(binom_vars[[2]]))
#Empirical p(x=18)
sum(binom_samples[[2]]==18)/1000

#Normal approximation to p(x=55)
pnorm(55.5, mean=binom_means[[3]], sd=sqrt(binom_vars[[3]])) -
  pnorm(54.5, mean=binom_means[[3]], sd=sqrt(binom_vars[[3]]))
#Empirical p(x=18)
sum(binom_samples[[3]]==55)/1000

# Poisson Distribution
lambda = c(1,5,25,100)
poisson_samples = lapply(lambda, function(x) rpois(n, x))
poisson_sample_means = lapply(poisson_samples, mean)
poisson_sample_vars = lapply(poisson_samples, var)

#Normal approximation
par(mfrow=c(2,2))
for (i in 1:4){
  hist(poisson_samples[[i]], main=paste('Lambda=',lambda[i]), freq=FALSE)
  x_norm = seq(0,10*lambda[i], by = 0.025)
  y_norm = dnorm(x_norm, mean=lambda[i], sd=sqrt(lambda[i]))
  lines(x_norm, y_norm)
}

# The Uniform Distribution
uniform_samples = runif(n)
par(mfrow=c(1,1))
plot(density(uniform_samples))
lines(c(0,0),c(0,1))
lines(c(0,1),c(1,1))
lines(c(1,1),c(0,1))

# The Normal Distribution
n_params = list(c(0,1),c(5,1),c(0,0.1),c(4,4))
norm_samples = lapply(n_params,function(x) rnorm(n,mean=x[1], sd=sqrt(x[2])))

par(mfrow=c(1,1))
colors = c('black', 'red', 'blue', 'green')
plot(density(norm_samples[[1]]), xlim=c(-3,9),ylim=c(0,1.25),
     col=colors[1], lwd=2, xlab='x', ylab='y', main='Plot of Normals')
for (i in 2:4){
  lines(density(norm_samples[[i]]), col=colors[i], lwd=2)
}
grid()
legend('topright',c('N(0,1)','N(5,1)','N(0,0.1)','N(4,4)'),
       lwd=2,lty=1,col=colors)

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