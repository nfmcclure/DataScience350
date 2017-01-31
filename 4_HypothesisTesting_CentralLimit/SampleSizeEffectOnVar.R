# Let's test what happens with samples of different sizes.

# First declare a probability of success
p = (1/4)

# We are going to run this over many different sample sizes
N = round(seq(10, 10000, length=500))

variances = sapply(N, function(x){
  # For each sample size, x, create a vector of wins and losses (0's and 1's)
  win_loss_vec = sample(c(1,0), x, replace=TRUE, prob=c(p, 1-p))
  # Now return the variance of that sample
  return(var(win_loss_vec))
})

# Plot variances for each sample size
plot(N, variances, ylim=c(0.15, 0.35), pch=16)
abline(h=p*(1-p), col="red", lwd=2, lty=2) # Theoretical line
