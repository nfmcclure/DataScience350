setwd('E:/Work/Teaching/PCE_Data_Science/5_HypothesisTesting_ConfidenceIntervals')

fb_data = read.csv('facebook_edge_list.csv', stringsAsFactors = FALSE)

fb_degree_list = as.numeric(table(fb_data$Source))

degree_mean = mean(fb_degree_list)

# Create k-s statistic function
ks_stat = function(x_min,x_max, dist_a, dist_b){
  x_seq = seq(x_min,x_max,len=1000)
  y_cdf1 = sapply(x_seq, function(x){
    sum(dist_a<x)/length(dist_a)
  })
  y_cdf2 = sapply(x_seq, function(x){
    sum(dist_b<x)/length(dist_b)
  })
  k_s_stat = max(abs(y_cdf1-y_cdf2))
  return(k_s_stat)
}

min_p = 1
max_p = 534

degree_poisson = dpois(fb_degree_list, degree_mean)

degree_ks = ks_stat(min_p, max_p, fb_degree_list, degree_poisson)


k_s_simulate = function(mean_lambda, length_vec){
  # Simulate two same poisson distributions with the same mean/length
  dist1 = rpois(length_vec, mean_lambda)
  dist2 = rpois(length_vec, mean_lambda)
  # Get k-s distance
  return(ks_stat(1, length_vec, dist1, dist2))
}

# Now that we can get 1 k-s stat under the null, let's get 10,000 of them.
k_s_distribution = sapply(1:1000, function(x){
  k_s_simulate(degree_mean, 534)
})

# Now we just have to sum up how many are equal to or bigger than 1.
# Note we can never get larger than 1).  Since none are larger than 1,
# we say our p-value is at most 1 / 10,000 = 0.0001.  So we reject the null.
