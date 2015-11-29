#HW 6 Hint
setwd('E:/Work/Teaching/PCE_Data_Science/6_Regression_FeatureSelection/HW')
library(pls)
library(glmnet)

# Retrieve Breast Cancer Expression Data From the following Study:
#http://www.ncbi.nlm.nih.gov/pubmed/21532620


micro_data=read.table("MicroArray.txt", header=TRUE)

dim(micro_data)

# Normalize each column
micro_data = scale(micro_data)

# Breast Cancer Samples:
cancer_samples = c(0,0,0,0,0,1,0,0,0,1,0,1,0,0,1,0,0,0,1)

# Think of this as ~ 10,000 possible variables (genes) to predict 19 outcomes.

# Convert to data.frame
micro_frame = data.frame(t(micro_data))
micro_frame$outcomes = cancer_samples

##-----Lasso Regression-----
# user glmnet, where family = 'binomial'

# Now use cv.glmnet to test different lasso cutoffs

# find the minumum lambda.min

# Find the coefficients that are greater than zero

# Plug this into the glm(...,family='binomial') to get the logistic outcome

# Compare with the real outcome, cancer_samples above