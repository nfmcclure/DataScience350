# SVD on features then regression on those features for crime prediction

crime_data = read.table('communities.data', sep=",", header=FALSE, na.strings = c("NA","?"))

crime_headers = read.table('crime_headers.txt')
names(crime_data) = crime_headers$V1

#drop features that are missing a majority of observations:
crime_data = crime_data[colSums(is.na(crime_data)) < 100]

# Disregard 'state' and 'communityname'.

# Consider 'ViolentCrimesPerPop' as the y-dependent variable.

# Create SVD features and perform linear regression.
