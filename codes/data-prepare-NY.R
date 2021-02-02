# Data preparation

#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())



to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]
##  bathrooms - 36923, bedrooms - 3588
# beds - 477

#  data$license

# unique(data[c("license")])
