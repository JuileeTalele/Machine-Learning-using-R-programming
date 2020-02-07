
# PCA - Reduce the dimension of the data without comprimising the variance
# (information in the data)
# To perform PCA on data having n<k we use NCI60 from ISLR package
# where n = No of observations and k = No of variable(predictors)

library(ISLR)
data("NCI60")
?NCI60

X = NCI60$data
Y = NCI60$labs

View(X)
dim(X)

class(NCI60)
class(NCI60$data)

NCI.pc = prcomp(X)
View(NCI.pc$x)
?prcomp

# To check the variance explained by each variable
round(NCI.pc$sdev^2/sum(apply(X,2,var))*100,4)

# Cumulative of the variance
cumsum(round(NCI.pc$sdev^2/sum(apply(X,2,var))*100,4))

#=========================================================================
# Output :
#[1] 14.8929 23.1936 29.7772 34.0832 37.9300 41.4367 44.3129
#[8] 47.1303 49.7687 51.9257 54.0257 56.0279 57.8584 59.6258
#[15] 61.2979 62.9083 64.4860 65.9400 67.3877 68.7994 70.1728
#[22] 71.4658 72.7263 73.9103 75.0898 76.1992 77.2609 78.2913
#[29] 79.3074 80.3015 81.2601 82.1986 83.0745 83.9411 84.7778
#[36] 85.6058 86.4170 87.2160 87.9746 88.7208 89.4507 90.1654
#[43] 90.8654 91.5460 92.2120 92.8417 93.4642 94.0564 94.6038
#[50] 95.1409 95.6586 96.1485 96.6227 97.0804 97.5100 97.9276
#[57] 98.3203 98.6816 99.0191 99.3112 99.5564 99.7902 99.9998
#[64] 99.9998

# Interpretation :
# We can say that 85.60% of variance of the data is explained by first 36 variables
# or 99.99% of variance of data is explained by 57 variable
# Where variance denote the information in the data

c = cov(X)
e = eigen(c)
round(e$values,4)
dim(c)

# In such case the eigen decomposition produces n-1 real and p-(n-1) null eigen
# values i.e we will have 63 non null values and 63 corresponding eigen vector
# and principle components.

