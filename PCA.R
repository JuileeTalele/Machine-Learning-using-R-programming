setwd("C:/Users/lenovo/Desktop/Praxis/Practice_doc/R")
cars = read.csv("cars.csv")

# A - 2 dimension case 
#============================================================================

data.2d = cars[,c("Horsepower","Weight")]
plot(data.2d,main= "The main data",pch=20,cex=0.8)
cor(cars$Horsepower,cars$Weight)

#Variance of the data
apply(data.2d,2,var)
sum(apply(data.2d, 2,var))
sum(apply(data.2d, 2,sd))

#----------------------------------------------------------------------------

# NORMALIZING THE DATA

for (i in 1:ncol(data.2d)) {
  data.2d[,i] = data.2d[,i] - mean(data.2d[,i])
}

plot(data.2d, main="The Normalized data")
abline(v=0,h=0,lty=2,col="red")

# Variance of the normalized data
apply(data.2d, 2,var)
sum(apply(data.2d, 2,var))


# STANDARDIZING THE DATA

data.2d = scale(data.2d)
plot(data.2d,main = "The Standardized data")
abline(v=0,h=0,lty=2,col="red")

# Variance of the standardized data

apply(data.2d, 2,var)
sum(apply(data.2d, 2,var))


# PCS for the 2-dimensional data (PC = principal component)
#=============================================================================

?princomp
data.2d = cars[,c("Horsepower","Weight")]
pc.2d = prcomp(data.2d)

# PC Loadings  (a from the notebook)
#-----------------------------------
pc.2d$rotation


# PC scores
#----------
pc = pc.2d$x
pc[1:5,]


# PCA on scale data
#------------------------------------------------------------------------------
# Method 1 :
data.2d = scale(cars[,c("Horsepower","Weight")])
pc.2d = prcomp(data.2d)

pc.2d$sdev^2/sum(pc.2d$sdev^2)*100
cumsum(pc.2d$sdev^2/sum(pc.2d$sdev^2)*100)

# Method 2 :
# By using scale = T in the prcomp fun
data.2d = cars[,c("Horsepower","Weight")]
pc.2d = prcomp(data.2d,scale =T)

pc.2d$sdev^2/sum(pc.2d$sdev^2)*100
cumsum(pc.2d$sdev^2)/sum(pc.2d$sdev^2)*100


# PCS FOR THE P-DIMENSIONAL DATA

pc = prcomp(cars[,-c(1,8,9)])
names(cars)
ncol(cars)-3

pc$rotation

pc$sdev^2

# TBC

cars = read.csv("cars.csv")
cars2 = cars[,c("Horsepower","Weight","Displacement")]
conmat = cov(cars2)
conmat

cov.egn = eigen(conmat)
cov.egn
cov.egn$values

pc = prcomp(cars2)
pc$rotation

pc$sdev^2

sum(apply(cars2,2,var))
sum(cov.egn$values)
