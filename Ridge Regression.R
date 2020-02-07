#RIDGE REGRESSION {glmnet package}
#===================================================================================

#LIBRARY
#---------------------------------------------------------------------------
install.packages("glmnet")
require(glmnet)


#DATASET
#---------------------------------------------------------------------------
library(ISLR)
View(Hitters)
?Hitters
dim(Hitters)
summary(Hitters)


hit = Hitters
#Imputing the missing values by median
#hit$Salary[is.na(hit$Salary)] <- median(hit$Salary, na.rm=T)

hit = hit[!is.na(hit$Salary),]
summary(hit)



#glmnet FUNCTION FOR REGULARISED REGRESSION
#---------------------------------------------------------------------------
#glmnet(predictors, response, alpha = c(0,1), lambda)
#alpha = 0 => Ridge Regression
#alpha = 1 => LASSO

#Predictors
x = model.matrix(Salary ~ ., hit)[,-1] #model.matrix automatically creates dummy variables for factor variables
View(x)                                #glmnet() can accept only numerical inputs

#Response
y = hit$Salary


#Grid for lambda
grid = 10^seq(10,-2,length=100)


#Fitting a Ridge Regression Model
ridge = glmnet(x, y, alpha = 0, lambda = grid)


#NOTE: Ridge regression by default standardise the variables to bring the variables on the same scale.
#TO turn off this default setting use the argument standardize = FALSE


#--------------------------------------------------------------------------
#RIDGE REGRESSION COEFFICIENTS OVER LAMBDAS
#--------------------------------------------------------------------------

#For each values of lambda we have a vector ridge regression coefficient
#SO we have a matrix of 20x100 (coeff by lambda)
#This matrix can be accessed by coef() function

dim(coef(ridge))
coef(ridge)

ridge$lambda
ridge$lambda[50] #50th lambda value in the sequence
coef(ridge)[,50] #50th column showing the coef corresponding to this lambda value

#l2 norm
sqrt(sum(coef(ridge)[-1,50]^2)) #we ignore the intercept in norm


ridge$lambda[90] #90th lambda value in the sequence
coef(ridge)[,90] #90th column showing the coef corresponding to this lambda value

#l2 norm
sqrt(sum(coef(ridge)[-1,90]^2)) #we ignore the intercept in norm


#NOTE: Larger the value of lambda smaller the l2 norm of coefficients and vice versa.


ridge$lambda[100]
coef(ridge)[,100]


ridge$lambda[1]
coef(ridge)[,1]


#Coefficient FOR A PARTICULAR VALUE OF LAMBDA
#-------------------------------------------------------------------------------

#lambda = 50 (say)

predict(ridge, s=50, type="coefficients")


#OUT OF SAMPLE ACCURACY
#---------------------------------------------------------------------------

#Dividing the data into test and train
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

#Model
ridge1 <- glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)

#Making Prediction for lambda=4
pred <- predict(ridge1, s=4, newx = x[test,]) #Choosing an arbitrary lambda = 4

#MSE
mean((pred - y.test)^2) #This is the test MSE

#Base error rate (Error rate of a model with only intercept)
mean((y.test - mean(y[train]))^2)


#Lets take a very large value of lambda and see what happens
pred <- predict(ridge1, s=1e10, newx = x[test,]) #Choosing an arbitrary lambda = 10^10
mean((pred - y.test)^2)



#DIFFERENCE IN PERFORMANCE BETWEEN RIDGE REGRESSION (with lambda=4, abritrary) 
#AND OLS REGRESSION
#-------------------------------------------------------------------------------------
#Set lambda = 0, we get OLS regression
pred <- predict(ridge1, s=0, newx = x[test,]) 
mean((pred - y.test)^2)



#------------------------------------------------------------------------------------
#CHOOSING LAMBDA
#USE CROSS VALIDATION TO CHOOSE LAMBDA
#-------------------------------------------------------------------------------------

set.seed(0)
cv.out <- cv.glmnet(x[train,], y[train], alpha=0)
cv.out$lambda.min
bestlambda <- cv.out$lambda.min
bestlambda


#PREDICTION USING BEST LAMBDA

fin.pred = predict(ridge1, s = bestlambda, newx = x[test,])
mean((y.test - fin.pred)^2)


#FINAL MODEL
ridge.fin = glmnet(x, y, alpha = 0)
predict(ridge.fin, type="coefficient", s = bestlambda)




#=========================================================================
# LASSO
#=========================================================================

lasso <- glmnet(x[train,], y[train], alpha = 1, lambda = grid)

set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha=1)
bestlam = cv.out$lambda.min
lasso.pred = predict(lasso, s=bestlam, newx=x[test,])
mean((lasso.pred - y.test)^2)


lasso.fin = glmnet(x,y, alpha=1, lambda=grid)
lasso.coef = predict(lasso.fin, type="coefficients", s=bestlam)
lasso.coef


plot(lasso, xvar="lambda", label=TRUE)
plot(ridge, xvar="lambda", label=TRUE)



#=========================================================================
