
getwd()
setwd("C:\\Users\\lenovo\\Desktop\\Praxis\\Practice_doc")

library(car)

names(mtcars)

model <- lm(mpg ~ cyl + disp + hp + drat + gear , data= mtcars)

vif(model)


#cyl     disp       hp     drat     gear 
#8.196937 7.436560 6.238280 2.953917 3.176515
# Shows cyl disp hp has highest vif i.e highly correlated 
# We drop it if vif exceeds 5
# But we dont directly 
# First drop predictor having highest vif

model <- lm(mpg ~ disp + hp + drat + gear , data= mtcars)

vif(model)

#disp       hp     drat     gear 
#6.478645 4.343206 2.791599 3.006843 
# Observe that the vif of hp has significantly decrease

model <- lm(mpg ~ hp + drat + gear , data= mtcars)

vif(model)

# The train error always reduces but we can never be sure of test errors.

model <- lm(mpg ~ wt + disp , data= mtcars)
summary(model)
Ry = summary(model)$r.squared

wt_r_sq <- lm(wt ~ disp, data = mtcars)
summary(wt_r_sq)
Ri = summary(wt_r_sq)$r.squared

xbar = mean(mtcars[['wt']])
ybar = mean(mtcars[['mpg']])

n = nrow(mtcars)
p = 2 

Y = sum((mtcars$mpg - ybar)^2)
X = sum((mtcars$wt - xbar)^2)

N = (1-Ry)*(Y)
D = (n-p-1)*(X)

S_E = N/D
se = S_E * (1/(1-Ri))

se

se^(1/2)


