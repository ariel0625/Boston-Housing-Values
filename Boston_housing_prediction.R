# Load standard libraries
library(tidyverse)
library(MASS) # Modern applied statistics functions

data(Boston)
summary(Boston)

#crim means per capital crime rate by town.
#zn means proportion of residential land zoned for lots over 25,000 sq.ft.
#indus means proportion of non-retail business acres per town.
#chas means Charles River dummy variable.
#nox means nitrogen oxides concentration.
#rm means average number of rooms per dwelling.
#age means proportion of owner-occupied units built prior to 1940.
#dis means weighted mean of distances to five Boston employment centres.
#rad means index of accessibility to radial highways.
#tax means full-value property-tax rate per $10,000.
#ptratio means pupil-teacher ratio by town.
#black means where BkBk is the proportion of blacks by town.
#lstat means lower status of the population.
#medv means median value of owner-occupied homes.
#There is no need for tidying data since there are no NA, missing values or
#weird mean or median values in each variable.

#response variable of interest
#MEDV - Median value of owner is the response variable of interest

f1=lm(formula=medv~crim,data=Boston)
summary(f1)

cor(Boston$crim,Boston$medv,method = c("pearson"))

f2=lm(formula=medv~zn,data=Boston)
summary(f2)

cor(Boston$zn,Boston$medv,method = c("pearson"))

f3=lm(formula=medv~indus,data=Boston)
summary(f3)

cor(Boston$indus,Boston$medv,method = c("pearson"))

f4=lm(formula=medv~chas,data=Boston)
summary(f4)

cor(Boston$chas,Boston$medv,method = c("pearson"))

f5=lm(formula=medv~nox,data=Boston)
summary(f5)

cor(Boston$nox,Boston$medv,method = c("pearson"))

f6=lm(formula=medv~rm,data=Boston)
summary(f6)

cor(Boston$rm,Boston$medv,method = c("pearson"))

f7=lm(formula=medv~age,data=Boston)
summary(f7)

cor(Boston$age,Boston$medv,method = c("pearson"))

f8=lm(formula=medv~dis,data=Boston)
summary(f8)

cor(Boston$dis,Boston$medv,method = c("pearson"))

f9=lm(formula=medv~rad,data=Boston)
summary(f9)

cor(Boston$rad,Boston$medv,method = c("pearson"))

f10=lm(formula=medv~tax,data=Boston)
summary(f10)

cor(Boston$tax,Boston$medv,method = c("pearson"))

f11=lm(formula=medv~ptratio,data=Boston)
summary(f11)

cor(Boston$ptratio,Boston$medv,method = c("pearson"))

f12=lm(formula=medv~black,data=Boston)
summary(f12)

cor(Boston$black,Boston$medv,method = c("pearson"))

f13=lm(formula=medv~lstat,data=Boston)
summary(f13)

cor(Boston$lstat,Boston$medv,method = c("pearson"))

pairs(~ medv + ptratio + black + lstat + dis + rm + crim, data = Boston, main = "Boston Data")
#lstat, rm and ptratio have a a statistically significant association


model <- lm(medv ~ crim+ zn + indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat, data = Boston)
summary(model)

#crim,zn,chas,nox,rm,dis,rad,tax,ptratio,black,lstat of p-value<0.05
#can reject the null hypothesis H0

#Take p-value as an example to illustrate the Estimate.
#For my result from(4), a multiple regression model,the p-value of crim is
#0.001087 and for my result from(3), a simple repression model, the p-value of
#crim is 2.2e-16, has been increased
#For my result from(4), a multiple regression model,the p-value of zn is
#0.000778 and for my result from(3), a simple repression model, the p-value of
#zn is 2.2e-16, has been increased
#For my result from(4), a multiple regression model,the p-value of indus is
#0.738288 and for my result from(3), a simple repression model, the p-value of
#zn is 2.2e-16, has been increased
#For my result from(4), a multiple regression model,the p-value of chas is
#0.001925 and for my result from(3), a simple repression model, the p-value of
#zn is 7.391e-05, has been increased
#For my result from(4), a multiple regression model,the p-value of nox is
#4.25e-06 and for my result from(3), a simple repression model, the p-value of
#zn is 2.2e-16, has been increased
#For my result from(4), a multiple regression model,the p-value of rm is
#2e-16 and for my result from(3), a simple repression model, the p-value of
#zn is 2.2e-16, has been decreased
#For my result from(4), a multiple regression model,the p-value of age is
#0.958229 and for my result from(3), a simple repression model, the p-value of
#zn is 2.2e-16, has been increased
10
#For my result from(4), a multiple regression model,the p-value of dis is
#6.01e-13 and for my result from(3), a simple repression model, the p-value of
#zn is 1.207e-08, has been increased
#For my result from(4), a multiple regression model,the p-value of rad is
#5.07e-06 and for my result from(3), a simple repression model, the p-value of
#zn is 2.2e-16, has been increased
#For my result from(4), a multiple regression model,the p-value of tax is
#0.001112 and for my result from(3), a simple repression model, the p-value of
#zn is 2.2e-16, has been increased
#For my result from(4), a multiple regression model,the p-value of ptratio is
#1.31e-12 and for my result from(3), a simple repression model, the p-value of
#zn is 2.2e-16, has been decreased
#For my result from(4), a multiple regression model,the p-value of black is
#0.000573 and for my result from(3), a simple repression model, the p-value of
#zn is 1.318e-14, has been increased
#For my result from(4), a multiple regression model,the p-value of lstat is
#2e-16 and for my result from(3), a simple repression model, the p-value of
#zn is 2.2e-16, has been decreased
#This is because in multiple regression model, variables are not independent so
#they may be affected by each other and lead to the change of p-value






