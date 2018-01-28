## Introduction to Statistical Learning
## Chapter 3 Exercises

## Set working directory
setwd("C:/Users/Ben/Documents/GitHub/islr-exercises/")

## Load libraries
library(ISLR)
library(MASS)

## 8.
## a)
## i, ii, iii.
## Load data
auto=read.csv("Auto.csv")
auto$horsepower=as.numeric(auto$horsepower)
summary(lm(mpg~horsepower,data=auto))

## iv.
fit1=lm(mpg~horsepower,data=auto)
predict(fit1,data.frame(horsepower=c(98)),interval="confidence")
predict(fit1,data.frame(horsepower=c(98)),interval="prediction")

## b)
plot(auto$mpg,auto$horsepower)
abline(fit1,col="red")

## c)
plot(fit1)

## 9.
attach(auto)

## a)
pairs(auto)

## b)
cor(auto)

## c)
fit2=lm(mpg~.-name,data=auto)
summary(fit2)

## d)
plot(fit2)

## 10.
attach(Carseats)

## a)
fit3=lm(Sales~Price+Urban+US,data=Carseats)
summary(fit3)

## e)
fit4=lm(Sales~Price+US,data=Carseats)
summary(fit4)

## g)
confint(fit4)

## h)
plot(hatvalues(fit4))
plot(predict(fit4),residuals(fit4))

## 11.
set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)

## a)
fit5=lm(y~x+0)
summary(fit5)

## b)
fit6=lm(x~y+0)
summary(fit6)

## f)
fit5=lm(y~x)
fit6=lm(x~y)
summary(fit5)
summary(fit6)

## 13.
## a)
x=rnorm(100)

## b)
eps=rnorm(100,mean=0,sd=0.25)

## c)
y=-1+0.5*x+eps

## d)
plot(x,y)

## e)
fit7=lm(y~x)
summary(fit7)

## f)
abline(fit7,col='red')
abline(a=-1,b=0.5,col='blue')

## g)
fit8=lm(y~x+I(x^2))
summary(fit8)
anova(fit7,fit8)

## 14.
## a)
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)

## b)
plot(x1,x2)

## c)
fit9=lm(y~x1+x2)
summary(fit9)

## d)
fit9=lm(y~x1)
summary(fit9)

## e)
fit9=lm(y~x2)
summary(fit9)
vif(fit9)

## g)
x1=c(x1,0.1)
x2=c(x2,0.8)
y=c(y,6)

fit9=lm(y~x1+x2)
fit10=lm(y~x1)
fit11=lm(y~x2)

plot(hatvalues(fit9))
plot(predict(fit9),residuals(fit9))
plot(fit9)

## 15.
## b)
attach(Boston)
fit12=lm(crim~.,data=Boston)
summary(fit12)
