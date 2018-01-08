## Introduction to Statistical Learning
## Chapter 2 Exercises

## Set working directory
setwd("C:/Users/Ben/Documents/GitHub/islr-exercises/")

## Load ISLR library
library(ISLR)

## 8. 
## b)
college=read.csv("College.csv")
fix(college)
rownames(college)=college[,1]
fix(college)
college=college[,-1]
fix(college)

## c)
## i.
summary(college)

## ii.
pairs(college[,1:10])


## iii.
plot(college$Private,college$Outstate)

## iv.
Elite=rep("No",nrow(college))
Elite[college$Top10perc>50]="Yes"
Elite=as.factor(Elite)
college=data.frame(college,Elite)
summary(college$Elite)
plot(college$Elite,college$Outstate)

## v.
par(mfrow=c(2,2))
hist(college$Expend)
hist(college$Expend,breaks=15)
hist(college$Grad.Rate,breaks=20)
hist(college$Room.Board,breaks=10)

## 9.
## a)
auto=read.csv("Auto.csv")
summary(auto)
sapply(auto,is.numeric)

## b)
sapply(auto[,sapply(auto,is.numeric)],range)

## c)
sapply(auto[,sapply(auto,is.numeric)],mean)
sapply(auto[,sapply(auto,is.numeric)],sd)

## d)
sapply(auto[-c(10:85),sapply(auto,is.numeric)],range)
sapply(auto[-c(10:85),sapply(auto,is.numeric)],mean)
sapply(auto[-c(10:85),sapply(auto,is.numeric)],sd)

## e)
pairs(auto)

## 10.
## a)
library(MASS)
?Boston

## b)
pairs(Boston)

## c)
## d)
sapply(Boston[,sapply(Boston,is.numeric)],range)

## e)
table(Boston$chas)

## f)
median(Boston$ptratio)

## g)
which.min(Boston$medv)
Boston[which.min(Boston$medv),]
sapply(Boston[,sapply(Boston,is.numeric)],range)

## h)
dim(Boston[Boston$rm>7,])
dim(Boston[Boston$rm>8,])
summary(Boston[Boston$rm>8,])
