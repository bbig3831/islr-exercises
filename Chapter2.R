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

## b)