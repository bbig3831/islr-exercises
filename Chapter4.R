## Introduction to Statistical Learning
## Chapter 3 Exercises

## Set working directory
setwd("C:/Users/Ben/Documents/GitHub/islr-exercises/")

## Load libraries
library(ISLR)
library(MASS)

## 10.
## a)
weekly=Weekly
summary(weekly)
pairs(weekly)

## b)
attach(weekly)
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            family='binomial',data=weekly)
summary(glm.fit)
table(glm.fit)

## c)
glm.probs=predict(glm.fit,type='response')
glm.pred=rep('Down',length(glm.probs))
glm.pred[glm.probs>0.5]='Up'
table(glm.pred,Direction)

## Accuracy
## (TP+TN)/Total
(54+557)/length(glm.pred)
## True positive (sensitivity/recall)
## TP/(TP+FN)
557/(557+48)
## False positive
## FP/(TN+FP)
430/(430+54)
## Specificity
## TN/(TN+FP)
54/(430+54)
## Precision
## TP/(TP+FP)
557/(430+557)
## Prevalence
## (TP+FN)/Total
(557+48)/length(glm.pred)

## d)
train=(Year<2009)
weekly.2009=weekly[!train,]
Direction.2009=Direction[!train]

## Training data
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=weekly[train,],family='binomial')
glm.probs=predict(glm.fit,weekly.2009,type='response')
glm.pred=rep('Down',length(glm.probs))
glm.pred[glm.probs>0.5]='Up'
table(glm.pred,Direction.2009)

## Accuracy
(31+17)/length(glm.pred)
## True positive rate
17/(17+44)
## False positive rate
12/(12+31)
## Specificity
31/(12+31)
## Precision
17/(12+17)
## Prevalence
(44+17)/length(glm.pred)

## e)
## Train on data
lda.fit=lda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=weekly,subset=train)
lda.pred=predict(lda.fit,weekly.2009)
lda.class=lda.pred$class
table(lda.class,Direction.2009)

## f)
qda.fit=qda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=weekly,subset=train)
qda.pred=predict(qda.fit,weekly.2009)
qda.class=qda.pred$class
table(qda.class,Direction.2009)

## g)
set.seed(1)
library(class)
train.X=cbind(Lag1,Lag2,Lag3,Lag4,Lag5,Volume)[train,]
test.X=cbind(Lag1,Lag2,Lag3,Lag4,Lag5,Volume)[!train,]
train.Direction=Direction[train]
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2009)

## h)
table(glm.pred,Direction.2009)
table(lda.class,Direction.2009)
table(qda.class,Direction.2009)
table(knn.pred,Direction.2009)

## 11.
## a)
attach(Auto)
Auto$mpg01=(mpg>median(mpg))

## b)
pairs(Auto)

## c)
library(caret)
trainIndex=createDataPartition(Auto$mpg01,p=0.7,
                               list=FALSE,times=1)
train=Auto[trainIndex,]
test=Auto[-trainIndex,]

## d)
lda.fit=lda(mpg01~horsepower+weight+displacement+
              acceleration,data=train)
lda.preds=predict(lda.fit,test)
lda.class=lda.preds$class
table(lda.class,test$mpg01)
mean(lda.class!=test$mpg01)

## e)
qda.fit=qda(mpg01~horsepower+weight+displacement+
              acceleration,data=train)
qda.preds=predict(qda.fit,test)
qda.class=qda.preds$class
table(qda.class,test$mpg01)
mean(qda.class!=test$mpg01)

## f) 
glm.fit=glm(mpg01~horsepower+weight+displacement+
              acceleration,data=train,family='binomial')
glm.probs=predict(glm.fit,test,type='response')
glm.pred=rep(FALSE,length(glm.probs))
glm.pred[glm.probs>0.5]=TRUE
table(glm.pred,test$mpg01)
mean(glm.pred!=test$mpg01)

## h)
train.X=train[,c('horsepower','weight','displacement',
                 'acceleration')]
test.X=test[,c('horsepower','weight','displacement',
               'acceleration')]
train.mpg01=train$mpg01
knn.pred1=knn(train.X,test.X,train.mpg01,k=1)
knn.pred3=knn(train.X,test.X,train.mpg01,k=3)
knn.pred5=knn(train.X,test.X,train.mpg01,k=5)
knn.pred7=knn(train.X,test.X,train.mpg01,k=7)
mean(knn.pred1!=test$mpg01)
mean(knn.pred3!=test$mpg01)
mean(knn.pred5!=test$mpg01)
mean(knn.pred7!=test$mpg01)

## 12.
## a)
Power=function(){
  print(2^3)
}

## b)
Power2=function(x,a){
  print(x^a)
}

## c)
Power2(10,3)
Power2(8,17)
Power2(131,3)

## d)
Power3=function(x,a){
  return(x^a)
}

## e) 
plot(x=seq(1,10),y=Power3(seq(1,10),2))

## f)
PlotPower=function(x,a){
  plot(x=x,y=Power3(x,a))
}

## 13.
attach(Boston)
Boston$crime01=(Boston$crim>median(Boston$crim))
pairs(Boston)
glm.fit=glm(crime01~black+ptratio+dis+nox,
            data=Boston)