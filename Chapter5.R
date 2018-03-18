## Introduction to Statistical Learning
## Chapter 3 Exercises

## Set working directory
setwd("C:/Users/Ben/Documents/GitHub/islr-exercises/")

## Load libraries
library(ISLR)
library(MASS)

## 5.
## a)
attach(Default)
glm.fit=glm(default~income+balance,data=Default,
            family='binomial')

## b)
## i.
library(caret)
trainIndex=createDataPartition(Default$default,p=0.5,
                               list=FALSE,times=1)
train=Default[trainIndex,]
test=Default[-trainIndex,]

## ii.
glm.fit2=glm(default~income+balance,data=train,
             family='binomial')

## iii.
glm.probs=predict(glm.fit,test,type='response')
glm.pred=rep("No",length(glm.probs))
glm.pred[glm.probs>0.5]="Yes"

## iv.
mean(glm.pred!=test$default)

## c)
train.test=function(p,n){
  set.seed(n)
  trainIndex=createDataPartition(Default$default,p=p,
                                 list=FALSE,times=1)
  train=Default[trainIndex,]
  test=Default[-trainIndex,]
  glm.fit2=glm(default~income+balance,data=train,
               family='binomial')

  glm.probs=predict(glm.fit,test,type='response')
  glm.pred=rep("No",length(glm.probs))
  glm.pred[glm.probs>0.5]="Yes"
  print(mean(glm.pred!=test$default))
}

train.test(0.5,1)
train.test(0.5,2)
train.test(0.5,3)

## d)
glm.fit3=glm(default~income+balance+student,data=train,
             family='binomial')

glm.probs=predict(glm.fit,test,type='response')
glm.pred=rep("No",length(glm.probs))
glm.pred[glm.probs>0.5]="Yes"

mean(glm.pred!=test$default)

## 6.
set.seed(1)

## a)
glm.fit4=glm(default~income+balance,data=Default,
             family='binomial')
summary(glm.fit4)

## b)
boot.fn=function(data,index){
  return(coef(glm(default~income+balance,data=Default,
                  family='binomial',subset=index)))
}

## c)
boot(Default,boot.fn,1000)

## 7.
## a)
attach(Weekly)
glm.fit5=glm(Direction~Lag1+Lag2,data=Weekly,
             family='binomial')

## b)
index=rep(TRUE,dim(Weekly)[1])
index[1]=FALSE
glm.fit6=glm(Direction~Lag1+Lag2,data=Weekly,
             family='binomial',subset=index)

## c)
glm.probs=predict(glm.fit6,Weekly[1,],type="response")
glm.pred=rep("Down",length(glm.probs))
glm.pred[glm.probs>0.5]="Up"
glm.pred==Weekly[1,'Direction']

## d)
results=rep(0,dim(Weekly)[1])
for(i in 1:dim(Weekly)[1]){
  glm.fit=glm(Direction~Lag1+Lag2,data=Weekly[-i,],
              family="binomial")
  glm.probs=predict(glm.fit,Weekly[i,],type="response")
  glm.pred=rep("Down",length(glm.probs))
  glm.pred[glm.probs>0.5]="Up"
  results[i]=(glm.pred==Weekly[i,"Direction"])
}

mean(results)

## 8.
## a)
set.seed(1)
y=rnorm(100)
x=rnorm(100)
y=x-2*x^2+rnorm(100)

## b)
df=data.frame(cbind(y,x))
glm.fit1=glm(y~x,data=df)
glm.fit2=glm(y~x+I(x^2),data=df)
glm.fit3=glm(y~x+I(x^2)+I(x^3),data=df)
glm.fit4=glm(y~x+I(x^2)+I(x^3)+I(x^4),data=df)

cv.err1=cv.glm(df,glm.fit1)
cv.err2=cv.glm(df,glm.fit2)
cv.err3=cv.glm(df,glm.fit3)
cv.err4=cv.glm(df,glm.fit4)

cv.err1$delta
cv.err2$delta
cv.err3$delta
cv.err4$delta

## d)
set.seed(2)
y=rnorm(100)
x=rnorm(100)
y=x-2*x^2+rnorm(100)
df=data.frame(cbind(y,x))
glm.fit1=glm(y~x,data=df)
glm.fit2=glm(y~x+I(x^2),data=df)
glm.fit3=glm(y~x+I(x^2)+I(x^3),data=df)
glm.fit4=glm(y~x+I(x^2)+I(x^3)+I(x^4),data=df)

cv.err1=cv.glm(df,glm.fit1)
cv.err2=cv.glm(df,glm.fit2)
cv.err3=cv.glm(df,glm.fit3)
cv.err4=cv.glm(df,glm.fit4)

cv.err1$delta
cv.err2$delta
cv.err3$delta
cv.err4$delta

## 9.
## a)
attach(Boston)
mu.hat=mean(Boston$medv)

## b)
se=sd(Boston$medv)/sqrt(length(Boston$medv))

## c)
boot.fn = function(data, index) return(mean(data[index]))
library(boot)
bstrap = boot(medv, boot.fn, 1000)
bstrap

## d)
t.test(medv)
c(bstrap$t0-2*0.4192,bstrap$t0+2*0.4192)

## e)
med.hat=median(Boston$medv)
med.hat

## f)
boot.med=function(data,index) return(median(data[index]))
bstrap.med=boot(medv,boot.med,1000)
bstrap.med

## g)
tp.hat=quantile(Boston$medv,probs=c(0.1))
tp.hat

## h)
boot.tp=function(data,index) return(quantile(data[index],probs=c(0.1)))
bstrap.tp=boot(medv,boot.tp,1000)
bstrap.tp
