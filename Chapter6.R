## Introduction to Statistical Learning
## Chapter 6 Exercises

library(ISLR)
library(MASS)
library(leaps)
library(glmnet)
library(pls)

## 8.
## a)
set.seed(2)
x=rnorm(100)
eps=rnorm(100)

## b)
y=2+2*x+2*x^2+2*x^3+eps
df=data.frame(y,x)

## c)
## Best subsets
regfit.full=regsubsets(y~poly(x,10,raw=T),df)
reg.sum=summary(regfit.full)

## Find min/max number of predictors
which.min(reg.sum$cp)
which.max(reg.sum$adjr2)
which.min(reg.sum$bic)

## Print coefficients
coef(regfit.full,3)

## Plot diagnostics of model errors
plot(reg.sum$adjr2,xlab='No. of Variables',
     ylab='R2 (Adj.)',type='b')
points(3,reg.sum$adjr2[3],col='red',cex=2,pch=20)
plot(reg.sum$bic,xlab='No. of Variables',
     ylab='BIC',type='b')
points(3,reg.sum$bic[3],col='red',cex=2,pch=20)

## d)
for(i in c('forward','backward')){
  regfit=regsubsets(y~poly(x,10,raw=T),df,method=i)
  reg.sum=summary(regfit)
  
  ## Find min/max number of predictors
  print(i)
  which.min(reg.sum$cp)
  which.max(reg.sum$adjr2)
  which.min(reg.sum$bic)
  
  ## Print coefficients
  coef(regfit,which.min(reg.sum$cp))
  
  ## Plot diagnostics of model errors
  plot(reg.sum$adjr2,xlab='No. of Variables',
       ylab='R2 (Adj.)',type='b')
  title(paste(i,'- R2 (Adj.)'))
  points(which.max(reg.sum$adjr2),reg.sum$adjr2[which.max(reg.sum$adjr2)],
         col='red',cex=2,pch=20)
  plot(reg.sum$bic,xlab='No. of Variables',
       ylab='BIC',type='b')
  title(paste(i,'- BIC'))
  points(which.min(reg.sum$bic),reg.sum$bic[which.min(reg.sum$bic)],
         col='red',cex=2,pch=20)
}

## e)
## Set lambda grid
grid=10^seq(10,-2,length=100)

## Fit lasso model
lasso.mod=glmnet(poly(x,10,raw=T),y,alpha=1,lambda=grid)
plot(lasso.mod)

## Cross validation
cv.out=cv.glmnet(poly(x,10,raw=T),y,alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min

## Coefficient estimates
lasso.best=glmnet(poly(x,10,raw=T),y,alpha=1,lambda=bestlam)
coef(lasso.best)


## f)
y=2+2*x^7+eps
## Best subset
regfit.full=regsubsets(y~poly(x,7,raw=T),data.frame(y,x))
reg.sum=summary(regfit.full)

## Find min/max number of predictors
which.min(reg.sum$cp)
which.max(reg.sum$adjr2)
which.min(reg.sum$bic)

## Print coefficients
coef(regfit.full,1)

## Plot diagnostics of model errors
plot(reg.sum$adjr2,xlab='No. of Variables',
     ylab='R2 (Adj.)',type='b')
points(1,reg.sum$adjr2[1],col='red',cex=2,pch=20)
plot(reg.sum$bic,xlab='No. of Variables',
     ylab='BIC',type='b')
points(1,reg.sum$bic[1],col='red',cex=2,pch=20)

## Fit lasso model
lasso.mod=glmnet(poly(x,10,raw=T),y,alpha=1,lambda=grid)
plot(lasso.mod)

## Cross validation
cv.out=cv.glmnet(poly(x,10,raw=T),y,alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min

## Coefficient estimates
lasso.best=glmnet(poly(x,10,raw=T),y,alpha=1,lambda=bestlam)
coef(lasso.best)

## 7.
## a)
attach(College)
train=sample(1:nrow(College),nrow(College)/2)
test=(-train)
college.train=College[train,]
college.test=College[test,]

## b)
lm.fit=lm(Apps~.,data=college.train)
lm.pred=predict(lm.fit,college.test)
mean((college.train[,'Apps']-lm.pred)^2)
