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

## 9.
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

## c)
x=model.matrix(Apps~.,college.train)[,-1]
y=college.train$Apps
cv.out=cv.glmnet(x,y,alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

x.test=model.matrix(Apps~.,college.test)[,-1]
y.test=college.test$Apps
ridge.pred=predict(cv.out,s=bestlam,newx=x.test)
mean((ridge.pred-y.test)^2)

## d)
cv.out.lasso=cv.glmnet(x,y,alpha=1)
plot(cv.out.lasso)
bestlam.lasso=cv.out.lasso$lambda.min
lasso.pred=predict(cv.out.lasso,s=bestlam,newx=x.test)
mean((lasso.pred-y.test)^2)
coef(cv.out.lasso)

## e)
pcr.fit=pcr(Apps~.,data=college.train,scale=T,
            validation='CV')
validationplot(pcr.fit,val.type='MSEP')
pcr.pred=predict(pcr.fit,x.test,ncomp=10)
mean((pcr.pred-y.test)^2)

## f)
pls.fit=plsr(Apps~.,data=college.train,scale=TRUE,
             validation='CV')
summary(pls.fit)

validationplot(pls.fit,val.type='MSEP')
pls.pred=predict(pls.fit,x.test,ncomp=10)
mean((pls.pred-y.test)^2)

##
mse=function(obj){
  return(mean((obj-y.test)^2))
}

r2=function(obj){
  num=mean((college.test[,"Apps"]-obj)^2)
  den=mean((college.test[,"Apps"]-mean(college.test[,"Apps"]))^2)
  return(1-num/den)
}

barplot(c(mse(lm.pred),mse(ridge.pred),mse(lasso.pred),mse(pcr.pred),mse(pls.pred)), 
        col="red", names.arg=c("OLS", "Ridge", "Lasso", "PCR", "PLS"),
        main="Test MSE")

barplot(c(r2(lm.pred),r2(ridge.pred),r2(lasso.pred),r2(pcr.pred),r2(pls.pred)), 
        col="red", names.arg=c("OLS", "Ridge", "Lasso", "PCR", "PLS"),
        main="Test R-Squared")

## 10.
## a)
p=20
n=1000
x=matrix(rnorm(n*p),n,p)
beta=runif(p)
beta[seq(1,p,5)]=0
eps=rnorm(n)
y=x%*%beta+eps

## b)
train=sample(1:nrow(x),100)
test=(-train)
x.train=x[train,]
y.train=y[train,]
x.test=x[test,]
y.test=y[test,]

## c)
regfit.full=regsubsets(y~x,data=data.frame(c(y,x)),
                       subset=train,nvmax=p)
reg.summary=summary(regfit.full)
reg.summary$rsq
plot(reg.summary$rss,xlab='No. of Variables',
     ylab='RSS',type='b')

val.errors = rep(NA, p)
x_cols = colnames(x, do.NULL = FALSE,prefix="x")
for (i in 1:p) {
  coefi = coef(regfit.full, id = i)
  pred = as.matrix(x.train[, x_cols %in% names(coefi)]) %*% coefi[names(coefi) %in% 
                                                                    x_cols]
  val.errors[i] = mean((y.train - pred)^2)
}

## d)
plot(val.errors, ylab = "Training MSE", pch = 19, type = "b")

## e)
min.val=which.min(val.errors)
coefi=coef(regfit.full,id=min.val)
plot(coefi[!(names(coefi) %in% "(Intercept)")]-beta[-16],
     ylab="Difference in Coefficients")
