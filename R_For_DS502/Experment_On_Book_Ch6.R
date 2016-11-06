rm()
library(ISLR)
library(leaps)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
# is.na() will return a TF vecter labeling NaN as Ture
# na.omit() remove observations with NaN values
Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))
# regsubset() use RSS find best subset
regfit.full=regsubsets(Salary~.,Hitters) #perform best subset selection using RSS
summary(regfit.full)
# control the output model # by nvmax
regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq
# plot all R2 curves
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Vars",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Nuber of Var",ylab="Adj. R2",type="l")
# use point() for adding points on plot
# which.max() give you the location of the max value in vector
# assign red to max adj R2 as follows
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11],col="red", cex=2, pch=20)
plot(reg.summary$cp,xlab="Nuber of Var",ylab="Cp",type="l")
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Nuber of Var",ylab="BIC",type="l")
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)
#return the coefficient estimates associated with the best 6-variablemodel
coef(regfit.full,6)
# regsubset()'s inner plot function
# black on the top are the best var.
par(mfrow=c(1,1))
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

###########################################
# Forward and Backward Stepwise Selection #
###########################################
regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)
coef(regfit.fwd,7)
coef(regfit.bwd,7)

#########################
# Choosing Among Models #
#########################
set.seed(1)
# 重复放回抽样
train=sample(c(TRUE,FALSE),nrow(Hitters),rep=TRUE) # True 大写
test=(!train)
#perform best subset selection to the training set
regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)
# model.matrix() 设计回归矩阵
#make a model matrix from the test data
test.mat=model.matrix(Salary~.,data=Hitters[test,])
#compute the validation set error for the best model for each model size
val.errors=rep(NA,19)

for(i in 1:19){
  #extract the coefficients from regfit.best for the best model of that size 'i'
  #there is no predict() to return the predicted response
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi #compute the predicted salary 
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
  }
val.errors  #MSE mean sum error
which.min(val.errors)


regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)


#write our own predict method
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}   # give a } in console if you miss it in scr.

regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19) #finally perform best subset selection on the full data
coef(regfit.best,10) #return more accurate coefficient estimates, and it would be different from the one using the training data

k=10 #10-fold CV
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE) #not equally spaced
folds=sample(c(rep(1:10,each=26),1,2,3),nrow(Hitters)) #almost equally spaced
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))
for(j in 1:k){
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19){
    pred=predict(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean( (Hitters$Salary[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors,2,mean) #average over the columns
mean.cv.errors
which.min(mean.cv.errors)
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
points(10,mean.cv.errors[10],col="red",cex=2,pch=20)
reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19) 
coef(reg.best,11) #obtain the best subset selecdtion on the full data and obtain the coefficient estimates of the 11-variable model


