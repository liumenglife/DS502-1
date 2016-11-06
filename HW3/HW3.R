#####Q2#####
#(a)
y=2
lambda=1
beta=seq(-10, 10, 0.1)
plot(beta, (y-beta)^2+lambda*abs(beta), pch=20, xlab="beta", ylab ="Lasso optimization")
beta.min=y-lambda/2
points(beta.min,(y-beta.min)^2+lambda*abs(beta.min), col="red", pch=4, lwd=5)
#(b)
y=2
lambda=1
beta=seq(-10, 10, 0.1)
plot(beta,(y-beta)^2+lambda*beta^2, pch=20, xlab="beta", ylab="Ridge optimization")
beta.min=y/(1+lambda)
points(beta.min,(y-beta.min)^2+lambda*abs(beta.min), col="red", pch=4, lwd=5)

###Q3####
#(a)
set.seed(1)
x=rnorm(100)
noise=rnorm(100)
#(b)
beta0=3
beta1=4
beta2=2
beta3=0.5
y=beta0+beta1*x+beta2*x^2+beta3*x^3+noise
#(c)
library(leaps)
data.full=data.frame(y=y,x=x)
regfit.full=regsubsets(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)+
                         I(x^7)+I(x^8)+I(x^9)+I(x^10),data=data.full,
                     nvmax=10)
reg.summary=summary(regfit.full)
par(mfrow=c(2,2))
#Cp
plot(reg.summary$cp,xlab="# of var.s", ylab="Cp",type="l")
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],
       col="red",cex=2,pch=20)
#BIC
plot(reg.summary$bic,xlab="# of var.s", ylab="BIC",type="l")
points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)],
       col="red",cex=2,pch=20)
#adj,R2
plot(reg.summary$adjr2,xlab="# of var.s",ylab="Adjusted R2",type="l")
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)],
       col="red",cex=2,pch=20)

coef(regfit.full, which.min(reg.summary$cp))
coef(regfit.full, which.min(reg.summary$bic))
coef(regfit.full, which.max(reg.summary$adjr2))






#(d)
data.full=data.frame(y=y,x=x)
regfit.fwd=regsubsets(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)
                      +I(x^7)+I(x^8)+I(x^9)+I(x^10),data=data.full,method="forward",
                       nvmax=10)
regfit.bwd=regsubsets(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)
                      +I(x^7)+I(x^8)+I(x^9)+I(x^10),data=data.full,method="backward",
                      nvmax=10)
reg.fwd.summary=summary(regfit.fwd)
reg.bwd.summary=summary(regfit.bwd)

par(mfrow=c(2,2))
#Cp
plot(reg.fwd.summary$cp,xlab="# of var.s", ylab="Cp",type="l")
points(which.min(reg.fwd.summary$cp),reg.fwd.summary$cp[which.min(reg.fwd.summary$cp)],
       col="red",cex=2,pch=20)
#BIC
plot(reg.fwd.summary$bic,xlab="# of var.s", ylab="BIC",type="l")
points(which.min(reg.fwd.summary$bic),reg.fwd.summary$bic[which.min(reg.fwd.summary$bic)],
       col="red",cex=2,pch=20)
#adj,R2
plot(reg.fwd.summary$adjr2,xlab="# of var.s",ylab="Adjusted R2",type="l")
points(which.max(reg.fwd.summary$adjr2),reg.fwd.summary$adjr2[which.max(reg.fwd.summary$adjr2)],
       col="red",cex=2,pch=20)
coef(regfit.fwd, which.min(reg.fwd.summary$cp))
coef(regfit.fwd, which.min(reg.fwd.summary$bic))
coef(regfit.fwd, which.max(reg.fwd.summary$adjr2))

par(mfrow=c(2,2))
#Cp
plot(reg.bwd.summary$cp,xlab="# of var.s", ylab="Cp",type="l")
points(which.min(reg.bwd.summary$cp),reg.bwd.summary$cp[which.min(reg.bwd.summary$cp)],
       col="red",cex=2,pch=20)
#BIC
plot(reg.bwd.summary$bic,xlab="# of var.s", ylab="BIC",type="l")
points(which.min(reg.bwd.summary$bic),reg.bwd.summary$bic[which.min(reg.bwd.summary$bic)],
       col="red",cex=2,pch=20)
#adj,R2
plot(reg.bwd.summary$adjr2,xlab="# of var.s",ylab="Adjusted R2",type="l")
points(which.max(reg.bwd.summary$adjr2),reg.bwd.summary$adjr2[which.max(reg.bwd.summary$adjr2)],
       col="red",cex=2,pch=20)
coef(regfit.bwd, which.min(reg.bwd.summary$cp))
coef(regfit.bwd, which.min(reg.bwd.summary$bic))
coef(regfit.bwd, which.max(reg.bwd.summary$adjr2))
par(mfrow=c(1,1))
#(e)
library(glmnet)
xmat=model.matrix(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)
                  +I(x^7)+I(x^8)+I(x^9)+I(x^10),data=data.full)[,-1]
cv.lasso=cv.glmnet(xmat,y,alpha=1)
plot(cv.lasso)
bestlamda=cv.lasso$lambda.min
bestlamda
fit.lasso=glmnet(xmat,y,alpha=1)
predict(fit.lasso,s=bestlamda,type="coefficients")[1:11,]

#(f)
beta7 = 2
y=beta0+beta7*x^7+noise
data.full=data.frame(y=y,x=x)
regfit.full=regsubsets(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)
                       +I(x^7)+I(x^8)+I(x^9)+I(x^10),data=data.full,
                       nvmax=10)
reg.summary=summary(regfit.full)
par(mfrow=c(2,2))
#Cp
plot(reg.summary$cp,xlab="# of var.s", ylab="Cp",type="l")
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],
       col="red",cex=2,pch=20)
#BIC
plot(reg.summary$bic,xlab="# of var.s", ylab="BIC",type="l")
points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)],
       col="red",cex=2,pch=20)
#adj,R2
plot(reg.summary$adjr2,xlab="# of var.s",ylab="Adjusted R2",type="l")
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)],
       col="red",cex=2,pch=20)
coef(regfit.full, 1)
coef(regfit.full, 2)
coef(regfit.full, 4)


xmat=model.matrix(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)+
                  +I(x^7)+I(x^8)+I(x^9)+I(x^10),data=data.full)[,-1]
cv.lasso=cv.glmnet(xmat,y,alpha=1)
bestlambda=cv.lasso$lambda.min
bestlambda
fit.lasso=glmnet(xmat,y,alpha=1)
predict(fit.lasso,s=bestlambda,type="coefficients")[1:11,]
















