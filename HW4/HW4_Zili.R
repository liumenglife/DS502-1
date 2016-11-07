library(ISLR)
attach(Wage)
library(gam)
library(gamclass)

#Q3
par(mfrow=c(1,1))
x=seq(-2,2,0.1)
y=1+x+(-2)*(x-1)^2*I(x>1)
plot(x,y)
x=1
y1=1+x+(-2)*(x-1)^2*I(x>1)
points(x,y1,col="red",lwd=2)

#Q4
x=seq(-2,2,0.1)
y=1*I(x>=0&x<=2)-(x-1)*I(c(x>=1&x<=2))+
3*((x-3)*I(x>=3&x<=4)+I(x>=4&x<=5))
plot(x,y)

#Q5
#Performak=10k-foldCV
library(ISLR)
library(boot)
set.seed(1)
myfolds=rep(NA,10)
for(i in 1:10){
fit=glm(wage~poly(age,i),data=Wage)
myfolds[i]=cv.glm(Wage,fit,K=10)$delta[1]
}
plot(1:10,myfolds,xlab="Degreeoffreedom",ylab="TESTMSE",type="l")
points(which.min(myfolds),myfolds[which.min(myfolds)],col="red",cex=2,pch=20)

Sys.setlocale("LC_ALL","Chinese")

fit1=lm(wage~age,data=Wage)
fit2=lm(wage~poly(age,2),data=Wage)
fit3=lm(wage~poly(age,3),data=Wage)
fit4=lm(wage~poly(age,4),data=Wage)
fit5=lm(wage~poly(age,5),data=Wage)
anova(fit1,fit2,fit3,fit4,fit4,fit5)

plot(wage~age,data=Wage,col="darkgrey")
agelims=range(Wage$age)
age.grid=seq(from=agelims[1],to=agelim[2])
fit=lm(wage~poly(age,3),data=Wage)
preds=predict(fit,newdata=list(age=age.grid),se=T)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
lines(age.grid,preds,col="red",lwd=2)
matlines(age.grid,se.bands,lwd=1,col="red",lty=3)

#Q5b
my_cv=rep(NA,10)
for(i in 2:10){
Wage$age.cut=cut(Wage$age,i)
fit=glm(wage~age.cut,data=Wage)
my_cv[i]=cv.glm(Wage,fit,K=10)$delta[1]
}
plot(2:10,my_cv[-1],xlab="cuts",ylab="TestMSE",type="l")
points(which.min(my_cv),my_cv[which.min(my_cv)],col="red",pch=20,cex=2)

fit=lm(wage~cut(age,8),data=Wage)
preds=predict(fit,newdata=list(age=age.grid),se=T)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
plot(age,wage,xlim=agelims,cex=.5,col='darkgrey')
#title("8cuts",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

#Q7
summary(Wage)
par(mfrow=c(2,2))
plot(Wage$maritl,wage)
plot(Wage$jobclass,wage)
plot(Wage$health_ins,wage)
plot(Wage$race,wage)

fit0=gam(wage~lo(year,span=0.7)+s(age,5)+education,data=Wage)
fit1=gam(wage~lo(year,span=0.7)+s(age,5)+education+jobclass,data=Wage)
fit2=gam(wage~lo(year,span=0.7)+s(age,5)+education+maritl,data=Wage)
fit3=gam(wage~lo(year,span=0.7)+s(age,5)+education+jobclass+maritl,data=Wage)
anova(fit0,fit1,fit2,fit3)
par(mfrow = c(3, 2))
plot(fit3, se = T, col = "blue")
table(maritl, I(wage>250))
summary(fit3)
coef(fit3)

