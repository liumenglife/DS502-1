library(ISLR)
attach(Wage)
fit=lm(wage~poly(age,4),data=Wage)
coef(summary(fit))
