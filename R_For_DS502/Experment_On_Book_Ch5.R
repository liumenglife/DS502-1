rm()
Sys.setlocale("LC_ALL","Chinese")
library(ISLR)
# 验证集方法
# 从sample总随机抽取train和test
set.seed(1)
train=sample(392,196)
data(Auto)
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
summary(lm.fit)
# 用mean来计算验证集中196个观测的均方误差
mean((mpg-predict(lm.fit,Auto))[-train]^2)  
# poly函数产生二次多项式
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

# 留一交叉验证 （LOOCV）
# 在glm（family=“binomial”）方法可以执行逻辑斯蒂回归
# 没有加family选项就是一般线性回归
glm.fit = glm(mpg~horsepower,data=Auto)
coef(glm.fit)
lm.fit = lm(mpg~horsepower,data=Auto)
coef(glm.fit)
# glm可以与cv（cross variance）联合使用
rm()
library(boot) #cv方法所需lib
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta
# for做循环测试
cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error

# K折交叉验证
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10

# 自助法
# 定义函数求alpha
alpha.fn=function(data,index) {
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
  }
alpha.fn(Portfolio,1:100)

set.seed(1)
# 随机从1到100中有放回的抽取100个观测 相当于建立一个新的自助法数据集
alpha.fn(Portfolio,sample(100,100,replace=T)) 
# boot可以让上述方法运行多次 平求出采样的alpha
boot(Portfolio,alpha.fn,R=1000)

# 如何使用boot来measure 估计和预测系数的波动性
# 换句话说 如何估计线性回归模型的精度
# boot估计Auto模型beta1余beta0的波动性
#建立boot.fn(),
# 1. 输入Auto traing set返回线性回归beta1和0
# 2. 全set计算

# 通过全部auto来估计系数
boot.fn=function(data,index) 
return(coef(lm(mpg~horsepower,data=data,subset=index)))
boot.fn(Auto,1:392)
# 通过有放回随机抽样估计系数
set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
# boot函数计算1000个系数的自助法估计的标准误差（SE）
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower,data=Auto))$coef

boot.fn2=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(1)
boot(Auto,boot.fn2,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef
