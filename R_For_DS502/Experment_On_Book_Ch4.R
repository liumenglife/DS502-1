Sys.setlocale("LC_ALL","Chinese")
library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
fix(Smarket)
cor(Smarket)  #变量必须都为定性，否则报错
cor(Smarket[,-9])  # -9的意思是
attach(Smarket)
plot(Smarket)
#glm represents generalized linear model
# biomial tells R to run a logistic regression rather than other
# generalized linear model
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]
glm.probs=predict(glm.fit,type="response") 
#预测在给定预测变量下市场上涨的概率 type表示只返回Y值
glm.probs[1:10] #输出前十个值
contrasts(Direction)  #R给定性/分类变量的负值
#c表示colon R里一个colon表示一个向量
glm.pred=rep("Down",1250)  #创建一个类/列表/向量 内含1250个Down的重复值
glm.pred[glm.probs>.5]="Up" #将向量glm.prob中大于0.5的都改为”up“
fix(glm.pred)
table(glm.pred,Direction)
mean(glm.pred==Direction) #计算正确预测的概率？

#___________________________________________________
#取2005年以前的数据做training 
train=(Year<2005)   #对象train是布尔向量内有1250个元素 小于2005为Ture
Smarket.2005=Smarket[!train,]  #2005年之后的做test
dim(Smarket.2005)
Direction.2005=Direction[!train]
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
#在数据集的子集中做training 用2005年后的数据做training
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)

glm.fit=glm(Direction~Lag1+Lag2,data=Smarket,
            family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)  #总体正确率

#————————LDA————————————————————
rm(Boston)  #remove the obj alreadly exsit
library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit,Smarket.2005)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)
sum(lda.pred$posterior[,1]>.5)
sum(lda.pred$posterior[,1]<.5)
lda.pred$posterior[1:20,1]
lda.class[1:20] #模型的后验概率对应市场下跌的概率
sum(lda.pred$posterior>.9)
#________QDA___________________
qda.fit=qda(Direction~Lag1+Lag2,Data=Smarket,subset=train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)
#________KNN_______________________
library(class)
#cbind=conlumn bind 可以将多个列向量合并为一个矩阵
train.X=cbind(Lag1,Lag2)[train,] #与训练数据相关的预测变量矩阵
test.X=cbind(Lag1,Lag2)[!train,] #与预测数据相关的预测变量矩阵
train.Direction=Direction[train] #包含训练观测类别标签的向量

set.seed(1)  #为了保证结果的课复制性
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)
knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)

#____另一个KNN__________________
#大篷车Caravan数据集
dim(Caravan)
names(Caravan)
attach(Caravan)
summary(Purchase)
standardized.X=scale(Caravan[,-86]) 
#标准化数据 KNN对变量范围非常敏感为了避免问题需要标准化数据
#删除86行 因为86行是定性行
var(Caravan[,1]) #第一列的var
var(Caravan[,2]) #第二列的var
var(standardized.X[,1])
var(standardized.X[,2]) #标准后的var， scale使每列均值为0，标准差为1
#将观测分为1000的test 和其他为train两个sets
test=1:1000 #构建向量
train.X=standardized.X[-test,]
test.X= standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
#_______K=1时候的情况——————————
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
table(knn.pred,test.Y)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
#————————K=3的时候情况——————————
knn.pred3=knn(train.X,test.X,train.Y,k=3)
table(knn.pred3,test.Y)  #最下行显示了sensitivity，真阳比例，
#对保险公司来说sensitivity是更加关注的值
knn.pred5=knn(train.X,test.X,train.Y,k=5)
table(knn.pred5,test.Y)

#使用Logistic regression做一遍Caravan
glm.fit=glm(Purchase~.,data=Caravan,family=binomial,subset=-test)
# names(glm.fit)
glm.probs=predict(glm.fit,Caravan[test,],type="response")
#names(glm.probs)
contrasts(Purchase) #确定定性变量在R中赋值
glm.pred=rep("No",1000)
#dim(glm.pred)
glm.pred[glm.probs>.5]="Yes"
#dim(glm.pred)
table(glm.pred,test.Y)
glm.pred[glm.probs>.25]="Yes"
#dim(glm.pred)
table(glm.pred,test.Y)
