#Q4.7_10_____________________________
library(ISLR)
summary(Weekly)
names(Weekly)
cor(Weekly[,-9])
attach(Weekly)
plot(Volume)
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Weekly,family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,type="response")
dim(Weekly)
contrasts(Direction)
glm.pred=rep("Down",1089)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction)

train=(Year<2009)
Weekly.2009=Weekly[!train,]
Direction.2009=Direction[!train]
glm.fit1=glm(Direction~Lag2,family=binomial,data=Weekly,subset=train)
glm.probs1=predict(glm.fit1,Weekly.2009,type="response")
glm.pred1=rep("Down",length(glm.probs1))
glm.pred1[glm.probs1>.5]="Up"
summary(glm.fit1)
table(glm.pred1,Direction.2009)
mean(glm.pred1==Direction.2009)

library(MASS)
lda.fit=lda(Direction~Lag2,data=Weekly,subset=train)
lda.fit
lda.pred=predict(lda.fit,Weekly.2009)
table(lda.pred$class,Direction.2009)
mean(lda.pred$class==Direction.2009)

qda.fit=qda(Direction~Lag2,data=Weekly,subset=train)
qda.fit
qda.pred=predict(qda.fit,Weekly.2009)
table(qda.pred$class,Direction.2009)
mean(qda.pred$class==Direction.2009)

library(class)
train.X=as.matrix(Lag2[train])                             #也可以使用as.matrix生成矩阵 
test.X=as.matrix(Lag2[!train])
train.Direction=Direction[train]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2009)
mean(knn.pred==Direction.2009)

#Logistic regression with Lag2:Lag1
glm.fit2=glm(Direction~Lag2:Lag1,data=Weekly,family=binomial,subset=train)
glm.probs2=predict(glm.fit2,Weekly.2009,type="response")
glm.pred2=rep("Down",length(glm.probs2))
glm.pred2[glm.probs2>.5]="Up"
table(glm.pred2,Direction.2009)
mean(glm.pred2==Direction.2009)

#LDA with Lag2:Lag1
lda.fit2=lda(Direction~Lag2:Lag1,data=Weekly,subset=train)
lda.pred2=predict(lda.fit2,Weekly.2009)
table(lda.pred2$class,Direction.2009)
mean(lda.pred2$class==Direction.2009)

# QDA with sqrt(abs(Lag2))
qda.fit2 <- qda(Direction ~ Lag2 + sqrt(abs(Lag2)), 
                data = Weekly, subset = train)
qda.pred2 <- predict(qda.fit2, Weekly.2009)
table(qda.pred2$class, Direction.2009)
mean(qda.pred2$class==Direction.2009)

#KNN K=10
knn.pred2=knn(train.X,test.X,train.Direction,k=10)
table(knn.pred2,Direction.2009)
mean(knn.pred2==Direction.2009)

#KNN K=50
knn.pred3=knn(train.X,test.X,train.Direction,k=50)
table(knn.pred3,Direction.2009)
mean(knn.pred3==Direction.2009)


#Q4.7____11_________________________
data(Auto)
mpg01=rep(0,length(mpg))
mpg01[mpg>median(mpg)]=1
Auto<-data.frame(Auto,mpg01)
plot(Auto)
cov(Auto[,-9])
boxplot(cylinders~mpg01,data=Auto,main="Cylinders vs mpg01")
boxplot(displacement~mpg01,data=Auto,main="Displacement vs mpg01")
boxplot(horsepower ~ mpg01, data = Auto, main = "Horsepower vs mpg01")
boxplot(weight ~ mpg01, data = Auto, main = "Weight vs mpg01")
boxplot(acceleration ~ mpg01, data = Auto, main = "Acceleration vs mpg01")
boxplot(year ~ mpg01, data = Auto, main = "Year vs mpg01")

train =(year %% 2 == 0)
Auto.train =Auto[train, ]
Auto.test =Auto[!train, ]
mpg01.test =mpg01[!train]

lda.fit=lda(mpg01 ~ cylinders + weight + displacement + horsepower,
            data = Auto, subset = train)
lda.fit
lda.pred=predict(lda.fit, Auto.test)
table(lda.pred$class, mpg01.test)
mean(lda.pred$class!=mpg01.test)

qda.fit=qda(mpg01 ~ cylinders + weight + displacement + horsepower,
            data = Auto, subset = train)
qda.fit
qda.pred=predict(qda.fit, Auto.test)
table(qda.pred$class, mpg01.test)
mean(qda.pred$class!=mpg01.test)

glm.fit=glm(mpg01 ~ cylinders + weight + displacement + horsepower,
            data = Auto, subset = train)
glm.fit
glm.probs=predict(glm.fit, Auto.test)
glm.pred=rep(0,length(glm.probs))
glm.pred[glm.probs>.5]=1
table(glm.pred,mpg01.test)
mean(glm.pred!=mpg01.test)


train.X <- cbind(cylinders, weight, displacement, horsepower)[train, ]
test.X <- cbind(cylinders, weight, displacement, horsepower)[!train, ]
train.mpg01 <- mpg01[train]
set.seed(1)
pred.knn <- knn(train.X, test.X, train.mpg01, k = 1)
table(pred.knn, mpg01.test)
mean(pred.knn != mpg01.test)

pred.knn <- knn(train.X, test.X, train.mpg01, k = 10)
table(pred.knn, mpg01.test)
mean(pred.knn != mpg01.test)

pred.knn <- knn(train.X, test.X, train.mpg01, k = 100)
table(pred.knn, mpg01.test)
mean(pred.knn != mpg01.test)
