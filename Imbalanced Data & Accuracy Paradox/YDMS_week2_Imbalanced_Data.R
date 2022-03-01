#데이터불러오기
credit<-read.csv("c:/project/creditcard.csv")

#EDA
View(credit)
summary(credit)

class2<-table(credit$Class)
barplot(class2)
library(dplyr)
credit%>%select(Time,V1:V28,Amount)->conticredit
View(conticredit)
conticredit<-na.omit(conticredit)
library(corrplot)
cor(conticredit)
round(cor(conticredit),2)->cc
corrplot(cc,method="circle",tl.col="black",tl.srt=30)

#데이터 분류
set.seed(1234)
idx <- sample(x = c("train", "test"),
              size = nrow(credit),
              replace = TRUE,
              prob = c(7, 3))

train<-credit[idx == "train", ]
test<-credit[idx == "test", ]
dim(train) 
dim(test)

#모델링
library(ROSE)
str(train)
str(test)
as.factor(train$Class)->train$Class
as.factor(test$Class)->test$Class
str(test)
str(train)
table(train$Class)
table(test$Class)

prop.table(table(train$Class))
prop.table(table(test$Class))

library(rpart)
library(rpart.plot)

treeimb<-rpart(Class~.,data=train)
pred.treeimb<-predict(treeimb,newdata = test)
pred.treeimb

accuracy.meas(test$Class,pred.treeimb[,2])
roc.curve(test$Class, pred.treeimb[,2], plotit=T)

#샘플링
data_balanced_over<-ovun.sample(Class~.,data=train,method="over",N=398290)$data
table(data_balanced_over$Class)
barplot(table(data_balanced_over$Class),main='Over Sampling')

data_balanced_under<-ovun.sample(Class~.,data=train,method="under",N=684)$data
table(data_balanced_under$Class)
barplot(table(data_balanced_under$Class),main='Under Sampling')

data_balanced_both <- ovun.sample(Class ~ ., data = train, method = "both", p=0.5, N=199487, seed = 1)$data 
table(data_balanced_both$Class)
barplot(table(data_balanced_both$Class),main='Under & Over Sampling')

data.rose <- ROSE(Class ~ ., data = train, seed = 1)$data
table(data.rose$cls)

#학습

tree.rose <- rpart(Class~.,data=data.rose)
tree.under <- rpart(Class~.,data=data_balanced_under)
tree.over <- rpart(Class~.,data=data_balanced_over)
tree.both <- rpart(Class ~ ., data = data_balanced_both)


pred.tree.rose <- predict(tree.rose, newdata = test)
pred.tree.under <- predict(tree.under, newdata = test)
pred.tree.over <- predict(tree.over, newdata = test)
pred.tree.both <- predict(tree.both, newdata = test)

#성능평가
accuracy.meas(test$Class,pred.tree.rose[,2])
roc.curve(test$Class,pred.tree.rose[,2])

accuracy.meas(test$Class,pred.tree.under[,2])
roc.curve(test$Class,pred.tree.under[,2])

accuracy.meas(test$Class,pred.tree.over[,2])
roc.curve(test$Class,pred.tree.over[,2])

accuracy.meas(test$Class,pred.tree.both[,2])
roc.curve(test$Class,pred.tree.both[,2])

roc.curve(test$Class,pred.tree.rose[,2])
roc.curve(test$Class,pred.tree.under[,2])
roc.curve(test$Class,pred.tree.over[,2])
roc.curve(test$Class,pred.tree.both[,2])













