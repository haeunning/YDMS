a<-read.csv("C:/project/01.demographic.csv")
b<-read.csv("C:/project/02.services.csv")
c<-read.csv("C:/project/03.account.csv")
View(a)
View(b)
View(c)

summary(d)
summary(a)

library(dplyr)
a<-distinct(a)
summary(a)

merge(b,c,by='customerID')->d
merge(a,d,by='customerID')->t
View(t)
summary(t)
str(t)

na.omit(t)->t
summary(t)
sum(is.na(t))

t$MultipleLines<-ifelse(t$MultipleLines=="Yes","Yes",ifelse(t$MultipleLines=="No","No","No"))
t$OnlineSecurity<-ifelse(t$OnlineSecurity=="Yes","Yes",ifelse(t$OnlineSecurity=="No","No","No"))
t$OnlineBackup<-ifelse(t$OnlineBackup=="Yes","Yes",ifelse(t$OnlineBackup=="No","No","No"))
t$DeviceProtection<-ifelse(t$DeviceProtection=="Yes","Yes",ifelse(t$DeviceProtection=="No","No","No"))
t$TechSupport<-ifelse(t$TechSupport=="Yes","Yes",ifelse(t$TechSupport=="No","No","No"))
t$StreamingTV<-ifelse(t$StreamingTV=="Yes","Yes",ifelse(t$StreamingTV=="No","No","No"))
t$StreamingMovies<-ifelse(t$StreamingMovies=="Yes","Yes",ifelse(t$StreamingMovies=="No","No","No"))

t$Churn<-ifelse(t$Churn=="Yes",1,0)

#modeling

h<-t[,-c(1)]
View(h)
set.seed(502)
idx <- sample(x = c("train", "test"),
              size = nrow(h),
              replace = TRUE,
              prob = c(7, 3))

train_1<-h[idx == "train", ]
test_2<-h[idx == "test", ]
dim(train_1) 
dim(test_2)

logit_train_1<-glm(Churn~.,data=train_1,family='binomial')
summary(logit_train_1)
step1 <- step(logit_train_1, direction = "both")
summary(step1)


library(moonBook)
extractOR(step1)
ORplot(step1)

logit.train_1<-glm(Churn~SeniorCitizen+MultipleLines+InternetService
                 +StreamingTV+StreamingMovies+tenure+Contract+PaperlessBilling+MonthlyCharges ,data=train_1,family='binomial')

summary(logit.train_1)
step.train_1<-step(logit.train_1,direction = "both")
summary(step.train_1)

extractOR(step.train_1)
ORplot(step.train_1)

pred_1 <- predict(step.train_1, train_1,type="response")
pred_1

predict_1 <- ifelse(pred_1>0.5, 1, 0)
predict_1
as.factor(predict_1)->predict_1
as.factor(train_1$Churn)->train_1$Churn
library(caret)
confusionMatrix(predict_1, train_1$Churn)


pred_test_2 <- predict(step.train_1, test_2,type="response")
pred_test_2
pred_test_2<-ifelse(pred_test_2>0.5, 1, 0)
as.factor(pred_test_2)->pred_test_2
as.factor(test_2$Churn)->test_2$Churn
confusionMatrix(pred_test_2, test_2$Churn)

pred_test_3 <- predict(step.train_1, test_2,type="response")
pred_test_3
pred_test_3<-ifelse(pred_test_3>0.4, 1, 0)
as.factor(pred_test_3)->pred_test_3
as.factor(test_2$Churn)->test_2$Churn
confusionMatrix(pred_test_3, test_2$Churn)



