library(ggplot2)
library(dplyr)
library(corrplot)
library(car)
cars<-read.csv("C:/project/cars_data.csv",header=T, fileEncoding="UTF-8-BOM")
View(cars)
round(cor(cars),2)->a;a
corrplot(a,method="circle",tl.col="black",tl.srt=30)

plot(cars$dist~cars$speed,xlab='speed',ylab='distance')
lm(dist~speed,data=cars)->car;car

#잔차
residuals(car)[1:50]
#회귀계수신뢰구간
confint(car,level=0.95)
#잔차제곱합
deviance(car)

summary(car)

plot(car,which=1)
plot(car,which=2)
plot(cars$speed, cars$dist, xlab = "speed", ylab = "distance")
abline(coef(car), col = "red")

######################################################################################################################################################

#토요타
tc<-read.csv("C:/project/ToyotaCorolla.csv",header=T, fileEncoding="UTF-8-BOM")
View(tc)
gon<-read.csv("C:/project/ToyotaCorolla.csv",header=T, fileEncoding="UTF-8-BOM")

#eda
hist(tc$Price,xlab='price',main= "Offer Price in EUROs ")
hist(tc$Age_08_04,xlab='age',main = "Age in months as in August 2004")
barplot(table(tc$Mfg_Month),xlab='month',main='Manufacturing Month')
barplot(table(tc$Mfg_Year),xlab='year',main='Manufacturing Year')
hist(tc$KM,xlab='km',main='Accumulated Killometers on odometer')
barplot(table(tc$Fuel_Type),col=c("lightyellow2","lightyellow3","lightyellow4"),xlab="Number of Fuel",main = "Fuel Type ")
hist(tc$HP,xlab='hp',main='')
barplot(table(tc$Met_Color),xlab='met color',main='')
barplot(table(tc$Color),xlab='color',main='')
barplot(table(tc$Automatic),xlab='Automatic',main='') 
hist(tc$CC,xlab='cc',main='') #이상치 제거
boxplot(tc$CC)
barplot(table(tc$Doors),xlab='doors',main='')
barplot(table(tc$Gears),xlab='Gears',main='') 
hist(tc$Quarterly_Tax,xlab='tax',main='')
hist(tc$Weight,xlab='Weight',main='')
barplot(table(tc$Guarantee_Period),xlab='Guarantee_Period',main='')

df$logcr <- log(df$cr)
tc$Price<-log(tc$Price)
hist(tc$Price,xlab='price',main= "Offer Price in EUROs ")

tc$Age_08_04<-(tc$Age_08_04)^2
hist(tc$Age_08_04,xlab='age',main = "Age in months as in August 2004")

tc$CC<-ifelse(tc$CC>10000,NA,tc$CC)
tc%>%filter(!is.na(CC))
hist(tc$CC,xlab='cc',main='')
barplot(table(tc$CC),xlab='cc',main='')

tc$HP<-ifelse(tc$HP>140,NA,tc$HP)
tc%>%filter(!is.na(HP))
hist(tc$HP,xlab='cc',main='')

tc$Weight<-ifelse(tc$Weight>1400,NA,tc$Weight)
tc%>%filter(!is.na(Weight))
hist(tc$Weight,xlab='Weight',main='')

tc%>%select(Price,Age_08_04,Mfg_Year,KM,HP,Quarterly_Tax,Weight,Guarantee_Period)->conti
View(conti)
conti2<-na.omit(conti)
cor(conti2)
round(cor(conti2),2)->a;a
corrplot(a,method="circle",tl.col="black",tl.srt=30)

sum(is.na(tc))
tc<-na.omit(tc)
sum(is.na(tc))

lmtc<-lm(Price~.,data=tc)

summary(tc)
str(tc)

h<-tc[,-c(1,2,15)]

model<-lm(Price~.,data=h)
summary(model)
h<-h[,-c(29)]
h<-h[,-c(11)]
h<-h[,-c(25)]

str(h)
View(h)
vif(model)

eunni<-step(model,direction="both")
summary(eunni)

aa<-lm(Price~.,data=h)
summary(aa)

aaa<-lm(Price~.,data=gon2)
summary(aaa)

gon2<-gon[,-c(1,2,15)]


























