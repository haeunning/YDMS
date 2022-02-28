#데이터 불러오기
boston<-read.csv("c:/project/Boston_houseprice.csv")
View(boston)
summary(boston)

#데이터별 비교
library(ggplot2)
library(dplyr)
library(corrplot)
#1_CRIM
hist(boston$CRIM,col='grey34',main='1000명당 범죄발생 횟수',xlab='CRIM',breaks=35)
boxplot(boston$CRIM,col='grey34',main='CRIM')

#2_ZN
hist(boston$ZN,col='yellow green',main='ZN',xlab='ZN')
boxplot(boston$ZN,col='yellow green',main='ZN')

#3_INDUS
hist(boston$INDUS,col='tan',main='INDUS',xlab='INDUS')
boxplot(boston$INDUS,col='tan',main='INDUS')

#4_CHAS
chas<-table(boston$CHAS)
barplot(chas,xlab='chas',main='CHAS',col=c('snow','powder blue'))
table(boston$CHAS)

#5_NOX
hist(boston$NOX,col='light cyan4',main='NOX',xlab='NOX')
boxplot(boston$NOX,col='light cyan4',main='NOX')

#6_RM
hist(boston$RM,col='plum',main='RM',xlab='RM')
boxplot(boston$RM,col='plum',main='RM')
mean(boston$RM)

#7_AGE
hist(boston$AGE,col='khaki',main='AGE',xlab='AGE')
boxplot(boston$AGE,col='khaki',main='AGE')
mean(boston$AGE)

#8_DIS
hist(boston$DIS,col='cornsilk',main='DIS',xlab='DIS')
boxplot(boston$DIS,col='cornsilk',main='DIS')
mean(boston$DIS)

#9_RAD
rad<-table(boston$RAD)
barplot(rad,xlab='RAD',main='RAD',col=heat.colors(9,alpha=0.4))
table(boston$RAD)

#10_TAX
hist(boston$TAX,col='gold',main='TAX',xlab='TAX')
boxplot(boston$TAX,col='gold',main='TAX')
plot(boston$TAX,main='Tax',xlab='tax',ylab='count',col='gold')

#11_PTRATIO
hist(boston$PTRATIO,col='linen',main='PTRATIO',xlab='PTRATIO')
boxplot(boston$PTRATIO,col='linen',main='PTRATIO')
mean(boston$PTRATIO)

#12_B
hist(boston$B,col='orange4',main='B',xlab='B')
boxplot(boston$B,col='orange4',main='B')

#13_LSTAT
hist(boston$LSTAT,col='azure',main='LSTAT',xlab='LSTAT')
boxplot(boston$LSTAT,col='azure',main='LSTAT')
mean(boston$LSTAT)

#14_MEDV
hist(boston$MEDV,col='lightblue',main='MEDV',xlab='MEDV')
boxplot(boston$MEDV,col='lightblue',main='MEDV')
mean(boston$MEDV)

#연속형 변수 간 관계
boston%>%select(CRIM,ZN,INDUS,NOX,RM,AGE,DIS,TAX,PTRATIO,B,LSTAT,MEDV)->conti
View(conti)
conti2<-na.omit(conti)
cor(conti2)
round(cor(conti2),2)->a;a
corrplot(a,method="circle",tl.col="black",tl.srt=30)

#INDUS
plot(boston$DIS~boston$INDUS,xlab='INDUS',ylab='DIS')
ggplot(data=boston,aes(x=INDUS,y=DIS))+geom_point(size=1, color="gold")+stat_smooth(color='red', fill='grey')

#NOX
plot(boston$NOX~boston$INDUS,xlab='INDUS',ylab='NOX')
ggplot(data=boston,aes(x=INDUS,y=NOX))+geom_point(size=1, color="gold")+stat_smooth(color='red', fill='grey')

plot(boston$NOX~boston$DIS,xlab='DIS',ylab='NOX')
ggplot(data=boston,aes(x=DIS,y=NOX))+geom_point(size=1, color="gold")+stat_smooth(color='red', fill='grey')

#RM
plot(boston$LSTAT~boston$RM,xlab='RM',ylab='LSTAT')
ggplot(data=boston,aes(x=RM,y=LSTAT))+geom_point(size=1, color="gold")+stat_smooth(color='red', fill='grey')

#AGE
plot(boston$DIS~boston$AGE,xlab='AGE',ylab='DIS')
ggplot(data=boston,aes(x=AGE,y=DIS))+geom_point(size=1, color="gold")+stat_smooth(color='red', fill='grey')


#target과의 비교

#1_CRIM
plot(boston$MEDV~boston$CRIM,xlab='CRIM',ylab='MEDV')
ggplot(data=boston,aes(x=CRIM,y=MEDV))+geom_point(size=1, color="gold")+stat_smooth(color='red', fill='grey')

#2_ZN
plot(boston$MEDV~boston$ZN,xlab='ZN',ylab='MEDV')
ggplot(data=boston,aes(x=ZN,y=MEDV))+geom_point(size=1, color="gold")+stat_smooth(color='red', fill='grey')

#3_INDUS
plot(boston$MEDV~boston$INDUS,xlab='INDUS',ylab='MEDV')
ggplot(data=boston,aes(x=INDUS,y=MEDV))+geom_point(size=1, color="gold")+stat_smooth(color='red', fill='grey')

#4_CHAS
boxplot(MEDV~CHAS,data=boston,col=c('snow','powder blue'))
aggregate(MEDV~CHAS,data=boston,mean)

#5_NOX
plot(boston$MEDV~boston$NOX,xlab='NOX',ylab='MEDV')
ggplot(data=boston,aes(x=NOX,y=MEDV))+geom_point(size=1, color="gold")+stat_smooth(color='red', fill='grey')

#6_RM
plot(boston$MEDV~boston$RM,xlab='RM',ylab='MEDV')
ggplot(data=boston,aes(x=RM,y=MEDV))+geom_point(size=1, color="gold")+stat_smooth(color='red', fill='grey')

#7_AGE
plot(boston$MEDV~boston$AGE,xlab='AGE',ylab='MEDV')
ggplot(data=boston,aes(x=AGE,y=MEDV))+geom_point(size=1, color="gold")+stat_smooth(color='red', fill='grey')

#8_DIS
plot(boston$MEDV~boston$DIS,xlab='DIS',ylab='MEDV')
ggplot(data=boston,aes(x=DIS,y=MEDV))+geom_point(size=1, color="gold")+stat_smooth(color='red', fill='grey')

#9_RAD
boxplot(MEDV~RAD,data=boston,col=heat.colors(9,alpha=0.4))
aggregate(MEDV~RAD,data=boston,mean)

#10_TAX
plot(boston$MEDV~boston$TAX,xlab='TAX',ylab='MEDV')
ggplot(data=boston,aes(x=TAX,y=MEDV))+geom_point(size=1, color="gold")+stat_smooth(color='red', fill='grey')

attach(boston)
boston<-transform(boston,
                  tax_level=ifelse(TAX<300,"LOW",
                             ifelse(TAX>=300&TAX<600,"MID","HIGH")))
detach(boston)                                   
barplot(boston$tax_level)

boxplot(MEDV~tax_level,data=boston,col=cm.colors(3,alpha=0.5))

#11_PTRATIO
plot(boston$MEDV~boston$PTRATIO,xlab='PTRATIO',ylab='MEDV')
ggplot(data=boston,aes(x=PTRATIO,y=MEDV))+geom_point(size=1, color="gold")+stat_smooth(color='red', fill='grey')

#12_B
plot(boston$MEDV~boston$B,xlab='B',ylab='MEDV')
ggplot(data=boston,aes(x=B,y=MEDV))+geom_point(size=1, color="gold")+stat_smooth(color='red', fill='grey')

#13_LSTAT
plot(boston$MEDV~boston$LSTAT,xlab='LSTAT',ylab='MEDV')
ggplot(data=boston,aes(x=LSTAT,y=MEDV))+geom_point(size=1, color="gold")+stat_smooth(color='red', fill='grey')











