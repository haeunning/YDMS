#데이터 불러오기
library(dplyr)
fram<-read.csv("c:/project/framingham.csv")
View(fram)
#전반적인 탐색
summary(fram)
sum(is.na(fram))
#변수 간 상관관계 파악 by 시각화

#변수별 그래프 그리기
library(ggplot2)

#1_male
count<-table(fram$male)
barplot(count,xlab="male",main = "male ",col="red")
replace(fram$male,fram$male=="F",0)->fram$male
replace(fram$male,fram$male=="M",1)->fram$male
ggplot(fram, aes(male))+geom_bar(fill='red')
summary(fram$male)

#2_age
age<-fram$age
hist(age,col="sky blue",main = "Age")
stem(fram$age,scale=3)
boxplot(fram$age,horizontal = F, col = "sky blue",xlab="age")


#3_education
barplot(table(fram$education),col='gold',main ='edaucation(교육수준)')
fram[is.na(fram$education),]
fram$education[is.na(fram$education)]<-5
summary(fram$education)
sum(is.na(fram$education))
barplot(table(fram$education),col='gold',main ='edaucation(교육수준)')

#4_current Smoker
barplot(table(fram$currentSmoker),col='gray',main='current Smoker(현재 흡연여부)')
fram[fram$currentSmoker==2,]
fram[fram$currentSmoker==0,]
replace(fram$currentSmoker,fram$currentSmoker==2,1)->fram$currentSmoker
barplot(table(fram$currentSmoker),col='gray',main='current Smoker(현재 흡연 여부)')

#5_cigs per day
fram[is.na(fram$cigsPerDay),]
fram[fram$cigsPerDay==1,]

#6_BMPeds
fram[is.na(fram$BPMeds),]
fram[fram$BPMeds==1,]

#7_prevalent Stroke
barplot(table(fram$prevalentStroke),col='yellow green',main='prevalent Stroke')
table(fram$prevalentStroke)

#8_prevalent HYP
barplot(table(fram$prevalentHyp),col='cadet blue',main='prevalent HYP')
table(fram$prevalentHyp)

#9_diabetes
barplot(table(fram$diabetes),col='dark khaki',main='diabetes')
fram$diabetes<-gsub("No","0",fram$diabetes)
table(fram$diabetes)
barplot(table(fram$diabetes),col='dark khaki',main='diabetes')

#10_totchol
fram[is.na(fram$totChol),]
sum(is.na(fram$totChol))

fram%>%filter(!is.na(totChol))->fram
sum(is.na(fram$totChol))
hist(fram$totChol,col="powder blue",main="totChol",xlab='totChol')
boxplot(fram$totChol,col="powder blue",main="totChol")

#연속형 확률변수 간 상관관계 파악
fram%>%select(age,cigsPerDay,totChol,sysBP,diaBP,BMI,heartRate,glucose)->conti
summary(conti)
View(conti)

library(dplyr)
conti2<-na.omit(conti)
cor(conti2)
library(corrplot)
round(cor(conti2),2)->a;a
corrplot(a,method="circle",tl.col="black",tl.srt=30)

#11_sysBP
hist(fram$sysBP,col='red3',main="sysBP")
boxplot(fram$sysBP,col="red3",main="sysBP")

#12_diaBP
fram[is.na(fram$diaBP),]

lm(conti2$diaBP~conti2$sysBP)
#y = 0.4247x + 26.6885

for(i in 1:4240){
  if(is.na(fram$diaBP[i]) == TRUE){
    fram$diaBP[i] <- 0.4247*fram$sysBP[i] + 26.6885
  }
}
fram

sum(is.na(fram$diaBP))
View(fram)

hist(fram$diaBP,col='red1',main='biaBP',xlab='diaBP')
boxplot(fram$diaBP,col='red1',main='diaBp')

##5_BPMeds
fram[is.na(fram$BPMeds),]
fram[fram$BPMeds=='1',]
fram[fram$sysBP>=120&fram$diaBP>=80,]
fram%>%filter(sysBP>=120&diaBP>=80&BPMeds==1)
fram%>%filter(BPMeds==1)->BPM
View(BPM)
fram%>%filter(sysBP>=120&diaBP>=80&BPMeds==0)

fram%>%filter(!is.na(BPMeds))->fram
sum(is.na(fram$BPMeds))

boxplot(sysBP~BPMeds,data=fram,col='red3',main='BPMeds별 sysBP')
boxplot(diaBP~BPMeds,data=fram,col='red1',main='BPMeds별 diaBP')
sum(is.na(fram$BPMeds))

#4_cigsperday
abs(fram$cigsPerDay)->fram$cigsPerDay
View(fram)

fram[is.na(fram$cigsPerDay),]

fram[fram$currentSmoker==1,]->smk
smk.mean<-mean(smk$cigsPerDay,na.rm=TRUE)
round(smk.mean)

fram$cigsPerDay[is.na(fram$cigsPerDay)]<-18
fram
sum(is.na(fram$cigsPerDay))
fram[fram$cigsPerDay==18,]

hist(fram$cigsPerDay,col='tan',main="cigs per day")
boxplot(fram$cigsPerDay,col='tan',main="cigs per day")

#13_BMI
fram%>%filter(!is.na(BMI))->fram
sum(is.na(fram$BMI))
hist(fram$BMI,col="medium purple",main="BMI")
boxplot(fram$BMI,col="medium purple",main="BMI")

#14_heartRate
fram%>%filter(!is.na(heartRate))->fram
sum(is.na(fram$heartRate))
hist(fram$heartRate,col="light pink",main="heartRate")
boxplot(fram$heartRate,col="light pink",main="heartRate")

#15_glucose
fram[fram$diabetes==1,]
fram[is.na(fram$glucose),]
fram%>%filter(glucose>1000)
fram%>%filter(glucose==55)
fram$glucose[fram$glucose==5500]<-55
fram%>%filter(glucose==55)

boxplot(glucose~diabetes,data=fram,col='gold3',main='당뇨병 여부에 따른 혈당수치')

fram[fram$diabetes==0,]->nondia
nondia.mean<-mean(nondia$glucose,na.rm=TRUE)
round(nondia.mean)
fram$glucose[is.na(fram$glucose)]<-79
sum(is.na(fram$glucose))
boxplot(glucose~diabetes,data=fram,col='gold3',main='당뇨병 여부에 따른 혈당수치')

#16_TenYearCHD
fram%>%filter(TenYearCHD==".")
fram%>%filter(TenYearCHD==1)->CHD;View(CHD)
fram$TenYearCHD[fram$TenYearCHD=="."]<-NA
fram%>%filter(!is.na(TenYearCHD))->fram

ften<-table(fram$TenYearCHD)
barplot(ften,col=c('black','snow'),main='십년 후 관상동맥질환 발병여부')

summary(fram$TenYearCHD)





