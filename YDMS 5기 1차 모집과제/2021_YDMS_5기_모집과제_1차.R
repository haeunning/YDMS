library(dplyr)
#데이터합치기
t1<-read.csv("c:/project/T1.csv")
t2<-read.csv("c:/project/T2.csv")
total<-full_join(t1,t2,by="ID")
View(total)
#데이터 탐색
names(total)
print(total)
str(t1)
str(t2)
str(total)
tail(t1,10)
tail(t2,10)
head(total,10)
tail(total,10)
summary(total)
sum(is.na(total)) #결측값 개수

#히스토그램, 막대그래프에 의한 관측값의 분포 및 상자그림
#v1
price<-total$V1
hist(price,ylim=c(0,550),breaks=12,col="indian red",main = "Offer Price in EUROs ")

boxplot(total$V1,main = "Offer Price in EUROs ",ylab="price", 
        horizontal = F, col = "indian red")
#v2
age<-total$V2
hist(age,col="light blue",main = "Age in months as in August 2004")
boxplot(total$V2,main="Age in months as in August 2004",ylab="age",horizontal = F,
        col="light blue")
table(total$V2)
#v3
Kilometers<-total$V3
hist(Kilometers,col="orange",main = "Accumulated Kilometers on odometer")
boxplot(total$V3,main="Accumulated Kilometers on odometer",col="orange"
        ,ylab="kilometers",horizontal=F)
#v4
count<-table(total$V4)
barplot(count,col=c("lightyellow2","lightyellow3","lightyellow4"),xlab="Number of Fuel",main = "Fuel Type ")
#v5
hp<-total$V5
hist(hp,col="lightsteelblue",main="Horse Power")
boxplot(total$V5,col="lightsteelblue",main="Horse Power",
        ylab="hp",horizontal = F)

total$hpgroup='hp1'
total$hpgroup[total$V5>=80]='hp2'
total$hpgroup[total$V5>=100]='hp3'
total$hpgroup[total$V5>=120]='hp4'
hpg<-table(total$hpgroup)
barplot(hpg,col=c("lightsteelblue1","lightsteelblue2","lightsteelblue3","lightsteelblue4"),main="Horse Power Group")

#v6_16000이라는 값의 이상치 존재
Volume<-total$V6
hist(Volume,breaks=seq(1300,16000,100),col="pink2",main = "Cylinder Volume in cubic centimeters")
#boxplot(total$V6,col="darkolivegreen1",main = "Cylinder Volume in cubic centimeters",ylab="volume")
#v6 히스토그램보단 막대그래프로.
table(total$V6)
total$volume='volume1'
total$volume[total$V6>=1400]='volume2'
total$volume[total$V6>=1500]='volume3'
total$volume[total$V6>=1600]='volume4'
total$volume[total$V6>=1700]='volume5'
total$volume[total$V6>=1800]='volume6'
total$volume[total$V6>=1900]='volume7'
total$volume[total$V6>=2000]='volume8'
total$volume[total$V6>=3000]='volume9'
volumelevel<-table(total$volume)
barplot(volumelevel,col=c("pink","pink1","palevioletred2","pink3","pink4","plum","plum1","plum2"),main="Cylinder Volume level")

#v7
count<-table(total$V7)
barplot(count,col=c("lightskyblue1","lightskyblue2","lightskyblue3","lightskyblue4"),xlab="Number of doors",main = "Number of doors")
number<-total$V7
hist(number,col="lightskyblue",main="Number of doors")
#v8
tax<-total$V8
hist(tax,col = "olivedrab",main = "Quarterly road tax in EUROs")
boxplot(total$V7,col = "olivedrab",main = "Quarterly road tax in EUROs",
        ylab="tax")

total$tax='low'
total$tax[total$V8>=100]='mid'
total$tax[total$V8>=200]='high'
taxlevel<-table(total$tax)
barplot(taxlevel,col=c("olivedrab2","olivedrab3","olivedrab4"),main="Quarterly road tax in EUROs")

#v9
auto<-table(total$V9)
barplot(auto,col=c("snow","snow3"),main = "Automatic (Yes=1, No=0)")

#v10
Within<-table(total$V10)
barplot(Within,col=c("snow","snow3"),main="Within Manufacturer's Guarantee period (Yes=1, No=0)")

#v11
#period<-total$V11
#hist(period,col="yellow4",main="Guarantee period in months")
#boxplot(total$V11,col="yellow4",main="Guarantee period in months",yab="period")
period<-table(total$V11)
barplot(period,col=c("yellow4"),main="Guarantee period in months",yab="period")

#v12
positions<-table(total$V12)
barplot(positions,col=c("slategray1","slategray3","slategray2","slategray"),main="Number of gear positions")
table(total$V12)
#v13
weight<-total$V13
hist(weight,col="gold",main="weight in Kilograms")
boxplot(total$V13,col="gold",main="weight in Kilograms",ylab="weight")

#weight<-table(total$V13)
#barplot(weight,col=c("gold"),main="weight in Kilograms")


#v14
color<-table(total$V14)
barplot(color,col=c("floralwhite"),main = "Color")
summary(total$V1)
boxplot(total$V1,main = "Offer Price in EUROs ",ylab="price", 
        horizontal = F, col = "indian red")

#두 개 이상의 변수간 관계 따져보기

#v2에따른 v1의 그래프 
library('ggplot2')
ggplot(data=total,aes(x=V2,y=V1))+geom_point(size=1,color="cornflowerblue")+stat_smooth(color='red', fill='grey')
cor(total$V2,total$V1,use='complete.obs',method='pearson')
summary(lm(V1~V2, total))

#v3 v1
ggplot(data=total,aes(x=V3,y=V1))+geom_point(size=1,color="orange")+stat_smooth(color='red', fill='greY')
cor(total$V3,total$V1,use='complete.obs',method='pearson')
summary(lm(V1~V3, total))


#V4 V1
#options(scipen=999)
#ggplot(data=total,aes(x=reorder(V4,V1),y=V1))+geom_bar(stat='identity',fill="gold")+labs(x='V4',y='V1')

boxplot(V1~V4,data=total,col=c("lightyellow2","lightyellow3","lightyellow4"))

mean.df<-as.data.frame(tapply(total$V1,total$V4,mean))
mean.df$V4<-rownames(mean.df)
names(mean.df)<-c("price","number")
mean.df
ggplot(mean.df,aes(number,price,fill=number))+geom_bar(stat="identity")+scale_fill_brewer(palette ="Greens")

#V5 V1
boxplot(V1~hpgroup,data=total,col=c("lightsteelblue1","lightsteelblue2","lightsteelblue3","lightsteelblue4"))

mean.df2<-as.data.frame(tapply(total$V1,total$hpgroup,mean))
mean.df2$hpgroup<-rownames(mean.df2)
names(mean.df2)<-c("price","hp_level")
mean.df2
ggplot(mean.df2,aes(hp_level,price,fill=hp_level))+geom_bar(stat="identity")+scale_fill_brewer(palette ="Blues")

#V6 V1
#ggplot(data=total,aes(x=V6,y=V1))+geom_point(size=1,color="orange")+stat_smooth(color='red', fill='grey')
#cor(total$V6,total$V1,use='complete.obs',method='pearson')
#summary(lm(V1~V6, total))
boxplot(V1~volume,data=total,col=c("oldlace","pink","pink1","pink2","pink3","pink4","plum","plum1","plum2"))

mean.df3<-as.data.frame(tapply(total$V1,total$volume,mean))
mean.df3$volume<-rownames(mean.df3)
names(mean.df3)<-c("price","volume_level")
mean.df3
ggplot(mean.df3,aes(volume_level,price,fill=volume_level))+geom_bar(stat="identity")+scale_fill_brewer(palette ="PuRd")

#V7 V1
boxplot(V1~V7,data=total,col=c("lightskyblue1","lightskyblue2","lightskyblue3","lightskyblue4"))
mean.df1<-as.data.frame(tapply(total$V1,total$V7,mean))
mean.df1$V7<-rownames(mean.df1)
names(mean.df1)<-c("price","number")
mean.df1
ggplot(mean.df1,aes(number,price,fill=number))+geom_bar(stat="identity")+scale_fill_brewer(palette ="Blues")

#V8 V1
boxplot(V1~tax,data=total,col=c("lemonchiffon","darkolivegreen2","darkolivegreen4"))

mean.df4<-as.data.frame(tapply(total$V1,total$tax,mean))
mean.df4$tax<-rownames(mean.df4)
names(mean.df4)<-c("price","tax_level")
mean.df4
ggplot(mean.df4,aes(tax_level,price,fill=tax_level))+geom_bar(stat="identity")+scale_fill_brewer(palette ="Greens")

#V9 V1
table(total$V9)
boxplot(V1~V9,data=total,col=c("gold","orange","snow"))

mean.df5<-as.data.frame(tapply(total$V1,total$V9,mean))
mean.df5$V9<-rownames(mean.df5)
names(mean.df5)<-c("price","utomatic")
ggplot(mean.df5,aes(utomatic,price,fill=utomatic))+geom_bar(stat="identity")+scale_fill_manual(values = c("#FFCC00","#FF9900","#FFFFFF"))
#v10 v1

boxplot(V1~V10,data=total,col=c("lightcyan","lightskyblue4","lightcyan","lightskyblue4"))

mean.df6<-as.data.frame(tapply(total$V1,total$V10,mean))
mean.df6$V10<-rownames(mean.df6)
names(mean.df6)<-c("price","in_period")
mean.df6
ggplot(mean.df6,aes(in_period,price,fill=in_period))+geom_bar(stat="identity")+scale_fill_manual(values=c("#CCFFFF","#336699","#CCFFFF","#336699"))

#v11 v1
table(total$V11)

boxplot(V1~V11,data=total,col=c("olivedrab1","olivedrab3","olivedrab4","yellow4"))

mean.df7<-as.data.frame(tapply(total$V1,total$V11,mean))
mean.df7$V11<-rownames(mean.df7)
names(mean.df7)<-c("price","guarantee")
mean.df7
ggplot(mean.df7,aes(guarantee,price,fill=guarantee))+geom_bar(stat="identity")+scale_fill_brewer(palette ="Greens")

#v12 v1
boxplot(V1~V12,data=total,col=c("slategray1","slategray3","slategray2","slategray"))

mean.df8<-as.data.frame(tapply(total$V1,total$V12,mean))
mean.df8$V12<-rownames(mean.df8)
names(mean.df8)<-c("price","gear")
mean.df8
ggplot(mean.df8,aes(gear,price,fill=gear))+geom_bar(stat="identity")+scale_fill_brewer(palette ="Blues")

#V13 V1
ggplot(data=total,aes(x=V13,y=V1))+geom_point(size=1,color="gold")+stat_smooth(color='red', fill='grey')
cor(total$V13,total$V1,use='complete.obs',method='pearson')
summary(lm(V1~V13, total))

#V14 V1
boxplot(V1~V14,data=total,col=c("floralwhite"))

mean.df9<-as.data.frame(tapply(total$V1,total$V14,mean))
mean.df9$V14<-rownames(mean.df9)
names(mean.df9)<-c("price","color")
mean.df9
ggplot(mean.df9,aes(color,price,fill=color))+geom_bar(stat="identity")+scale_fill_brewer(palette ="PiYG")

#total의 중복값 확인
nrow(t1)
nrow(unique(t1))
nrow(t2)
nrow(unique(t2))

nrow(total)
nrow(unique(total))
total[which(duplicated(total)),]

new<-unique(total)
View(new)
nrow(new)
nrow(unique(new))

#결측값 확인
sum(is.na(new))
colSums(is.na(new))

#결측치 행 찾기 (T2에 없었을 ID를 중심으로)
new[!complete.cases(new),]

#결측치 제거
library(dplyr)
final<-na.omit(new)
sum(is.na(final))
new[!complete.cases(final),]
View(final)

#찾아바꾸기
final$V10<-gsub("Yes", "1", final$V10) 
final$V10<-gsub("No", "0", final$V10) 
final$V10

#v10 수정
final_V10<-table(final$V10)
barplot(final_V10,col=c("snow","snow3"),main="Within Manufacturer's Guarantee period (Yes=1, No=0)")

#v9 수정
replace(final$V9,final$V9==2,NA)->final$V9
final$V9[is.na(final$V9)]<-999
final$V9[final$V9==999]<-NA

Rfinal<-na.omit(final)
sum(is.na(Rfinal))
new[!complete.cases(Rfinal),]
View(Rfinal)

r9<-table(Rfinal$V9)
barplot(r9,col=c("snow","snow3"),main = "Automatic (Yes=1, No=0)")

table(total$V13)
#v13 수정
replace(final$V13,final$V13>=2000,NA)->final$V13
final$V13[is.na(final$V13)]<-999
final$V13[final$V13==999]<-NA

Rfinal<-na.omit(final)
sum(is.na(Rfinal))
new[!complete.cases(Rfinal),]
View(Rfinal)

r13<-Rfinal$V13
hist(r13,col="gold",main="weight in Kilograms")
boxplot(Rfinal$V13,col="gold",main="weight in Kilograms",ylab="weight")

ggplot(data=Rfinal,aes(x=V13,y=V1))+geom_point(size=1,color="gold")+stat_smooth(color='red', fill='grey')
cor(Rfinal$V13,Rfinal$V1,use='complete.obs',method='pearson')
summary(lm(V1~V13, Rfinal))

