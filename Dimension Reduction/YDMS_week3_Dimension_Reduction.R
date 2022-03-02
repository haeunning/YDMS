cereals<-read.csv(file ="c:/project/Cereals.csv",header = T, fileEncoding="UTF-8-BOM")
View(cereals)
summary(cereals)

library(ggplot2)
library(dplyr)
library(corrplot)

#개별 변수 eda

summary(cereals$name)
summary(cereals$mfr)
summary(cereals$type)

#mfr
mfr_numer<-as.numeric(cereals$mfr)
mfr_n<-table(mfr_numer)
barplot(mfr_n,names=c("A","G","K","N","P","Q","R"),col='light cyan4',main='mfr')
#type
type_numer<-as.numeric(cereals$type)
type_n<-table(type_numer)
barplot(type_n,names=c("C","H"),col=c('dodger blue','red'),main='type')
#calories
hist(cereals$calories,col='yellow',main='calories',xlab='calories')
boxplot(cereals$calories,col='yellow',main='calroies')
shapiro.test(cereals$calories)
#protain
protein_n<-table(cereals$protein)
barplot(protein_n,col='wheat',main='protain')
#fat
barplot(table(cereals$fat),col='wheat',main='fat')
#sodium
hist(cereals$sodium,col='wheat',main='sodium',xlab='sodium (mg)')
boxplot(cereals$sodium,col='wheat',main='sodium')
#fiber
hist(cereals$fiber,col='wheat',main='fiber',xlab='fiber (g)')
boxplot(cereals$fiber,col='wheat',main='fiber')
#carbo
hist(cereals$carbo,col='wheat',main='carbo',xlab='carbo (g)')
boxplot(cereals$carbo,col='wheat',main='carbo')
#sugar
hist(cereals$sugars,col='wheat',main='sugar',xlab='sugar (g)')
boxplot(cereals$sugars,col='wheat',main='sugar')
#potass
hist(cereals$potass,col='wheat',main='potass',xlab='potass (mg)')
boxplot(cereals$potass,col='wheat',main='potass')
#vitamins
barplot(table(cereals$vitamins),col='wheat',main='vitamins')
#shaelf
barplot(table(cereals$shelf),col='khaki 1',main='shelf')
#weight
barplot(table(cereals$weight),col='khaki 2',main='weight')
w<-table(cereals$weight);w
#cups
hist(cereals$cups,col='khaki 3',main='cups',xlab='cups')
boxplot(cereals$cups,col='khaki 3',main='cups')
#rating
hist(cereals$rating,col='khaki 4',main='rating',xlab='rating')
boxplot(cereals$rating,col='khaki 4',main='rating')

#연속형 변수 사이의 관계
cereals%>%select(calories,sodium,fiber,carbo,sugars,potass,cups,rating)->conti
View(conti)
conti2<-na.omit(conti)
View(conti2)
cor(conti2)
round(cor(conti2),2)->a;a
corrplot(a,method="circle",tl.col="black",tl.srt=30)
conti3<-conti2%>%select(calories,sodium,fiber,carbo,sugars,potass,cups)
View(conti3)

h<-lm(rating~.,data=conti2)
summary(h)

s<-lm(rating~.,data=cereals3)
summary(s)

#회귀분석
library(caret)
set.seed(1234)
idx<-createFolds(conti2$rating,k=5)
test<-data.frame(conti2[idx$Fold5,])
train<-data.frame(conti2[-idx$Fold5,])

fit<-lm(rating~.,data=train)
summary(fit)

#pca
pca<-prcomp(conti3,scale=T)
summary(pca)
plot(pca,type='l',pch=19,lwd=2,col='red',main='Scree Plot')

cereals2<-as.matrix(conti3)%*%pca$rotation[,1:4]
cereals3<-cbind(cereals2,as.data.frame(conti2$rating))
colnames(cereals3)[5]<-'rating'
View(cereals3)

idx_pca<-createFolds(cereals3,k=5)
test_pca<-data.frame(cereals3[idx_pca$Fold5,])
train_pca<-data.frame(cereals3[-idx_pca$Fold5,])

fit_pca<-lm(rating~.,data=train_pca)
summary(fit_pca)

#
install.packages('factoextra')
library(factoextra)
biplot(pca, main="Biplot")
pca$rotation
dim_1st<-fviz_contrib(pca,'var',axes =1 );dim_1st
dim_2nd<-fviz_contrib(pca,'var',axes =2 );dim_2nd
dim_3rd<-fviz_contrib(pca,'var',axes =3 );dim_3rd
dim_4th<-fviz_contrib(pca,'var',axes =4 );dim_4th
