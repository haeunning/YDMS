utilities<-read.csv("C:/project/Utilities.csv",header = T, fileEncoding="UTF-8-BOM")
View(utilities)
str(utilities)
head(utilities)

#회사이름을 row이름으로 변경
row.names(utilities)<-utilities[,1]
utilities<-utilities[,-1]
head(utilities)
distance<-dist(utilities,method="euclidean")
distance

#데이터정규화
#sapply함수이용: 연산을 해주는 함수. 반복함수를 이용하는 대신에 이용하는 함수
utilities.norm<-sapply(utilities,scale) #전체 데이터 (표준화)를 한번에 할 수 있음
row.names(utilities.norm)<-row.names(utilities)
head(utilities.norm)

#k-means 군집화 
#초기 시드 설정(기준점 설정) 군집수 6개
set.seed(2)
km<-kmeans(utilities.norm,6)
km
km$centers #평균거리
km$cluster #군집


#그래프

plot(c(0),xaxt='n',ylab="",type="l",
     ylim=c(min(km$centers),max(km$centers)),
     xlim=c(0,8))
axis(1,at=c(1:8),labels=names(utilities))

for(i in c(1:6))
lines(km$centers[i,],lty=i,lwd=2,col=ifelse(i%in% c(1,3,5),"black","dark grey"))
text(x=0.5, y=km$centers[,1],
     labels=paste("Cluster",c(1:6)))

dist(km$centers)

#엘보우챠트
km$withinss
km$tot.withinss

set.seed(2)
k.max<-6
elow.m<-sapply(1:k.max, function(k){kmeans(utilities.norm,k,nstart=50, iter.max=15)$tot.withinss})
elow.m

plot(1:k.max, elow.m, type="b",pch=19, frame=FALSE, xlab="군집 수", ylab="군집 내 거리 제곱합")


library(fpc)
plotcluster(utilities.norm,km$cluster, color=TRUE, shade=TRUE)














