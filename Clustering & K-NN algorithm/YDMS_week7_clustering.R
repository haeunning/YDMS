utilities<-read.csv("C:/project/Utilities.csv",header = T, fileEncoding="UTF-8-BOM")
View(utilities)
str(utilities)
head(utilities)

#####계층적 군집분석#####

#회사이름을 row이름으로 변경
row.names(utilities)<-utilities[,1]
utilities<-utilities[,-1]
head(utilities)
distance<-dist(utilities,method="euclidean")
distance

#데이터정규화
#sapply함수이용: 연산을 해주는 함수. 반복함수를 이용하는 대신에 이용하는 함수
utilities.norm<-sapply(utilities,scale) #전체 데이터 정규화(표준화)를 한번에 할 수 있음
row.names(utilities.norm)<-row.names(utilities)
head(utilities.norm)

#거리측정
#"euclidean","maximum","mahattan","canberra","binary","minkowski"
d.norm<-dist(utilities.norm,method="euclidean") #euclidean=유클리드
d.norm

#계층적 군집화 :hclust() 
#"single","complete","average","median","centroid","ward.D"
#단일계산법(최단)
hc.s<-hclust(d.norm,method="single")
plot(hc.s,hang=-1,ann=FALSE)
#평균연결법
hc.a<-hclust(d.norm,method="average")
plot(hc.a,hang=-1,ann=FALSE)

#군집수 결정
memb.s<-cutree(hc.s,k=6)
memb.s
memb.a<-cutree(hc.s,k=6)
memb.a

#군집번호를 회사명 옆에 부착
row.names(utilities.norm)<-paste(memb.s,":",row.names(utilities),sep="")
head(utilities.norm)
View(utilities.norm)

heatmap(as.matrix(utilities.norm),Colv=NA,hclustfun=hclust,col=rev(paste("gray",1:99,sep="")))
