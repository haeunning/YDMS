library(arulesViz)
library(arules)
library(dplyr)
all.books.df<-read.csv("C:/project/CharlesBookClub.csv",header = T, fileEncoding="UTF-8-BOM")
View(all.books.df)
summary(all.books.df)

count.books.df<-all.books.df[,8:18]
incid.books.df<-ifelse(count.books.df>0,1,0)
incid.books.mat<-as.matrix(incid.books.df[,-1])

books.trans<-as(incid.books.mat,"transactions")
inspect(books.trans)

itemFrequencyPlot(books.trans)

rules<-apriori(books.trans, parameter=list(support=200/4000,confidence=0.5,target="rules"))
summary(rules)
inspect(sort(rules,by="lift"))




###################################################################

library(recommenderlab)

count <- read.csv("C:/project/CharlesBookClub2.csv", stringsAsFactors = FALSE)
count
count_matrix <- as(as(count, "matrix"), 'realRatingMatrix')
UBCF <- Recommender(count_matrix, method = 'UBCF', param = list(method = 'cosine')) # 유사도 = cosine
tail(count) 
who <- 3999 # 3999번째 사람
head(as(predict(UBCF, count_matrix[who, ], type = 'ratings'), 'list')[[1]])
as(predict(UBCF, count_matrix[who, ], type = 'topNList', n = 3), 'list')



























