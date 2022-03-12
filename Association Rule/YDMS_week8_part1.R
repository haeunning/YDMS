library(sas7bdat)
library(arulesViz)
library(arules)
library(dplyr)
prodsales<-read.sas7bdat("C:/project/prodsales.sas7bdat")
prodhierarchy<-read.sas7bdat("C:/project/prodhierarchy.sas7bdat")
assocs<-read.sas7bdat("C:/project/assocs.sas7bdat")
View(assocs)

View(prodsales)
View(prodhierarchy)
summary(prodsales)
summary(prodhierarchy)
str(prodsales)
str(prodhierarchy)

ps.list<-split(prodsales$Item,prodsales$Customer)
ps.list

ps.trans<-as(ps.list,"transactions")
ps.trans

summary(ps.trans)

itemFrequencyPlot(ps.trans,support=0.01,main="item Frequency Plot above support 1%")
image(sample(ps.trans,replace=FALSE),main="matrix diagram") #검정=거래가 발생한 지점

#연관규칙 분석
ps_rule<-apriori(ps.trans)
summary(ps_rule)
inspect(ps_rule)
ps_rule <- apriori(ps.trans, parameter = list(support = 0.4, confidence = 0.5))

summary(ps_rule)

plot(ps_rule)
plot(ps_rule, method = "grouped")
plot(ps_rule, method = "graph")
plot(ps_rule, method = "graph", control = list(type = "items"))

plot(ps_rule, method = "paracoord", control = list(reorder = TRUE))







