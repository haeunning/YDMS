
library(arulesSequences)
library(arules)

# basket 형식의 assocsSeq.txt 불러오기 
X=read_baskets(con ="C:/project/assocsSeq.txt", info = c("sequenceID","eventID","SIZE"))

# 자료 보기 
as(X, 'data.frame')

# 지지도가 0.1 이상인(디폴트) 시퀀스 찾기
seq = cspade(X) 

summary(seq)

#지지도 상위 25개
subseq=head(sort(seq, by="support"),25)

as(subseq,  "data.frame")

# 지지도 0.1 이상인 규칙들 중 신뢰도가 0.3 이상인 연관성 규칙 찾기
rules = ruleInduction(seq, confidence = 0.3)


summary(rules)

# 신뢰도가 가장 큰 25개의 규칙 찾기
subrules=head(sort(rules, by="confidence"), 25)

# data.frame으로 결과 보기 
T=as(subrules, 'data.frame')
T
