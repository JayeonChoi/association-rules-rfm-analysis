library(data.table)
library(arules)
save.image("arules.RData")
purchase = fread("L:/project/bigdata/purchase.txt") 
goods = fread("L:/project/bigdata/goods.txt")

head(purchase)
head(goods)
product = goods[,c(4,6)]
colnames(product) = c("code3", "name")
colnames(purchase) = c("company", "rec","code1","code2","code3","ID","store","date","time","price")

#회사별로 네개의 데이터 프레임으로 나누기
com_a = purchase[company=="A",]
com_b = purchase[company=="B",]
com_c = purchase[company=="C",]
com_d = purchase[company=="D",]

library(sqldf)
a = sqldf("select ID, date, rec from com_a group by ID, date, rec")
head(a,30)
as <-split(a$rec,list(a$date, a$ID))
head(as)
rm(as)

head(merge.a,50)

com.a = com_a[,c(2,5)]
merge.a = merge(com.a, product, by="code3", all.x=T)
merge.a = merge.a[order(merge.a$rec),]
duplicated(merge.a$rec.no)

com.b = com_b[,c(2,5)]
merge.b = merge(com.b, product, by="code3", all.x=T)
merge.b = merge.b[order(merge.b$rec),]

com.c = com_c[,c(2,5)]
merge.c = merge(com.c, product, by="code3", all.x=T)
merge.c = merge.c[order(merge.c$rec),]


com.d = com_d[,c(2,5)]
merge.d = merge(com.d, product, by="code3", all.x=T)
merge.d = merge.d[order(merge.d$rec),]



merge.d2$name[merge.d2$ID==id.df[1]&merge.d2$date==dt.df[1]]



length(unique(d$date))
length(unique(d$ID))
730*3791
#########A
com.a2 = com_a[,c(2,5,6,8)]
merge.a2 = merge(com.a2, product, by="code3", all.x=T)
m = merge.a2
m2 = merge.a3
> com.a3 = com_a[,c(2,5,6,8,10)]
> merge.a3 = merge(com.a3, product, by="code3", all.x=T)
#평균 가격 비싼 제품 위주
length(unique(merge.a3$name))
kk = sqldf("select name, avg(price) from m2 group by name")
colnames(kk)[2] = 'avgprice'
kk = kk[order((kk$avgprice),decreasing=T),]
top100 = head(kk,300)

toprules <- subset(rule_a2, items %in% top100$name)  
inspect(toprules)


da = sqldf("select ID, date, group_concat(name) from m group by ID, date")
b = da[,3]
basket = strsplit(b,",")
trans_a2 <-as(basket,"transactions")
itemFrequencyPlot(trans_a2, topN=30, main="A계열사 지지도 상위 30개 거래 품목2")

rule_a2<-apriori(data=trans_a2, parameter=list(support=0.0001, confidence=0.5, minlen=2))
berryrules <- subset(groceryrules, items %in% "berries")

rule_a3<-apriori(data=trans_a2, parameter=list(support=0.0005, confidence=0.5, minlen=2))
rule_a4<-apriori(data=trans_a2, parameter=list(support=0.001, confidence=0.5, minlen=2))


summary(rule_a3)
summary(rule_a2)
inspect(rule_a2)
rule_a2=sort(rule_a2, by = "lift")
write(rule_a2, file = "ruldA2.csv",
      sep = ",", quote = TRUE, row.names = FALSE) #엑셀파일 생성

########


head(merge.a); nrow(merge.a)
head(merge.b); nrow(merge.b)
head(merge.c); nrow(merge.c)
head(merge.d); nrow(merge.d)

list_a <-split(merge.a$name,merge.a$rec)
list_b <-split(merge.b$name,merge.b$rec)
list_c <-split(merge.c$name,merge.c$rec)
list_d <-split(merge.d$name,merge.d$rec)
head(list_a, 20)
trans_a <-as(list_a,"transactions")
trans_b <-as(list_b,"transactions")
trans_c <-as(list_c,"transactions")
trans_d <-as(list_d,"transactions")
trans_l <-as(dl,"transactions")

itemFrequencyPlot(trans_a, topN=30, main="A계열사 지지도 상위 30개 거래 품목")
itemFrequencyPlot(trans_b, topN=30, main="B계열사 지지도 상위 30개 거래 품목")
itemFrequencyPlot(trans_c, topN=30, main="C계열사 지지도 상위 30개 거래 품목")
itemFrequencyPlot(trans_d, topN=30, main="D계열사 지지도 상위 30개 거래 품목")
itemFrequencyPlot(trans_l, topN=30, main="D계열사 지지도 상위 30개 거래 품목2")

####지지도(support)가 1% 이상인 item, 상위 30개 거래품목 막대 그래프####
############support : 거래품목 중 거래에서 차지하는 비율################
#support(X->Y) : 전체 거래 중에서 X와 Y를 포함할 확률. 값이 클수록 자주 발생하는 거래
#confidence(X->Y) : X를 구매한 경우, 이 중에서 얼마나 Y를 구매할 것인지
#lift(X->Y) : X를 구매한 경우, 그 거래가 Y를 포함하는 경우와 Y가 임의로 구매되는 경우의 비.
#             X와 Y의 구매패턴이 독립적인지, 상관관계가 있는지를 의미. 1보다 클수록 양의 상관관계, 1은 독립
library(arules)

rule_a<-apriori(data=trans_a, parameter=list(support=0.001, confidence=0.4, minlen=2))
rule_b<-apriori(data=trans_b, parameter=list(support=0.001, confidence=0.2, minlen=2))
rule_c<-apriori(data=trans_c, parameter=list(support=0.001, confidence=0.4, minlen=2))
rule_d<-apriori(data=trans_d, parameter=list(support=0.001, confidence=0.2, minlen=2))

rule_l<-apriori(data=trans_l, parameter=list(support=0.001, confidence=0.2, minlen=2))

#옵션 : support - 규칙의 최소 지지도 default=0.1
#       confidence - 규칙의 최소 신뢰도 default=0.8
#       minlen - 규칙에 포함되는 최소 물품 수 default=1
#       maxlen - 규칙에 포함되는 최대 물품 수 default=10
#       smax - 규칙의 최대 지지도 default=1
#콘솔 창에 계산과정 띄우지 않을 때 control 옵션

rule_a
rule_b
rule_c
rule_d

summary(rule_a)
summary(rule_b)
summary(rule_c)
summary(rule_d)

inspect(rule_a)
inspect(rule_b)
inspect(sort(rule_a, by = "lift")) # 상위 지지도 or 향상도 추출
inspect(sort(rule_b, by = "lift"))
inspect(sort(rule_c, by = "lift"))
inspect(sort(rule_d, by = "lift"))# 상위 지지도 or 향상도 추출
inspect(sort(rule_l, by = "lift"))

###시각화
library(arulesViz)
plot(rule_a) #scatter plot
plot(sort(rule_a, by = "support")[1:40], method="grouped") #grouped matrix
plot(sort(rule_a, by = "lift")[1:40], method="grouped")
plot(sort(rule_a, by = "confidence")[1:40], method="grouped")

plot(sort(rule_b, by = "support")[1:40], method="grouped") #grouped matrix
plot(sort(rule_c, by = "support")[1:40], method="grouped") #grouped matrix
plot(sort(rule_d, by = "support")[1:40], method="grouped") #grouped matrix


plot(rule_a[1:40], method="graph", control=list(type="items")) #network graph
plot(rule_a[1:20], method="graph", control=list(type="items"), interactive=T) #network graph, interactive
#화살표 : left -> right hand rule 방향
#원의 크기 : 지지도에 비례해서 커짐
#원의 색깔 : 향상도에 비례해서 진해짐
#type="itemsets" 화살표의 두께로 지지도를, 색깔로 향상도를 표현
plot(rule_a, method="paracoord") #parallel coordinates plot, 1을 산 사람이 rhs를 산다.로 해석

write(rule_a, file = "ruleA.csv",
      sep = ",", quote = TRUE, row.names = FALSE) #엑셀파일 생성
write(rule_b, file = "ruleB.csv",
      sep = ",", quote = TRUE, row.names = FALSE) #엑셀파일 생성
write(rule_c, file = "ruleC.csv",
      sep = ",", quote = TRUE, row.names = FALSE) #엑셀파일 생성
write(rule_d, file = "ruldD.csv",
      sep = ",", quote = TRUE, row.names = FALSE) #엑셀파일 생성
