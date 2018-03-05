library(data.table)
customer = fread("L:/project/bigdata/customer.txt")
purchase = fread("L:/project/bigdata/purchase.txt")
goods = fread("L:/project/bigdata/goods.txt")
others = fread("L:/project/bigdata/others.txt")
membership = fread("L:/project/bigdata/membership.txt")
channel = fread("L:/project/bigdata/channel.txt")

colnames(customer)= c("id", "gender", "age", "resi")
colnames(others) = c("id", "comp","other","date")
colnames(purchase) = c("comp", "rec.no","code1","code2","code3","id","store","date","time","purchase")
colnames(membership)=c('id','mem','date')
colnames(channel)=c('id','channel','num')
com_a = purchase[company=="A",]
com_b = purchase[company=="B",]
com_c = purchase[company=="C",]
com_d = purchase[company=="D",]

##EDA
#채널 이용 비율
df2 = sqldf("select id, channel, sum(num) from channel group by id, channel")
channel = df2
c.id = unique(channel$id)
a.cus = customer[c.id,]
table(a.cus$age)/table(customer$age)

##
19세이하	20세~24세	25세~29세	30세~34세	35세~39세	40세~44세	45세~49세	50세~54세	55세~59세	60세이상 
0.1764706  0.3568905   0.4189781   0.4824300   0.4604550   0.3818330    0.3358648  0.3018626   0.2204059   0.1412126 
barplot(t, main='나이별 채널 이용비율', ylim=c(0,0.5))

df3 = sqldf("select 

#채널 이용 비율은 19세 이하/60세 이상이 가장 낮고, 30대가 가장 높다.

##
q = a.cus[a.cus$age%in%c('30세~34세','35세~39세')]
t = table(channel$channel[channel$id %in% q$id])

A_MOBILE/APP B_MOBILE/APP B_ONLINEMALL C_MOBILE/APP C_ONLINEMALL D_MOBILE/APP 
         586         1614          532           14          152           62 




#그렇다면 나이별로 사용 총액 평균은?
sum = sqldf("select id, sum(purchase) from purchase group by id")
sum = cbind(sum, customer$age,customer$gender)
colnames(sum) = c('id', 'purchase','age','gender')
result = aggregate(sum[,2], list(sum$age, sum$gender), mean)
re.f = result[1:10,]
re.m = result[11:20,]
f = as.numeric(re.f[,3])
m = as.numeric(re.m[,3])
 barplot(cbind(f,m),beside=T, names=c("여자","남자"),col=rainbow(10), ylim=c(5000000, 50000000))
 legend(10.5,50000000,legend=as.character(re.f[,1]), fill=rainbow(10))


    age     gender   purchase      age  gender purchase
1   19세이하       F  8992569  19세이하       M  6945896
2  20세~24세       F 14257035 20세~24세       M 11082774
3  25세~29세       F 30287151 25세~29세       M 15449963
4  30세~34세       F 32433280 30세~34세       M 24027913
5  35세~39세       F 33670189 35세~39세       M 49302832
6  40세~44세       F 33346834 40세~44세       M 35758428
7  45세~49세       F 30727544 45세~49세       M 40066542
8  50세~54세       F 34486004 50세~54세       M 40918250
9  55세~59세       F 42009757 55세~59세       M 40418628
10  60세이상       F 46340293  60세이상       M 47328624

> 남녀 모두 60세 이상이 가장 높다!(34세까지는 여자가 소비가 많고 40~54세는 남자 소비가 많으며 55세부터는 양쪽 모두 많다)

#매출에서 차지하는 비중
sum$ratio = sum$purchase/sum(sum$purchase)
ratio = sort(sum$ratio, decreasing=T)
pur = sort(sum$purchase, decreasing=T)
seq = round(seq(1, nrow(sum), length=11))
seq2 = round(seq(1, nrow(sum), length=21))

min(which(cum>0.2))

k = vector()
for (i in 1:10) k[i] = min(which(cum>=(0.1*i)))/length(ratio)
df = data.frame(profit=paste0(seq(10,100,by=10),'%'), customer=paste0(round(k*100,2),'%'))

k2 = vector()
for (i in 1:10){
	start=seq[i]
	end = seq[i+1]
	k2[i] = sum(pur[start:end])/sum(pur)}
 plot(k2, type='o')

k3 = vector()
for (i in 1:20){
	start=seq2[i]
	end = seq2[i+1]
	k3[i] = sum(pur[start:end])/sum(pur)}
plot(k3, type='o', main="상위 구매고객이 매출에서 차지하는 비중",pch=2, ylab="매출비율", xlab="고객비율",
 col='blue',axes=FALSE)
axis(1, at=1:20, lab=paste0(seq(5,100,by=5),'%'))
axis(2, ylim=c(0.00,0.30))



###
male = customer[customer$gender=='M',]
female = customer[customer$gender=='F',]
 tm = table(male$age)
 tf = table(female$age)

#고객(여자) 나이대별 분포
pct2 = round(tf/sum(tf)*100,2)
lab2 = paste(pct2," %")
pie(tf, radius=1, init.angle=90, label=lab2, col=rainbow(10))
legend(1,1.1, as.character(re.f[,1]), fill=rainbow(10))


19세이하 20세~24세 25세~29세 30세~34세 35세~39세 40세~44세 45세~49세 50세~54세 55세~59세  60세이상 
11       250       542      1283      2171      2996      3465      2636      1490      1054 

#고객(남자) 나이대별 분포
pct = round(tm/sum(tm)*100,2)
lab = paste(pct," %")
pie(tm, radius=1, init.angle=90, label=lab, col=rainbow(10))
legend(1,1.1, as.character(re.f[,1]), fill=rainbow(10))

19세이하 20세~24세 25세~29세 30세~34세 35세~39세 40세~44세 45세~49세 50세~54세 55세~59세  60세이상 
6       33       143      396      598      681      617      478       284       249
>남자 여자 모두 40대가 가장 많음

#경쟁사 이용 비율
 uni = unique(others$id)
 o.cus = customer[uni,]
c('19세이하'=0, table(o.cus$age)/table(customer$age)

20세~24세 25세~29세 30세~34세 35세~39세 40세~44세 45세~49세 50세~54세 55세~59세  60세이상 
0.2897527 0.3007299 0.3394878 0.3243048 0.2888224 0.2836845 0.3066795 0.2919955 0.3315426 

#굉장히.. 골고루..

#멤버쉽 가입비율
 uni.m = unique(membership$id)
 m.cus = customer[uni.m,]
 table(m.cus$age)/table(customer$age)

  19세이하  20세~24세  25세~29세 30세~34세  35세~39세  40세~44세  45세~49세 50세~54세  55세~59세  60세이상 
0.5294118 0.6113074 0.5459854 0.5193568 0.4525099 0.3268969 0.2839294 0.2418112 0.2412627 0.2110514 


#10~30대 매우 높음. 나이들수록 가입률 낮음


#멤버쉽 가입자중 경쟁사 이용비율
idx = intersect(uni, uni.m)
idx2 = customer[idx,]
c('19세이하'=0, table(idx2$age))/table(m.cus$age)

 19세이하 20세~24세 25세~29세 30세~34세 35세~39세 40세~44세 45세~49세 
0.0000000 0.3352601 0.3529412 0.3314220 0.3032721 0.2861897 0.2674720 
50세~54세 55세~59세  60세이상 
0.2908367 0.3224299 0.3236364 

#멤버쉽 비가입자중 경쟁사 이용비율
memno = setdiff(customer$id,uni.m)
no.cus = customer[memno,]
idx3 = intersect(uni, setdiff(customer$id,uni.m))
idx4 = customer[idx3,]
c('19세이하'=0,table(idx4$age))/table(no.cus$age)

 19세이하 20세~24세 25세~29세 30세~34세 35세~39세 40세~44세 45세~49세 
0.0000000 0.2181818 0.2379421 0.3482032 0.3416887 0.2901010 0.2901129 
50세~54세 55세~59세  60세이상 
0.3117323 0.2823180 0.3336576 




##구매데이터 조사
sum2 =  sqldf("select id, sum(purchase),comp,count(*) from purchase group by id, comp")
고객별/제휴사별 구매금액 총액과 구매횟수


