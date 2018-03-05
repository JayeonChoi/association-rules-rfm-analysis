library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(knitr)
library(rmarkdown)

purchase <- purchase %>% 
  mutate(rec.no=as.factor(rec.no),
         date=as.Date(as.character(date), '%Y%m%d'), id = as.factor(id))

RFM = purchase %>%
  group_by(id) %>%
  summarise(recency = as.numeric(as.Date("2016-01-01")-max(date)),
            frequency = n_distinct(rec.no), monitery=sum(purchase)/n_distinct(rec.no))

RFM$log.rec = log(RFM$recency)
RFM$log.mon = log(RFM$monitery)

RFM = RFM[-which(is.na(RFM$monitery)),]


RFM$recency_group = cut(RFM$recency, breaks=c(0,1,3,max(RFM$frequency)), labels=c(3,2,1))
RFM$frequency_group = cut(RFM$frequency, breaks=c(0,495,617, max(RFM$frequency)), labels=c(1,2,3))
RFM$monitery_group = cut(RFM$monitery, breaks=c(0,23904,48094, max(RFM$monitery)), labels=c(1,2,3))

head(RFM)
RFM3 = RFM[,-c(2,3,4)]
l = list(l, df3)
l
head(RFM3, 30)

length(which(RFM3$recency_group==1 & RFM3$frequency_group==1 & RFM3$monitery_group==1))

list = list()
q = 0
n = vector()
for (i in 1:3){
  for (j in 1:3) {
    for (k in 1:3) {
      q = q+1
      n[q] = paste0("(",i,",",j,",",k,")")
      list[[q]] = RFM3[which(RFM3$recency_group==i & RFM3$frequency_group==j & RFM3$monitery_group==k),]
      
    }
  }
}
names(list) = n
list
sapply(list,nrow)

list2 = list()
q2 = 0
n2 = vector()
for (i in 1:5){
  for (j in 1:5) {
    for (k in 1:5) {
      q = q+1
      n[q] = paste0("(",i,",",j,",",k,")")
      list[[q]] = RFM3[which(RFM3$recency_group==i & RFM3$frequency_group==j & RFM3$monitery_group==k),]
      
    }
  }
}


Recency – How recently did the customer purchase?
  ```{r} 
hist(RFM$recency, xlim=c(0,700))
head(sort(RFM$recency, decreasing=T),50)
hist(RFM$frequency, breaks = 50, xlim=c(0,1500), main="Frequency 히스토그램")
hist(RFM$monitery, breaks = 50, xlim=c(0,1000000))
RFM$monitery <- log(RFM$monitery)#정규화
hist(RFM$monitery, main="Monetary 히스토그램")
hist(log(RFM$monitery), xlim=c(8,14), main="로그 변환 후 Monetary 히스토그램")

hist(RFM$recency, main="Recency 히스토그램")
hist(log(RFM$recency), main="로그 변환 후 Recency 히스토그램")

#클러스터링
### Clustering
```{r}
RFM2 <- RFM
row.names(RFM2) <- RFM2$id
head(RFM2)
RFM2$id <- NULL
RFM2 <- scale(RFM2)
summary(RFM2)
```

```{r}
d <- dist(RFM2)
c <- hclust(d, method = 'ward.D2')

plot(c)
```

#### Cut
```{r}
members <- cutree(c,k = 8)

members[1:5]
table(members)

aggregate(RFM[,2:4], by=list(members), mean)

#####단계별로

