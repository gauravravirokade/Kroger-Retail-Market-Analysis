rm(list=ls())
install.packages("rmarkdown")
library(rio)
library(moments)
library(car)
library(lattice)
library(PerformanceAnalytics)
library(dplyr)
library(tidyverse)
library(tidyquant)
library(stargazer)
library(pscl)
library(plm)
library(lme4)  
library(plm)
library(AER)

setwd("C:/Users/gaura/Downloads/SDM Fall 21/")
df=import("alldata.csv")
View(df)
colnames(df)=tolower(make.names(colnames(df)))
str(df)
attach(df)

histogram(df$spendingavg)
histogram(log(df$spendingavg))
densityplot(df$spendingavg)
histogram(df$weeklyspend)
densityplot(df$weeklyspend)
cor(weeklyspend,spendingavg)
cor(t_loydisc_fb,t_loydisc_cat)
cor(df[8:13])
#chart.Correlation(df[8:13])
df=df %>%
  group_by(id, group = cumsum(weeklyspend != 0)) %>%
  mutate(recency = calendarweek - first(calendarweek)) %>%
  ungroup() %>%
  select(-group) %>%
  group_by(id) %>%
  mutate(recency = ifelse(recency == 0, lag(recency) + calendarweek - lag(calendarweek), recency))

#as inturpurtime is inbetween difference recency will be +1 of it. 
table(df$recency)
df$recency=df$recency-1


HUNIQUE <- length(unique(df$id))#finding unique hh counts
Hh <- unique(df$id); # coping unique HH values 
temp = 0 # varible to store freq
df1 = NULL # new df
for(i in 1: HUNIQUE)
{  # running for loop for length of unique hh.
  temp = NULL
  temp = df[df$id == Hh [i],]
  temp$freq = rollapplyr(temp$purweek, 4, sum, partial = TRUE, na.rm = TRUE)
  df1 = rbind(df1,temp)
}


colSums(is.na(df1))  # checking for null values.
histogram(df1$recency)
histogram(log(df1$recency))
histogram(df1$freq)
densityplot(df1$freq)
histogram(df1$spendingavg)
histogram(log(df1$spendingavg))
densityplot(df1$spendingavg)
densityplot(log(df1$spendingavg))
table(df1$spendingavg)
drfm=df1[c(70,71,9)]
view(drfm)
chart.Correlation(drfm)

#combining 30 campaingns and categorizing them as A,B & C
df1$Camp_A= ifelse(df1$c8==1 |df1$c13==1 | df1$c18==1 | df1$c26==1 |
                        df1$c30==1,1,0)
df1$Camp_C=ifelse(df1$c3==1 | df1$c6==1 | df1$c14==1| df1$c15==1|df1$c20==1 | df1$c27==1,1,0)

df1$Camp_B= ifelse(df1$c1==1 | df1$c2==1 | df1$c4==1 | df1$c5==1 |
                     df1$c7==1|df1$c9==1|df1$c10==1|df1$c11==1|df1$c12==1|
                     df1$c16==1| df1$c17==1| df1$c19==1| df1$c21==1|
                     df1$c22==1| df1$c23==1| df1$c24==1| df1$c25==1|
                     df1$c28==1|df1$c29==1,1,0)

#modeling effect of other attributes with respect to recency
m1=glm(recency~spendingavg+freq+Camp_C+Camp_A+Camp_B+t_loydisc_cat+t_display+t_favbrndcoup,data = df1,family = poisson())
summary(m1)
m2=glm(recency~spendingavg+freq+Camp_C+Camp_A+Camp_B+t_loydisc_cat+t_display+t_favbrndcoup+age+married+numkids,data = df1,family = poisson())
summary(m2)
m3=glm(recency~spendingavg+freq+Camp_C+Camp_A+Camp_B+t_loydisc_cat+t_display+t_favbrndcoup+hhsize+numkids+age,data = df1,family = poisson())
summary(m3)
stargazer(m1,m2,m3,type = "text",single.row = TRUE)

# as we have almost 55,000 Zeros values in recency, Hurdle model would give better results than Poission
hur=hurdle(recency~spendingavg+freq+Camp_C+Camp_A+Camp_B
           +t_loydisc_cat+t_display+t_favbrndcoup
            +hhsize+numkids+age,data = df1,link="logit",zero.dist="negbin")
summary(hur)
stargazer(hur,m2,m3,type="text",single.row = TRUE)
dispersiontest(m3)

# fixed effects model for recency
pooled=plm(recency~spendingavg+freq+Camp_C+Camp_A
           +Camp_B+t_loydisc_cat+t_display+t_favbrndcoup
           +hhsize+numkids+age+married,data=df1, model="pooling")
summary(pooled)
vif(m3)
vif(hur)
summary(hur)
dispersiontest(m2)

fixed1 <-  plm(weeklyspend ~ hhsize+interpurtime + freq+factor(Camp_A)+factor(Camp_B)+factor(Camp_C), data=df1, model="within")
summary(fixed1)

# data visualizing weeklyspend for campaigns A, B and C
densityplot(~spendingavg,data=df1)
densityplot(~weeklyspend|Camp_A,data=df1)
densityplot(~weeklyspend|Camp_B,data=df1)
densityplot(~weeklyspend|Camp_C,data=df1)

#creating subset of houseid hh= 1,7,8,13,17,18 to examins the effect of campaigns 
df2=subset(df1,df1$id==1|df1$id==7|df1$id==8|df1$id==13|df1$id==17|df1$id==18)
densityplot(~weeklyspend|id,data=df2)
densityplot(~weeklyspend|id*Camp_A,data=df2)
stargazer(pooled,hur,m2,m3,type="text",single.row = TRUE)

# modeling random effect & fixed effect of R-F-M & camp A, B & C on weeklyspend
ml1=lmer(weeklyspend ~ recency + freq + income + Camp_A + Camp_B + Camp_C + (1| id),data=df1, REML=FALSE)

# fixed effect
summary(ml1)

#random effect 
ranef(ml1)

#modelling weeklyspend against other attributes considering House Hold ID as fixed, random
mlpooled <- plm(weeklyspend ~recency+freq+Camp_C+Camp_A
              +Camp_B+t_loydisc_cat+t_display+t_favbrndcoup+t_mailer+
              +hhsize+numkids+age+married, data=df1, index="id", model="pooling")
summary(mlpooled)
#fixed
mlfixed  <- plm(weeklyspend ~recency+freq+Camp_C+Camp_A
              +Camp_B+t_loydisc_cat+t_display+t_favbrndcoup+t_mailer+
              +hhsize+numkids+age+married, data=df1, index="id", model="within")
summary(mlfixed)
summary(fixef(mlfixed))
#random
mlrandom <- plm(weeklyspend ~recency+freq+Camp_C+Camp_A
              +Camp_B+t_loydisc_cat+t_display+t_favbrndcoup+t_mailer+
              +hhsize+numkids+age+married, data=df1, index="id", model="random")
#considering householdid and weeksin
mlrandom2 <- plm(weeklyspend ~recency+freq+Camp_C+Camp_A
                +Camp_B+hhsize+numkids+age+married, data=df1, 
                index=c("id","weeksin"),model="random")
summary(mlrandom2)
summary(mlrandom)
summary(ranef(mlrandom))
ranef(mlrandom2)
stargazer(mlfixed,mlfixed,mlrandom,type="text",single.row = TRUE)

histogram(log(df1$weeklyspend))
#calculating d = log(number of weeks each household i since last purchase)
uniquehh=unique(df1$id)
uniquehh
c=0
count=0
x <- length(unique(df1$id))
y = unique(df1$id)
for(j in 1:x)
{
  for(i in 1:nrow(df1))
  {
    if(df1$id[i] == y[j])
    {
      
      if (df1$weeklyspend!=0)
      {
        df1$d[i]=count+1
        count=df1$d [i]
      }
      else{df1$d[i]=0}
      
    }
    else{count=0;
    break}
  }
}

df2=df1
df2$range=seq(1:nrow(df2))
str(df2$range)
df_sub1=subset(df2,df2$numall_now==0)

table(df_sub1$d)
df2=subset(df2,df2$numall_now!=0)
df2=df2%>%group_by(id)%>%mutate(d=row_number())
df_sub1$d=0
df123=rbind(df2,df_sub1)
dfcombined=rbind(df2,df_sub1)
attach(dfcombined)
table(df123$d)
histogram(df123$d)
colSums(is.na(dfcombined))
df123=df123[order(range),]
table(dfcombined$d)
str(dfcombined)
df123$d=log(df123$d)
hist(dfcombined$d)
table(df123$d)
df123$d=ifelse(df123$d<0,0,df123$d)
table(df123$d)
df123$id=factor(df123$id)

# modeling random effect & fixed effect of R-F-M-D & camp A, B & C on weeklyspend
fixedwithid=lm(weeklyspend~recency+freq+d+t_loydisc_cat+t_display+t_favbrndcoup+t_mailer
                   +hhsize+numkids+income+married+t_loydisc_cat+age+id,data = df123)
summary(fixedwithid)

m2=lm(weeklyspend~recency+freq+t_loydisc_cat+t_display+t_favbrndcoup+t_mailer
        +hhsize+numkids+age+married+id, data=df123)
summary(m2)

fixedwithcamp=lm(weeklyspend~recency+freq+spendingavg+d+t_loydisc_cat+t_display+t_favbrndcoup+t_mailer+
        +hhsize+numkids+income+age+married+Camp_A+Camp_B+Camp_C, data=df123)
summary(fixedwithcamp)
stargazer(fixedwithid,fixedwithcamp,out = "fixedeffect.html",single.row = TRUE)
library(lme4) 
fixedwithidlm=lmer(log(weeklyspend)~recency+freq+spendingavg+d+
                 income+married+t_loydisc_cat+(1|id),data = df123, REML = FALSE)
summary(fixedwithidlm)
hist(weeklyspend)
hist(log(weeklyspend))

#remobing null values and scaling data
df111=na.omit(df123)
dfcombined=select_if(df111,is.numeric)
dfcombined = as.data.frame(sapply(dfcombined, as.numeric))
str(dfcombined)
dfcluster=scale(dfcombined)

set.seed(123)
#install.packages("biganalytics")
#nstall.packages("factoextra")

library(biganalytics)
library(factoextra)

#kmeans for 3 segment
#dfcluster3segment=dfcluster
cluster3segment =bigkmeans(dfcluster,centers = 3,iter.max = 100,nstart = 1,dist = "euclid")
table(cluster3segment$cluster)
all(is.finite(dfcluster))
#plotting the clusters
fviz_cluster(cluster3segment, data = dfcluster, geom = c("point"),ellipse.type = "euclid")
dfcombined$index=cluster3segment$cluster
#calculating mode
Mode <- function(x, na.rm = FALSE) {if(na.rm){
  x = x[!is.na(x)]}
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])}
clustervector = aggregate(index~id,dfcombined,Mode)
table(clustervector)

householdVec = aggregate(index~id,dfcombined,Mode)
table(householdVec)
hhidc1 = subset(householdVec,index==1)
hhidc2 =subset(householdVec,index==2)
hhidc3 = subset(householdVec,index==3)
#creating clusters
seg3data1 = subset(dfcombined,id %in% hhidc1$id)
seg3data2 =subset(dfcombined,id %in% hhidc2$id)
seg3data3 =subset(dfcombined,id %in% hhidc3$id)

# running models for segment 3

seg3c1id=lm(weeklyspend~recency+freq+spendingavg+d+t_loydisc_cat+t_display+t_favbrndcoup+t_mailer
               +hhsize+numkids+income+married+t_loydisc_cat+age+as.factor(id),data = seg3data1)
seg3c2id=lm(weeklyspend~recency+freq+d+spendingavg+t_loydisc_cat+t_display+t_favbrndcoup+t_mailer
               +hhsize+numkids+income+married+t_loydisc_cat+age+as.factor(id),data = seg3data2)
seg3c3id=lm(weeklyspend~recency+freq+d+spendingavg+t_loydisc_cat+t_display+t_favbrndcoup+t_mailer
               +hhsize+numkids+income+married+t_loydisc_cat+age+as.factor(id),data = seg3data3)
stargazer(seg3c1id,seg3c2id,seg3c3id,out = "seg3id.html",single.row = TRUE)

summary(seg3c1id)
seg3c1camp=lm(weeklyspend~recency+freq+spendingavg+d+t_loydisc_cat+t_display+t_favbrndcoup+t_mailer+
                   +hhsize+numkids+income+age+married+Camp_A+Camp_B+Camp_C, data=seg3data1)
seg3c2camp=lm(weeklyspend~recency+freq+spendingavg+d+t_loydisc_cat+t_display+t_favbrndcoup+t_mailer+
                   +hhsize+numkids+income+age+married+Camp_A+Camp_B+Camp_C, data=seg3data2)
seg3c3camp=lm(weeklyspend~recency+freq+spendingavg+d+t_loydisc_cat+t_display+t_favbrndcoup+t_mailer+
                   +hhsize+numkids+income+age+married+Camp_A+Camp_B+Camp_C, data=seg3data3)

stargazer(seg3c1camp,seg3c2camp,seg3c3camp,out = "seg3camp.html",single.row = TRUE)
#Kmeans for 4 clusters
set.seed(123)
cluster4segment =bigkmeans(dfcluster,centers = 4,iter.max = 100,nstart = 1,dist = "euclid")
table(cluster3segment$cluster)
#plotting the clusters
fviz_cluster(cluster4segment, data = dfcluster, geom = c("point"),ellipse.type = "euclid")
dfcombined$index4=cluster4segment$cluster

Mode <- function(x, na.rm = FALSE) {if(na.rm){
  x = x[!is.na(x)]}
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])}
clustervector = aggregate(index4~id,dfcombined,Mode)
table(clustervector)

householdVec = aggregate(index4~id,dfcombined,Mode)
table(householdVec)
hhidc1 = subset(householdVec,index4==1)
hhidc2 =subset(householdVec,index4==2)
hhidc3 = subset(householdVec,index4==3)
hhidc4 = subset(householdVec,index4==4)

seg4data1 = subset(dfcombined,id %in% hhidc1$id)
seg4data2 =subset(dfcombined,id %in% hhidc2$id)
seg4data3 =subset(dfcombined,id %in% hhidc3$id)
seg4data4 =subset(dfcombined,id %in% hhidc4$id)

seg4c1id=lm(weeklyspend~recency+freq+d+t_loydisc_cat+t_display+t_favbrndcoup+t_mailer
            +hhsize+numkids+income+married+t_loydisc_cat+age+as.factor(id),data = seg4data1)
seg4c2id=lm(weeklyspend~recency+freq+d+t_loydisc_cat+t_display+t_favbrndcoup+t_mailer
            +hhsize+numkids+income+married+t_loydisc_cat+age+as.factor(id),data = seg4data2)
seg4c3id=lm(weeklyspend~recency+freq+d+t_loydisc_cat+t_display+t_favbrndcoup+t_mailer
            +hhsize+numkids+income+married+t_loydisc_cat+age+as.factor(id),data = seg4data3)
seg4c4id=lm(weeklyspend~recency+freq+d+t_loydisc_cat+t_display+t_favbrndcoup+t_mailer
            +hhsize+numkids+income+married+t_loydisc_cat+age+as.factor(id),data = seg4data4)
stargazer(seg4c1id,seg4c2id,seg4c3id,seg4c4id,out = "seg4id.html",single.row = TRUE)

seg4c1camp=lm(weeklyspend~recency+freq+spendingavg+d+t_loydisc_cat+t_display+t_favbrndcoup+t_mailer+
                +hhsize+numkids+income+age+married+Camp_A+Camp_B+Camp_C, data=seg4data1)
seg4c2camp=lm(weeklyspend~recency+freq+spendingavg+d+t_loydisc_cat+t_display+t_favbrndcoup+t_mailer+
                +hhsize+numkids+income+age+married+Camp_A+Camp_B+Camp_C, data=seg4data2)
seg4c3camp=lm(weeklyspend~recency+freq+spendingavg+d+t_loydisc_cat+t_display+t_favbrndcoup+t_mailer+
                +hhsize+numkids+income+age+married+Camp_A+Camp_B+Camp_C, data=seg4data3)
seg4c4camp=lm(weeklyspend~recency+freq+spendingavg+d+t_loydisc_cat+t_display+t_favbrndcoup+t_mailer+
                +hhsize+numkids+income+age+married+Camp_A+Camp_B+Camp_C, data=seg4data4)
stargazer(seg4c1camp,seg4c2camp,seg4c3camp,seg4c4camp,out = "seg4camp.html",single.row = TRUE)

mean(seg3data1$recency)
mean(seg3data2$recency)
mean(seg3data3$recency)

mean(seg3data1$freq)
mean(seg3data2$freq)
mean(seg3data3$freq)

mean(seg3data1$spendingavg)
mean(seg3data2$spendingavg)
mean(seg3data3$spendingavg)

mean(seg3data1$d)
mean(seg3data2$d)
mean(seg3data3$d)

mean(seg4data1$d)
mean(seg4data2$d)
mean(seg4data3$d)
mean(seg4data4$d)

mean(seg4data1$spendingavg)
mean(seg4data2$spendingavg)
mean(seg4data3$spendingavg)
mean(seg4data4$spendingavg)

mean(seg4data1$freq)
mean(seg4data2$freq)
mean(seg4data3$freq)
mean(seg4data4$freq)

mean(seg4data1$recency)
mean(seg4data2$recency)
mean(seg4data3$recency)
mean(seg4data4$recency)

mean(dfcombined$recency)
mean(dfcombined$freq)
mean(dfcombined$spendingavg)
mean(dfcombined$d)
