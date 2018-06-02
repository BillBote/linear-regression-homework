#1向前法
setwd("/Users/mac/Documents/statistics and R/linner regression/hw4")
library(xlsx)
rawdata<-read.xlsx("5-7.xlsx",1)
attach(rawdata)
fit.raw<-lm(y~1,rawdata)
add1(fit.raw,y~x1+x2+x3+x4+x5,test = "F")
fit1<-lm(y~x4,rawdata)
add1(fit1,y~x1+x2+x3+x4+x5,test = "F")
fit2<-lm(y~x3+x4,rawdata)
add1(fit2,y~x1+x2+x3+x4+x5,test = "F")
#逐步回归
step(fit.raw,y~x1+x2+x3+x4+x5)
#计算所有子集的回归模型和RSSq以及AIC
num<-c(2,3,4,5,6)
n<-length(rawdata[,1])
RSSq<-c()
AIC<-c()
RSSq<-cbind(RSSq,deviance(fit.raw)/(n-1))
AIC<-cbind(AIC,AIC(fit.raw))
g1<-combn(num,1)
for(i in 1:length(g1)){
  fit<-lm(rawdata[,1]~rawdata[,g1[i]])
  RSSq<-cbind(RSSq,deviance(fit)/(n-2))
  AIC<-cbind(AIC,AIC(fit))
}
g2<-combn(num,2)
for(i in 1:length(g2[1,])){
  fit<-lm(rawdata[,1]~rawdata[,g2[,i][1]]+rawdata[,g2[,i][2]])
  RSSq<-cbind(RSSq,deviance(fit)/(n-3))
  AIC<-cbind(AIC,AIC(fit))
}
g3<-combn(num,3)
for(i in 1:length(g3[1,])){
  fit<-lm(rawdata[,1]~rawdata[,g3[,i][1]]+rawdata[,g3[,i][2]]+rawdata[,g3[,i][3]])
  RSSq<-cbind(RSSq,deviance(fit)/(n-4))
  AIC<-cbind(AIC,AIC(fit))
}
g4<-combn(num,4)
for(i in 1:length(g4[1,])){
  fit<-lm(rawdata[,1]~rawdata[,g4[,i][1]]+rawdata[,g4[,i][2]]+rawdata[,g4[,i][3]]+rawdata[,g4[,i][4]])
  RSSq<-cbind(RSSq,deviance(fit)/(n-5))
  AIC<-cbind(AIC,AIC(fit))
}
fit.all<-lm(y~x1+x2+x3+x4+x5,rawdata)
RSSq<-cbind(RSSq,deviance(fit.all)/(n-6))
AIC<-cbind(AIC,AIC(fit.all))
#选出最优组合
which(AIC==min(AIC))
which(RSSq==min(RSSq))
#对最优组合进行回归
fit.final<-lm(y~.,rawdata)
summary(fit.final)
