setwd("/Users/mac/Documents/statistics and R/linner regression")
library(xlsx)
raw.data<-read.xlsx("hw1.xlsx",sheetIndex = 1)
library(scatterplot3d)
par(fig=c(0.1,1,0.1,1))
attach(raw.data)
#绘制y关于x1和x2的图像
scatterplot3d(raw.data$x1,raw.data$x2,raw.data$y,main="3D plot of correlation between x1,x2 and y",xlab = "x1",ylab = "x2",zlab="y")
#绘制y关于x1的图像
plot(raw.data$y,raw.data$x1,main="plot of y and x1",xlab="x1",ylab="y")
#绘制y关于x2的图像
plot(raw.data$y,raw.data$x2,main="plot of y and x2",xlab="x2",ylab="y")

#线性回归
fit<-lm(y~.,data=raw.data)
fit
#随机模拟
set.seed(1234)
x<-as.data.frame(cbind(raw.data$x1,raw.data$x2))
names(x)<-c("x1","x2")
pred<-predict(fit,x)
e<-raw.data$y-pred
SD<-sd(e)
l<-length(raw.data$y)
#单次模拟
eSim<-rnorm(l,sd=SD)
ySim<-eSim+pred
dataSim<-as.data.frame(cbind(x,ySim))
names(dataSim)<-c("x1","x2","y")
fitSim<-lm(y~.,data=dataSim)
fitSim
beta<-as.data.frame(fitSim$coefficients)[,1][c(1,2,3)]
#重复N次
N<-1000
for(i in 1:N){
  eSim<-rnorm(l,sd=SD)
  ySim<-eSim+pred
  dataSim<-as.data.frame((cbind(x,ySim)))
  names(dataSim)<-c("x1","x2","y")
  fitSim<-lm(y~.,data=dataSim)
  beta0<-as.data.frame(fitSim$coefficients)[,1][c(1,2,3)]
  beta<-cbind(beta,beta0)
}
#计算模拟的均值和方差阵
rowMeans(beta)
cov(t(beta))
