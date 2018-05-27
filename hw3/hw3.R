library(xlsx)
library(pracma)
rawdata<-read.xlsx("3-15.xlsx",1)
rawdata<-rawdata[,-1]
#fit<-lm(rawdata$Y~.,data=rawdata)
#summary(fit)
n<-length(rawdata[,1])
X<-rawdata$X
Y<-rawdata$Y
beta<-t(X)%*%Y/(t(X)%*%X)
yHead<-X*beta
#原始最小二乘
eHead<-Y-yHead
sigma<-t(eHead)%*%eHead/(n-1)
H<-X%*%solve(t(X)%*%X)%*%t(X)
r<-c()
for(i in 1:n){
  r[i]<-eHead[i]/sqrt(sigma*(1-H[i,i]))
}
plot(yHead,r,main="residual plot")

#对Y开方最小二乘
U<-sqrt(Y)
betaU<-t(X)%*%U/(t(X)%*%X)
UHead<-X*betaU
eUHead<-U-UHead
sigmaU<-t(eUHead)%*%eUHead/(n-1)
Hu<-X%*%solve(t(X)%*%X)%*%t(X)
rU<-c()
for(i in 1:n){
  rU[i]<-eUHead[i]/sqrt(sigmaU*(1-Hu[i,i]))
}
plot(UHead,rU,main="residual plot of U")

#BoxCox最小二乘
BoxCox<-function(x,lambda){
  n<-length(x)
  z<-c()
  if(lambda==0){
    for(i in 1:n){
      z[i]<-log(x[i])*(prod(x))^(1/n)}
  }
  if(lambda!=0){
    for(i in 1:n){
      z[i]<-x[i]^lambda/(prod(x))^((lambda-1)/n)}
  }
  return(z)
}
seq<-seq(from=0,to=1,by=0.01)
rss<-c()
for(i in seq){
  z<-BoxCox(Y,i)
  HI<-eye(n,n)-X%*%solve(t(X)%*%X)%*%t(X)
  rss[which(seq==i)]<-t(z)%*%HI%*%z
}
plot(seq,rss,xlab="lambda",main="plot of RSS and lambda",ylab="RSS")
mint<-which(rss==min(rss))
seq[mint]#找出最小RSS对应的lambda

#计算Cook统计量
D<-c()
for(i in 1:n){
  D[i]<-(H[i,i]/(1-H[i,i]))*r[i]^2
}
