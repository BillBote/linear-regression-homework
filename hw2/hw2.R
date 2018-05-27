library(pracma)#这个包比较好用，熟悉matlab的人用起来非常顺手
Rawdata<-read.csv("longley.csv")
X<-Rawdata[,-7]
mean<-c()
var<-c()
for(i in 1:length(X[1,])){
  mean[i]<-mean(X[,i])
  var[i]<-var(X[,i])
}
mat<-as.matrix(X)
matsd<-mat-ones(16,1)%*%mean#中心化
matsd2<-cbind(matsd[,1]/var[1],matsd[,2]/var[2],matsd[,3]/var[3],matsd[,4]/var[4],matsd[,5]/var[5],matsd[,6]/var[6])
y<-as.matrix(Rawdata[,7])
#计算x'x的特征值
Eigen<-eigen(t(matsd2)%*%matsd2)$values
k<-max(Eigen)/min(Eigen)
k
#主成分分析
data.pr<-princomp(X,cor=T)
summary(data.pr,loadings = T)
#取前三个主成分
pre<-predict(data.pr)
Rawdata$z1<-pre[,1]
Rawdata$z2<-pre[,2]
Rawdata$z3<-pre[,3]
fit<-lm(Rawdata$Employed~Rawdata$z1+Rawdata$z2+Rawdata$z3,data=Rawdata)

vector<-eigen(t(matsd2)%*%matsd2)$vectors
 lm.f<-function(x){
  y<-fit$coefficients[1]
  for(i in 1:3)
    y<-y+fit$coefficients[i+1]*(t(x-mean)/sqrt(var))%*%vector[,i]
  return(y)
}

#岭估计
#Hoerl-Kennard公式
fa<-eigen(t(matsd)%*%matsd)$vectors
Z<-matsd%*%fa
alpha<-solve(t(Z)%*%Z)%*%t(Z)%*%y
lm.raw<-lm(Rawdata$Employed~.,data=Rawdata)
pred.raw<-predict(lm.raw,Rawdata[,-7])
e<-Rawdata[,7]-pred.raw#估计sigma
sigma<-var(e)
k<-sigma/(max(alpha))^2
#岭迹法
library(MASS)
lmRidge<-lm.ridge(Rawdata$Employed~Rawdata$GNP.deflator+Rawdata$GNP+Rawdata$Unemployed+Rawdata$Armed.Forces+Rawdata$Population+Rawdata$Year,data=Rawdata,lambda=seq(0,0.5,0.001))
plot(lmRidge)
