#6.4
data1<-read.table("6-4.txt")
names(data1)<-c("factory","life")
data1[,1]<-as.factor(data1[,1])
aov.1<-aov(life~factory,data1)
summary(aov.1)
a1<-data1[which(data1[,1]==1),2]
b1<-data1[which(data1[,1]==2),2]
c1<-data1[which(data1[,1]==3),2]
lowAB<-mean(a1)-mean(b1)-sqrt(18.4)*sqrt(1/6+1/6)*qt(1-0.05/6,15)
highAB<-mean(a1)-mean(b1)+sqrt(18.4)*sqrt(1/6+1/6)*qt(1-0.05/6,15)
lowAC<-mean(a1)-mean(c1)-sqrt(18.4)*sqrt(1/6+1/6)*qt(1-0.05/6,15)
highAC<-mean(a1)-mean(c1)+sqrt(18.4)*sqrt(1/6+1/6)*qt(1-0.05/6,15)
lowBC<-mean(b1)-mean(c1)-sqrt(18.4)*sqrt(1/6+1/6)*qt(1-0.05/6,15)
highBC<-mean(b1)-mean(c1)+sqrt(18.4)*sqrt(1/6+1/6)*qt(1-0.05/6,15)

#6.6
data2<-read.table("6-6.txt")
data2[,1]<-as.factor(data2[,1])
data2[,2]<-as.factor(data2[,2])
names(data2)<-c("factory","worker","life")
aov.2<-aov(life~factory:worker+factory+worker,data2)
summary(aov.2)
aov.2<-aov(life~factory+worker,data2)
summary(aov.2)

#6.9
data3<-read.table("6-9.txt")
names(data3)<-c("density","temp","result")
data3[,1]<-as.factor(data3[,1])
data3[,2]<-as.factor(data3[,2])
aov.3<-aov(result~density+temp+density:temp,data3)
summary(aov.3)

