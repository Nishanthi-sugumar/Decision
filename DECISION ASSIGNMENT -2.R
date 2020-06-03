companydata<-read.csv(file.choose())
data(companydata)
View(companydata)
library(caret)
library(C50)
high=ifelse(companydata$Sales<10,'no','yes')
CD=data.frame(companydata,high)
CD_train<-CD[1,200]
CD_test<-CD[201,400]
local<-createDataPartition(CD$high,p=.75,list=F)
training<-CD[local,]
View(training)
testing<-CD[local,]
View(testing)
model<-C5.0(training$high~.,data=training,trails=40)
summary(model)
pred<-predict.C5.0(model,testing[,-12])
pred
table(pred)
a<-table(testing$high,pred)
a
sum(diag(a)/sum(a))
plot(model)
acc<-c()
for(i in 1:100)
{
  print(i)
  local<-createDataPartition(CD$high,p=.85,list=F)
  training1<-CD[local,]
  testing<-CD[-local,]
  
  fittree<-C5.0(training1$high~.,data=training1)
  pred<-predict.C5.0(fittree,testing[,-12])
  a<-table(testing$high,pred)
  
  acc<-c(acc,sum(diag(a))/sum(a))
  
}
summary(acc)
acc
