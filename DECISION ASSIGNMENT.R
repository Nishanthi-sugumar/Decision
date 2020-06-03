frauddata<-read.csv(file.choose())
data(frauddata)
View(frauddata)
library(caret)
library(C50)
Risky_Good = ifelse(frauddata$Taxable.Income<= 30000, "Risky", "Good")
FC=data.frame(frauddata,Risky_Good)

FC_train <- FC[1:300,]

FC_test <- FC[301:600,]

inTraininglocal <- createDataPartition(FC$Risky_Good,p=.75,list=F)
training<-FC[inTraininglocal,]
View(training)
testing<-FC[-inTraininglocal,]
View(testing)
model <- C5.0(training$Risky_Good~.,data = training,trails = 40)
summary(model)
pred <- predict.C5.0(model,testing[,-7])
pred
table(pred)
a <- table(testing$Risky_Good,pred)
a
sum(diag(a)/sum(a))
plot(model)
acc<-c()
for(i in 1:100)
{
  print(i)
  inTraininglocal<-createDataPartition(iris$Species,p=.85,list=F)
  training1<-iris[inTraininglocal,]
  testing<-iris[-inTraininglocal,]
  
  fittree<-C5.0(training1$Species~.,data=training1)
  pred<-predict.C5.0(fittree,testing[,-5])
  a<-table(testing$Species,pred)
  
  acc<-c(acc,sum(diag(a))/sum(a))
  
}
summary(acc)
acc