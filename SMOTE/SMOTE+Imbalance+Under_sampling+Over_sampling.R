x1=read.csv(file.choose(),head=FALSE) 
x2=read.csv(file.choose(),head=FALSE)
xx1=rbind(x1,x2)                
xx=scale(xx1,center=T,scale=T)
y1<- matrix(1,nrow=920,ncol=1)
y2<- matrix(0,nrow=920,ncol=1)
yy=rbind(y1,y2)
mydata<-data.frame(yy,xx)
##SMOTE
library(DMwR)
#mydata_smote<-data.frame(yy,xx_standard)
mydata$yy<-factor(mydata$yy)
newData <- SMOTE(yy ~ ., mydata)
#newData <- SMOTE(yy ~ ., mydata)
##Imbalance
newData <-data.frame(yy,xx_standard)
newData$yy <-factor(newData$yy)
##Under-sampling and Over-sampling
library(ROSE)
newData <-data.frame(yy,xx_standard)
newData$yy <-factor(newData$yy)
newData<- ovun.sample(yy ~., data = mydata, method = "over",N=4810)$data
newData<- ovun.sample(yy ~., data =mydata, method = "under", N =1756)$data
library(caret)
folds <- createFolds(newData$yy,k=10)

library(randomForest)
sum<-0
sum1<-0
sum2<-0
sum3<-0
A<-c(99,99)
B<-c(99,99)
for(i in 1:10)
{fold_test <- newData[folds[[i]],]     
fold_train <- newData[-folds[[i]],]   
print(i)
m<-randomForest(fold_train[,-1],fold_train$yy,ntree=500)
p<-predict(m,fold_test[,-1],type="response")
p1<-predict(m,fold_test[,-1],type="prob")
#p2<-data.frame(p1)
A<-rbind(A,p1)
duibi<-data.frame(prob=p,obs=fold_test$yy)
B<-rbind(B,duibi)
library(caret)
jieguo<-confusionMatrix(duibi$prob,duibi$obs,positive = "1")
print(jieguo)
sum<-jieguo$overall[1]+sum
average<-sum/10
sum1<-jieguo$byClass[1]+sum1
average1<-sum1/10
sum2<-jieguo$byClass[2]+sum2
average2<-sum2/10
p11=data.frame(p1)
roc_results=data.frame(fold_test$yy,p11)
library(ROCR)
pred<-prediction(predictions = roc_results$X1,labels = roc_results$fold_test.yy)
perf<-performance(pred,measure = "tpr",x.measure = "fpr")
plot(perf,main="RandomForest",col="blue",lwd=3)
abline(a=0,b=1,lty=2)
perf.auc<-performance(pred,measure = "auc")
str(perf.auc)
unlist(perf.auc@y.values)
sum3<-as.numeric(perf.auc@y.values)+sum3
average3<-sum3/10
}
A<-as.data.frame(A)
A<-A[-1,]
B<-as.data.frame(B)
B<-B[-1,]
print(average)
print(average1)
print(average2)
print(average3)
write.csv(A,file="probability_AMP.csv")
write.csv(B,file="label_AMP.csv")