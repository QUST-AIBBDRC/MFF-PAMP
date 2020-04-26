x1=read.csv(file.choose(),head=FALSE) 
x2=read.csv(file.choose(),head=FALSE)
xx=rbind(x1,x2)
y1<- matrix(1,nrow=879,ncol=1)
y2<- matrix(0,nrow=2405,ncol=1)
yy=rbind(y1,y2)
mydata<-data.frame(yy,xx)
xx_matrix<-as.matrix(xx)
yy_matrix<-as.matrix(yy)
library(DMwR)
xx_standard<-xx_matrix
mydata_smote<-data.frame(yy,xx_standard)
mydata_smote$yy<-factor(mydata_smote$yy)
newData <- SMOTE(yy ~ ., mydata_smote, perc.over = 200,perc.under=100)
library(caret)
folds <- createFolds(newData$yy,k=10)
sum<-0
sum1<-0
sum2<-0
sum3<-0
for(i in 1:10)
{fold_test <- newData[folds[[i]],]       
fold_train <- newData[-folds[[i]],]   
print(i)
newData$yy<-factor(newData$yy,levels = c(0,1),labels = c("0","1"))
fit.logit<-glm(yy~.,data = fold_train,family = "binomial")
p1<-predict(fit.logit,fold_test,type = "response")
p<-factor(p1<0.5,levels = c(TRUE,FALSE),labels = c("0","1"))
duibi<-data.frame(prob=p,obs=fold_test$yy)
library(caret)
jieguo<-confusionMatrix(duibi$prob,duibi$obs)
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
pred<-prediction(predictions = roc_results$p1,labels = roc_results$fold_test.yy)
perf<-performance(pred,measure = "tpr",x.measure = "fpr")
plot(perf,main="LogisticsRegression",col="blue",lwd=3)
abline(a=0,b=1,lty=2)
perf.auc<-performance(pred,measure = "auc")
str(perf.auc)
unlist(perf.auc@y.values)
sum3<-as.numeric(perf.auc@y.values)+sum3
average3<-sum3/10
}

print(average)
print(average1)
print(average2)
print(average3)
write.csv(A,file="probability_AMP.csv")
write.csv(B,file="label_AMP.csv")