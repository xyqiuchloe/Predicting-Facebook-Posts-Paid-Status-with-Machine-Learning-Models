#set work directory
setwd("C:/Users/qiuxi/Desktop/Fall 2020/STAT 4630")
set.seed(2021)
#package loading
library(MASS)
library(klaR)
library(ICS)
library(rAverage)
library(ggplot2)
library(grid)
library(gridExtra)
library(MASS)
library(car)
#data loading
df<-read.csv('facebook_dataset_cleaned', header = TRUE, sep = ",")
df<-df[,c('Paid','like','Lifetime.Post.Consumptions',
          'Lifetime.Engaged.Users','comment','share','Lifetime.Post.Total.Reach')]
df<-na.omit(df2[!(df2$like == 0),])
df$Paid<-as.factor(df$Paid)

head(df)
summary(df)
#split data
RNGkind(sample.kind = "Rejection")
set.seed(2020)
#fbclean<-na.omit(df[!(df$like == 0),])
sample.data <- sample.int(nrow(df), floor(.50*nrow(df)), replace = F)
train<-df[sample.data, ]
test<-df[-sample.data, ]

# extra: logistic regression model multicollinearity
result<-glm(Paid ~ ., data=train, family="binomial")
car::vif(result)

##mvn tests
paid<-df[which(df$Paid==1),]
notpaid<-df[which(df$Paid==0),]
mvnorm.kur.test(paid[,2:7])
mvnorm.skew.test(notpaid[,2:7])

#LDA
lda.model<-lda(Paid ~ ., data=train)
partimat(Paid ~ like + Lifetime.Post.Consumptions + Lifetime.Engaged.Users + comment +Lifetime.Post.Total.Reach, nplots.vert=3, nplots.hor=3, data=train, method="lda")

#confusion matrix
##apply lda model to test data
lda.test <- predict(lda.model,test)

##confusion matrix, with actual values in rows, prediction in columns 
table(test$Paid,lda.test$class)

table(test$Paid, lda.test$posterior[,2]>0.3)
table(test$Paid, lda.test$posterior[,2]>0.5)

##overall accuracy
mean(test$Paid == lda.test$class)
#
table(test$Paid, lda.test$posterior[,2]>0.5)
library(ROCR)
par(mfrow = c(1, 1))
preds<-lda.test$posterior[,2] ##Use column 2 since Virginica since that is the class coded as 1
rates<-prediction(preds, test$Paid)
roc_result<-performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve for LDA model")
lines(x = c(0,1), y = c(0,1), col="red")

##AUC
auc<-performance(rates, measure = "auc")

performance(rates,measure='auc')@y.values[[1]]

#k fold
library(MASS) ##for lda function
library(ipred) ##for errorest function needed for k fold CV
install.packages('ipred')
install.packages('boot')
lda.result<-lda(Paid ~ ., data=train, CV=TRUE)
cv.da <- function(object, newdata) 
  
{
  
  return(predict(object, newdata = newdata)$class)
  
} 

##this returns the k fold CV estimate for the test classification error rate
set.seed(2020)
errorest(Paid ~ ., data=train, model=lda, estimator="cv", est.para=control.errorest(k=5), predict=cv.da)$err 
errorest(Paid ~ ., data=train, model=lda, estimator="cv", est.para=control.errorest(k=10), predict=cv.da)$err 

result<-glm(Paid ~ ., data=df, family="binomial")
five.fold<-cv.glm(df,result, K=5) ##k specified
five.fold$delta

result
