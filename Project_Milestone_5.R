fb_clean<- read.csv("facebook_dataset_cleaned")
fb_clean<- na.omit(fb_clean)
fb_clean <- fb_clean[!(fb_clean$like ==0),] 
fb_clean$Paid <- as.factor(fb_clean$Paid)
fb_clean$Post.Weekday <- as.factor(fb_clean$Post.Weekday)
fb_clean$Type <- as.factor(fb_clean$Type)

## splitting data
RNGkind(sample.kind = "Rejection")
set.seed(2020)
sample.data <- sample.int(nrow(fb_clean), floor(.50*nrow(fb_clean)), replace = F)
train<-fb_clean[sample.data, ]
test<-fb_clean[-sample.data, ]

## Classification Tree
library(tree)
fit.tree <- tree(like ~ Type + Lifetime.Post.Consumptions + Paid + Post.Weekday + Lifetime.Engaged.Users + comment + share + Category, data=train)
summary(fit.tree)

## graphical output of tree
plot(fit.tree)
text(fit.tree, cex=0.75)

## Pruning the tree
set.seed(2020)
cv.all<-cv.tree(fit.tree, K=10)

plot(cv.all$size, cv.all$dev, type="b", xlab="Size of Tree", ylab="Deviance")
trees.num<-cv.all$size[which.min(cv.all$dev)]

prune.full<- prune.tree(fit.tree, best=trees.num)
summary(prune.full)
plot(prune.full)
text(prune.full, cex=0.75, pretty=0) 

## Finding test MSE for recursive tree and pruned tree
predict.tree <- predict(fit.tree, newdata=test)
testMSE<- mean((predict.tree - test$like)^2) # test MSE recursive binary tree

pruned.predict.tree <- predict(prune.full, newdata=test) 
testMSEpruned <- mean((pruned.predict.tree - test$like)^2) # test MSE for pruned tree

## Random Forest
library(randomForest)
set.seed(2020)
preds <- floor(sqrt(8))
rf.fb <- randomForest(like ~ Type + Lifetime.Post.Consumptions + Paid + Post.Weekday + Lifetime.Engaged.Users + comment + share + Category, data=train, mtry=preds, importance=TRUE)
rf.fb

round(importance(rf.fb),2) # important variables
varImpPlot(rf.fb)

predictrf <- predict(rf.fb, newdata=test) 
testMSErf <- mean((predictrf-test$like)^2) # test MSE for random forest

