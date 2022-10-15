#set work directory
setwd("C:/Users/qiuxi/Desktop/Fall 2020/STAT 4630")
set.seed(2021)
#package loading
library(rAverage)
library(ggplot2)
library(grid)
library(gridExtra)
library(MASS)
library(car)
#data loading
df<-read.csv('facebook_dataset_cleaned', header = TRUE, sep = ",")
head(df)
summary(df)
#subset df to have only columns interested in 
df2<-df[,c('like','Type','Lifetime.Post.Consumptions','Paid','Post.Weekday',
           'Lifetime.Engaged.Users','comment','share','Category')]
head(df2)
df2$Paid<-as.factor(df2$Paid)
df2$Type<-as.factor(df2$Type)
df2$Post.Weekday<-as.factor(df2$Post.Weekday)
df2$Category<-as.factor(df2$Category)

#split train and test:
RNGkind(sample.kind = "Rejection")
set.seed(2020)
fbclean<-na.omit(df2[!(df2$like == 0),])
sample.data <- sample.int(nrow(fbclean), floor(.50*nrow(fbclean)), replace = F)
train<-fbclean[sample.data, ]
test<-fbclean[-sample.data, ]

#EDA graphic summaries redo

#scatterplot: Q. vs. Q.
p1<-ggplot(train, aes(x=Lifetime.Post.Consumptions, y=like)) + geom_point()
p2<-ggplot(train, aes(x=Lifetime.Engaged.Users, y=like)) + geom_point()
p3<-ggplot(train, aes(x=comment, y=like)) + geom_point()
p4<-ggplot(train, aes(x=share, y=like)) + geom_point()

grid.arrange(p1,p2,p3,p4,nrow=2)
#boxplots: Q. vs. C.
par(mfrow = c(2, 2))
train$ol1<- ifelse(train$like %in% boxplot.stats(train$like)$out, 1, 0)
train[train$ol1 ==1 , "like"] = NA
boxplot(like~Type,
        data=train,
        ylab="Like count",
        xlab="Type",
        main='Boxplot of Post Likes based on Types')

boxplot(like~Paid,
        data=train,
        ylab="Like count",
        xlab="Paid",
        main='Boxplot of Post Likes based on Promotion')


boxplot(like~Post.Weekday,
        data=train,
        ylab="Like count",
        xlab="Post Weekday",
        main='Boxplot of Post Likes based on Post Weekday')

boxplot(like~Category,
        data=train,
        ylab="Like count",
        xlab="Category",
        main='Boxplot of Post Likes based on Category')



#REGRESSION ANALYSIS
#check linearity
fit <- lm(like ~ Type + Lifetime.Post.Consumptions + Paid + Post.Weekday + Lifetime.Engaged.Users+ comment + share + Category, data=train)
par(mfrow = c(2, 2))

plot(fit)


#check normality of variables(hist or qqplot)
plot(fit,2)


#check multicolinearity
car::vif(fit)



