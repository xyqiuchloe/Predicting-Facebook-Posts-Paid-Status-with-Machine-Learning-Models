#set work directory

df<-read.csv('dataset_Facebook.csv', header = TRUE, sep = ";")
head(df)
summary(df)
library(ggplot2)
library(grid)
library(gridExtra)

#response variables
par(mfrow=c(1,1))

barplot(table(df$like),
        main="Distribution of Number of Like for post",
        xlab="number of likes",
        ylab="count of post",
        border="blue",
        col="blue",
        density=10)

tb1<-table(df$Paid)
names(tb1)<-c('not promotion','promotion')
barplot(tb1,
        main="Distribution of Promotion for post",
        ylab="count of post",
        border="blue",
        col="blue",
        density=10)
#independent variables interested:
#Q1:quantitative: page total likes, lifetime post consumption, lifetime engaged users, lifetime post total reach
p1<-ggplot(df, aes(x=Page.total.likes, y=like)) + geom_point()
p2<-ggplot(df, aes(x=Lifetime.Post.Consumptions, y=like)) + geom_point()
p3<-ggplot(df, aes(x=Lifetime.Engaged.Users, y=like)) + geom_point()
p4<-ggplot(df, aes(x=Lifetime.Post.Total.Reach, y=like)) + geom_point()

grid.arrange(p1,p2,p3,p4,nrow=2)

#Q2:quantitative: number of comments, number of likes, number of shares, lifetime post consumption, page total likes,month
moddf<-df
#moddf$like>5000
moddf[245,]<-NA
boxplot(like~Paid,
        data=moddf,
        ylab="like",
        xlab="Promoted or not",
        main='Boxplot of Post Likes')
boxplot(comment~Paid,
        data=moddf,
        ylab="comment",
        xlab="Promoted or not",
        main='Boxplot of Post Comments')
boxplot(share~Paid,
        data=moddf,
        ylab="share",
        xlab="Promoted or not",
        main='Boxplot of Post Shares')
boxplot(Lifetime.Post.Consumptions~Paid,
        data=moddf,
        ylab="Lifetime Post Consumptions",
        xlab="Promoted or not",
        main='Boxplot of ifetime Post Consumptions')


#categorical: 
#Q1:type of post, paid
boxplot(like~Type,
        data=moddf,
        ylab="Like count",
        xlab="Type",
        main='Boxplot of Post Likes based on Types')

#Q2:type of post,category of post, 
x2<-xtabs(~ Type + Paid, data=df)
#tb2<-table(df$Type)
#names(tb2)
colnames(x2)<-c('not promotion','promotion')
barplot(x2,main="Distribution of Type and Promotion",
        ylab="count",
        beside = TRUE,
        legend = TRUE)
#tb3<-table(df$Category)
#names(tb3)<-c('action', 'product', 'inspiration')
x3<-xtabs(~ Category + Paid, data=df)
rownames(x3)<-c('action', 'product', 'inspiration')
colnames(x3)<-c('not promotion','promotion')

barplot(x3,main="Distribution of Category and Promotion",
        ylab="count",
        beside = TRUE,
        legend = TRUE)
#plot of means

#numerical summaries
#Q1: lifetime post consumption, lifetime engaged users, lifetime post total reach
#Q2:quantitative: number of comments, number of likes, number of shares,  page total likes,month

num<-df[,c(1,8,10,12,16,17,18)]
summary(num)
