# iconv -f gb18030 -t utf-8 原文件.csv > 目标文件.csv
#https://www.kaggle.com/ruiqurm/lianjia
install.packages("nortest")
install.packages("tidyverse")
library(nortest)
library(tree)
library(gbm)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(gridExtra)
library(ggplot2) 

library(tidyverse)
library(psych)
library(lubridate)
library(xts)
library(tseries)
library(forecast)
#Visualisation 
library(corrplot)
library(plotly)
library(viridis)
library(ggmap)
library(knitr)
library(dygraphs)
library(ggthemes)
#nnew is the kaggle by 

library(readr)
dir()
nnew <- read_csv("nnew.csv", col_types = cols(floor = col_number()))

dim(nnew)
head(nnew)
new_utf8=nnew

#data cleaning
d = na.omit(new_utf8) # delete NA
names(d)

dt = d[,c(7:9,11:20,22:24)] #drop Url, GPS inof,  unit price, ladderRatio,tradetime, district code

dt = dt[-1,] # make itg even row
names(dt)
dt$constructionTime=as.numeric(dt$constructionTime)
dt$constructionTime=2020-dt$constructionTime
dt$constructionTime[is.na(dt$constructionTime)]=60
summary(dt$totalPrice)
barplot(dt$totalPrice)
summary(dt)
str(dt$constructionTime)


#check data
summary(dt)
barplot(dt$totalPrice,main="TotalPrice Barplot")
boxplot(dt$square,main="size")
boxplot(dt$totalPrice, main="TotalPrice Boxplot")
boxplot(dt$constructionTime)
barplot(dt$fiveYearsProperty)
ggplot(data=dt, aes(x=totalPrice, constructionTime)) +
  geom_point() +
  geom_rug(col="steelblue",alpha=0.1, size=1.5)


ggplot(data=dt, aes(x=square, totalPrice)) +
  geom_point() +
  geom_rug(col="steelblue",alpha=0.1, size=1.5)

qqnorm(dt$totalPrice)
qqline(dt$totalPrice)

cor_numVar <- cor(select_if(dt, is.numeric), #correlations of all numeric variables
                  use="all.obs",method="spearman")
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)

#create a training and test set to test for later use:
set.seed(1)
size=dim(dt)
train = sample(1:size[1],size[1]/2)
dt.train = dt[train,]
dim(dt.train)
#
set.seed(3)
dt.other = dt[-train,]
valid = sample(1:nrow(dt.other),size[1]/2)
dt.valid = dt.other[valid,]
dim(dt.valid)

#lm model for all 
reg1 = lm(totalPrice~., data = dt)
summary(reg1)
reg1
reg1sum=summary(reg1)
mean(abs(reg1sum$residuals))
msereg1=mean(reg1sum$residuals^2)  
sqrt(msereg1)
msereg1
36539.85



#what about a small model, which only look at the top 5 factor
reg2 <- lm(totalPrice~square+constructionTime+subway+elevator, data = dt)
summary(reg2)
anova(reg2, reg1, test = "Chisq")
# P-value is near zero Reject H0
# Reg1 whith all factors is better


set.seed(1)
tree.all = tree(totalPrice~., data = dt) #take account all factors
plot(tree.all)
text(tree.all,pretty = 0)
tree.all
yhat.tree.all = predict(tree.all, dt)
yhat.tree.all
MSE.tree.all = mean((dt$totalPrice-yhat.tree.all)^2)
MSE.tree.all
#37556.88

#prune the tree to get rid off some branch :

prune.tree.all = prune.tree(tree.all)
prune.tree.all
plot(prune.tree.all)

prune.tree = prune.tree(tree.all,best = 6) # we only look 6 branch
plot(prune.tree)
text(prune.tree, pretty = 0)
yhat.prune.tree = predict(prune.tree, dt)
MSE.prune.tree  = mean((dt$totalPrice - yhat.prune.tree)^2)
MSE.prune.tree
#41410.15
# getting worse


#Generalized Boosted Regression Models

boost <- gbm(totalPrice~., data = dt.train, distribution = "gaussian", 
             n.trees = 2000, interaction.depth = 12)
summary(boost)

boost
yhat.boost <- predict(boost, dt.valid, n.trees=2000)
MSE.boost <- mean((dt.valid$totalPrice - yhat.boost)^2)
MSE.boost
#26528.66
 
# there is no normality, so try cluster analysis\
ad.test(dt$totalPrice)
ad.test(dt$square)
ad.test(dt$totalPrice/dt$square)


#check price <2000
mean(dt$followers)

dt1=subset(dt,dt$DOM<50&dt$followers >100)
reg2000 = lm(totalPrice~., data = dt1)
summary(reg2000)
reg2000sum=summary(reg2000)
mean(abs(reg2000sum$residuals))
mean(abs(reg2000sum$residuals))^2

msereg2000=mean(reg2000sum$residuals^2)  
msereg2000
sqrt(msereg2000)
msereg1
head(dt1)
35834.98
dt1











#https://uc-r.github.io/kmeans_clustering


df <- na.omit(dt[901:1000,3:4])

#As we don’t want the clustering algorithm to depend to an arbitrary variable unit, 
#we start by scaling/standardizing the data using the R function scale:
df <- scale(df)
summary(df)
distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
k6=kmeans(df, centers = 6, nstart = 25)
str(k6)
k6
fviz_cluster(k6, data = df)

k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k6, geom = "point", data = df) + ggtitle("k = 6")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")


grid.arrange(p2, p3, p4, p1, nrow = 2)


set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

set.seed(123)

fviz_nbclust(df, kmeans, method = "wss")

set.seed(123)
final <- kmeans(df, 7, nstart = 25)
fviz_cluster(final, data = df)

##stop here 


