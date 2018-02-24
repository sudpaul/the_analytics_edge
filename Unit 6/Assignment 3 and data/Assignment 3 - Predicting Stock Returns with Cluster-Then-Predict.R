Sys.setlocale("LC_ALL", "C")

#Read the data.
stocks = read.csv("StocksCluster.csv")

#How many observations are in the dataset?
nrow(stocks)
##Answer: 11580

#What proportion of the observations have positive returns in December?
mean(stocks$PositiveDec)
##Answer: 0.546114

#What is the maximum correlation between any two return variables in the dataset? 
cor(stocks)
##Answer: 0.19167279, between ReturnOct and ReturnNov.

#Which month (from January through November) has the largest mean return across 
#all observations in the dataset?
summary(stocks)
##Answer: April, with mean 0.026308.

#Which month (from January through November) has the smallest mean return across
#all observations in the dataset?
##Answer: September, with mean -0.014721.


#Problem 2 - Initial Logistic Regression Model


#Split the data into a training set and testing set.
library(caTools)
set.seed(144)
split = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
train = subset(stocks, split == TRUE)
test = subset(stocks, split == FALSE)

#Use the train data frame to train a logistic regression model to predict
#PositiveDec using all the other variables as independent variables.
log = glm(PositiveDec~., data=train, family=binomial)

#What is the overall accuracy on the training set, using a threshold of 0.5?
pred.train.log = predict(log,type="response")
table(train$PositiveDec, pred.train.log>0.5)
##Answer: (990+3640)/nrow(train) = 0.5711818

#Now obtain test set predictions from StocksModel. What is the overall accuracy
#of the model on the test, again using a threshold of 0.5?
pred.test.log = predict(log, newdata=test, type="response")
table(test$PositiveDec, pred.test.log>0.5)
##Answer: (417+1553)/nrow(test) = 0.5670697

#What is the accuracy on the test set of a baseline model that always predicts 
#the most common outcome?
table(test$PositiveDec)
##Answer: 1897/nrow(test) = 0.5460564


#Problem 3 - Clustering Stocks


#Now, let's cluster the stocks. The first step in this process is to remove the
#dependent variable using the following commands:
ltrain = train
ltrain$PositiveDec = NULL
ltest = test
ltest$PositiveDec = NULL

#Why do we need to remove the dependent variable in the clustering phase of the 
#cluster-then-predict methodology?
##Answer: Needing to know the dependent variable value to assign an observation 
##to a cluster defeats the purpose of the methodology.

library(caret)
preproc = preProcess(ltrain)
ntrain = predict(preproc, ltrain)
ntest = predict(preproc, ltest)

#What is the mean of the ReturnJan variable in ntrain?
mean(ntrain$ReturnJan)
##Answer: 2.100586e-17

#What is the mean of the ReturnJan variable in ntest?
mean(ntest$ReturnJan)
##Answer: -0.0004185886

#Why is the mean ReturnJan variable much closer to 0 in ntrain than in ntest?
##Answer: The distribution of the ReturnJan variable is different in the 
##training and testing set.

#Run k-means clustering with 3 clusters on ntrain, storing the result in an 
#object called kmc.
set.seed(144)
kmc = kmeans(ntrain, centers=3)

#Which cluster has the largest number of observations?
str(kmc)
##Answer: 2

#use the flexclust package to obtain training set and testing set cluster 
#assignments for our observations.
library(flexclust)
kmc.kcca = as.kcca(kmc, ntrain)
trainCluster = predict(kmc.kcca)
testCluster = predict(kmc.kcca, newdata=ntest)

#How many test-set observations were assigned to Cluster 2?
table(testCluster)
##Answer: 2080


#Problem 4 - Cluster-Specific Predictions


#Using the subset function, build data frames train1, train2, and train3, 
#containing the elements in the train data frame assigned to clusters 1, 2, and
# 3, respectively. Similarly build test1, test2, and test3 from the test data
#frame.
train1 = subset(train, trainCluster==1)
train2 = subset(train, trainCluster==2)
train3 = subset(train, trainCluster==3)
test1 = subset(test, testCluster==1)
test2 = subset(test, testCluster==2)
test3 = subset(test, testCluster==3)

#Which training set data frame has the highest average value of the dependent 
#variable?
mean(train1$PositiveDec)
mean(train2$PositiveDec)
mean(train3$PositiveDec)
##Answer: train1

#Build logistic regression models log1, log2, and log3, which predict
#PositiveDec using all the other variables as independent variables. 
#Log1 should be trained on train1, log2 should be trained on train2, and log3 
#should be trained on train3.
log1 = glm(PositiveDec~., data=train1, family=binomial)
log2 = glm(PositiveDec~., data=train2, family=binomial)
log3 = glm(PositiveDec~., data=train3, family=binomial)

#Which variables have a positive coefficient in at least one of the models and
#a negative coefficient in at least one of the models?
summary(log1)
summary(log2)
summary(log3)
##Answer: ReturnJan, ReturnFeb, ReturnMar, ReturnJune, ReturnAug, ReturnOct

#Using log1, make test-set predictions called pred1 on the data frame test1. 
#Repeat for log2/pred2/test2 and log3/pred3/test3.
pred1 = predict(log1, newdata=test1, type="response")
pred2 = predict(log2, newdata=test2, type="response")
pred3 = predict(log3, newdata=test3, type="response")

#What is the overall accuracy of log1 on the test set test1, using a threshold 
#of 0.5? Of log2 on test2? Of log3 on test3?
table(test1$PositiveDec, pred1>0.5)
table(test2$PositiveDec, pred2>0.5)
table(test3$PositiveDec, pred3>0.5)
##Answer: (30+774)/nrow(test1)=0.6194145; (388+757)/nrow(test2)=0.5504808;
##(49+13)/nrow(test3)= 0.6458333

#To compute the overall test-set accuracy of the cluster-then-predict approach,
#we can combine all the test-set predictions into a single vector and all the 
#true outcomes into a single vector.
all.test.preds = c(pred1, pred2, pred3)
all.test.outcomes = c(test1$PositiveDec, test2$PositiveDec, test3$PositiveDec) 

#What is the overall test-set accuracy of the cluster-then-predict approach, 
#again using a threshold of 0.5?
table(all.test.outcomes, all.test.preds>0.5)
##Answer:(467+1544)/nrow(test) = 0.5788716




