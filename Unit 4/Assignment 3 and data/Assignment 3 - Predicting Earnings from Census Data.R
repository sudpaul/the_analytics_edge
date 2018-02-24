Sys.setlocale("LC_ALL", "C")

#Load the data.
census = read.csv("census.csv")


#Problem 1 - A Logistic Regression Model


#Let's begin by building a logistic regression model to predict whether an 
#individual's earnings are above $50,000 using all of the other variables as 
#independent variables. Split the data into a training and testing set with 
#60% and 40% of the data respectively and a seed of 2000.
library(caTools)
set.seed(2000)
split = sample.split(census$over50k, SplitRatio=0.6)
train = subset(census, split==TRUE)
test = subset(census, split==FALSE)
log = glm(over50k~., data=train, family=binomial)

#Which variables are significant, or have factors that are significant?
summary(log)
##Answer: workclass, education, maritalstatus, occupation, relationship, sex,
##capital gain, capitalloss, hoursperweek

#What is the accuracy of the model on the testing set? Use a threshold of 0.5.
test.log.pred = predict(log, newdata=test, type="response")
table(test$over50k, test.log.pred>0.5)
##Answer: (9051+1888)/nrow(test) = 0.8552107

#What is the baseline accuracy for the testing set?
table(test$over50k)
##Answer: 9713/nrow(test) = 0.7593621

#What is the area-under-the-curve (AUC) for this model on the test set?
library(ROCR)
ROCRpred = prediction(test.log.pred, test$over50k)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc
##Answer: 0.9061598


#Problem 2 - A CART Model


#Build a classification tree to predict "over50k",
library(rpart)
library(rpart.plot)
tree = rpart(over50k~., data=train)

#How many splits does the tree have in total?
prp(tree)
##Answer: 4

#Which variable does the tree split on at the first level?
##Answer: relationship

#Which variables does the tree split on at the second level?
##Answer: capitalgain, education

#What is the accuracy of the model on the testing set? Use a threshold of 0.5. 
test.tree.pred = predict(tree, newdata=test)
table(test$over50k, test.tree.pred[,2]>0.5)
##Answer: (9243+1596)/nrow(test) = 0.8473927

#Plot the ROC curve for the CART model you have estimated.
ROCRtree.pred = prediction(test.tree.pred[,2], test$over50k)
ROCRtree.perf = performance(ROCRtree.pred, "tpr", "fpr")
plot(ROCRtree.perf)

#Observe that compared to the logistic regression ROC curve, the CART ROC 
#curve is less smooth than the logistic regression ROC curve. Why?
##Answer: The probabilities from the CART model take only a handful of values
##(five, one for each end bucket/leaf of the tree); the changes in the ROC 
##curve correspond to setting the threshold to one of those values.

#What is the AUC of the CART model on the test set?
auc.cart = as.numeric(performance(ROCRtree.pred, "auc")@y.values)
auc.cart
##Answer: 0.8470256


#Problem 3 - A Random Forest Model


#Define a new training set to be used when building our random forest model,
#that contains 2000 randomly selected obervations from the original training 
#set.
set.seed(1)
trainSmall = train[sample(nrow(train), 2000),]

#Let us now build a random forest model to predict "over50k", using the 
#dataset "trainSmall" as the data used to build the model.
library(randomForest)
set.seed(1)
forest = randomForest(over50k~., data=trainSmall)

#What is the accuracy of the model on the test set, using a threshold of 0.5?
test.forest.pred = predict(forest, newdata=test)
table(test$over50k, test.forest.pred)
##Answer: (9586+1093)/nrow(test) = 0.8348839


#One metric that we can look at is the number of times, aggregated over all 
#of the trees in the random forest model, that a certain variable is selected
#for a split. To view this metric, run the following lines of R code:
vu = varUsed(forest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(forest$forest$xlevels[vusorted$ix]))

#Which of the following variables is the most important in terms of the number 
#of splits?
##Answer: age

#A different metric we can look at is related to "impurity", which measures 
#how homogenous each bucket or leaf of the tree is. One way to measure the 
#importance of a variable is to average the reduction in impurity, taken over
#all the times that variable is selected for splitting in all of the trees in
#the forest. To compute this metric, run the following command in R:
varImpPlot(forest)

#Which variable is the most important in terms of mean reduction in impurity?
##Answer: occupation


#Problem 4 - Selecting cp by Cross-Validation


#Let us select the cp parameter for our CART model using k-fold cross 
#validation, with k = 10 folds.
set.seed(2)
library(caret)
folds = trainControl(method="cv", number=10)
cpGrid = expand.grid(.cp=seq(0.002, 0.01, 0.002))
train(over50k~., data=train, method="rpart", trControl=folds, tuneGrid=cpGrid)

#Which value of cp does the train function recommend?
##Answer: 0.002

#Fit a CART model to the training data using this value of cp. 
tree2 = rpart(over50k~., data=train, method="class", cp=0.002)

#What is the prediction accuracy on the test set?
test.tree2.pred = predict(tree2, newdata=test, type="class")
table(test$over50k, test.tree2.pred)
##Answer: (9178+1838)/nrow(test) = 0.8612306

#Should we favor tree2 over tree?
prp(tree2)
##Answer: Its accuracy is slightly higher. However, from the plot we can see
##it has many more splits (18), meaning it is more difficult to interpret and
##is at risk of being an over-fitting. Therefore tree is probably favorable.
