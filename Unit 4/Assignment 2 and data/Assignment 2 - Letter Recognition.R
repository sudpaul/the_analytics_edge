Sys.setlocale("LC_ALL", "C")


#Problem 1 - Predicting B or not B


#Load the data.
letters = read.csv("letters_ABPR.csv")

#Create a new variable isB in the dataframe, which takes the value "TRUE" if 
#the observation corresponds to the letter B, and "FALSE" if it does not.
letters$isB = as.factor(letters$letter=="B")

#Split the data set into a training and testing set, putting 50% of the data
#in the training set. Set the seed to 1000 before making the split.
library(caTools)
set.seed(1000)
split = sample.split(letters$isB, SplitRatio=0.5)
train = subset(letters, split==TRUE)
test = subset(letters, split==FALSE)

#Before building models, let's consider a baseline method that always predicts
#the most frequent outcome, which is "not B". What is the accuracy of this
#baseline method on the test set?
sum(letters$isB==FALSE)/nrow(letters)
##Answer: 0.754172

#Build a classification tree to predict whether a letter is a B or not, using
#the training set to build your model. Remember to remove the variable
#"letter" out of the model, as this is related to what we are trying to
#predict.
library(rpart)
library(rpart.plot)
tree = rpart(isB~.-letter, data=train, method="class")

#What is the accuracy of the CART model on the test set?
tree.pred = predict(tree, newdata=test, type="class")
table(test$isB, tree.pred)
##Answer: (1118+340)/nrow(test) = 0.9358151

#Now, build a random forest model to predict whether the letter is a B or not 
#using the training set.
library(randomForest)
set.seed(1000)
forest = randomForest(isB~.-letter, data=train)

#What is the accuracy of the model on the test set?
forest.pred = predict(forest, newdata=test)
table(test$isB, forest.pred)
##Answer: (1165+374)/nrow(test) = 0.9878049


#Problem 2 - Predicting the letters A, B, P, R


#Generate new training and testing sets of the letters data frame using 
#letters$letter as the first input to the sample.split function. Before 
#splitting, set your seed to 2000. Again put 50% of the data in the training
#set.

set.seed(2000)
split2 = sample.split(letters$letter, SplitRatio=0.5)
train2 = subset(letters, split2==TRUE)
test2 = subset(letters, split2==FALSE)

#What is the baseline accuracy on the testing set?
table(test$letter)
##Answer: 396/nrow(test) = 0.254172

#Now build a classification tree to predict "letter", using the training set
#to build your model. You should use all of the other variables as independent
#variables, except "isB", since it is related to what we are trying to predict.
tree2 = rpart(letter~.-isB, data=train2, method="class")

#What is the test set accuracy of your CART model?
tree2.pred = predict(tree2, newdata=test2, type="class")
table(test2$letter, tree2.pred)
##Answer: (348+318+363+340)/nrow(test2) = 0.8786906

#Now build a random forest model on the training data, using the same 
#independent variables as in the previous problem. Set the seed to 1000.
set.seed(1000)
forest2 = randomForest(letter~.-isB, data=train)

#What is the test set accuracy of your random forest model?
forest2.pred = predict(forest2, newdata=test)
table(test$letter, forest2.pred)
##Answer: (380+373+391+383)/nrow(test) = 0.9801027
