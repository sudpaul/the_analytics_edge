
#Problem 1 - Loading the Data


#Use the read.csv function to load the contents of bank.csv into a data frame 
#called bank. What is the average age in the data set?
bank = read.csv("bank.csv")
mean(bank$age)
##Answer: 39.5814


#Problem 2 - Call Durations by Job


#Build a boxplot that shows the call duration distributions over different
#jobs. Which three jobs have the longest average call durations?
boxplot(bank$duration~bank$job)
sort(tapply(bank$duration, bank$job, mean))
##Answer: retired, self-employed, housemand


#Problem 3 - Multicolinearity


#Examine the correlation between the following variables: emp.var.rate, 
#cons.price.idx, cons.conf.idx, euribor3m, and nr.employed. 
df = data.frame(bank$cons.price.idx, bank$cons.conf.idx, bank$euribor3m, bank$nr.employed)
cor(df)

#Which of the following statements are correct (limited to just these selected 
#variables)?
#i)cons.conf.idx does NOT seem to have severe multicolinearity with the other
#variables.
#ii)emp.var.rate and nr.employed have the highest correlation between two 
#different variables.
#iii)cons.price.idx and cons.conf.idx have the lowest correlation between two different
#variables.
##Answer: i) is true; the largest correlation is 0.2822054. ii) false
##iii)true


#Problem 4 - Splitting into a Training and Testing Set


#Obtain a random training/testing set split.
library(caTools)
set.seed(201)
split = sample.split(bank$y, SplitRatio=0.7)
train = subset(bank, split==TRUE)
test = subset(bank, split==FALSE)

#Why do we use the sample.split() function to split into a training and
#testing set?
##Answer: It balances the dependent variable between the training and testing
#sets.


#Problem 5 - Training a Logistic Regression Model


#Train a logistic regression model using independent variables age, job, 
#marital, education, default, housing, loan, contact, month, day_of_week, 
#campaign, pdays, previous, poutcome, emp.var.rate, cons.price.idx, and 
#cons.conf.idx, using the training set to obtain the model.
logistic_model = glm(y~.-duration-euribor3m-nr.employed, data=train, family="binomial")

#Which characteristics are statistically significantly POSITIVELY (at 0.05
#level) associated with an increased chance of subscribing to the product?
summary(logistic_model)
##Answer: age, month is August, month is March, poutcome is non-existent,
##cons.price.idx


#Problem 6 - Interpreting Model Coefficients


#What is the meaning of the coefficient labeled "monthmar" in the logistic
#regression summary output?
##Answer: The coefficient is 1.286, so when the month is March, then monthmar=1
##and so the logit is increased by 1.286 and so the odds are multiplied by
##exp(1.286) = 3.618284, meaning a 261.8% increase in the odds compared to an
##otherwise identical contact.


#Problem 7 - Obtaining Test Set Predictions


#Using your logistic regression model, obtain predictions on the test set.
pred.logistic.test = predict(logistic_model, newdata=test, type="response")

#Then, using a probability threshold of 0.5, create a confusion matrix for
#the test set.
table(test$y, pred.logistic.test>0.5)

#What is the number of test set observations where the prediction from the 
#logistic regression model is different than the prediction from the baseline
#model?
##Answer: From the confusion matrix we see that the baseline model always
##predicts y=0 and the logistic regression predicts y=1 50+44=94 times.


#Problem 8 - Computing Test-Set AUC


#What is the test-set AUC of the logistic regression model?
library(ROCR)
ROCRpred.log = prediction(pred.logistic.test, test$y)
auc.log = as.numeric(performance(ROCRpred.log, "auc")@y.values)
auc.log
##Answer: 0.7507334


#Problem 9 - Interpreting AUC


#What is the meaning of the AUC?
##Answer: The proportion of the time the model can differentiate between a 
##randomly selected client who subscribed to a term deposit and a randomly
##selected client who did not subscribe.


#Problem 10 - ROC Curves


#Which logistic regression threshold is associated with the upper-right corner
#of the ROC plot (true positive rate 1 and false positive rate 1)?
##Answer: 0


#Problem 11 - ROC Curves


#Plot the colorized ROC curve for the logistic regression model's performance
#on the test set.
ROCRperf.log = performance(ROCRpred.log, "tpr", "fpr")
plot(ROCRperf.log, colorize=TRUE)

#At roughly which logistic regression cutoff does the model achieve a true
#positive rate of 60% and a false positive rate of 25%?
##Answer: 0.11


#Problem 12 - Cross-Validation to Select Parameters


#What best describes how 10-fold cross-validation works when selecting between
#4 different parameter values?
##Answer: 40 models are trained on subsets of the training set and evaluated
##on a portion of the training set.


#Problem 13 - Cross-Validation for a CART Model


#Perform 10-fold cross validation with the training data set to select the 
#best cp value for a CART model that predicts the dependent variable y using
#the same set of independent variables as in the logistic regression. Select
#the cp value from a grid consisting of the 50 values 0.001, 0.002, ..., 0.05.
library(caret)
set.seed(201)
folds = trainControl(method="cv", number=10)
cpGrid = expand.grid(.cp=seq(0.001, 0.05, 0.001))
train(y~.-duration-euribor3m-nr.employed, data=train, method="rpart", tuneGrid=cpGrid, trControl=folds)

#What cp value maximizes the cross-validation accuracy?
##Answer: 0.012


#Problem 14 - Train CART Model


#Build and plot the CART model trained with the parameter identified in 
#Problem 13, again predicting the dependent variable using the same set of 
#independent variables. What variable is used as the first (upper-most) split 
#in the tree?
library(rpart)
library(rpart.plot)
regression.tree = rpart(y~.-duration-euribor3m-nr.employed, data=train, method="class", cp=0.012)
prp(regression.tree)
##Answer: emp.var.rate


#Problem 15 - Test-Set Accuracy for CART Model


#Using the CART model you created in Problem 14, obtain predictions on the test
#set. Then, create a confusion matrix for the test set.
pred.tree.test = predict(regression.tree, newdata=test, type="class")
table(test$y, pred.tree.test)

#What is the accuracy of your CART model?
##Answer: (1303+28)/nrow(test) = 0.8873333