Sys.setlocale("LC_ALL", "C")


#Problem 1 - Preparing the Dataset


#Load the data set.
loans = read.csv("loans.csv")

#What proportion of the loans in the dataset were not paid in full? 
table(loans$not.fully.paid)
##Answer: 1533/(1533+8045) = 0.1600543

#Which of the variables has at least one missing observation? 
summary(loans)
##Answer: log.annual.inc, days.with.cr.line, revol.util, inq.last.6mths,
##delinq.2yrs, pub.rec

#Why should we want to fill in the missing values for these variables instead of
#removing observations with missing data? 
##Answer: We want to be able to predict risk for all borrowers, instead of just 
##the ones with all data reported.

#Fill in the missing values by loading the loans_imputed data set.
imputed = read.csv("loans_imputed.csv")

#What best describes the process we just used to handle missing values?
##Answer: We predicted missing variable values using the available independent
##variables for each observation.


#Problem 2 - Prediction Models


#Split the sample into a training and testing set using a split ratio of 0.7
#and a seed of 144, splitting over the dependent variable not.fully.paid.
library(caTools)
set.seed(144)
split = sample.split(imputed$not.fully.paid, SplitRatio = 0.7)
train = subset(imputed, split==TRUE)
test = subset(imputed, split==FALSE)

#Use logistic regression trained on the training set to predict the dependent
#variable not.fully.paid using all the independent variables.
log = glm(not.fully.paid~., data=train, family=binomial)

#Which independent variables are significant in our model (at least one *)? 
summary(log)
log = glm(not.fully.paid~., data=train, family=binomial)
##Answer: credit.policy, purpose, installment, log.annual.inc, fico, revol.bal,
##inq.last.6mths, pub.rec  

#Consider two loan applications, which are identical other than the fact that
#the borrower in Application A has FICO credit score 700 while the borrower
#in Application B has FICO credit score 710.

#What is the value of Logit(A) - Logit(B)?
##Answer: The coefficient of fico is -0.009317 so Logit(A) - Logit(B) = 
##-0.009317(700) - (-0.009317(710)) = 0.09317.

#What is the value of Odds(A)/Odds(B)? 
##Answer: Odds(A)/Odds(B) = exp(Logit(A))/exp(Logit(B))
##=exp(Logit(A)-Logit(B))= exp(0.09317) = 1.0976

#Predict the probability of the test set loans not being paid back in full and
#add it to your test set.
predicted.risk = predict(log, newdata=test, type="response")
test$predicted.risk = predicted.risk

#Compute the confusion matrix using a threshold of 0.5.
table(test$not.fully.paid, predicted.risk>0.5)

#What is the accuracy of the logistic regression model?
##Answer: (2400+3)/nrow(test) = 0.8364079

#What is the accuracy of the baseline model?
##Answer: The baseline model predicts all loans will be paid in full and will
##be correct 2413 out of 2873 times for an accuracy of 0.8399.

#Compute the test set AUC.
library(ROCR)
ROCRpred = prediction(predicted.risk, test$not.fully.paid)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc
##Answer: 0.6720995


#Problem 3 - A "Smart Baseline"


#Using the training set, build a logistic regression model that predicts the 
#dependent variable not.fully.paid using only the variable int.rate.
log2 = glm(not.fully.paid~int.rate, data=train, family=binomial)

#The variable int.rate is highly significant in the bivariate model, but it is
#not significant at the 0.05 level in the model trained with all the 
#independent variables. What is the most likely explanation for this
#difference?
##Answer: int.rate is correlated with other risk-related variables, and 
##therefore does not incrementally improve the model when those other 
##variables are included.

#Make test set predictions for log2.
test.pred.2 = predict(log2, newdata=test, type="response")

#What is the highest predicted probability of a loan not being paid in full on
#the testing set?
max(test.pred.2)
##Answer: 0.426624

#With a logistic regression cutoff of 0.5, how many loans would be predicted
#as not being paid in full on the testing set?
table(test$not.fully.paid, test.pred.2>0)
##Answer: From the table we see the answer is 0.

#What is the test set AUC of log2?
ROCRpred2 = prediction(test.pred.2, test$not.fully.paid)
auc = as.numeric(performance(ROCRpred2, "auc")@y.values)
auc
##Answer: 0.6239081


#Problem 4 - Computing the Profitability of an Investment


#How much does a $10 investment with an annual interest rate of 6% pay back 
#after 3 years, using continuous compounding of interest?
#Answer: 10*exp(3*0.06) = $11.97

#What is the profit to the investor if the investment is paid back in full,
#where c is the investment, r is the interest rate and t is the number of years.
##Answer: c*exp(rt) - c

#What is the profit to the investor if none of the loan is paid back?
##Answer: -c


#Problem 5 - A Simple Investment Strategy


#What is the maximum profit of a $10 investment in any loan in the 
#testing set?
10*exp(max(test$int.rate)*3) - 10
##Answer: 8.894769
#We use 3 because the investments are over 3 years.

#Create variable profit in test that has value exp(test$int.rate*3) - 1 for
#the loans that are fully paid and -1 for those that aren't.
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1


#Problem 6 - An Investment Strategy Based on Risk


#We will analyze an investment strategy in which the investor only purchases 
#loans with a high interest rate (a rate of at least 15%), but amongst these 
#loans selects the ones with the lowest predicted risk of not being paid back 
#in full. We will model an investor who invests $1 in each of the most 
#promising 100 loans.

#Build a data frame called highInterest consisting of the test set loans with
#an interest rate of at least 15%.
loans.above.15 = subset(test, int.rate>=0.15)

#What is the average profit of a $1 investment in one of these high-interest 
#loans?
mean(loans.above.15$profit)
##Answer: 0.2251015

#What proportion of the high-interest loans were not paid back in full?
table(loans.above.15$not.fully.paid)
##Answer: 110/(110+327) = 0.2517162

#Determine the 100th smallest predicted probability of not paying in full
#in loans.above.15.
cutoff = sort(loans.above.15$predicted.risk, decreasing=FALSE)[100]
cutoff
##Answer: 0.1763305

#Create a subset of loans.above.15 by choosing the 100 smallest predictions
#of not being fully paid.
selected.loans = subset(loans.above.15, predicted.risk<=cutoff) 

#What is the profit of the investor, who invested $1 in each of these 
#100 loans?
sum(selected.loans$profit)
##Answer:31.27825

#How many of 100 selected loans were not paid back in full?
table(selected.loans$not.fully.paid)
##Answer: 19
