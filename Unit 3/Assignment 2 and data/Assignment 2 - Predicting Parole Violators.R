Sys.setlocale("LC_ALL", "C")


#Problem 1 - Loading the Dataset


#Load the data.
parole = read.csv("parole.csv")

#How many parolees are contained in the dataset?
nrow(parole)
##Answer: 675

#How many of the parolees in the dataset violated the terms of their parole?
table(parole$violator)
##Answer: 


#Problem 2 - Preparing the Dataset


#Which variables in this dataset are unordered factors with at least three
#levels?
##Answer: state, crime

#Convert the unordered factor variables to type factor.
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

#How does the output of summary() change for a factor variable as compared to 
#a numerical variable?
##Answer: For a numerical variable the summary displays some basic statistics
##whereas for a factor variable it displays how many observations there are
##for each of the factors.


#Problem 3 - Splitting into a Training and Testing Set


#Split the data by "violator" into train and test sets using sample.split, a 
#split ratio of 0.7 and a seed of 144. We need package caTools do perform
#sample.split.
library(caTools)
set.seed(144)
split = sample.split(parole$violator, SplitRatio=0.7)
train = subset(parole, split==TRUE)
test = subset(parole, split==FALSE)

#Roughly what proportion of parolees have been allocated to the training and 
#testing sets?
##Answer: 0.7 and 0.3 respectively, according to the split ratio.

#Suppose you re-ran lines [1]-[5] of the previous problem. What would you
#expect?
##Answer: train and test would be the same because we set the seed.

#If you instead ONLY re-ran lines [3]-[5], what would you expect?
##Answer: train and test would be different because we didn't set the seed.

#If you instead called set.seed() with a different number and then re-ran
#lines [3]-[5], what would you expect?
##Answer: train and test would be different because we have a different seed.


#Problem 4 - Building a Logistic Regression Model


#Train a logistic regression model on the training set. Your dependent 
#variable is "violator", and you should use all of the other variables as 
#independent variables.
log = glm(violator~., data=train, family=binomial)

#What variables are significant in this model?
summary(log)
##Answer: race, state, multiple.offenses

#What can we say based on the coefficient of the multiple.offenses variable?
##Answer: Given that the coefficient is 1.612 this means that the odds that a parolee
##who committed multiple offenses will be a violator is exp(1.612) times as
##high as a parolee who hasn't committed multiple offenses but who is otherwise
##identical in all other stats.

#Consider a parolee who is male, of white race, aged 50 years at prison 
#release, from the state of Maryland, served 3 months, had a maximum sentence 
#of 12 months, did not commit multiple offenses, and committed a larceny.

#According to the model, what are the odds this individual is a violator?
##Answer: Logit = -4.2411574 + 0.3869904*(1) + 0.8867192*(1) - 0.0001756*(50) 
##-0.1238867 *(3) + 0.0802954*(12) + 1.6119919*(0) + 0.6837143*(1) and Odds = 
##exp(Logit) = 0.1825687.

#According to the model, what is the probability this individual is a violator?
##Answer: Odds = P(violator)/[1-P(violator)]=0.1825686 --> P(violator)=0.1543


#Problem 5 - Evaluating the Model on the Testing Set


#Obtain the model's predicted probabilities for parolees in the testing set.
test.pred = predict(log, newdata=test, type="response")

#What is the maximum predicted probability of a violation?
max(test.pred)
##Answer: 0.9072791


#What is the model's sensitivity, specificity and accuracy using a threshold
#of 0.5?
table(test$violator, test.pred>0.5)
##Answer: 12/(12+11) = 0.5217391; 167/(167+12) = 0.9329609; 
##(167+12)/nrow(test) = 0.8861386

#What is the accuracy of a simple model that predicts that every parolee is a
#non-violator?
sum(test$violator==0)/nrow(test)
##Answer: 0.8861386



#Consider a parole board using the model to predict whether parolees will be 
#violators or not. The job of a parole board is to make sure that a prisoner is
#ready to be released into free society, and therefore parole boards tend to be
#particularily concerned about releasing prisoners who will violate their
#parole. What most likely describes their preferences and best course of action?
##Answer: The board assigns more cost to a false negative than a false positive,
##and should therefore use a logistic regression cutoff less than 0.5. 

#Which of the following is the most accurate assessment of the value of the
#logistic regression model with a cutoff 0.5 to a parole board, based on the 
#model's accuracy as compared to the simple baseline model?
##Answer: They have the same accuracy, however the parole board is going to
##assign more negative weight to a false-negative than to a false-positive and
##the baseline model has 23 false-negatives compared to the logistic model's
##12 false-negatives. Moreover, the logistic model can have even fewer false-
##negatives by decreasing the threshold below 0.5.

#What is the AUC value for the model?
library(ROCR)
ROCRpred = prediction(test.pred, test$violator)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc
##Answer: 0.8945834

#Describe the meaning of AUC in this context.
##Answer: The probability the model can correctly differentiate between a 
##randomly selected parole violator and a randomly selected parole non-violator.
