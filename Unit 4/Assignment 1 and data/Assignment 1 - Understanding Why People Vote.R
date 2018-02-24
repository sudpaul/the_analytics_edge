Sys.setlocale("LC_ALL", "C")


#Problem 1 - Exploration and Logistic Regression


#Load the data
gerber = read.csv("gerber.csv")

#What proportion of people in this dataset voted in this election?
table(gerber$voting)
##Answer: 108696/nrow(gerber) = 0.3158996

#Which of the four "treatment groups" had the largest percentage of people who 
#actually voted?
table(gerber$hawthorne, gerber$voting)
table(gerber$civicduty, gerber$voting)
table(gerber$neighbors, gerber$voting)
table(gerber$self, gerber$voting)
##Answer: hawthorne: 12316/(12316+25888) = 0.3223746; civicduty:
##12021/(12021+26197) = 0.3145377; neighbors: 14438/(14438+23763) = 0.3779482
##self: 13191/(13191+25027) = 0.3451515. The largest percentage belnongs to
##neighbors.

#Build a logistic regression model for voting using the four treatment group 
#variables as the independent variables.
log = glm(voting~hawthorne+civicduty+neighbors+self, data=gerber, family=binomial)

#Which of the following coefficients are significant in the logistic 
#regression model?
summary(log)
##Answer: All of them are *** significant.

#Using a threshold of 0.3, what is the accuracy of the logistic regression model?
pred.log = predict(log, type="response")
table(gerber$voting, pred.log>0.3)
##Answer: (134513+51966)/nrow(gerber) =  0.5419578

#Using a threshold of 0.5, what is the accuracy of the logistic regression model?
table(gerber$voting, pred.log>0.5)
##Answer: 235388/nrow(gerber) = 0.6841004

#Compare your previous two answers to the percentage of people who did not vote 
#(the baseline accuracy) and compute the AUC of the model.
#Answer: From table(gerber$voting) we find that 68.4% of people don't vote
library(ROCR)
ROCRpred = prediction(pred.log, gerber$voting)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc
##Answer: 0.5308461

#What is happening here?
##Answer: Even though all of the variables are significant, this is a weak
##predictive model given that the AUC is only 0.53 and its accuracy is no
##better than that of the baseline.


#Problem 2 - Trees


#Build a regression tree for voting using all data and the same four treatment 
#variables we used before. 
library(rpart)
library(rpart.plot)
tree = rpart(voting~hawthorne+civicduty+neighbors+self, data=gerber)

#Plot the tree. What happens, and if relevant, why?
prp(tree)
##Answer: No variables are used (the tree is only a root node) - none of the 
##variables make a big enough effect to be split on.

#Now force tree to split.
tree2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)

#Plot the tree. What do you observe about the order of the splits?
prp(tree2)
##Answer: Neighbor is the first split, civic duty is the last.

#Using only the CART tree plot, determine what fraction of "Civic Duty" people
#voted:
##Answer: The splits are neighbor<0.5, self<0.5, hawthorn<0.5 and civicdut<0.5.
##Because these are all binary variables moving along the left branch of a
##split corresponds to an observation not being of that treatment group.
##Therefore we move left along all splits except for the civicdut<0.5 split to
##find that 31% of "Civic Duty" people voted.

#Make a new tree that includes the "sex" variable, again with cp = 0.0.
tree3 = rpart(voting~civicduty+hawthorne+self+neighbors+sex, data=gerber, cp=0.0)
prp(tree3)

#In the control group, which gender is more likely to vote?
##Answer: The control group is found by traveling along the left branches of 
##each of the splits neighbor<0.5, self<0.5, hawthorn<0.5 and civicdut<0.5 and
##we find that the right leaf stemming from sex>=0.5, that is, the leaf
##corresponding to sex=0 (for male) is higher, so males are more likely to vote
##than females in the control group.

#In the "Civic Duty" group, which gender is more likely to vote?
##Answer: The sexes of the civic duty group are found by traveling along the
##left branches of each of the splits neighbor<0.5, self<0.5, hawthorn<0.5 and
##then right along civicdut<0.5. From here we find  that the right leaf stemming 
##from sex>=0.5, that is, the leaf corresponding to sex=0 (for male) is higher,
##so males are more likely to vote than females in the civic duty group.
 

#Problem 3 - Interaction Terms


#Create a regression tree using just the "control" variable, then create another 
#tree with the "control" and "sex" variables, both with cp=0.0.
tree4 = rpart(voting~control, data=gerber, cp=0.0)
tree5 = rpart(voting~control+sex, data=gerber, cp=0.0)

#In the "control" only tree, what is the absolute value of the difference in 
#the predicted probability of voting between being in the control group versus
#being in a different group? 
prp(tree4, digits=6)
##Answer: The tree tells us for the control the predicted probability is
##0.296638 and for the others is 0.34, giving an absolute difference of
##0.043362.

#Now, using the second tree, who is affected more by NOT being in
#the control group, men or women?
prp(tree5, digits=6)
##Answer: The men in the control group have a predicted probability of 0.302795
##whereas the men not in the control group have a predicted probability of 
##0.345818 for an absolute difference of 0.043023. The women in the control 
##group have a predicted probability of 0.290456 whereas the women not in the 
##control group have a predicted probability of 0.334176 for an absolute 
##difference of 0.043720. Therefore we see they are affected about the same.

#Going back to logistic regression now, create a model using "sex" and 
#"control". Interpret the coefficient for "sex":
log2 = glm(voting~sex+control, data=gerber, family=binomial)
summary(log2)
##Answer: The coefficient is negative, which means the higher value for the
##sex value, 1 for female, is predictive of less probability of voting.

#Create a data frame with all possibilities for sex and control, and evaluate
#your logistic regression using the predict function.
Possibilities = data.frame(sex=c(0,0,1,1), control=c(0,1,0,1))
pred.sc = predict(log2, newdata=Possibilities, type="response")

#What is the absolute difference between the tree and the logistic regression 
#for the (Woman, Control) case? Give an answer with five numbers after the 
#decimal point.
##Answer: From the tree the value is 0.290456 and from the logistic regression
##it is pred.sc[4], corresponding to sex=1 and control=1.
abs(0.290456-pred.sc[4])

#We're going to add a new term to our logistic regression now, that is the 
#combination of the "sex" and "control" variables - so if this new variable is
#1, that means the person is a woman AND in the control group. 
log3 = glm(voting~sex+control+sex:control, data=gerber, family="binomial")

#Interpret the coefficient of sex:control in log3 in isolation.
summary(log3)
##Answer: It is negative, meaning if a person is a woman and in the control 
##group then she is less likely to vote than if she wasn't both.

#Run the same code as before to calculate the average for each group.
pred.sc.2 = predict(log3, newdata=Possibilities, type="response")

#Now what is the difference between the logistic regression model and the CART 
#model for the (Woman, Control) case?
abs(0.290456-pred.sc.2[4])
#The difference is about 0.

#Should we always include all possible interaction terms of the independent 
#variables when building a logistic regression model?
##Answer: No, because this could quickly lead to overfitting by having too
##many variables relative to the size of the data set.
