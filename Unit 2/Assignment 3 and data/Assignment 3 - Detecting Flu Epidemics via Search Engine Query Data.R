Sys.setlocale("LC_ALL", "C")

#Load the data
flu = read.csv("fluTrain.csv")


#Problem 1 - Understanding the Data


#Which week corresponds to the highest percentage of ILI-related physician 
#visits?
which.max(flu$ILI)
#This returns 303
flu$Week[303]
##Answer: 2009-10-18 to 2009-10-24

#Which week corresponds to the highest percentage of ILI-related query fraction?
which.max(flu$Queries)
#This returns 303
flu$Week[303]
##Answer: 2009-2010-18 to 2009-10-24

#Plot the histogram of the dependent variable, ILI. 
hist(flu$ILI)

#What best describes the distribution of values of ILI?
##Answer: The values are skewed right.

#Plot the natural logarithm of ILI versus Queries. 
plot(flu$Queries,log(flu$ILI))

#What does the plot suggest?
##Answer: There is a strong positive correlation between Queries and log(ILI).


#Problem 2 - Linear Regression Model


#Which model best describes our estimation problem?
##Answer: All points are in the first quadrant of the plane and there is a general
##upward trend so log(ILI) = intercept + c*Queries where c is positive.

#Build a linear regression model where Queries predicts ILI.
lrm = lm(log(ILI)~Queries, data=flu)

#What is the training set R-squared value for FluTrend1 model?
summary(lrm)
##Answer: 0.709

#For a single variable linear regression model, there is a direct relationship
#between the R-squared and the correlation between the independent and the 
#dependent variables. What is the relationship we infer from our problem? The
#options are i) R-squared = Correlation^2 ii)R-squared = log(1/Correlation)
#iii)R-squared = exp(-0.5*Correlation)
##Answer: Correlation = cor(log(flu$ILI), flu$Queries)
##Plug Correlation into each of i), ii) and iii) and see which gives the desired
##R-Squared value.


#Problem 3 - Performance on the Test Set


#Load the test data
test = read.csv("fluTest.csv")

#Create predictions for the ILI variable on the test set.
test.pred = exp(predict(lrm, newdata=test))

#What is our estimate for the percentage of ILI-related physician visits for 
#the week of March 11, 2012?
which(test$Week == "2012-03-11 - 2012-03-17")
#This outputs 11
test.pred[11]
##Answer: 2.187378 

#What is the relative error betweeen the prediction and the observed value for 
#the week of March 11, 2012?
(test$ILI[11] - test.pred[11])/test$ILI[11]
##Answer: 0.04623827 

#What is the Root Mean Square Error (RMSE) between our estimates and the actual
#observations for the percentage of ILI-related physician visits, on the test 
#set?
SSE.test = sum((test$ILI - test.pred)^2)
RMSE.test = sqrt(SSE.test/nrow(test))
RMSE.test
##Answer: 0.7490645


#Problem 4 - Training a Time Series Model


#We build a time series model that uses the values of the ILI variable from
#two weeks prior to predict the current ILI variable in the current week.

#Load the zoo package, used for time series models.
library(zoo)

#Create the new variable that takes the train$ILI values from two weeks prior.
ILILag2 = lag(zoo(flu$ILI), -2, na.pad=TRUE)
#na.pad=TRUE means to add missing values for the first two weeks of our 
#dataset, which don't have data from 2 weeks prior in the data set.

#Add the new variable to our train set.
flu$ILILag2 = coredata(ILILag2)

#How many values are missing in the new ILILag2 variable?
table(is.na(flu$ILILag2))
##Answer: 2, namely the values corresponding to the first two weeks.

#Use the plot() function to plot the log of ILILag2 against the log of ILI. 
#What best describes the relationship between these two variables?
plot(log(flu$ILILag2), log(flu$ILI))
##Answer: There is a strong positive correlation between the two.

#Train a linear regression model on the FluTrain dataset to predict the log of 
#the ILI variable using the Queries variable as well as the log of the ILILag2 
#variable.
lrm.2 = lm(log(ILI)~Queries+log(ILILag2), data=flu)

#Which coefficients are significant at the p=0.05 level in this regression
#model? What is the R^2 value of the FluTrend2 model?
summary(lrm.2)
##Answer: 0.9063

#On the basis of R-squared value and significance of coefficients, how does 
#lrm.2 compare to lrm?
##Answer: Both have *** significance for all coefficients, however lrm.2 has an
##R^2 of 0.9063 whereas lrm has an R^2 of 0.709, so lrm.2 is a better model on 
##the training set flu.


#Problem 5 - Evaluating the Time Series Model in the Test Set


#Add an ILILag2 variable to the FluTest data frame
ILILag2 = lag(zoo(test$ILI), -2, na.pad=TRUE)
test$ILILag2 = coredata(ILILag2)

#How many missing values are there in this new variable?
table(is.na(test$ILILag2))
##Answer: 2, namely those corresponding to the first two weeks.

#Which value should be used to fill in the ILILag2 variable for the first 
#observation in test?
##Answer: The second-to last observation from flu, because the last week of flu
##corresponds to the week before the first week of test.

#Which value should be used to fill in the ILILag2 variable for the second 
#observation in test?
##Answer: The last observation from flu, because the last week of flu corresponds 
##to the week before the first week of test.

#Fill in these missing values.
test$ILILag2[1] = flu$ILI[416]
test$ILILag2[2] = flu$ILI[417]
#We use 416 and 417 because flu has 417 observations.

#Obtain test set predictions of the ILI variable from the lrm.2 model.
test.pred.2 = exp(predict(lrm.2, newdata=test))

#What is the test-set RMSE of the lrm.2 model?
SSE.test.2= sum((test.pred.2 - test$ILI)^2)
RMSE.test.2 = sqrt(SSE.test.2/nrow(test))
RMSE.test.2
##Answer: 0.2942029

#Which model obtained the best test-set RMSE?
##Answer: RMSE.test.2<RMSE.test and therefore lrm.2 has a better test-set RMSE.

