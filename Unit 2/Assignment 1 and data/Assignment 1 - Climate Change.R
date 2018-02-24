Sys.setlocale("LC_ALL", "C")


#Problem 1 - Creating Our First Model


#Load the data set.
climate = read.csv("climate_change.csv")

#Split the data into training and testing sets.
train = subset(climate, Year<=2006)
test = subset(climate, Year>=2007)

#Build a linear regression model and find its R-squared value.
lrm = lm(Temp~MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols, data=train)
summary(lrm)
##Answer: 0.7509

#Which variables are significant in the model?
##Answer: MEI, CO2, CFC.11, CFC.12, TSI, Aerosols


#Problem 2 - Understanding the Model


#Which of the independent variables is N2O highly correlated with 
#(absolute correlation greater than 0.7)? What about CFC.11?
cor(climate)
##Answer: Year, CO2, CH4, CFC.12; CH4, CFC.12


#Problem 3 - Simplifying the Model


#Build a simplified linear regression model.
lrm2 = lm(Temp~MEI+TSI+Aerosols+N2O, data=train)
summary(lrm2)

#What is the coefficient of N2O in this reduced model?
##Answer: 0.02532

#What is the R-squared value?
##Answer: 0.7261


#Problem 4 - Automatically Building the Model


#Use the step function to automatically build a linear regression model 
#from the original.
step.lrm = step(lrm)


#What is the R-squared value of the model produced by the step function?
summary(step.lrm)
##Answer: 0.7508

#Which variable(s) were eliminated from the full model by the step function?
##Answer: CH4


#Problem 5 - Testing on Unseen Data


#Using the model produced from the step function, calculate temperature 
#predictions for the testing data set.
test.pred = predict(step.lrm, newdata=test)

#Enter the testing set R-squared.
SSE = sum((test.pred - test$Temp)^2)
SST = sum((mean(train$Temp) - test$Temp)^2)
R2 = 1-SSE/SST
R2
##Answer: 0.6286051




