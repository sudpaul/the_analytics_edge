Sys.setlocale("LC_ALL", "C")

#Load the data sets
train = read.csv("pisa2009train.csv")
test = read.csv("pisa2009test.csv")


#Problem 1 - Dataset size


#How many students are there in the training set?
str(train)
##Answer: 3663

#What is the average reading test score of males? Of females?
tapply(train$readingScore, train$male, mean)
##Answer: 483.5325; 512.9406 

#Which variables are missing data in at least one observation in the 
#training set?
summary(train)
##Answer: raceeth, preschool, expectBachelors, motherHS, motherBachelors,
##motherWork, fatherHS, fatherBachelors, fatherWork, selfBornUS, motherBornUS,
##fatherBornUS, englishAtHome, computerForSchoolwork, read30MinsADay,
##minutesPerWeekEnglish, studentsInEnglish, schoolHasLibrary, sccholSize

#Remove observations in train and test that have any NA values.
train = na.omit(train)
test = na.omit(test)

#How many observations are now in the training set? In the testing set?
str(train)
str(test)
##Answer: 2414; 990


#Problem 2 - Factor variables


#Which of the following variables is an unordered factor with at least
#3 levels? Grade/Male/Raceeth
#Answer: Raceeth

#Which of the following variables is an ordered factor with at least
#3 levels? Grade/Male/Raceeth
##Answer: Grade

#Which binary variables will be included in the regression model, given
#that raceeth is a factor variable with factors "American Indian/Alaska
#Native", "Asian", "Black", "Hispanic", "More than one race", "Native
#Hawaiian/Other Pacific Islander", and reference level "White"
##Answer: raceethAmerican Indian/Alaska Native, raceethAsian, raceethBlack,
#raceethHispanic, raceethMore than one race, raceethNative Hawaiian/Other
#Pacific Islander

#For a student who is Asian, which binary variables would be set to 0?
##Answer: All of the binary variables would be 0 except for raceethAsian, 
#which would be 1.

#For a student who is white, which binary variables would be set to 0? All 
#remaining variables will be set to 1. 
##Answer: All of the binary variables would be 0.


#Problem 3 - Building a model


#Change the reference level of raceeth to White for train and test.
train$raceeth = relevel(train$raceeth, ref="White")
test$raceeth = relevel(test$raceeth, ref="White")

#Build a linear regression model using the training set to predict
#readingScore using all the remaining variables.
lrm = lm(readingScore~., data=train)

#What is the Multiple R-squared value of lrm on the training set?
summary(lrm)
##Answer: 0.3251

#What is the training-set root-mean squared error of lrm?
lrm.pred = predict(lrm)
SSE = sum((lrm.pred - train$readingScore)^2)
RMSE = sqrt(SSE/nrow(train))
RMSE
##Answer: 73.36555

#Consider two students A and B. They have all variable values the 
#same, except that student A is in grade 11 and student B is in 
#grade 9. What is the predicted reading score of student A minus 
#the predicted reading score of student B?
##Answer: From summary(lrm) the coefficient of grade is 29.542707 so
##the difference is 29.542707*(11-9) = 59.085414

#What is the meaning of the coefficient associated with variable
#raceethAsian?
##Answer: It is the predicted difference in scores of an Asian student
##and a white student who are identical in all other variables.

#Based on the significance codes, which variables are candidates for
removal from the model?
##Answer: Looking at summary(lrm) we remove any variable which does not
##have at least a one * significance code.


#Problem 4 - Predicting on unseen data


#Use the lmScore model to predict the reading scores of students in test.
lrm.pred.test = predict(lrm, newdata=test)

#What is the range between the maximum and minimum predicted 
#reading score on the test set?
summary(lrm.pred.test)
##Answer: 637.7-353.2 = 284.5

#What is the sum of squared errors of lrm on the 
#testing set?
SSE = sum((lrm.pred.test - test$readingScore)^2)
SSE
##Answer: 

#What is the root-mean squared error of lrm on the
#testing set?
RMSE = sqrt(SSE/nrow(test))
RMSE
##Answer: 5762082

#What is the predicted test score used in the baseline model?
mean(train$readingScore)
##Answer: 517.9629

#What is the sum of squared errors of the baseline model on the
#testing set?
SST = sum((mean(train$readingScore) - test$readingScore)^2)
SST
##Answer: 7802354

#What is the test-set R-squared value of lrm?
1 - SSE/SST
##Answer: 0.2614944