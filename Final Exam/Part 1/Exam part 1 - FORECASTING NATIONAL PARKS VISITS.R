#Problem 1 - Number of National Parks in Jan 2016


#Load the data.
visits <- read.csv("park_visits.csv")

#Let's first look at the visits in July 2016. Subset the observations to this year
#and month, name it visits2016jul. 
visits2016 <- subset(visits, Year==2016 & Month==7)

#Which park type has the most number of parks?
sort(table(visits2016$ParkType), decreasing = TRUE)
##Answer: National Historic Site

#Which specific park has the most number of visitors?
which.max(visits2016$logVisits)
#This outputs 1654
visits2016$ParkName[1654]
##Answer: Great Smoky Mountains NP


#Problem 2 - Relationship Between Region and Visits


#Which region has the highest average log visits in July 2016?
sort(tapply(visits2016$logVisits, visits2016$Region, mean), decreasing = TRUE)
##Answer: Pacific West

#What is the average log visits for the region in July 2016 with the highest 
#average log visits?
##Answer: 10.767849

#What is the average log visits for the region in July 2016 with the highest 
#average log visits?
##Answer: 9.374157


#Problem 3 - Relationship Between Cost and Visits


#What is the correlation between entrance fee (the variable cost) and the log 
#visits in July 2016?
cor(visits2016$cost, visits2016$logVisits)
##Answer: 0.4010611

#Choose the most reasonable explanation:
##Answer: Higher entrance fees are associated with higher log visits, likely 
##because more expensive parks are often more popular due to other features of the
##parks.


#Problem 4 - Time Series Plot of Visits


#Subset the original data (visits) to "Yellowstone NP" only and save as ys.
yellowstone.np <- subset(visits, ParkName=="Yellowstone NP")

#Use the following code to plot the logVisits through the months between 2010 and
#2016:
yellowstone.np_ts=ts(yellowstone.np$logVisits,start=c(2010,1),freq=12)
plot(yellowstone.np_ts)

#What observations do you make?
##Answer: The log visits are highly cyclical, with the peaks in the summer time,
##but similar from year to year.


#Problem 5 - Missing Values


#Why do we have NA's in the laglogVisits and laglogVisitsYear? These variables were
#created by lagging the log visits by a month or by a year.
##Answer: These are lagged variables and the earlier data is not available for the
##first months.


#To deal with the missing values, we will simply remove the observations with the 
#missing values first.
visits <- visits[rowSums(is.na(visits)) == 0, ]

#How many observations are there in visits now?
nrow(visits)
##Answer: 21855


#Problem 6 - Predicting Visits


#We are interested in predicting the log visits. Before doing the split, let's also
#make Month a factor variable.
visits$Month <- as.factor(visits$Month)

#Subset our dataset into a training and a testing set by splitting based on the 
#year: training would contain 2010-2014 years of data, and testing would be 
#2015-2016 data.
train <- subset(visits, Year<=2014)
test <- subset(visits, Year>2014)

#Let's build now a simple linear regression model "mod" using the training set to
#predict the log visits. As a first step, we only use the laglogVisits variable.
model1 <- lm(logVisits~laglogVisits, data=train)

#What's the coefficient of the laglogVisits variable?
summary(mod)
##Answer: 0.927945

#What's the out-of-sample R-squared in the testing set for this simple model?
pred.mod.test <- predict(model1, newdata=test)
SSE <- sum((test$logVisits - pred.mod.test)^2)
SST <- sum((test$logVisits - mean(train$logVisits))^2)
1-SSE/SST
##Answer: 0.8975923


#Problem 7 - Add New Variables


#We see that the model achieves good predictive power already simply using the
#previous month's visits. To see if the other knowledge we have about the parks can
#improve the model, let's add these variables in a new model:
#laglogVisits, laglogVisitsYear, Year, Month, Region, ParkType, and cost.
model2 <- lm(logVisits~laglogVisits+laglogVisitsYear+Year+Month+Region+ParkType+cost, data=train)

#Looking at the model summary, what can we say?
summary(model2)
##Answer: Both the log visits from last month and last year are significant and are
##positively associated with the current log visits. None of the park types are 
##significant from the baseline park type (National Battlefield), as we can see that
##all of the factor levels have no stars.


#Problem 8 - Out-of-Sample R-squared


#In the new model, what's the out-of-sample R-squared in the testing set?
pred.mod2.test <- predict(model2, newdata=test)
SSE2 <- sum((test$logVisits - pred.mod2.test)^2)
SST2 <- sum((test$logVisits - mean(train$logVisits))^2)
1-SSE2/SST2
##Answer: 0.937253


#Problem 9 - Regression Trees


#In addition to the logistic regression model, we can also train a regression tree.
#Use the same set of variables as the previous problem, train a regression tree 
#with cp = 0.05.
library(rpart)
library(rpart.plot)
tree <- rpart(logVisits~laglogVisits+laglogVisitsYear+Year+Month+Region+ParkType+cost, data=train, cp=0.05)

#Looking at the plot of the tree, how many different predicted values are there?
prp(tree)
##Answer: 4 (the number of leaves)

#What is the out-of-sample R-squared on the testing set?
pred.tree.test <- predict(tree, newdata=test)
SSE3 <- sum((test$logVisits - pred.tree.test)^2)
SST3 <- sum((test$logVisits - mean(train$logVisits))^2)
1-SSE3/SST3
##Answer: 0.7864307


#Problem 10 - Regression Trees with CV


#The out-of-sample R2 does not appear to be very good under regression trees, 
#compared to a linear regression model. We could potentially improve it via cross
#validation. Set seed to 201, run a 10-fold cross-validated cart model, with cp 
#ranging from 0.0001 to 0.005 in increments of 0.0001. 
library(caret)
set.seed(201)
folds <- trainControl(method="cv", number=10)
cpGrid <- expand.grid(.cp=seq(0.0001,0.005,0.0001))
train(logVisits~laglogVisits+laglogVisitsYear+Year+Month+Region+ParkType+cost, data=train, method="rpart", tuneGrid=cpGrid, trControl=folds)

#What is optimal cp value on this grid?
##Answer: 0.0001

#Looking at the validation R-squared versus the cp value, we can further refine the
#cp range. In what direction should it change?
##We can see from the table that R-squared decreases as cp increases, with its max
##in the table being at cp=0.0001. Therefore, we should refine our cp range so that
##we include smaller values of cp.


#Problem 11 - Final Regression Tree


#Rerun the regression tree on the training data, now using the cp value equal to
#the one selected in the previous problem 
tree2 <- rpart(logVisits~laglogVisits+laglogVisitsYear+Year+Month+Region+ParkType+cost, data=train, cp=0.0001)

#What is the out-of-sample R-squared in the testing set?
pred.tree2.test <- predict(tree2, newdata=test)
SSE4 <- sum((test$logVisits - pred.tree2.test)^2)
SST4 <- sum((test$logVisits - mean(train$logVisits))^2)
1-SSE4/SST4
##Answer: 0.937113


#Problem 12 - Random Forest


#We can potentially further improve the models by using a random forest. Set seed
#to 201 again. Train a random forest model with the same set of covariates, 
#and using just default parameters (no need to specify). This may take a few 
#minutes.
library(randomForest)
set.seed(201)
forest <- randomForest(logVisits~laglogVisits+laglogVisitsYear+Year+Month+Region+ParkType+cost, data=train) 

#What is the R-squared on the testing set for the random forest model?

pred.forest.test <- predict(forest, newdata=test)
SSE5 <- sum((test$logVisits - pred.forest.test)^2)
SST5 <- sum((test$logVisits - mean(train$logVisits))^2)
1-SSE5/SST5
##Answer: 0.947239






