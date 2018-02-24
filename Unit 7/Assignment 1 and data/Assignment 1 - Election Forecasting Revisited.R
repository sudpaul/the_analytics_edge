Sys.setlocale("LC_ALL", "C")


#Problem 1 - Drawing a Map of the US


#Load the ggplot2, maps, and ggmap packages.
library(ggplot2)
library(maps)
library(ggmap)

#Load the data.
statesMap = map_data("state")

#One of the variables, group, defines the different shapes or polygons on the map.
#Sometimes a state may have multiple groups, for example, if it includes islands.
#How many different groups are there?
table(statesMap$group)
##Answer: 63

#Draw a map of the United States.
ggplot(statesMap, aes(x=long, y=lat, group=group)) + geom_polygon(fill="white", color="black")

#We specified two colors in geom_polygon -- fill and color. Which one defined 
#the color of the outline of the states?
##Answer: color


#Problem 2 - Coloring the States by Predictions


#Load PollingImputed so we can color the map of the US according to our 2012 US
#presidential election predictions from the Unit 3 Recitation. 
polling = read.csv("PollingImputed.csv")

#Split the data into train and test sets.
train = subset(polling, Year<=2008)
test = subset(polling, Year>2008)

#Create a logistic regression model and make predictions on the test set.
log2 = glm(Republican~SurveyUSA+DiffCount, data=train, family=binomial)
test.pred = predict(log2, newdata=test, type="response")
test.pred.binary = as.numeric(test.pred>0.5)

#Now, put the predictions and state labels in a data frame so that we can use
#ggplot.
predDF = data.frame(test.pred, test.pred.binary, test$State)

#For how many states is our binary prediction 1 (corresponding to Republican)?
table(predDF$test.pred.binary)
##Answer: 22

#What is the average predicted probability of our model on the test set?
mean(predDF$test.pred)
##Answer: 0.4852626

#Merge "predictionDataFrame" with the map data "statesMap"
predDF$region = tolower(predDF$test.State)
predMap = merge(statesMap, predDF, by="region")

#Make sure the "order" variable in predMap is in order so that the map is drawn
#correctly.
predMap = predMap[order(predMap$order),]

#How many observations are there in predMap?
nrow(predMap)
##Answer: 15034

#How many observations are there in statesMap?
nrow(statesMap)
##Answer: 15537

#When we merged the data in the previous problem, it caused the number of 
#observations to change. Why?

#Because we only make predictions for 45 states, we no longer have observations
#for some of the states. These observations were removed in the merging process.

#Color the US map with our predictions.
ggplot(predMap, aes(x=long, y=lat, group=group, fill=test.pred.binary))+geom_polygon(color="black")

#The states appear light blue and dark blue in this map. Which color represents
#a Republican prediction?
##Answer: Looking at the gradient we see that light blue corresponds to
##binary prediction of 1, which represents Republican.

#Let's replot the map with discrete outcomes and change the color scheme to 
#blue for Democrat and red for Republican. Change the title of the legend to
#Prediction 2012.
ggplot(predMap, aes(x=long, y=lat, group=group, fill=test.pred.binary))+geom_polygon(color="black") + scale_fill_gradient(low="blue", high="red", guide="legend", breaks=c(0,1), labels=c("Democrat", "Republican"), name="Prediction 2012")

#Alternatively, we could plot the probabilities instead of the binary 
#predictions. Change the plot command above to instead color the states by the 
#variable test.pred. You should see a gradient of colors ranging from red
#to blue. Do the colors of the states in the map for TestPrediction look
#different from the colors of the states in the map with TestPredictionBinary? 
#Why or why not?
ggplot(predMap, aes(x=long, y=lat, group=group, fill=test.pred))+geom_polygon(color="black") + scale_fill_gradient(low="blue", high="red")
##Answer: The two maps look very similar. This is because most of our predicted
##probabilities are close to 0 or close to 1.


#Problem 3 - Understanding the Predictions


#In the 2012 election, the state of Florida ended up being a very close race.
#It was ultimately won by the Democratic party. Did we predict this state
#correctly or incorrectly? 
##Answer: Our models predict red for Republican, so no, our prediction is
##not correct.

#What was our predicted probability for the state of Florida?
which(predMap$region=="florida")
##This outputs many observation locations corresponding to Florida, so we
##can choose any, say 1148.
predMap$test.pred[1148]
##Answer: 0.9640395

#What does this imply?
##AnswerOur prediction model did not do a very good job of correctly 
##predicting the state of Florida, although we were very confident in our 
##incorrect prediction.


#Problem 4.1 - Parameter Settings


#Plots (1) and (2) were created by changing different parameters of 
#geom_polygon from their default values.

#What is the name of the parameter we changed to create plot (1)?
##Answer: We see the borders are now broken lines. This is changed
## by linetype.

#What is the name of the parameter we changed to create plot (2)?
##Answer: We see the borders are now thicker. This is changed by
##size.

#Plot (3) was created by changing the value of a different geom_polygon
#parameter to have value 0.3. Which parameter did we use?
##Answer: We see the fills are now light pink and light purple. This
##is changed by alpha.
