Sys.setlocale("LC_ALL", "C")

#Load the data.
songs = read.csv("songs.csv")


#Problem 1 - Understanding the Data


#How many observations (songs) are from the year 2010?
table(songs$year)
##Answer: 373

#How many songs does the dataset include for which the artist name is "Michael 
#Jackson"?
table(songs$artistname == "Michael Jackson")
##Answer: 18


#Which of these songs by Michael Jackson made it to the Top 10? i)Beat It ii)You
#Rock My World iii)Billie Jean iv)You Are Not Alone
songs$Top10[which(songs$songtitle == "Beat It")]
songs$Top10[which(songs$songtitle == "You Rock My World")]
songs$Top10[which(songs$songtitle == "Billie Jean")]
songs$Top10[which(songs$songtitle == "You Are Not Alone")]
#The which() function outputs the observation number corresponding to a song
#and then the value of Top10 (0 or 1) is retrieved for that observation number
#0 meaning it is not a Top10 song, 1 meaning it is.
##Answer: "You Rock My World", "You Are Not Alone"


#What are the values of variable timesignature that occur in our dataset? 
#Which timesignature value is the most frequent among songs in our dataset?
table(songs$timesignature)
##Answer: 0, 1, 3, 4, 5, 7


#Problem 2 - Creating Our Prediction Model


#Split the data into a training set "train" consisting of all the 
#observations up to and including 2009 song releases, and a testing set 
#"test", consisting of the 2010 song releases.
train = subset(songs, songs$year<=2009)
test = subset(songs, songs$year==2010)

#How many observations are in the training set?
nrow(train)
##Answer: 7201


#Build a logistic regression model from train that predicts Top10 from all of
#the other variables except year, songtitle, artistname, songID, artistID.
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
train = train[ , !(names(train) %in% nonvars) ]
test = test[ , !(names(test) %in% nonvars) ]
log = glm(Top10~., data=train, family=binomial)
#nonvars defines all of the variables we will remove, and the next two commands
#actually remove them


#What is the value of the Akaike Information Criterion (AIC)?
summary(log)
##Answer: 4827.2


#Given that the coefficients of timesignature_confidence, tempo_confidence and
#key_confidence are positive as seen in summary(log), what does this say about
#these variables?
##Answer: The higher the confidence is in any of these three the more likely a
##song is to be in the Top10.

#In general, if the confidence is low for the time signature, tempo, and key, 
#then the song is more likely to be complex. What does Model 1 suggest in
#terms of complexity?
##Answer: People prefer songs with higher confidence variables, meaning songs
##that are less complex.

#Songs with heavier instrumentation tend to be louder and more energetic.
#By inspecting the coefficient of the variable "loudness", what does Model 1 
#suggest?
##Answer: The coefficient is positive and therefore people prefer loud music,
##which suggests heavier instrumentation.

#By inspecting the coefficient of the variable "energy", do we draw the same 
#conclusions as above?
##Answer: The coefficient is negative and therefore people prefer less
##energetic music, which suggests lighter instrumentation.


#Problem 3 - Beware of Multicollinearity Issues!


#What is the correlation between the variables "loudness" and "energy" 
#in the training set?
cor(train$loudness, train$energy)
##Answer: 0.7399067

#Create log2, which is log1 without the independent variable "loudness".
log2 = glm(Top10~.-loudness, data=train, family=binomial)

#What do you observe about the coefficient of the variable "energy" in log2?
summary(log2)
##Answer: The coefficient is positive and so this model predicts that high energy
##music is predictive of a Top10 song. This contradicts what model log claims.

#Now, create log3, which should be exactly like log, but without the 
#variable "energy".
log3 = glm(Top10~.-energy, data=train, family=binomial)

#Do we make the same observation about the popularity of heavy instrumentation
#as we did with log2?
summary(log3)
##Answer: We see that the coefficient of the variable "loudness" is positive
##in log3 just as the coefficient of the variable "energy" is positive in log2,
##so both models suggest that listeners prefer music with heavier instrumentation.


#Problem 4 - Validating Our Model


#Make predictions on the test set using log3. What is the accuracy of Model
#3 on the test set, using a threshold of 0.45? 
test.pred = predict(log3, newdata=test, type="response")
table(test$Top10, test.pred>0.45)
##Answer: (309+19)/nrow(test) = 0.8793566


#What would the accuracy of the baseline model be on the test set?
sum(test$Top10==0)/nrow(test)
##Answer: 0.8418231
#We saw from the confusion matrix above that more songs are not Top10 hits
#than songs that are, so baseline model would predict not Top10 for each song.

#How many songs does log3 correctly predict as Top 10 hits in 2010?
#How many non-hit songs does log3 predict will be Top 10 hits in 2010?
##Answer: 19; 5

#What is the sensitivity of log3 on the test set, using a threshold of 0.45?
#What is the specificity of log3 on the test set, using a threshold of 0.45?
##Answer: 19/(40+19) = 0.3220339; 309/(309+5) = 0.9840764

#What conclusions can you make about our model?
#The model has a high specificity and a low sensitivity. While the model
#accurately predicts less than half of the Top10 songs, it also predicts few
#non-Top10 songs as being Top10, so we can be confident in the songs that are
#predicted as being Top10 quality. 

