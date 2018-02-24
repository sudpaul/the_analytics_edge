Sys.setlocale("LC_ALL", "C")


#Problem 1 - Loading the Dataset


#Load the data.
emails=read.csv("emails.csv", stringsAsFactors=FALSE)

#How many emails are in the dataset?
nrow(emails)
##Answer: 5728

#How many of the emails are spam?
sum(emails$spam)
##Answer: 1368

#Which word appears at the beginning of every email in the dataset?
emails$text[1]
##Answer: Subject

#Could a spam classifier potentially benefit from including the frequency of the
#word that appears in every email?
##Answer: Yes -- the number of times the word appears might help us 
##differentiate spam from ham.

#How many characters are in the longest email in the dataset?
max(nchar(emails$text))
##Answer: 43952

#Which row contains the shortest email in the dataset?
which.min(nchar(emails$text))
##Answer: 1992


#Problem 2 - Preparing the Corpus


#Build and pre-process the corpus.
library(tm)
corpus = VCorpus(VectorSource(emails$text))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)

#How many terms are in dtm?
dtm
##Answer: 28687

#Limit dtm to contain terms appearing in at least 5% of documents, and store 
#this result as spdtm
spdtm = removeSparseTerms(dtm, 0.95)

#How many terms are in spdtm?
spdtm
##Answer: 330

#Build a data frame called emailsSparse from spdtm, and use the make.names
#function to make the variable names of emailsSparse valid.
emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))

#What is the word stem that shows up most frequently across all the emails 
#in the dataset? 
sort(colSums(emailsSparse))
#Answer: enron

#Add a variable called "spam" to emailsSparse containing the email spam labels.
emailsSparse$spam = emails$spam

#How many word stems appear at least 5000 times in the ham emails in the 
#dataset?
subset.ham = subset(emailsSparse, spam==0)
sort((colSums(subset.ham)))
##Answer: From the table we see there are 6 word stems with at least 5000
##appearances.

#How many word stems appear at least 1000 times in the spam emails in the 
#dataset?
spam.subset = subset(emailsSparse, spam==1)
sort((colSums(spam.subset)))
##Answer: From the table we see there are 3 word stems with at least 1000
##appearances. (We don't count "spam", which is the dependent variable and
##not a word stem.)

#The lists of most common words are significantly different between the spam
#and ham emails. What does this likely imply?
##Answer: The frequencies of these most common words are likely to help
##differentiate between spam and ham.

#Several of the most common word stems from the ham documents, such as 
#"enron", "hou" (short for Houston), "vinc" (the word stem of "Vince") and 
#"kaminski", are likely specific to Vincent Kaminski's inbox. What does this
#mean about the applicability of the text analytics models we will train for
#the spam filtering problem?
##Answer: The models we build are personalized, and would need to be further
##tested before being used as a spam filter for another person.


#Problem 3 - Building machine learning models


#Convert the dependent variable to a factor.
emailsSparse$spam = as.factor(emailsSparse$spam)

#Split the data into a training and testing set
library(caTools)
set.seed(123)
split = sample.split(emailsSparse$spam, SplitRatio=0.7)
train = subset(emailsSparse, split==TRUE)
test = subset(emailsSparse, split==FALSE)

#Train a logistic regression model called spamLog and obtain the predicted
#spam probabilities for the training set. 
spamLog = glm(spam~., data=train, family=binomial)
pred.log = predict(spamLog, type="response")

#How many of the training set predicted probabilities from spamLog are less
#than 0.00001?
sum(pred.log < 0.00001)
##Answer: 3046

#How many of the training set predicted probabilities from spamLog are more
#than 0.99999?
sum(pred.log > 0.99999)
##Answer: 954

#How many of the training set predicted probabilities from spamLog are 
#between 0.00001 and 0.99999?
sum(pred.log >= 0.00001 & pred.log <= 0.99999)
##Answer: 10

#How many variables are labeled as significant (at the p=0.05 level) in the
#logistic regression summary output?
summary(spamLog)
##Answer: 0

#Train a CART model called spamCART and obtain the predicted spam 
#probabilities for the training set. 
library(rpart)
library(rpart.plot)
spamCART = rpart(spam~., data=train, method="class")

#How many of the word stems "enron", "hou", "vinc", and "kaminski" appear 
#in the CART tree? 
prp(spamCART)
##Answer: 2, namely vinc and enron.

#What is the training set accuracy of spamLog, using a threshold of 0.5 for
#predictions?
pred.log = predict(spamLog, type="response")
table(train$spam, pred.log>0.5)
##Answer: (3052+954)/nrow(train) = 0.9990025

#What is the training set AUC of spamLog?
library(ROCR)
ROCRpred.log = prediction(pred.log, train$spam)
auc.log = as.numeric(performance(ROCRpred.log, "auc")@y.values)
auc.log
##Answer: 0.9999959

#What is the training set accuracy of spamCART, using a threshold of 0.5 
#for predictions?
pred.CART = predict(spamCART)
table(train$spam, pred.CART[,2]>0.5)
##Answer: (2885+894)/nrow(train) = 0.942394

#What is the training set AUC of spamCART?
ROCRpred.CART = prediction(pred.CART[,2], train$spam)
auc.CART = as.numeric(performance(ROCRpred.CART, "auc")@y.values)
auc.CART
##Answer: 0.9696044

#Train a random forest model called spamRF and obtain the predicted spam
#probabilities for the training set. 
library(randomForest)
set.seed(123)
spamRF = randomForest(spam~., data=train)

#What is the training set accuracy of spamRF, using a threshold of 0.5 
#for predictions?
pred.RF = predict(spamRF, type="prob")
table(train$spam, pred.RF[,2]>0.5)
##Answer: (3013+914)/nrow(train) = 0.9793017

#What is the training set AUC of spamRF?
ROCRpred.RF = prediction(pred.RF[,2], train$spam)
auc.RF = as.numeric(performance(ROCRpred.RF, "auc")@y.values)
auc.RF
##Answer: 0.9979116

#Which model had the best training set performance, in terms of accuracy 
#and AUC?
##Answer: spamLog


#Problem 4 - Evaluating on the Test Set


#Obtain predicted probabilities for the testing set for each of the models,
#again ensuring that probabilities instead of classes are obtained.
pred.log.test = predict(spamLog, newdata=test, type="response")

#What is the testing set accuracy of spamLog, using a threshold of 0.5 for predictions?
table(test$spam, pred.log.test>0.5)
##Answer: (1257+376)/nrow(test) = 0.9505239

#What is the testing set AUC of spamLog?
ROCRpred.log.test = prediction(pred.log.test, test$spam)
auc.log.test = as.numeric(performance(ROCRpred.log.test, "auc")@y.values)
auc.log.test
##Answer: 0.9627517

#What is the testing set accuracy of spamCART, using a threshold of 0.5 for predictions?
pred.CART.test = predict(spamCART, newdata=test)
table(test$spam, pred.CART.test[,2]>0.5)
##Answer: (1228+386)/nrow(test) = 0.9394645

#What is the testing set AUC of spamCART?
ROCRpred.CART.test = prediction(pred.CART.test[,2], test$spam)
auc.CART.test = as.numeric(performance(ROCRpred.CART.test, "auc")@y.values)
auc.CART.test
##Answer: 0.963176

#What is the testing set accuracy of spamRF, using a threshold of 0.5 for predictions?
pred.RF.test = predict(spamRF, newdata=test, type="prob")
table(test$spam, pred.RF.test[,2]>0.5)
##Answer: (1290+385)/nrow(test) = 0.9749709

#What is the testing set AUC of spamRF?
ROCRpred.RF.test = prediction(pred.RF.test[,2], test$spam)
auc.RF.test = as.numeric(performance(ROCRpred.RF.test, "auc")@y.values)
auc.RF.test
##Answer: 0.9975656

#Which model had the best testing set performance, in terms of accuracy and 
#AUC?
##Answer: The random forest outperformed logistic regression and CART in 
##both measures.

#Which model demonstrated the greatest degree of overfitting?
##Answer: spamLog because it has such a high accuracy and AUC on the train
##set but smaller values on the test set. For spamCART and spamRF the AUC
##and accuracy on their train sets are similar to those on their test sets.




