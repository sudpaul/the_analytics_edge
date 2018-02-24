Sys.setlocale("LC_ALL", "C")

#Load the data.
wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)


#Problem 1 - Bags of Words


#Convert the "Vandal" column to a factor.
wiki$Vandal = as.factor(wiki$Vandal)

#How many cases of vandalism were detected in the history of this page?
table(wiki$Vandal)
##Answer: 1815

#Build a document term matrix from the "Added" variable.
library(tm)
corpus = VCorpus(VectorSource(wiki$Added))
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtmAdded = DocumentTermMatrix(corpus)

#How many terms appear in dtmAdded?
dtmAdded
##Answer: 6675

#Filter out sparse terms by keeping only terms that appear in 0.3% or more of
#the revisions, and call the new matrix sparseAdded.
sparseAdded = removeSparseTerms(dtmAdded, 0.997)

#How many terms appear in sparseAdded?
sparseAdded
##Answer: 166

#Convert sparseAdded to a data frame called wordsAdded, and then prepend all 
#the words with the letter A.
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

#Now repeat all of the steps we've done so far to create a Removed bag-of-words
#dataframe, called wordsRemoved, except this time, prepend all of the words
#with the letter R.
corpus2 = VCorpus(VectorSource(wiki$Removed))
corpus2 = tm_map(corpus2, removeWords, stopwords("english"))
corpus2 = tm_map(corpus2, stemDocument)
dtmRemoved = DocumentTermMatrix(corpus2)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

#How many words are in the wordsRemoved data frame?
ncol(wordsRemoved)
##Answer: 162

#Combine the two data frames into a data frame called wikiWords.
wikiWords = cbind(wordsAdded, wordsRemoved)

#Add the Vandal column.
wikiWords$Vandal = wiki$Vandal

#Set the random seed to 123 and then split the data set using sample.split 
#from the "caTools" package to put 70% in the training set.
library(caTools)
set.seed(123)
split = sample.split(wikiWords$Vandal, SplitRatio=0.7)
train = subset(wikiWords, split==TRUE)
test = subset(wikiWords, split==FALSE)

#What is the accuracy on the test set of a baseline method that always 
#predicts "not vandalism" 
table(wikiWords$Vandal)
##Answer: 2061/nrow(wikiWords) = 0.5317337

#Build a CART model to predict Vandal, using all of the other variables
#as independent variables.
library(rpart)
library(rpart.plot)
tree = rpart(Vandal~., data=wikiWords, method="class")

#What is the accuracy of the model on the test set, using a threshold of 0.5?
tree.pred = predict(tree, newdata=test, type="class")
table(test$Vandal, tree.pred)
##Answer: (618+13)/nrow(test) = 0.5425623

#Plot the CART tree. How many word stems does the CART model use?
prp(tree)
##Answer: 2

#Given the performance of the CART model relative to the baseline, what is 
#the best explanation of these results?
##Answer: Although it beats the baseline, bag of words is not very predictive
##for this problem.


#Problem 2 - Problem-specific Knowledge


#Create a copy of your dataframe from the previous question.
wikiWords2 = wikiWords

#Make a new column in wikiWords2 that is 1 if "http" was in Added.
wikiWords2$HTTP = as.numeric(grepl("http", wiki$Added, fixed=TRUE))

#Based on this new column, how many revisions added a link?
sum(wikiWords2$HTTP)
##Answer: 217

#Create new train and test sets using the original split.
wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)

#Then create a new CART model using this new variable as one of the 
#independent variables. What is the new accuracy of the CART model on the test 
#set, using a threshold of 0.5?
tree2 = rpart(Vandal~., data=wikiTrain2, method="class")
tree2.pred = predict(tree2, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, tree2.pred)
##Answer: (609+57)/nrow(wikiTest2) = 0.5726569

#Sum the rows of dtmAdded and dtmRemoved and add them as new variables in your 
#data frame wikiWords2.
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

#What is the average number of words added?
mean(wikiWords2$NumWordsAdded)
#Answer: 4.050052

#Make new training and testing sets with wikiWords2 using "split" from before.
#Create the CART model again.
wikiTrain3 = subset(wikiWords2, split==TRUE)
wikiTest3 = subset(wikiWords2, split==FALSE)
tree3 = rpart(Vandal~., data=wikiTrain3, method="class")

#What is the new accuracy of the CART model on the test set?
tree3.pred = predict(tree3, newdata=wikiTest3, type="class")
table(wikiTest3$Vandal, tree3.pred)
##Answer: (514+248)/nrow(wikiTest3) = 0.6552021


#Problem 3 - Using Non-Textual Data


#Make a copy of wikiWords2. Then add the two original variables Minor and
#Loggedin.
wikiWordsX = wikiWords2
wikiWordsX$Minor = wiki$Minor
wikiWordsX$Loggedin = wiki$Loggedin

#Make new training and testing sets with wikiWords3 using the same split as
#before.
trainX = subset(wikiWordsX, split==TRUE)
testX = subset(wikiWordsX, split==FALSE)

#Build a CART model using all the training data. What is the accuracy of the
#model on the test set?
tree4 = rpart(Vandal~., data=trainX, method="class")
tree4.pred = predict(tree4, newdata=testX, type="class")
table(testX$Vandal, tree4.pred)
##Answer: (595+241)/nrow(testX) = 0.7188306

#Plot the CART tree. How many splits are there in the tree?
prp(tree4)
##Answer: 3

#So this last model has a higher accuracy than the previous tree but is only
#slightly more complex (3 splits v.s. 2). It is therefore probably a better
#model.


