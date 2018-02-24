Sys.setlocale("LC_ALL", "C")

#Load the data.
trials=read.csv("clinical_trial.csv", stringsAsFactors=FALSE)


#Problem 1 - Loading the Data


#How many characters are there in the longest abstract?
max(nchar(trials$abstract))
##Answer: 3708

#How many search results provided no abstract?
sum(trials$abstract == "")
##Answer: 112

#Find the observation with the minimum number of characters in the title 
#out of all of the observations in this dataset. What is the text of the title
#of this article? 
min(nchar(trials$title))
#This outputs 28.
which(nchar(trials$title) == 28)
#This outputs 1258
trials$title[1258]
##Answer: "A decade of letrozole: FACE."


#Problem 2 - Preparing the Corpus


#Build a corpus from trials$title and preprocess it.
library(tm)
corpusTitle = VCorpus(VectorSource(trials$title))
corpusTitle = tm_map(corpusTitle, content_transformer(tolower))
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmTitle = as.data.frame(as.matrix(dtmTitle))

#Build a corpus from trials$abstract and preprocess it.
corpusAbstract = VCorpus(VectorSource(trials$abstract))
corpusAbstract = tm_map(corpusAbstract, content_transformer(tolower))
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, stemDocument)
dtmAbstract = DocumentTermMatrix(corpusAbstract)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

#How many terms remain in dtmTitle after removing sparse terms?
ncol(dtmTitle)
##Answer: 31

#How many terms remain in dtmAbstract?
ncol(dtmAbstract)
##Answer: 335

#What is the most likely reason why dtmAbstract has so many more terms than
#dtmTitle?
##Answer: Abstracts typically have more words than titles.

#What is the most frequent word stem across all the abstracts?
sort((colSums(dtmAbstract)))
##Answer: patient


#Problem 3 - Building a model


#We want to combine dtmTitle and dtmAbstract into a single data frame to make 
#predictions. However, some of the variables in these data frames have the 
#same names. Let's fix this.
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

#What was the effect of these functions?
##Answer: Adding the letter T in front of all the title variable names and 
##adding the letter A in front of all the abstract variable names.

#Using cbind(), combine dtmTitle and dtmAbstract into a single data frame
#called dtm.
dtm = cbind(dtmTitle, dtmAbstract)

#Add the dependent variable "trial" to dtm, copying it from the original 
#data frame called trials.
dtm$trial = trials$trial

#How many columns are in this combined data frame?
ncol(dtm)
##Answer: 366

#Now that we have prepared our data frame, it's time to split it into a 
#training and testing set and to build regression models. Set the random seed 
#to 144 and use the sample.split function from the caTools package to split 
#dtm into data frames named "train" and "test", putting 70% of the data in 
#the training set.
library(caTools)
set.seed(144)
split = sample.split(dtm$trial, SplitRatio=0.7)
train = subset(dtm, split==TRUE)
test = subset(dtm, split==FALSE)

#What is the accuracy of the baseline model on the training set?
table(train$trial)
##Answer: 730/nrow(train) = 0.5606759

#Build a CART model using all the independent variables in the training set to
#train the model, and then plot the CART model.
library(rpart)
library(rpart.plot)
tree = rpart(trial~., data=train, method="class")
prp(tree)

#What is the name of the first variable the model split on?
##Answer: Tphase

#What is the maximum predicted probability on the train set of results being
#trials
tree.train.pred = predict(tree)
max(tree.train.pred[,2])
##Answer: 0.8718861

#Without running the analysis, how do you expect the maximum predicted
#probability to differ in the testing set?
##Answer: from prp(tree) we see there are not too many leaves, whereas test
##consists of 558 observations. It is probable that at least one of its 
##observations will fall in the leaf corresponding to the leaf that gives
##the max predicted probability of a result being a trial, so it would likely 
##be exactly the same as the max from the train set.

#What is the training set accuracy of the CART model? The sensitivity? The
#specificity? Use a threshold of 0.5.
table(train$trial, tree.train.pred[,2]>0.5)
##Answer: accuracy = (631+441)/nrow(train) = 0.8233487;
##sensitivity = 441/(441+131) = 0.770979; specificity=631/(631+99)=0.8643836


#Problem 4 - Evaluating the model on the testing set


#Evaluate the CART model on the testing set using the predict function for
#probabilities.
predTest = predict(tree, newdata=test)

#What is the testing set accuracy, assuming a probability threshold of 0.5
#for predicting that a result is a clinical trial?
table(test$trial, predTest[,2]>0.5)
##Answer: (261+162)/nrow(test) = 0.7580645

#What is the testing set AUC of the prediction model?
library(ROCR)
ROCRpred = prediction(predTest[,2], test$trial)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc
##Answer: 0.8371063


#Problem 5 - Decision-Maker Tradeoffs

#1)The decision maker for this problem, a researcher performing a review of the
#medical literature, would use a model in the following workflow:
#For all of the papers retreived in the PubMed Search, predict which papers 
#are clinical trials using the model. This yields some initial Set A of papers
#predicted to be trials, and some Set B of papers predicted not to be trials.
#2)Then, the decision maker manually reviews all papers in Set A, verifying that
#each paper meets the study's detailed inclusion criteria (for the purposes of 
#this analysis, we assume this manual review is 100% accurate at identifying 
#whether a paper in Set A is relevant to the study). This yields a more 
#limited set of papers to be included in the study, which would ideally be all
#papers in the medical literature meeting the detailed inclusion criteria for
#the study. 3)Then, perform the study-specific analysis, using data extracted 
#from the limited set of papers identified in step 2.

#What is the cost associated with the model in Step 1 making a false negative
#prediction?
##Answer: A paper that should have been included in Set A will be missed,
##affecting the quality of the results of Step 3.

#What is the cost associated with the model in Step 1 making a false positive
#prediction?
##Answer: A paper will be mistakenly added to Set A, 
##yielding additional work in Step 2 of the process but not affecting the
##quality of the results of Step 3.

#Are false-positives or false-negatives more costly? Should a threshold above
#or below 0.5 be used?
##Answer: False-negatives are more costly and so we should therefore use a
##threshold below 0.5.