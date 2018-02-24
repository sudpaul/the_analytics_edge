Sys.setlocale("LC_ALL", "C")


#Problem 1 - Preparing the Data


#Load the data.
tweets=read.csv("tweets.csv", stringsAsFactors=FALSE)

#Preprocess the data without stemming or removing sparse terms.
library(tm)
corpus = VCorpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
dtm = DocumentTermMatrix(corpus)
words = as.data.frame(as.matrix(dtm))

#How many unique words are there across all the documents?
dtm
##Answer: There are 3780 unique words (terms).

#What is the most compelling rationale for skipping the stemming step when 
#visualizing text data?
##Answer: It will be easier to read and understand the word cloud if it 
#includes full words instead of just the word stems.

#Problem 2 - Building a Word Cloud

#Which function can we apply to words to get a vector of the words in our 
#dataset, which we'll pass as the first argument to wordcloud()?
##Answer: colnames

#Which function should we apply to words to obtain the frequency of each word 
#across all tweets?
##Answer: colSums

#Use words to build a word cloud.
library(wordcloud)
wordcloud(colnames(words), colSums(words))

#What is the most common word across all the tweets?
##Answer: apple, as it is the largest.

#Repeat the steps to load and pre-process the corpus, this time removing the
#most frequent word in addition to all elements of stopwords("english") in the
#call to tm_map with removeWords.
corpus2 = VCorpus(VectorSource(tweets$Tweet))
corpus2 = tm_map(corpus2, content_transformer(tolower))
corpus2 = tm_map(corpus2, removePunctuation)
corpus2 = tm_map(corpus2, removeWords, c("apple", stopwords("english")))
dtm2 = DocumentTermMatrix(corpus2)
words2 = as.data.frame(as.matrix(dtm2))

#Create a word cloud with the updated corpus. 
wordcloud(colnames(words2), colSums(words2), scale=c(2,0.25))

#What is the most common word in this new corpus?
##Answer: iphone is the largest and therefore the most frequent.


#Problem 3 - Size and Color
#These questions are based on four wordclouds that are displayed.


#Which word cloud is based only on the negative tweets (tweets with Avg value
#-1 or less)?
##Answer: Word Cloud C, with words like "hate" and "freak"

#Only one word cloud was created without modifying parameters min.freq or 
#max.words. Which word cloud is this?
##Answer: Word Cloud A; min.freq sets a lower bound for the frequency of a
##word for it to be displayed and max.words sets an upper bounder for the
##total number of displayed words, so they both set an upper limit to the size
##of the word cloud. Word Cloud A is the largest of the four.

#Which word clouds were created with parameter random.order set to FALSE?
##Answer: random.order = FALSE prints more frequent words in the middle of the
##word cloud and less frequent words around the perimeter. This is the case
##in word clouds B and D.

#Which word cloud was built with a non-default value for parameter rot.per?
##Answer: The default value is 0.1, meaning 10% of the words are vertical by
##default. This is not the case with Word Cloud A, which has a high proportion
##of vertical words.

#In Word Cloud C and Word Cloud D, we provided a color palette ranging from 
#light purple to dark purple as the parameter colors. For which word cloud was
#the parameter random.color set to TRUE?
##Answer: When random.color=TRUE this means the words are colored randomly
##from the available list of colors; when FALSE they are colored based on 
##frequency. We see that Word Cloud D they are colored randomly because there
##are large words and small words of the same color.


#Problem 4 - Selecting a Color Palette


#In the RColorBrewer palette, which color palette would be most appropriate
#for use in a word cloud for which we want to use color to indicate word
#frequency? Choose between Accent, Set2 and YlOrRd.
display.brewer.all()
##Answer: We see that Accent and Set2 are random colors with no relationship
##between them, whereas the colors in YlOrRd are gradually increasing in 
##darkness, which could represent different frequencies in an obvious way, so
##we would choose YlOrRd.

#Which RColorBrewer palette name would be most appropriate to use when 
#preparing an image for a document that must be in grayscale?
##Answer: Greys

#In sequential palettes, sometimes there is an undesirably large contrast 
#between the lightest and darkest colors. What command addresses this issue by
#removing the first 4 elements of the 9-color palette of blue colors?
##Answer: brewer.pal(9, "Blues")[c(-1, -2, -3, -4)]  or
##brewer.pal(9, "Blues")[c(5, 6, 7, 8, 9)]