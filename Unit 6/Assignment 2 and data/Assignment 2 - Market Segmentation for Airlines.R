Sys.setlocale("LC_ALL", "C")


#Problem 1 - Normalizing the Data


#Load the data.
airlines = read.csv("AirlinesCluster.csv")

#Which TWO variables have (on average) the smallest values?
summary(airlines)
##Answer: BonusTrans, FlightTrans

#Which TWO variables have (on average) the largest values?
##Answer: Balance, BonusMiles

#Why is it important to normalize the data before clustering?
##Answer: If we don't normalize the data, the clustering will be dominated by 
##the variables that are on a larger scale.

#Normalize the data.
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)

#In the normalized data, which variable has the largest maximum value?
summary(airlinesNorm)
##Answer: FlightMiles

#In the normalized data, which variable has the smallest minimum value?
##Answer: DaysSinceEnroll


#Problem 2 - Hierarchical Clustering


#Run the Hierarchical clustering algorithm on the normalized data.
distances = dist(airlinesNorm, method="euclidean")
hmodel = hclust(distances, method="ward.D")

#Suppose the airline is looking for somewhere between 2 and 10 clusters.
#According to the dendrogram, which of 2,3,6 and 7 is NOT a good choice for 
#the number of clusters?
plot(hmodel)
##Answer: Using the horizontal line method we see that 2,3 or 7 is good but 6
##is not, as the horizontal line corresponding to 6 has barely any room to
##move up or down without crossing over other horizontal lines.

#Divide the data points into 5 clusters by using the cutree function.
clusterGroup = cutree(hmodel, k=5)
cluster1 = subset(airlinesNorm, clusterGroup==1)
cluster2 = subset(airlinesNorm, clusterGroup==2)
cluster3 = subset(airlinesNorm, clusterGroup==3)
cluster4 = subset(airlinesNorm, clusterGroup==4)
cluster5 = subset(airlinesNorm, clusterGroup==5)

#How many data points are in Cluster 1?
nrow(cluster1)
##Answer: 776

#Now, use tapply to compare the average values in each of the variables from 
#the original data set for each of the 5 clusters in the original data.
tapply(airlines$Balance, clusterGroup, mean)
tapply(airlines$QualMiles, clusterGroup, mean)
tapply(airlines$BonusMiles, clusterGroup, mean)
tapply(airlines$BonusTrans, clusterGroup, mean)
tapply(airlines$FlightMiles, clusterGroup, mean)
tapply(airlines$FlightTrans, clusterGroup, mean)
tapply(airlines$DaysSinceEnroll, clusterGroup, mean)

#Compared to the other clusters, Cluster 1 has the largest average values in
#which variables?
##Answer:DaysSinceEnroll

#How would you describe the customers in Cluster 1?
##Answer: They are infrequent fliers, given that they have the smallest mean
##FlightTrans, but they have nonetheless have a reasonable number of bonus
##transactions and a decent-sized Balance.

#Compared to the other clusters, Cluster 2 has the largest average values in 
#which variables?
##Answer: QualMiles, FlightMiles, FlightTrans

#How would you describe the customers in Cluster 2?
##Answer: The three variables above are relatively much larger in cluster2
##than in any of the others. They are customers who have accumulated a large 
##amount of miles, and the ones with the largest number of flight transactions.

#Compared to the other clusters, Cluster 3 has the largest average values in
#which variables?
##Answer: Balance, BonusMiles, BonusTrans

#How would you describe the customers in Cluster 3?
##Answer: Customers who have accumulated a large amount of miles, mostly
##through non-flight transactions, as we see their mean FlightMiles is low.

#Compared to the other clusters, Cluster 4 has the largest average values in
#which variables?
##Answer: None

#How would you describe the customers in Cluster 4?
##Answer: They are relatively new customers who seem to be accumulating miles,
##mostly through non-flight transactions as we can see their mean Balance is
##52335.91 and their mean DaysSinceEnroll is 2840.823 and their mean
##BonusMiles is 20788.766 while their mean FlightTrans is only 0.34447.

#Compared to the other clusters, Cluster 5 has the largest average values in
#which variables?
##Answer: None

#How would you describe the customers in Cluster 5?
##Answer: They are relatively new customers who don't use the airline very
##often, given that they are low in all of the variables.


#Problem 3 - K-Means Clustering


#Now run the k-means clustering algorithm on the normalized data, again
#creating 5 clusters. Set the seed to 88 right before running the clustering
#algorithm, and set the argument iter.max to 1000.
set.seed(88)
kmc = kmeans(airlinesNorm, centers=5, iter.max=1000)

#How many clusters have more than 1,000 observations?
kmc$size
##Answer: 2

#Do you expect Cluster 1 of the K-Means clustering output to necessarily be 
#similar to Cluster 1 of the Hierarchical clustering output?
##Answer: No, because cluster ordering is not meaningful in either k-means 
##clustering or hierarchical clustering.

























