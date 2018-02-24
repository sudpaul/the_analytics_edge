Sys.setlocale("LC_ALL", "C")

#Load the data.
kos = read.csv("dailykos.csv")


#Problem 1 - Hierarchical Clustering


#Build a hierarchical clustering model. 
distances = dist(kos, method="euclidean")
hmodel = hclust(distances, method="ward.D")

#Running the dist function will probably take you a while. Why? 
##Answer: There are many observations and many variables.

#Plot the dendrogram of your hierarchical clustering model. Just looking at the 
#dendrogram, which of the following seem like good choices for the number of 
#clusters?
plot(hmodel)
##Answer: Two or 3 looks like a good choice using the horizontal line method.

#In this problem, we are trying to cluster news articles or blog posts into 
#groups. This can be used to show readers categories to choose from when trying 
#to decide what to read. Just thinking about this application, what are good 
#choices for the number of clusters between 2, 3, 7 and 8?
##Answer: Two or 3 would not be enough to cluster the variety that exists
##among articles and posts, so 7 or 8 would be better.

#Use the cutree function to split your data into 7 clusters.
clusterGroup = cutree(model, k=7)

#Create 7 new datasets, each containing the observations from one of the 
#clusters.
cluster1 = subset(kos, clusterGroup==1)
cluster2 = subset(kos, clusterGroup==2)
cluster3 = subset(kos, clusterGroup==3)
cluster4 = subset(kos, clusterGroup==4)
cluster5 = subset(kos, clusterGroup==5)
cluster6 = subset(kos, clusterGroup==6)
cluster7 = subset(kos, clusterGroup==7)

#How many observations are in each of the clusters?
nrow(cluster1)
nrow(cluster2)
nrow(cluster3)
nrow(cluster4)
nrow(cluster5)
nrow(cluster6)
nrow(cluster7)
##Answer: In order, the clusters have 1266, 321, 374, 139, 407, 714 and 209
##observations. cluster1 has the most and cluster4 has the least.

#Look at the mean frequency of the 6 most common words in each cluster.
tail(sort(colMeans(cluster1)))
tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))

#What is the most frequent word in cluster1, in terms of average value?
##Answer: bush

#Which words best describe cluster 2?
##Answer: november, poll, vote, challenge

#Which cluster could best be described as the cluster related to the Iraq war?
##Answer: cluster5, as the words "iraq" and "war" are the two most frequent.

#In 2004, one of the candidates for the Democratic nomination for the President
#of the United States was Howard Dean, John Kerry was the candidate who won the
#democratic nomination, and John Edwards was the running mate of John Kerry.
#Given this information, which cluster best corresponds to the democratic party?
##Answer: cluster 7, as "dean" and "kerry" are the two most frequent words and
##"edward" is the fourth most frequent.


#Problem 2 - K-Means Clustering


#Run k-means clustering with a seed of 1000 and 7 cluster groups.
set.seed(1000)
kmc = kmeans(kos, centers=7)

#Subset your data into the 7 clusters.
kcluster1 = subset(kos, kmc$cluster==1)
kcluster2 = subset(kos, kmc$cluster==2)
kcluster3 = subset(kos, kmc$cluster==3)
kcluster4 = subset(kos, kmc$cluster==4)
kcluster5 = subset(kos, kmc$cluster==5)
kcluster6 = subset(kos, kmc$cluster==6)
kcluster7 = subset(kos, kmc$cluster==7)

#How many observations are in each of the clusters?
nrow(kcluster1)
nrow(kcluster2)
nrow(kcluster3)
nrow(kcluster4)
nrow(kcluster5)
nrow(kcluster6)
nrow(kcluster7)
##Answer: In order they have 146, 144, 277, 2063, 163, 329, 308

#Which cluster has the fewest number of observations?
##Answer: kcluster2

#Which cluster has the most number of observations?
##Answer: kcluster4

#Output the six most frequent words in each cluster.
tail(sort(colMeans(kcluster1)))
tail(sort(colMeans(kcluster2)))
tail(sort(colMeans(kcluster3)))
tail(sort(colMeans(kcluster4)))
tail(sort(colMeans(kcluster5)))
tail(sort(colMeans(kcluster6)))
tail(sort(colMeans(kcluster7)))

#Which k-means cluster best corresponds to the Iraq War?
##Answer: kcluster3, as we see "war" is the most frequent word and "iraqi" is
##the fourth most frequent.

#Which k-means cluster best corresponds to the democratic party? 
##Answer: kcluster2 as we see that "dean" and "kerry" are the two most frequent
##words and "edward" is the fourth most frequent.

#Compare how observations were assigned to clusters in the two different methods.
table(clusterGroup, kmc$cluster)

#Which Hierarchical Cluster best corresponds to K-Means Cluster 2?
##Answer: From the table we see that the kcluster2 column is largest in its 
##bottom row, corresponding to cluster7.

#Which Hierarchical Cluster best corresponds to K-Means Cluster 3?
##Answer: From the table we see that the kcluster3 column is largest in its 
##fifth row, corresponding to cluster5.

#Which Hierarchical Cluster best corresponds to K-Means Cluster 7?
##Answer: From the table we see that the kcluster7 column is largest in its 
##fourth row, corresponding to cluster4. However, this represents less than half
##of the data in kcluster7.

#Which Hierarchical Cluster best corresponds to K-Means Cluster 6?
##Answer: From the table we see that the kcluster6 column is largest in its 
##second row, corresponding to cluster2.



































