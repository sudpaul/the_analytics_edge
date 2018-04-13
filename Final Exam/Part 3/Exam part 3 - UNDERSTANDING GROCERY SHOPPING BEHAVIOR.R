Sys.setlocale("LC_ALL", "C")

#Load the data.
orders = read.csv("orders.csv")


#Problem 1 - Reading the Data


#What time of day are most orders placed?
table(orders$order_hour_of_day)
##Answer: We see the numbers of orders are largest for the middle hours, so
##midday.

#What is the average days since prior order?
mean(orders$days_since_prior_order)
##Answer: 17.093


#Problem 2 - Descriptive Statistics


#What's the correlation between the orders of "fresh.fruits" and 
#"fresh.vegetables"?
cor(orders$fresh.fruits, orders$fresh.vegetables)
##Answer: 0.3955114

#In the dataset, what proportion of orders have at least one item from the 
#frozen.pizza aisle?
sum(orders$frozen.pizza>0)/nrow(orders)
##Answer: 0.0522


#Problem 3 - Normalizing the Data


#Run the following command to construct a subset of only aisle information on
#the orders.
orders.aisle = orders[,5:ncol(orders)]

#Normalize all of the variables in the orders.aisle dataset by entering the
#following commands in your R console:
library(caret)
preproc = preProcess(orders.aisle)
ordersNorm = predict(preproc, orders.aisle)

#What is the maximum value of frozen.dessert after normalization?
max(ordersNorm$frozen.dessert)
##Answer: 11.74144

#What is the minimum value of soft.drinks in the normalized dataset?
min(ordersNorm$soft.drinks)
##Answer: -0.2873327

distances = dist(ordersNorm, method="euclidean")
hmodel = hclust(distances, method="ward.D")
plot(hmodel, labels=FALSE)

#Based on the dendrogram, how many clusters do you think would NOT be
#appropriate for this problem (2,3,4 or 5)?
##Answer: Using the horizontal line method we see 5 would be a bad choice.


#Problem 5 - K-means Clustering


#Run the k-means clustering algorithm on your dataset limited to the aisle 
#information only, selecting 4 clusters. Right before using the kmeans
#function, type "set.seed(200)" in your R console.
set.seed(200)
kmc = kmeans(ordersNorm, centers=4)

#How many observations are in the smallest cluster?
str(kmc)
##Answer:  36

#How many observations are in the largest cluster?
##Answer:  3409


#Problem 6 - Understanding the Clusters


#Which cluster best fits the description "orders mostly consist of cleaning
#supplies, beauty, and some pantry foods"?
tapply(ordersNorm$cleaning.products, kmc$cluster, mean)
tapply(ordersNorm$beauty, kmc$cluster, mean)
tapply(ordersNorm$trail.mix.snack.mix, kmc$cluster, mean)
##Answer: We see these values are largest in cluster 1.


#Problem 7 - Understanding the Clusters


#Which cluster best fits the description "frozen desserts"?
tapply(ordersNorm$frozen.dessert, kmc$cluster, mean)
##Answer: We see this value is largest in cluster 4.


#Problem 8 - Understanding the Clusters


#Which cluster on average has the smallest amount of items ordered?
cluster1 = subset(ordersNorm, kmc$cluster==1)
cluster2 = subset(ordersNorm, kmc$cluster==2)
cluster3 = subset(ordersNorm, kmc$cluster==3)
cluster4 = subset(ordersNorm, kmc$cluster==4)
mean(rowSums(cluster1))
mean(rowSums(cluster2))
mean(rowSums(cluster3))
mean(rowSums(cluster4))
##Answer: cluster3 has the smallest mean.


#Problem 9 - Random Behavior


#If we ran hierarchical clustering a second time without making any additional
#calls to set.seed, we would expect:
##Answer: Identical results to the first hierarchical clustering because there
##is no randomness in the hierarchical clustering algorithm. 

#If we ran k-means clustering a second time without making any additional
#calls to set.seed, we would expect:
##Answer: Different results than the first hierarchical clustering because 
##there is randomness involved in the hierarchical clustering algorithm. 

#If we ran k-means clustering a second time, again running the command
#set.seed(200) right before doing the clustering, we would expect:
##Answer: Identical results to the first k-means clustering.

#If we ran k-means clustering a second time, again running the command
#set.seed(100) right before doing the clustering, we would expect:
##Answer: Different results than the first k-means clustering because the seed
##is different.


#Problem 10 - The Number of Clusters


#Suppose it was decided that the 4 clusters were too general, and they wanted
#more specific clusters to describe the order behavior. Would they want to
#increase or decrease the number of clusters?
##Answer: increase


#Problem 11 - Describing the Clusters


#Let's now look at the other information available about each order 
#(day of the week, hour of the day, days since prior order) and see if they
#also differ by cluster, even though we did not use them as clustering
#variables.

#Which cluster has the latest average hour of the day?
tapply(orders$order_hour_of_day, kmc$cluster, mean)
##Answer: cluster4


#Problem 12 - Understanding Centroids


#Why do we typically use cluster centroids to describe the clusters?
##Answer: The cluster centroid captures the average behavior in the cluster,
##and can be used to summarize the general pattern in the cluster.


#Problem 13 - Using a Visualization


#Which visualizations could be used to observe the 
#distribution of days_since_prior_order, broken down by cluster?
##Answer: A box plot of the variable days_since_prior_order, subdivided by
##cluster; ggplot with the cluster number on the x-axis and 
##days_since_prior_order on the y-axis, cluster number as group, plotting with
##geom_boxplot() 

#Which cluster has the longest average days since prior order?
tapply(orders$days_since_prior_order, kmc$cluster, mean)

##Answer: cluster1
