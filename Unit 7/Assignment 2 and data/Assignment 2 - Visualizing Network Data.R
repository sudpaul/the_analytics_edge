Sys.setlocale("LC_ALL", "C")


#Problem 1 - Summarizing the Data


#Load the data.
users = read.csv("users.csv")
edges = read.csv("edges.csv")

#How many Facebook users are there in our dataset?
nrow(users)
##Answer: 59

#What is the average number of friends per user?
nrow(edges)
##Answer: There are 146 edges (friendships) between 59 users. However, each edge
##contributes +2 to the total friendship count since there are two people whose
##friendship count is being increased by that edge. So the average is 
##2*146/59 = 4.949153

#Out of all the students who listed a school, what was the most common locale?
table(users$locale)
##Answer: B

#Is it possible that either school A or B is an all-girls or all-boys school?
table(users$school, users$gender)
##Answer: We see that in the row for students who went to A and B (AB) there is
##one student of gender A and one student of gender B, meaning there is one
##student of each gender who went to both schools. Therefore, neither can be
##all-boys or all-girls.


#Problem 2 - Creating a Network


#Based on ?graph.data.frame, how do we create a graph g describing our social
#network, with the attributes of each user correctly loaded?
library(igraph)
g = graph.data.frame(edges, FALSE, users)

#Plot with no text labels and smaller vertices.
plot(g, vertex.size=5, vertex.label=NA)

#How many connected components with at least 2 nodes are there in the graph?
##Answer: 4

#How many users are there with no friends in the network?
##Answer: Counting the vertices with degree 0, we find there are 7 such users.

#How many users are friends with 10 or more other Facebook users in this network?
sum(degree(g)>=10)
##Answer: 9

#Change the size of the vertices so the vertices with high degrees are larger.
V(g)$size = degree(g)/2 + 2

#Plot with no text labels.
plot(g, vertex.label=NA)

#What is the largest size we assigned to any node in our graph?
max(V(g)$size)
##Answer: 11

#What is the smallest size we assigned to any node in our graph?
min(V(g)$size)
##Answer: 2


#Problem 3 - Coloring Vertices


#We can update the colors by setting the color to black for all vertices, then
#setting it to red for the vertices with gender A and setting it to gray for the
#vertices with gender B.
V(g)$color = "black"
V(g)$color[V(g)$gender=="A"] = "red"
V(g)$color[V(g)$gender=="B"] = "gray"
plot(g, vertex.label=NA)

#Plot the resulting graph. What is the gender of the users with the highest 
#degree in the graph?
##Answer: The largest vertex is color gray, corresponding to gender "B".

#Color the vertices based on the school that each user in our network attended.
V(g)$color = "black"
V(g)$color[V(g)$school=="A"] = "red"
V(g)$color[V(g)$school=="AB"] = "yellow"
plot(g, vertex.label=NA)

#Are the two users who attended both schools A and B Facebook friends with each
#other?
##Answer: Yes, there is an edge joining the two yellow vertices.

#What best describes the users with highest degree?
##Answer: Some, but not all, of the high-degree users attended school A.

#Now, color the vertices based on the locale of the user.
V(g)$color = "black"
V(g)$color[V(g)$locale=="A"] = "red"
V(g)$color[V(g)$locale=="B"] = "yellow"
plot(g, vertex.label=NA)

#The large connected component is most associated with which locale?
##Answer: It is mostly yellow, so locale "B".

#The 4-user connected component is most associated with which locale?
##Answer: It is all red, so locale "A".


#Problem 4 - Other Plotting Options


#Which igraph plotting function would enable us to plot our graph in 3-D?
#?igraph.plotting
##Answer: rglplot


#What parameter to the plot() function would we use to change the edge width
#when plotting g?
##Answer: edge.width












