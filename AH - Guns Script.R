#Import dataset with each variable being averaged out per state. This data preperations was done on Excel
str(gunsAverage)
rownames(gunsAverage)

#Remove 'year' and 'law' labels
gunsAverage = gunsAverage[,-1]
gunsAverage = gunsAverage[,-12]


#Set state values to rownames
rownames(gunsAverage) = gunsAverage[,11]
gunsAverage = gunsAverage[,-11]

#Find Euclidean distance
d = dist(gunsAverage, method = "euclidean")
d

#Normalize the data
data.norm = sapply(gunsAverage, scale)
data.norm

rownames(data.norm) = rownames(gunsAverage)
data.norm


#Find the euclidean distances within the normalized gunsAverage dataset
d.norm = dist(data.norm, method = "euclidean")         
d.norm

#Cluster the gunsAverage dataset based on variables such as 'violent' and 'population'
d.norm = dist(data.norm[,c(1,8)], method = "euclidean")         
d.norm

#Now that I have the distance between states based on violence and population size, I will make 4 clusters through k-means clustering
km = kmeans(data.norm, 4)
km$cluster
km$centers
km$withinss
km$size

plot(c(0), xaxt = 'n', ylab ="", type = "l", ylim =c(min(km$centers), max(km$centers)), xlim = c(0,10))
# Revisit K-means

#Agglomerative Hierarchical clustering (Bottom Up)

#Used to find preferred linkage method
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

ac <- function(x){
  agnes(data.norm[,c(1,8)], method= x) $ac
}

install.packages("purrr")
library(purrr)

install.packages("cluster")
library(cluster)

map_dbl(m,ac)
