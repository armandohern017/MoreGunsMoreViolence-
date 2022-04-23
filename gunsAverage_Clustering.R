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


#Cluster the gunsAverage dataset based on variables such as 'violent' and 'population'
d.norm = dist(data.norm[,c(1,8)], method = "euclidean")         
d.norm

#Agglomerative Hierarchical clustering (Bottom Up)

#Used to find preferred linkage method
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

ac <- function(x){
  agnes(data.norm, method= x) $ac
}

install.packages("purrr")
library(purrr)

install.packages("cluster")
library(cluster)

map_dbl(m,ac)
#Since the Ward's variance method having the highest coefficient, this is the best method to use to compare clusters. 

Ag_hc1 = agnes(d.norm, method = "ward")
pltree(Ag_hc1, cex = 0.6, hang = -1, main = "Dendrogram of agnes - State Clustering")


#Divisive Hierarchical clustering (Top Down)
Di_hc2 <- diana(data.norm)

#Finds the coefficient of Divisive. The one with the higher coefficient is the better cluster. In this case, it's the Agglomerative
Di_hc2$dc
Ag_hc1$ac

cutree(as.hclust(Di_hc2), k = 4)
pltree(Di_hc2, cex = 0.6, hang = -1, main = "Dendrogram of diana - State Clustering")

install.packages("factoextra")
library(factoextra)

fviz_nbclust(df, FUN = hcut, method = "wss")

HC <- hclust (d.norm, method = "ward.D2")
HC          

clusternum = cutree(HC, k=4)
clusternum
table(clusternum)

plot(HC, cex = 0.6, hang = -1, main = "Dendrogram of HC - State Clustering") 

rect.hclust(HC, k = 4, border = 2:5)

abline(h = 2, col = 'red')


fviz_cluster(list(data = gunsAverage, cluster = clusternum),
             choose.vars = c(8,1),
             show.clust.cent = FALSE,
             main = "HC Clustering - Ward's Method") +
             theme(legend.position = "none")

