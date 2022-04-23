#Fix a multiple regression model with all possible independent variables in your dataset and display output. What can you interpret from the regression equation giv

linearMod <- lm(violent ~ law + year + murder + robbery + prisoners + afam + cauc + male + population + income + density + state, data=guns)
summary(linearMod)

#Model Adjusted for significant variables 

linearMod2 <- lm(violent ~ murder + robbery + prisoners + male + population + state + law, data=guns)
summary(linearMod2)

#Violence rates (Box-Plot)
library(ggplot2)
ggplot(data = guns) +
  aes(x = year,y = violent, color = state) +
  geom_boxplot()

#Violence rates (Dots)

ggplot(guns) + ggtitle("Violence Rates Per State Throughout The Years") +
  aes(x = year, y = violent ,colour = state) +
  geom_point() +
  scale_color_hue()

#Violence rates over the years per state
library("lattice")
xyplot(log(violent) ~ (year)| state, data = guns, type = "l")

#Summary of unit of central tendency amongs Murders, Roberries, and Violence
summary(guns$violent)
summary(guns$murder)
summary(guns$robbery)

#NAIVE 
install.packages("e1071")
library(e1071)

#Train-Test Split Procedure
guns1train <- subset(guns,select=c(4,5,6,9,10,14))
traing <- guns1train[1:940,]
testg <- guns1train[941:1173,]

#Build Naive Bayes classifier 
NBmodel = naiveBayes(law ~ ., data = traing)
NBmodel

#Predict Probability using testing dataset
pred.prob = predict(NBmodel, testg, type = "raw")
pred.prob

#Predicting the class of test data flights 
pred.class = predict(NBmodel, testg)
pred.class

View(testg)

#Asses accuracy 
newdata = data.frame(actual = testg$law, predicted = pred.class, pred.prob)
newdata

#library for confusion matrix
library(caret)

#Confusion Matrix TP,TN,FP,FN
confusionMatrix(testg$law, pred.class)


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