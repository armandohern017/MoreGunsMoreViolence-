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




#Logistics Regression

#Find the missing Values 
# sum(is.na(x)) is the function gives the missing values in each of the columns
sapply(guns, function(x) sum(is.na(x)))

#length(unique(x)) function gives the number of unique values in each of the columns
sapply(guns, function(x) length(unique(x)))

#Load the library into the memory of Rstudio
library(Amelia)

#missmap draws a map of the missingness in a dataset using the image function
missmap(guns, main = "Missing values vs observed")

#murder + robbery + prisoners + male + population + state + law
# Choose the columns that you want to train your model on
#murder, robbery, prisioners, male, population, state, law
guns1train <- subset(guns,select=c(4,5,6,9,10,14))

# Check if law is a factor
is.factor(guns$law)

# Check if Embarked is a factor
is.factor(guns$male)

#view number of classes in sex and embarked
contrasts(guns$law)
contrasts(guns$state)

# Divide the data into training and testing data
rownames(guns) <- NULL
traing <- guns1train[1:940,]
testg <- guns1train[941:1173,]

#Build Regression Model
model <- glm(law~.,family=binomial(link='logit'),data=traing)
summary(model)

#the function anova compares the following models in sequential order.
anova(model, test="Chisq")

#select independent variable rows from test dataset without the DV (Dependent Variable)
newdata=subset(testg,select=c(1,2,3,4,5))

#predict the test dataset values using the built model
predicted <- predict(model,newdata,type='response')
predicted

#Round the predicted values
predicted.round <- ifelse(predicted > 0.5,1,0)
predicted.round

#view model estimates or coeficients
summary(model)$coef

#find Odd ratios i.e., exponential estimates or coeficients
exp(coefficients(model))

#install package
install.packages("InformationValue")

#load package
library(InformationValue)

#Calculate Misclassification error
misClasificError <- mean(predicted.round != testg$law)

#Print Accuracy of your model
print(paste('Accuracy',1-misClasificError))

#Display confusion matrix
confusionMatrix(test$law, predicted.round)

#plot ROC curve
plotROC(test$law, predicted)




#