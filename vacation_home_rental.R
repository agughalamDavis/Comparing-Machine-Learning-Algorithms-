#install packages
install.packages('MASS')
install.packages('caTools')
install.packages('data.table')
install.packages('mice')
install.packages('tree')
install.packages('randomForest')
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)
library(Amelia)
library(caTools)
library(MASS)
library(dplyr)
library(mice)
library(tree)
library(randomForest)
library(ggplot2)


#set working directory and import file
setwd(choose.dir())
rentals <- read.csv("rentals.csv")


#data processing
#drop unwanted columns
View(rentals)
rentals <- rentals[,-c(1,2,3,4,5,6,7,8,9,10,11,12,17,19,20,21,25,26)]
View(rentals)

#change datatypes
rentals$numBathroom <- as.integer(rentals$numBathroom)
rentals$numPeople <- as.integer(rentals$numPeople)

#strip strings from numbers
rentals$prices.minstay <- gsub("[a-zA-Z ]", "", rentals$prices.minStay)
rentals$prices.price <- gsub("[a-zA-Z ]", "", rentals$prices.price)
rentals <- rentals[,-c(5,6)]

#convert to integer
rentals$prices.price <- as.integer(rentals$prices.price)
rentals$prices.minstay<-as.integer(rentals$prices.minstay)
str(rentals)


#view missing values
#count missing values in each column
sapply(rentals,function(x) sum(is.na(x)))


#graphically view missing values
#Visual representation of missing data
missmap(rentals, main = "Missing values vs observed")

#drop number of beds column as more than 50 percent of the data is missing
rentals <- rentals[,-3]

#drop rows with missing values as they are very little.
rentals <- rentals[!is.na(rentals$prices.minstay), ]
rentals <- rentals[!is.na(rentals$prices.price), ]

#data exploration
#view data distribution
# Histogram with density plot
ggplot(rentals, aes(x= prices.price)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 


#logtransform for better distribution
rentals$log_price <- log(rentals$prices.price)
# Histogram with density plot
ggplot(rentals, aes(x= log_price)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 


#As the data is free from missing values, divide into test and train data
#use caTools to split into train and test subsets
set.seed(111)
rentals_subset <- sample.split(rentals, SplitRatio = 0.70, group=NULL)

#subsetting into Train data
rentals_train = subset(rentals,rentals_subset==TRUE)
train <- rentals_train[,-5]

#subsetting into Test data
rentals_test = subset(rentals,rentals_subset==FALSE)
rentals_tests <- rentals_test[,-7]
test <- rentals_tests[,-5]

#check correlation
library(ggcorrplot)
ggcorrplot(cor(rentals[,-4]))


#Multiple Linear regression
price_model <- lm(log_price ~ . -prices.price, data = rentals_train)
summary(price_model)
price_pred =predict(price_model ,newdata=rentals_tests)
mean((price_pred - rentals_test$log_price)^2)

#number of people did not make a significant contribution to the model. Run the model without it, add
sec_price_model <- lm(log_price ~ . -prices.price -numPeople, data = rentals_train)
summary(sec_price_model)
price_pred =predict(sec_price_model ,newdata=rentals_test)
l_MSE <- (mean((price_pred - rentals_test$log_price)^2))
l_MAE <- (mean(abs(price_pred - rentals_test$log_price)))

#decision tree regression
#tree.prices <- tree(log_price ~. -prices.period -prices.price, data = rentals_train)
#summary(tree.prices)
tree.rpart <- rpart(log_price~.  -prices.period -prices.price, data=rentals_train)
rpart.plot(tree.rpart)
price_predict =predict(tree.rpart,newdata=rentals_tests)
tree_MSE <- mean((price_predict - rentals_test$log_price)^2)
tree_MAE <- mean(abs(price_predict - rentals_test$log_price))


#random forest
set.seed(1)
bag.rentals =randomForest(log_price~.,data= train, mtry=5, importance =TRUE)
bag.rentals
pred.bag = predict(bag.rentals ,newdata = test)
ggplot( data = rentals_test, aes(x=pred.bag, y=log_price)) + geom_point() + geom_smooth(method=lm, se=FALSE)

r_MSE <- mean(( pred.bag - rentals_test$log_price)^2)
r_MAE <- mean(abs( pred.bag - rentals_test$log_price))


#comparing metrics
metrics <- data.frame(method =rep(c("Linear Regression", "Decision_Tree","Bagged_Random_Forest"), each=3),
                  metric= rep(c("MSE",'MAE', 'RMSE'),3),
                  Values = c(l_MSE, l_MAE,sqrt(l_MSE),tree_MSE,tree_MAE, sqrt(tree_MSE),r_MSE,r_MAE,sqrt(r_MSE)))

#bar plot comparing metrics
# Use position=position_dodge()
ggplot(data=metrics, aes(x=metric, y=Values, fill=method)) +
  geom_bar(stat="identity", position=position_dodge())
