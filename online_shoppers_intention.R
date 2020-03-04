install.packages('caret')
library(caret)
install.packages('e1071')
library(e1071)
install.packages('kernlab')
library(kernlab)
install.packages('gmodels')
library(gmodels)
install.packages('caTools')
library(caTools)
install.packages('randomForest')
library(randomForest)
install.packages('Amelia')
library(Amelia)
install.packages('pROC')
library(pROC)

#import and view dataset
shoppers <- read.csv('online_shoppers_intention.csv')
str(shoppers)
summary(shoppers)

#convert to variables to factors
shoppers$OperatingSystems <- as.factor(shoppers$OperatingSystems)
shoppers$Browser <- as.factor(shoppers$Browser)
shoppers$Region <- as.factor(shoppers$Region)
shoppers$TrafficType <- as.factor(shoppers$TrafficType)
shoppers$Weekend <- as.factor(shoppers$Weekend)
shoppers$Revenue <- as.factor(shoppers$Revenue)


#dummy variables through one hot encoding for categorical data
dummy <- dummyVars(" ~ .", data = shoppers[,-18])
shoppers_data  <- data.frame(predict(dummy, newdata = shoppers[,-18]))

#add dependent variable back to dataset
shoppers_data$Revenue <- shoppers[,18]


#convert depedent variable to 1 and 0
shoppers_data$Revenue = factor(shoppers_data$Revenue, 
                               levels = c("TRUE", "FALSE"),
                               labels = c(1,0),
                               ordered = FALSE)

#select best predictors
#variable importance
fit=randomForest(Revenue~., data=shoppers_data)
varImpPlot(fit,type=2)


#final dataset with best 12 predictors
final_shoppers <- shoppers_data[,c('PageValues','ProductRelated_Duration','ExitRates','ProductRelated',
                                   'Administrative_Duration','BounceRates','Administrative','Informational_Duration',
                                   'Month.Nov','Informational','TrafficType.2','Region.1','Revenue')]

#explore dataset
#plot data imbalance
ggplot(data = final_shoppers) +
  geom_bar(mapping = aes(x = Revenue))

ggplot(shoppers, mapping = aes(x=Month, fill=Revenue)) + geom_bar()

ggplot(data=shoppers,mapping = aes(x=VisitorType,y=shoppers$ExitRates,fill=Weekend)) + geom_boxplot() 

#check for missing values
sapply(final_shoppers,function(x) sum(is.na(x)))


#Visual representation of missing data
missmap(final_shoppers, main = "Missing values vs observed")

#split data into test and train
set.seed(123)
shoppers_subset <- sample.split(final_shoppers, SplitRatio = 0.70, group=NULL)

#subset into train
shoppers_train <- subset(final_shoppers, shoppers_subset==TRUE)

#upsample
set.seed(100)
'%ni%' <- Negate('%in%')
shoppers_up_train <- upSample(x = shoppers_train[, colnames(shoppers_train) %ni% "Revenue"],
                              y = shoppers_train$Revenue)
#View new classes
table(shoppers_up_train$Class)


#subset into test
shoppers_test <- subset(final_shoppers, shoppers_subset==FALSE)

#remove dependent variable from test dataset
shoppers_t <- shoppers_test[,-13]
View(shoppers_t)

#use caret for automated parameter tuning for Linear kernel SVM
svm_model <- train(Class~., data = shoppers_up_train, method='svmLinear')

#predict with SVMLinear
shoppers_prediction <- predict(svm_model, shoppers_t)

#Svm Linear Confusion matrix
svm_linear_matrix <- confusionMatrix(shoppers_prediction, shoppers_test$Revenue)

#SVM linear ROC curve
sl_roc <- roc(shoppers_prediction, as.numeric(shoppers_test$Revenue))
plot(sl_roc)
auc(sl_roc)


#use caret for automated parameter tuning for Radial basis kernel SVM
svm_radial_model <- train(Class~., data = shoppers_up_train, method='svmRadial')


#predict with SVMRadial
shoppers_radial_prediction <- predict(svm_radial_model, shoppers_t)

#Svm Radial Confusion matrix
svm_radial_matrix <- confusionMatrix(shoppers_radial_prediction, shoppers_test$Revenue)

#SVM radial ROC curve
sr_roc <- roc(shoppers_radial_prediction, as.numeric(shoppers_test$Revenue))
plot(sr_roc)
auc(sr_roc)

#Logistic regression
logistic_model <- glm(Class~., data=shoppers_up_train, family = 'binomial')


#predict with logistic model
logistic_pred <- predict(logistic_model, newdata = shoppers_t, type = "response")


#recode predictions
y_pred_num <- ifelse(logistic_pred > 0.5, 0, 1)
y_pred <- factor(y_pred_num, levels=c(1, 0))

#logistic regression confusion matrix
logistic_conf <- confusionMatrix(y_pred,shoppers_test$Revenue)


#logistic regression ROC curve
logr_roc <- roc(y_pred,as.numeric(shoppers_test$Revenue))
plot(logr_roc)
auc(logr_roc)
