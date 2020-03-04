install.packages('caTools')
install.packages('data.table')
install.packages('mlbench')
install.packages('randomForest')
install.packages('caret')
install.packages('class')
install.packages('tree')
install.packages('ggplot2')
install.packages('ipred')
library(ipred)
library(class)
library(randomForest)
library(tree)
library(Amelia)
library(caTools)
library(readxl)
library(caret)

#import dataset
default_of_credit_card_clients <- read_excel("default of credit card clients.xls")
credit_default <- default_of_credit_card_clients

View(credit_default)

#rename for better visualizations
credit_default$AGE<-cut(credit_default$AGE, breaks = c( 10,30,50,100), labels = c("youth", "middle-aged","elderly"))
credit_default$SEX<-cut(credit_default$SEX, 2,labels = c("F","M"))
credit_default$MARRIAGE<-cut(credit_default$MARRIAGE, 4,labels = c("Married","Single","Divorce","Other"))


#convert integers to factors
credit_default$SEX <- as.factor(credit_default$SEX)
credit_default$EDUCATION <- as.factor(credit_default$EDUCATION)
credit_default$MARRIAGE <- as.factor(credit_default$MARRIAGE)
credit_default$PAY_0 <- as.factor(credit_default$PAY_0)
credit_default$PAY_2 <- as.factor(credit_default$PAY_2)
credit_default$PAY_3 <- as.factor(credit_default$PAY_3)
credit_default$PAY_4 <- as.factor(credit_default$PAY_4)
credit_default$PAY_5 <- as.factor(credit_default$PAY_5)
credit_default$PAY_6 <- as.factor(credit_default$PAY_6)
credit_default$default <- as.factor(credit_default$`default payment next month`)
credit_default <- credit_default[,-c(1,25)]

# ggplot boxplot for age ,limit bal and default
ggplot(data=credit_default,mapping = aes(x=AGE,y=credit_default$LIMIT_BAL,fill=default)) + geom_boxplot() 

# bar plot
ggplot(credit_default, mapping = aes(x=MARRIAGE, fill=default)) + geom_bar()


#check for missing values
sapply(credit_default,function(x) sum(is.na(x)))


#Visual representation of missing data
missmap(credit_default, main = "Missing values vs observed")

#plot data imbalance
ggplot(data = credit_default) +
  geom_bar(mapping = aes(x = default))


View(credit_default)

#feature selection
#variable importance
fit = randomForest(default~., data=credit_default)
varImpPlot(fit,type=2)

knn_data <- credit_default[,c(6,12,13,14,15,17,24)]
knn_data$PAY_0 <- as.numeric(knn_data$PAY_0)
str(knn_data)

#function to normalize data
normalize <- function(x){
  return ((x-min(x)) / (max(x) - min(x)))
}


#split data
set.seed(111)
split = sample.split(knn_data, SplitRatio = 0.75)

#train data
knn_train = subset(knn_data, split == TRUE)

#upsample
set.seed(100)
'%ni%' <- Negate('%in%')
up_train <- upSample(x = knn_train[, colnames(knn_train) %ni% "default"],
                     y = knn_train$default)

#down sample
set.seed(100)
'%ni%' <- Negate('%in%')
down_train <- downSample(x = knn_train[, colnames(knn_train) %ni% "default"],
                         y = knn_train$default)

#test data
knn_test = subset(knn_data, split == FALSE)



#use KNN to run knn for up sampled data 
train_normalized <- as.data.frame(lapply(up_train[,-7], normalize))
test_normalized <- as.data.frame(lapply(knn_test[,-7], normalize))
k_mod <- knn(train = train_normalized, test = test_normalized, cl = up_train$Class , k=159, prob=TRUE)


#check confusion matrix
k_conf <- confusionMatrix(k_mod, knn_test$default, positive = '1')


#use KNN to run knn for down sampled data using caret and CV
set.seed(123)
down_model <- train(
  Class ~., data = down_train, method = "knn",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 200
)

#use model to predict 
knn_predictions <- predict(down_model, knn_test[,-7])

#check confusion matrix
down_conf <- confusionMatrix(knn_test$default, knn_predictions, positive = '1')

#decision tree with all the variables
#split data for decision tree
set.seed(123)
split = sample.split(credit_default, SplitRatio = 0.75)

#train data
credit_train = subset(credit_default, split == TRUE)

#upsample
set.seed(100)
'%ni%' <- Negate('%in%')
upsample_train <- upSample(x = credit_train[, colnames(credit_train) %ni% "default"],
                           y = credit_train$default)

#test data
credit_test = subset(credit_default, split == FALSE)
View(credit_test)

#build decision tree
tree.default =tree(Class~., upsample_train)
summary(tree.default)
plot(tree.default)
text(tree.default, pretty = 0)
tree.default
tree.pred =predict(tree.default,credit_test[,-24],type ="class")


#check confusion matrix
confusionMatrix(tree.pred, credit_test$default, positive = '1')

#prune tree
cv.default =cv.tree(tree.default ,FUN=prune.misclass)
names(cv.default )
cv.default
par(mfrow =c(1,2))
plot(cv.default$size ,cv.default$dev ,type="b")
plot(cv.default$k ,cv.default$dev ,type="b")

#decision tree with only the variables used for KNN
tree.data =tree(Class~., up_train)
summary(tree.data)
plot(tree.data)
text(tree.data, pretty = 0)
tree.data
tree.data_pred =predict(tree.data,knn_test[,-7],type ="class")


#confusion matrix
confusionMatrix(tree.data_pred, knn_test$default, positive = '1')


#bagged decision tree
dec_bag <- bagging(Class~., data= up_train, nbagg=25)

#predict with bagged decision tree
dec_bag_pred <- predict(dec_bag, knn_test[,-7], type="class")

#confusion matrix
confusionMatrix(dec_bag_pred, knn_test$default, positive = "1")
