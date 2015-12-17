setwd("C:/Users/DELL/uni/pr/digitrecognition")

library(nnet)
library(glmnet)
library(class)

# Load in data
if(!exists("mnist")){
  mnist   = read.csv("mnist.csv")
  mnist[,1] = as.factor(mnist[,1])
  samplerows <- sample(nrow(mnist), 1000)
  train <- mnist[samplerows,]
  test <- mnist[-samplerows,]
}

# LASSO
# lasso <- glmnet(as.matrix(train[,-1]), train[,1], family="multinomial")
# lasso.pred <- predict(lasso, as.matrix(test[,-1]), type="class")
lasso.cv <- cv.glmnet(as.matrix(train[,-1]), train[,1], family="multinomial", type.measure="class")
# plot(lasso.cv)
lasso.cv.pred <- predict(lasso.cv, as.matrix(test[,-1]), type="class")

# KNN
knn.pred <- knn(train, test, train[,1], k = 1)
# knn.cv <- knn.cv(train[-1], train[,1], k = 1)
# knn.cv.pred <- predict(knn.cv, as.matrix(test[,-1]), type="class")

# Calculate accuracy of prediction, using confusion matrix
CalculateAccuracy <- function(pred){
  confmat <- table(test[,1], pred)
  paste("Accurracy of ", deparse(substitute(pred)),":", sum(diag(confmat))/sum(confmat))
}

# CalculateAccuracy(lasso.pred)
CalculateAccuracy(lasso.cv.pred)
CalculateAccuracy(knn.pred)
# CalculateAccuracy(knn.cv.pred)