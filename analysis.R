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



reducedf = function(df, g){
  df = df[,-1]
  # Inputs are data-frame and desired granularity
  # Flatten (horizontally)
  for(i in 1:(784/g)){
    new_col = matrix(rep(0, 42000), nrow = 42000)
    for(j in 1:g){
      new_col = new_col + df[,(i-1)*g + j]
    }
    df[,i] = new_col
  }
  
  # Flatten (vertically)
  for(i in 1:(784/g^2)){
    new_col = matrix(rep(0, 42000), nrow = 42000)
    for(j in 1:g){
      new_col = new_col + df[,i + (i%/%28)*28 + (j-1)*28]
    }
    df[,i] = new_col 
  }
  
  df = df[,1:(784/g^2)]
  df[,i] = df[,i]/g^2
  
  df = cbind(label = mnist[,1], df)
  return(df)
}

# mnist49 = reducedf(mnist, 4)
# samplerows <- sample(nrow(mnist49), 1000)
# train <- mnist49[samplerows,]
# test <- mnist49[-samplerows,]


# LASSO
# lasso <- glmnet(as.matrix(train[,-1]), train[,1], family="multinomial")
# lasso.pred <- predict(lasso, as.matrix(test[,-1]), type="class")
# lasso.cv <- cv.glmnet(x = as.matrix(train[,-1]), y = train[,1], family="multinomial", type.measure="class")
# plot(lasso.cv)
# lasso.cv.pred <- predict(lasso.cv, as.matrix(test[,-1]), type="class")
# CalculateAccuracy(lasso.cv.pred)

for(i in 1:5){
  # In sample (determining k)
  knn.cv = knn.cv(train[-1], train[,1], k = i)
  confmat = table(train[,1], knn.cv)
  pred.train = sum(diag(confmat))/sum(confmat)
  
  # Out of sample
#   knn.pred = knn(train, test, train[,1], k = i)
#   confmat2 = table(test[,1], knn.pred)
#   pred.test = sum(diag(confmat2))/sum(confmat2)
  
  msg = paste("Prediction in sample: ", toString(pred.train))
          # "Prediction out of sample: ", toString(pred.test))
  
  print(msg)
}



# # Calculate accuracy of prediction, using confusion matrix
# CalculateAccuracy <- function(pred){
#   confmat <- table(test[,1], pred)
#   print(sum(diag(confmat))/sum(confmat))
# }