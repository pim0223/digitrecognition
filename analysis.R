setwd("C:/Users/DELL/uni/pr/digitrecognition")

library(nnet)
library(glmnet)
library(class)

if(!exists("mnist")){
  # Load in data
  mnist   = read.csv("mnist.csv")
  mnist[,1]   = as.factor(mnist[,1])
}

samplerows <- sample(nrow(mnist), 1000)
train <- mnist[samplerows,]
test <- mnist[-samplerows,]

reducedf <- function(df, g){
  df = df[,-1]
  for(i in 1:(784/g)){
    new_col = matrix(rep(0, 42000), nrow = 42000)
    for(j in 1:g){
      new_col = new_col + df[,(i-1)*g + j]
    }
    df[,i] = new_col
  }
  df = df[,1:(784/g)]
  df = cbind("label" = mnist[,1], df)
  return(df)
}

# LASSO
time1 = proc.time()
lasso.cv <- cv.glmnet(as.matrix(train[,-1]), train[,1], family="multinomial", type.measure="class")
plot(lasso.cv)
lasso.pred <- predict(lasso.cv, as.matrix(test[,-1]), type="class")
lasso.confmat <- table(test[,1], lasso.pred)
lasso.accuracy = vector(length = 10)
for(i in 1:10){
  lasso.accuracy[i] = lasso.confmat[i,i] / sum(lasso.confmat[i,])
}
print(lasso.accuracy)
print(sum(diag(lasso.confmat))/sum(lasso.confmat))
proc.time() - time1

# KNN
for(i in 1:10){
  knn.cv1 <- knn.cv(train[,c((2:785),787)], train[,1], k = i)
  knn.confmat <- table(train[,1], knn.cv1)
  print(sum(diag(knn.confmat))/sum(knn.confmat))
}

knn.pred <- knn(train, test, train[,1], k = 1)
lasso.confmat <- table(test[,1], lasso.pred)
lasso.accuracy = vector(length = 10)
for(i in 1:10){
  lasso.accuracy[i] = lasso.confmat[i,i] / sum(lasso.confmat[i,])
}
print(lasso.accuracy)
print(sum(diag(lasso.confmat))/sum(lasso.confmat))
proc.time() - time1


# knn.cv.pred <- predict(knn.cv, as.matrix(test[,-1]), type="class")

