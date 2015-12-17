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
  knn.train.confmat <- table(train[,1], knn.cv1)
  print(sum(diag(knn.train.confmat))/sum(knn.train.confmat))
}

time1 = proc.time()
knn.pred <- knn(train, test, train[,1], k = 1)
knn.confmat <- table(test[,1], knn.pred)
knn.accuracy = vector(length = 10)
for(i in 1:10){
  knn.accuracy[i] = knn.confmat[i,i] / sum(knn.confmat[i,])
}
print(knn.accuracy)
print(sum(diag(knn.confmat))/sum(knn.confmat))
proc.time() - time1


# knn.cv.pred <- predict(knn.cv, as.matrix(test[,-1]), type="class")

# SVM

#svm preprocess. Take out columns with 0's.
#pixels only
svm.train <- train[,c(1:785)]
svm.train <- svm.train[, c(1, colSums(train[,2:785]) != 0)]

#pixels and passes
svm.train <- train[,c(1:785,790:845)]
svm.train <- svm.train[, c(1,colSums(svm.train[,2:841]) != 0)]

#svm.cv <- svm(label ~ ., data = train[,c(1,791:817,819:844)], cross = 10) #left out rows with always 0
svm.cv <- svm(label ~ ., data = svm.train, cross = 10, gamma = 0.5, cost = 100) #left out rows with always 0
svm.pred <- predict(svm.cv, test[,-1])
svm.confmat <- table(test[,1], svm.pred)
svm.accuracy <- vector(length = 10)
for(i in 1:10)
{
  svm.accuracy[i] = svm.confmat[i,i] / sum(svm.confmat[i,])
}
print(svm.accuracy)
print(sum(diag(svm.confmat))/sum(svm.confmat))

#with tuning
svm.tuning <- tune.svm(label ~ ., data = svm.train, cross = 10, gamma = 2^(10), cost = 2^(10))


