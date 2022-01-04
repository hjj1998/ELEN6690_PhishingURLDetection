library(foreign)
library(e1071)

rm(list =ls())

set.seed(123)
dataset = read.arff("PhishingData.arff")
dataset$Result[which(dataset$Result==-1)] <- 0

# separate data set in training set and test set
train.ind <- sort(sample(x = 1353, size = round(0.7*1353), replace = FALSE))
train.data <- dataset[train.ind,]
test.data <- dataset[-train.ind,]


# cross-validation model selection
set.seed (123)
tune.out=tune(svm , Result???., data=train.data, kernel ="polynomial",
              ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000),
                           degree=c(1,2,3,4,5) ))
summary(tune.out)

# train linear
start.time = Sys.time()
svmfit = svm (Result~., data = train.data, kernel = "polynomial", cost = 100, degree = 3, scale = TRUE)
end.time = Sys.time()
print(end.time-start.time)
summary(svmfit)

# test
start.time = Sys.time()
pred <- predict(svmfit,test.data)
end.time = Sys.time()
print(end.time-start.time)
pred = as.numeric(pred) -2


# confusion table
TP = 0
FP = 0
FN = 0
TN = 0
for (i in 1:406) {
  if (pred[i] == 1 & test.data$Result[i] == 1) # TP
    TP = TP + 1
  else if (pred[i] == 1 & test.data$Result[i] == 0) # FP
    FP = FP + 1
  else if (pred[i] == 0 & test.data$Result[i] == 1) # FN
    FN = FN + 1
  else if (pred[i] == 0 & test.data$Result[i] == 0) # TN
    TN = TN + 1
}

logistic.table = matrix(0,2,2)
logistic.table[1,1] <- TN 
logistic.table[1,2] <- FN 
logistic.table[2,1] <- FP 
logistic.table[2,2] <- TP

logistic.accuracy = (TP+TN)/(TP+FP+FN+TN)
logistic.precision = TP/(TP+FP) 
logistic.recall = TP/(TP+FN) 
logistic.F1 = 2*logistic.precision*logistic.recall/(logistic.precision+logistic.recall)

print(logistic.accuracy)
print(logistic.precision)
print(logistic.recall)
print(logistic.F1)
