library(xgboost)
library(foreign)


set.seed(123)
dataset <- read.arff("C:\\Users\\11705\\OneDrive\\??????\\6690 Final 12.14&12.16\\PhishingData.arff")
dataset$Result[which(dataset$Result==-1)] <- 0

# separate data set in training set and test set
train.ind <- sort(sample(x = 1353, size = round(0.7*1353), replace = FALSE))
train.data <- dataset[train.ind,]
test.data <- dataset[-train.ind,]
dtrain = as.matrix(sapply(train.data, function(train.data){train.data=as.numeric(train.data)-2}))
dtest = as.matrix(sapply(test.data, function(train.data){train.data=as.numeric(train.data)-2}))
input_train = dtrain[,1:9]
input_test = dtest[,1:9]
#train
start.time = Sys.time()
bst <- xgboost(data = input_train, label=dtrain[,10], max_depth = 10, eta = 1, nrounds = 10,
               nthread = 2, objective = "binary:logistic")
end.time = Sys.time()
print(end.time-start.time)

#test
start.time = Sys.time()
pred <- predict(bst, input_test)
end.time = Sys.time()
print(end.time-start.time)

prediction <- as.numeric(pred > 0.5)


# confusion matrix
P_P = 0
L_P = 0
P_L = 0
L_L = 0
for (i in 1:406) {
  if (prediction[i] == 1 & test.data$Result[i] == 1) # P_P
    P_P = P_P + 1
  else if (prediction[i] == 1 & test.data$Result[i] == 0) # L_P
    L_P = L_P + 1
  else if (prediction[i] == 0 & test.data$Result[i] == 1) # P_L
    P_L = P_L + 1
  else if (prediction[i] == 0 & test.data$Result[i] == 0) # L_L
    L_L = L_L + 1
}

confusion_matrix = matrix(0,2,2)
confusion_matrix[1,1] <- L_L 
confusion_matrix[1,2] <- P_L 
confusion_matrix[2,1] <- L_P 
confusion_matrix[2,2] <- P_P

acc = (P_P+L_L)/(P_P+L_L+P_L+L_P) # 0.903940886699507
precision = P_P/(P_P+L_P) # 0.875739644970414
recall = P_P/(P_P+P_L) # 0.891566265060241
F1 = 2*precision*recall/(precision+recall) #0.883582089552239