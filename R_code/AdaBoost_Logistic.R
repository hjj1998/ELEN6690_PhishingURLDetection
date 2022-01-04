library(adabag)
library(RWeka)

set.seed(123)
dataset <- read.arff("G:\\files\\21fall\\6690\\pro\\PhishingData.arff")
dataset$Result[which(dataset$Result==-1)] <- 0

# separate data set in training set and test set
train.ind <- sort(sample(x = 1353, size = round(0.7*1353), replace = FALSE))
train.data <- dataset[train.ind,]
test.data <- dataset[-train.ind,]


# boosting
# train
start.time = Sys.time()
spam.adaboost <- boosting(Result~., data = train.data, mfinal = 100)
end.time = Sys.time()
print(end.time-start.time)

# training err
E <- errorevol(spam.adaboost,train.data)
plot(E$error,type="l",main="AdaBoost error vs iterations") 


# test
start.time = Sys.time()
spam.predboosting <- predict.boosting(spam.adaboost, newdata = test.data)
end.time = Sys.time()
print(end.time-start.time)
# error = 0.09359606

# confusion table
#   [[TN  FN
#     FP  TP]]

adaboost.table <- spam.predboosting[["confusion"]][1:2,1:2]

TN <- adaboost.table[1,1]
FN <- adaboost.table[1,2]
FP <- adaboost.table[2,1]
TP <- adaboost.table[2,2]


adaboost.precision = TP/(TP+FP) # 0.8764706
adaboost.recall = TP/(TP+FN) # 0.8975904
adaboost.F1 = 2*adaboost.precision*adaboost.recall/(adaboost.precision+adaboost.recall) # 0.8869048

# importance plot
importanceplot(spam.adaboost)


##################################################################

# Logistic Regression
# train
start.time = Sys.time()
fit <- glm(Result~.,train.data,family=binomial(link='logit'))
end.time = Sys.time()
print(end.time-start.time)



# test
start.time = Sys.time()
pred <- predict(fit,test.data)
end.time = Sys.time()
print(end.time-start.time)

pred <- pred > 0
pred[which(pred==TRUE)] <- 1
pred[which(pred==FALSE)] <- 0



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

logistic.precision = TP/(TP+FP) # 0.8757396
logistic.recall = TP/(TP+FN) # 0.8915663
logistic.F1 = 2*logistic.precision*logistic.recall/(logistic.precision+logistic.recall) # 0.8835821
