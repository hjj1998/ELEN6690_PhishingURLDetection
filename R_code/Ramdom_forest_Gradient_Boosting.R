library(foreign)
data = read.arff("PhishingData.arff")

#install.packages("randomForest")
#install.packages('caTools')
#install.packages("caret")
#install.packages("gbm")
library(randomForest)
library(caTools)
library(caret)
library(gbm)

data$Result[which(data$Result==-1)] <- 0
sample = sample.split(data$Result, SplitRatio = .7)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)

train <- droplevels(train)
test <- droplevels(test)
set.seed(123)

######################################  Random Forest  ###########################

mtry <- tuneRF(train[0:9],train$Result, ntreeTry=100,
               stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

start.time = Sys.time()
rf <-randomForest(train$Result ~ ., data=train, mtry=best.m, importance=TRUE, ntree=100)
end.time = Sys.time()
print(end.time-start.time)

print(rf)

start.time = Sys.time()
pred = predict(rf, test[-10])
end.time = Sys.time()
print(end.time-start.time)

confusionMatrix(pred, test$Result)
cm = confusionMatrix(pred, test$Result)['table'][[1]]
TN <- cm[1,1]
FN <- cm[1,2]
FP <- cm[2,1]
TP <- cm[2,2]

precision_rf = TP/(TP+FP)
recall_rf = TP/(TP+FN)
F1_rf = 2*precision_rf*recall_rf/(precision_rf+recall_rf)

######################################  Gradient Boosting  ###########################
start.time = Sys.time()
gbm1 <- gbm(as.character(train$Result) ~ .,
            data = train,
            distribution = "bernoulli",
            cv.folds=5,
            n.trees = 500)  
end.time = Sys.time()
print(end.time-start.time)

summary(gbm1)
n_tree = gbm.perf(gbm1, method = "OOB")

start.time = Sys.time()
pred1 <- predict.gbm(gbm1 , 
                     newdata=test,
                     n.trees = n_tree,
                     type="response")
end.time = Sys.time()
print(end.time-start.time)

pred2 = as.factor(ifelse(pred1>0.5, 1,0))
confusionMatrix(pred2, test$Result)
cm1 = confusionMatrix(pred2, test$Result)['table'][[1]]
TN <- cm1[1,1]
FN <- cm1[1,2]
FP <- cm1[2,1]
TP <- cm1[2,2]
precision = TP/(TP+FP)
recall = TP/(TP+FN)
F1 = 2*precision*recall/(precision+recall)
