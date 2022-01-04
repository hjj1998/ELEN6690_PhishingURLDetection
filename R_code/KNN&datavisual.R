library(foreign)
library(kknn)
library(pROC)

rm(list =ls())

set.seed(123)
dataset = read.arff("PhishingData.arff")
dataset$Result[which(dataset$Result==-1)] <- 0

# separate data set in training set and test set
train.ind <- sort(sample(x = 1353, size = round(0.7*1353), replace = FALSE))
train.data <- dataset[train.ind,]
test.data <- dataset[-train.ind,]

logistic.accuracy = vector()
logistic.precision = vector()
logistic.recall = vector()
logistic.F1 = vector()

for (j in 3:10) {
  start.time = Sys.time()
  phishing_knn <- kknn(Result~.,train.data, test.data, k=j, distance = 2)
  end.time = Sys.time()
  print(end.time-start.time)
  pred = phishing_knn$fitted.values
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
  
  logistic.accuracy[j-2] = (TP+TN)/(TP+FP+FN+TN)
  logistic.precision[j-2]= TP/(TP+FP) 
  logistic.recall[j-2] = TP/(TP+FN)
  logistic.F1[j-2] = 2*logistic.precision[j-2]*logistic.recall[j-2]/(logistic.precision[j-2]+logistic.recall[j-2])
} 

print(logistic.accuracy)
print(logistic.precision)
print(logistic.recall)
print(logistic.F1)

# plot 
parameter=c("accuracy","accuracy","accuracy","accuracy","accuracy","accuracy","accuracy","accuracy","recall","recall","recall","recall","recall","recall","recall","recall","precision","precision","precision","precision","precision","precision","precision","precision","F1score","F1score","F1score","F1score","F1score","F1score","F1score","F1score")
neighbor=c(3,4,5,6,7,8,9,10,3,4,5,6,7,8,9,10,3,4,5,6,7,8,9,10,3,4,5,6,7,8,9,10)
value=c(logistic.accuracy,logistic.recall,logistic.precision,logistic.F1)
knn_fig=data.frame(parameter,neighbor,value)
knn_fig
ggplot(knn_fig, aes(x=factor(neighbor), y=value, colour=parameter, group=parameter)) + geom_line(size=0.5) + geom_point(size=4, shape=20)

