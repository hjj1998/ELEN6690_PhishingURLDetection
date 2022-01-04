library(tree)
library(foreign)

set.seed(123)
dataset <- read.arff("C:\\Users\\11705\\OneDrive\\??????\\Homework\\EECS6690\\6690 Final Project\\PhishingData.arff")
dataset$Result[which(dataset$Result==-1)] <- 0

train.ind <- sort(sample(x = 1353, size = round(0.7*1353), replace = FALSE))
train.data <- dataset[train.ind,]
test.data <- dataset[-train.ind,]

start.time = Sys.time()
tree.data = tree(train.data$Result~., train.data)
end.time = Sys.time()
print(end.time-start.time)

plot(tree.data)
text(tree.data, pretty = 0)

cv.dataset = cv.tree(tree.data, FUN = prune.misclass)
prune.data = prune.misclass(tree.data, best = 8)
tree.pred2 = predict(prune.data, test.data, type = 'class')
table(tree.pred2, test.data$Result)



start.time = Sys.time()
tree.pred = predict(tree.data, test.data, type = 'class')
end.time = Sys.time()
print(end.time-start.time)

tree.table = table(tree.pred, test.data$Result)
tree.table

TP = tree.table[3,3]
TN = tree.table[2,2]
FN = tree.table[2,3]
FP = tree.table[3,2]

acc = (TP+TN)/(TP+TN+FP+FN) #0.87684729
precision = TP/(TP+FP) #0.822222
recall = TP/(TP+FN) #0.89156626
F1 = 2*precision*recall/(precision+recall) #0.8554913


