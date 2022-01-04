library(ggplot2)

rm(list =ls())

# Create the data frame of SVM.
comp <- data.frame(
  parameter = c("accuracy","accuracy","accuracy","accuracy","recall","recall","recall","recall","precision","precision","precision","precision","F1score","F1score","F1score","F1score"),
  kernel = c("Linear", "RBF","Polynomial","Sigmoid","Linear", "RBF","Polynomial","Sigmoid","Linear", "RBF","Polynomial","Sigmoid","Linear", "RBF","Polynomial","Sigmoid"), 
  value = c(0.9014778,0.91133,0.9162562,0.7438424,0.9096386,0.9036145,0.9216867,0.6445783, 0.8579545,0.8823529,0.8793103,0.7039474,0.8830409,0.8928571,0.9,0.672956)
)
comp

# plot histogram by group
ggplot(comp,aes(x=kernel, y=value, fill=parameter))+geom_bar(stat='identity',position="dodge")

# create the 
logistic.accuracy
