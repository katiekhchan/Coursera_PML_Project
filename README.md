Coursera_PML_Project
====================

Project for Practical Machine Learning Class


library(caret)

#Read in training and testing data:
training=read.csv("pml-training.csv", header=T)
testing=read.csv("pml-training.csv", header=T)

#Remove irrelevant variables: e.g. personal identifier, timestamp, window, etc
#Remove avg, min, max, var, stddev, kurtosis, skewness related variables
#Remove variables with a lot of missing values, i.e. those prefixed with "amplitude"
training2=training[-c(1:7)]
training3=training2[c(1:4,17:19,30:42,53:61,74:79,92:95,106:117,130:133,144:153)]
training4=training3[-c(5:6,30:32,36:37,52:53)]
train_clean=training4[-c(5,31,45)]

#Create same set for testing data:
testing=read.csv("pml-testing.csv", header=T)
testing2=testing[-c(1:7)]
testing3=testing2[c(1:4,17:19,30:42,53:61,74:79,92:95,106:117,130:133,144:153)]
testing4=testing3[-c(5:6,30:32,36:37,52:53)]
test_clean=testing4[-c(5,31,45)]

#partition training data for cross validation based on classe variable:
inTrain = createDataPartition(train_clean$classe, p = 0.7)[[1]]
train_clean_train = train_clean[inTrain,]
train_clean_test = train_clean_train[-inTrain,]

#train model using training data with regression tree analysis and returns the object unchanged using na.pass
#impute data using k nearest neighbors
modelfit=train(classe~., data=train_clean_train, method="rpart", preProcess = c("knnImpute"), na.action  = na.pass)

#make prediction using cross validation data:
pred=predict(modelfit, train_clean_test)
#train_clean_test$predcorrect=pred==train_clean_test$classe

#plot the tree:
library(rattle)
jpeg('figure1.jpg')
fancyRpartPlot(modelfit$finalModel)
dev.off()

#check out of sample error:
missClass = function(values,prediction){
sum((prediction != values))/length(values)}
missClass(t(train_clean_test$classe),pred)
#out of sample error= 0.5009732

#make preduction using testing data:
pred_test=predict(modelfit, test_clean)

#plot the tree:
jpeg('figure2.jpg')
fancyRpartPlot(modelfit$finalModel)
dev.off()

#output results for test cases for submission:
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(pred_test)
