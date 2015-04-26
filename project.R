# install.packages("data.table")
# install.packages("caret")
# install.packages("ggplot2")
# install.packages("doMC")
# install.packages("knitr")
# install.packages("xtable")
# install.packages("randomForest")
# install.packages("RCurl")
# install.packages("Hmisc")
# install.packages("foreach")
# install.packages("doParallel")

library(data.table)
library(caret)
library(ggplot2)
library(doMC)
library(knitr)
library(xtable)
library(randomForest)
library(RCurl)
library(Hmisc)
library(foreach)
library(doParallel)
library(gridExtra)

options(warn=-1)

set.seed(2048)


URLTrain <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
t1 <- getURL(URLTrain)

URLEvaluate <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
e1 <- getURL(URLEvaluate)


training_data <- read.csv(textConnection(t1), na.strings=c("#DIV/0!")) 

evaluation_data <- read.csv(textConnection(e1), na.strings=c("#DIV/0!")) 


for(i in c(8:ncol(training_data)-1)) {training_data[,i] = as.numeric(as.character(training_data[,i]))}

for(i in c(8:ncol(evaluation_data)-1)) {evaluation_data[,i] = as.numeric(as.character(evaluation_data[,i]))}



feature_set <- colnames(training_data[colSums(is.na(training_data)) == 0])[-(1:7)]
model_data <- training_data[feature_set]
feature_set


idx <- createDataPartition(y=model_data$classe, p=0.60, list=FALSE )

training <- model_data[idx,]
testing <- model_data[-idx,]


grid.arrange(
  ggplot(training, aes(x=accel_arm_z, y=yaw_belt, colour=classe)) + geom_point(),
  ggplot(training, aes(x=pitch_dumbbell, y=total_accel_belt, colour=classe)) + geom_point(),
  ggplot(training, aes(x=total_accel_arm, y=yaw_forearm, colour=classe)) + geom_point(),
  ggplot(training, aes(x=roll_arm, y=total_accel_forearm, colour=classe)) + geom_point(),
  ncol=2
)


registerDoParallel()
x <- training[-ncol(training)]
y <- training$classe

rf <- foreach(ntree=rep(150, 6), .combine=randomForest::combine, .packages='randomForest') %dopar% {
  randomForest(x, y, ntree=ntree) 
}


predictions1 <- predict(rf, newdata=training)
confusionMatrix(predictions1,training$classe)


predictions2 <- predict(rf, newdata=testing)
confusionMatrix(predictions2,testing$classe)


# Applying the model



pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}


x <- evaluation_data
x <- x[feature_set[feature_set!='classe']]
answers <- predict(rf, newdata=x)

answers

pml_write_files(answers)









