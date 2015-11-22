## Practical Machine Learning Project
## Use sensor data to predict quality of weight lifiting by person


## Load main training data and read it in
trainURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
trainfile <- "train.csv"
download.file(trainURL,trainfile,method="curl")
training <- read.csv("train.csv",header=TRUE, na.strings=NA,stringsAsFactors=FALSE)

## Load final testing data and read it in
testURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
testfile <- "test.csv"
download.file(testURL,testfile,method="curl")
testing <- read.csv("test.csv",header=TRUE, na.strings=NA, stringsAsFactors=FALSE)


library(caret)
library(ggplot2)
library(dplyr)

## Data prep for modeling, getting rid of unused columns such as time stamps and 
## correcting data types
training <- training[,-c(1,3,4,5,6,7)]
training$classe <- as.factor(training$classe)
training$user_name <- as.factor(training$user_name)


## Selecting columns to build model on
training <- select(training,
                 - c(starts_with("kurt"),starts_with("skew"),starts_with("max"),
                     starts_with("min"),
                     starts_with("ampli"),starts_with("var"),starts_with("avg"),
                     starts_with("stddev"), starts_with("new_w")))

## Split data for training and validation
inTrain <- createDataPartition(y=training$classe, p=0.7, list=FALSE)
trainx <-tbl_df(training[inTrain,])
testx <-tbl_df(training[-inTrain,])


## Train model
rfmodel2 <- train(classe ~ .,method="rf", data=trainx, prox=TRUE)


## Model Results

rfmodel2

getTree(rfmodel2$finalModel,k=2)


library(randomForest)
tst <- rfcv(trainx = training[,-54], trainy = training[,54], scale = "log", step=0.7)
tst

imp <- importance(rfmodel2$finalModel)
round(imp[order(imp,decreasing=TRUE),],0)


rfmodel2$results

## Validate model

install.packages("ROCR")

library(ROCR)

evalset <-   predict(rfmodel2,testx) 

evalset.correct <- evalset == testx$classe
print(paste("Percent Correct Predictions on Evaluation Set =",round(mean(evalset.correct)*100,2),"%"))

confusionMatrix(evalset,testx$classe)

performance(rfmodel2$finalModel,)

plot(rfmodel2$finalModel)

evalset.probs <- 1- unlist(treeresponse(efmodel2, newdata=testx), use.names=F)[seq(1,nrow(evalset)*2,2)]

      # Use the model to predict the evaluation.
        x.evaluate$prediction <- predict(x.model, newdata=x.evaluate)
        
        # Calculate the overall accuracy.
        x.evaluate$correct <- x.evaluate$prediction == x.evaluate$Kyphosis
        print(paste("% of predicted classifications correct", mean(x.evaluate$correct)))
        
        # Extract the class probabilities.
        x.evaluate$probabilities <- 1- unlist(treeresponse(x.model,
                                                           newdata=x.evaluate), use.names=F)[seq(1,nrow(x.evaluate)*2,2)]
        
        # Plot the performance of the model applied to the evaluation set as
        # an ROC curve.
        require(ROCR)
        pred <- prediction(x.evaluate$probabilities, x.evaluate$Kyphosis)
        perf <- performance(pred,"tpr","fpr")
        plot(perf, main="ROC curve", colorize=T)
        
        # And then a lift chart
        perf <- performance(pred,"lift","rpp")
        plot(perf, main="lift curve", colorize=T)        
        
        
        
        

## Prepare test data and apply model to final test data for submission

testing <- testing[,-c(1,3,4,5,6,7)]
testing$user_name <- as.factor(testing$user_name)
testing <- select(testing,
                    + - c(starts_with("kurt"),starts_with("skew"),starts_with("max"),
                          starts_with("min"),
                          + starts_with("ampli"),starts_with("var"),starts_with("avg"),
                          starts_with("stddev"), starts_with("new_w")))
predict(rfmodel2,testing)
 
## Write submission files

answers <- predict(rfmodel2,testing)
pml_write_files = function(x){
                 n = length(x)
                 for(i in 1:n){
                                 filename = paste0("problem_id_",i,".txt")
                                 write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
                         }
         }
pml_write_files(answers)
 


saveRDS(rfmodel2,file="rfmodel2")


rfmodel2$finalmodel



rfmodel2$confusion
importance(rfmodel1)
?randomForest
rfmodel2$importance
