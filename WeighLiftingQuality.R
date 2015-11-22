trainURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
trainfile <- "train.csv"
download.file(trainURL,trainfile,method="curl")
training <- read.csv("train.csv",header=TRUE, na.strings=NA,stringsAsFactors=FALSE)


testURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
testfile <- "test.csv"
download.file(testURL,testfile,method="curl")
testing <- read.csv("test.csv",header=TRUE, na.strings=NA, stringsAsFactors=FALSE)

library(caret)
library(ggplot2)
library(dplyr)


training <- training[,-1]
training$classe <- as.factor(training$classe)
training$user_name <- as.factor(training$user_name)
training <- filter(training, new_window != "yes")
training <- select(training,
                 - c(starts_with("kurt"),starts_with("skew"),starts_with("max"),starts_with("min"),
                     starts_with("ampli"),starts_with("var"),starts_with("avg"),starts_with("stddev"), starts_with("new_w")))
View(training)


inTrain <- createDataPartition(y=training$classe, p=0.7, list=FALSE)
trainx <-tbl_df(training[inTrain,])
testx <-tbl_df(training[-inTrain,])



rfmodel1 <- train(classe ~ .,method="rf", data=trainx, prox=TRUE)


rfmodel1$finalmodel


rfmodel1$results

rfmodel1$confusion
importance(rfmodel1)
?randomForest
rfmodel1$importance

predict(rfmodel1,testx) == testx$classe



starts_with("kurt"),starts_with("skew")
str(zz)

saveRDS(rfmodel1,file="rfmodel1")

head(zz,10)



str(zz)


i
j <-  noquote(i)

j

trainx$j





for(i in names(trainx)) {
        print(i)
##  if(class(trainx[i]) =="character") {trainx[i] <- as.double(trainx[i])}
}

l <- lapply(trainx, class)[]=="character"

l

mutate_each_(trainx,funs(double()),lapply(trainx, class)[]=="character")



?mutate_each_
glimpse(trainx)



summary(training)
str(training)

head(training, 10)







#########
subset_colclasses <- function(data_f, colclasses="numeric") {
        data_f[,sapply(data_f, function(colm, test) class(colm) %in% test, test=colclasses)]
}



zz <- subset_colclasses(trainx, "character")

for( i in 6:(ncol(training)-1)) {training[i] <- as.numeric(training[i])}
