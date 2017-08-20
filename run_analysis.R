## Getting and Cleaning Data Course Project

##Merges the training and the test sets to create one data set.
setwd("C:/Users/psingh2424/Desktop/R")
subjecttrain <- read.table(file="subject_train.txt")
xtrain <- read.table(file="X_train.txt")
ytrain <- read.table(file="y_train.txt")
xtest <- read.table(file="X_test.txt")
ytest <- read.table(file="y_test.txt")

#import column names and select the right columns
features <- read.table(file="features.txt")
library(dplyr)
features2<- select(features,V2)
features2 <- t(features2)

##rename columns
colnames(xtrain) <- paste(features2)
colnames(xtest) <- paste(features2)

##Extracts only the measurements on the mean and the standard deviation for each measurement
xtrain <- subset(xtrain, select= grep("mean|std", names(xtrain)))
xtest <- subset(xtest, select= grep("mean|std", names(xtest)))

##change ytrain, ytest to character vectors in order to rename rows
ytrain2 <- as.character(ytrain$V1)
ytest2  <- as.character(ytest$V1)

library(plyr)
ytrain3 <- revalue(ytrain2, c("1"="WALKING", "2"="WALKING_UPSTAIRS", "3"="WALKING_DOWNSTAIRS", "4"="SITTING", "5"="STANDING", "6"="LAYING"))
ytest3 <- revalue(ytest2, c("1"="WALKING", "2"="WALKING_UPSTAIRS", "3"="WALKING_DOWNSTAIRS", "4"="SITTING", "5"="STANDING", "6"="LAYING"))

xytrain <- cbind(ytrain3,xtrain)
xytest <- cbind(ytest3,xtest)

## Rename first column in each data set
names(xytrain)[1]<-paste("Activity")
names(xytest)[1]<-paste("Activity")
## combine train and test data
xytraintest <- rbind(xytrain,xytest)

##  Uses descriptive activity names to name the activities in the data set
## 1 WALKING, 2 WALKING_UPSTAIRS, 3 WALKING_DOWNSTAIRS, 4 SITTING, 5 STANDING, 6 LAYING

##library(plyr)
##ytrain3 <- revalue(ytrain2, c("1"="WALKING", "2"="WALKING_UPSTAIRS", "3"="WALKING_DOWNSTAIRS", "4"="SITTING", "5"="STANDING", "6"="LAYING"))
##ytest3 <- revalue(ytest2, c("1"="WALKING", "2"="WALKING_UPSTAIRS", "3"="WALKING_DOWNSTAIRS", "4"="SITTING", "5"="STANDING", "6"="LAYING"))
##xytraintest <- revalue(xytraintest$Activity, c("1"="WALKING", "2"="WALKING_UPSTAIRS", "3"="WALKING_DOWNSTAIRS", "4"="SITTING", "5"="STANDING", "6"="LAYING"))



##  Extracts the column mean and standard deviation for each measurement
## From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject  

means <- vector()
sds <- vector()
standingdatamean <- vector()
standingdatasd <- vector()
walkingdatamean <- vector()
walkingdatasd <- vector()
walking_upstairsdatamean <- vector()
walking_upstairsdatasd <- vector()
walking_downstairsdatamean <- vector()
walking_downstairsdatasd <- vector()
sittingdatamean <- vector()
sittingdatasd <- vector()
layingdatamean <- vector()
layingdatasd <- vector()

library(dplyr)
standingdata <- filter(xytraintest, Activity == "STANDING")
walkingdata <- filter(xytraintest, Activity == "WALKING")
walking_upstairsdata <- filter(xytraintest, Activity == "WALKING_UPSTAIRS")
walking_downstairsdata <- filter(xytraintest, Activity == "WALKING_DOWNSTAIRS")
sittingdata <- filter(xytraintest, Activity == "SITTING")
layingdata <- filter(xytraintest, Activity == "LAYING")



for (i in 2:80) {
  means[i] <- mean(xytraintest[, i])
  sds[i] <- sd(xytraintest[, i])
  standingdatamean[i] <- mean(standingdata[, i])
  standingdatasd[i] <- sd(standingdata[, i])
  walkingdatamean[i] <- mean(walkingdata[, i])
  walkingdatasd[i] <- sd(walkingdata[, i])
  walking_upstairsdatamean[i] <- mean(walking_upstairsdata[, i])
  walking_upstairsdatasd[i] <- sd(walking_upstairsdata[, i])
  walking_downstairsdatamean[i] <- mean(walking_downstairsdata[, i])
  walking_downstairsdatasd[i] <- sd(walking_downstairsdata[, i])
  sittingdatamean[i] <- mean(sittingdata[, i])
  sittingdatasd[i] <- sd(sittingdata[, i])
  layingdatamean[i] <- mean(layingdata[, i])
  layingdatasd[i] <- sd(layingdata[, i])
}

## Combine data of means of each column and activity
tidydata <- cbind(means, sds, standingdatamean, standingdatasd, walkingdatamean, walkingdatasd, walking_upstairsdatamean, walking_upstairsdatasd, walking_downstairsdatamean, walking_downstairsdatasd, sittingdatamean, sittingdatasd, layingdatamean, layingdatasd)
