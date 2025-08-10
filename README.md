# Getting-and-Cleaning-Data

library(dplyr)

#download the zip file if it hasn't been downloaded
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
getdataURL <- "UCI HAR Dataset.zip"

if(!file.exists(getdataURL)) {
  download.file(URL, getdataURL, mode = "wd")
}

#unzip the zip file
dataaPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(getdataURL)
}

##Read the data

#read training data
trainingSubject <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

#read test data
testSubject <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

# read features, don't convert text labels to factors
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

# read activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

##STEP 1 - merge the train and test data sets into a single data table
Activities <- rbind (
    cbind(trainingSubject, trainingValues, trainingActivity),
    cbind(testSubject, testValues, testActivity)
)

# remove the individual data tables to save some memory
rm(trainingSubjects, trainingValues, trainingActivity, testSubjects, testValues, testActivity)

# assign column names
colnames(Activity) <- c("subject", features[, 2], "activity")

##STEP 2 - Only extract the mean and standard deviation for each measurement
retainCol <- grepl("subject|activity|mean|std", colnames(Activity))

Activity <- Activity[, retainCol]

##describe each activity
Activity$activity <- factor(Activity$activity, levels = activities[, 1], labels = activities[, 2])

## STEP 3 - label the descriptive variable names

#column names
ActivityCols <- colnames(Activity)

#expand abbreviations and clean up names
ActivityCols <- gsub("^f", "frequencyDomain", ActivityCols)
ActivityCols <- gsub("^t", "timeDomain", ActivityCols)
ActivityCols <- gsub("Acc", "Accelerometer", ActivityCols)
ActivityCols <- gsub("Gyro", "Gyroscope", ActivityCols)
ActivityCols <- gsub("Mag", "Magnitude", ActivityCols)
ActivityCols <- gsub("Freq", "Frequency", ActivityCols)
ActivityCols <- gsub("mean", "Mean", ActivityCols)
ActivityCols <- gsub("std", "StandardDeviation", ActivityCols)

##STEP 4 - Create a second, independent tidy set with the aveage of each variable for each
##activity and each subject

ActivityMeans <- humanActivity %>% 
    group_by(subject, activity) %>%
    summarise_each(funs(mean))

##make a text file "tidy_data.text"
write.table(ActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
