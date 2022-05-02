#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement.
#3. Uses descriptive activity names to name the activities in the data set.
#4. Appropriately labels the data set with descriptive variable names.
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(dplyr)

# Download and unzip data
Url1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
path1 <- getwd()
download.file(Url1, file.path(path1, "Dataset.zip"))
unzip(zipfile = "Dataset.zip")

# Define the path in which the new folder has been unziped
dataFile <- file.path(path1, "UCI HAR Dataset")

# Create a new file that has the file names
files <- list.files(dataFile, recursive = TRUE)

# Read the training tables
xtrain <- read.table(file.path(dataFile, "train/X_train.txt"))
ytrain <- read.table(file.path(dataFile, "train/y_train.txt"))
subject_train <- read.table(file.path(dataFile, "train/subject_train.txt"))

# Read the testing tables
xtest <- read.table(file.path(dataFile, "test/X_test.txt"))
ytest <- read.table(file.path(dataFile, "test/y_test.txt"))
subject_test <- read.table(file.path(dataFile, "test/subject_test.txt"))

# Read the features data
features <- read.table(file.path(dataFile, "features.txt"))

# Read the activity labels data
activityLabels <- read.table(file.path(dataFile, "activity_labels.txt"))

# Creat values to the train data
colnames(xtrain) <- features[, 2]
colnames(ytrain) <- "activityID"
colnames(subject_train) <- "subjectID"

# Creat values to the test data
colnames(xtest) <- features[, 2]
colnames(ytest) <- "activityID"
colnames(subject_test) <- "subjectID"

# Creat check for the activity lables value
colnames(activityLabels) <- c("activityID", "activityType")

# Merge the train and test data
mrg_train <- cbind(ytrain, subject_train, xtrain)
mrg_test <- cbind(ytest, subject_test, xtest)

# Merge both data tables
data_all <- rbind(mrg_train, mrg_test)

# Read all the available values
col_Names <- colnames(data_all)

# Set a subject of all the mean and standard deviation
mean_std <- (grepl("activityID", col_Names) | grepl("subjectID", col_Names) | grepl("mean.", col_Names) | grepl("std.", col_Names))

# Creat subset to get the dataset
sub_mean_std <- data_all[, mean_std == TRUE]

# Uses descriptive activity names to name the activities in the data set
names_activities <- merge(sub_mean_std, activityLabels, by = "activityID", all.x = TRUE)
levels(names_activities$activityID) <- c("WALKING", "WALKING UPSTAIRS", "WALKING DOWNSTAIRS", "SITTING", "STANDING", "LYING")
names(names_activities) <- gsub("^t", "Time", names(names_activities))
names(names_activities) <- gsub("^f","Frequency", names(names_activities))
names(names_activities) <- gsub("Acc","Accelerometer", names(names_activities))
names(names_activities) <- gsub("Gyro","Gyroscope", names(names_activities))
names(names_activities) <- gsub("Mag","Magnitude", names(names_activities))
names(names_activities) <- gsub("BodyBody","Body", names(names_activities))
names(names_activities) <- gsub("tBody","TimeBody", names(names_activities))
names(names_activities) <- gsub("angle","Angle", names(names_activities))
names(names_activities) <- gsub("gravity","Gravity", names(names_activities))
names(names_activities) <- gsub("-mean()",".Mean", names(names_activities))
names(names_activities) <- gsub("-std()",".STD", names(names_activities))
names(names_activities) <- gsub("-freq()",".Frequency", names(names_activities))

# Create a second, independent tidy data set with the average of each variable for each activity and each subject
secTidyData <- names_activities %>% group_by(subjectID, activityID) %>% summarise(across(everything(), mean))

# Save data as text file
write.table(secTidyData, "secTidyData.txt", row.names = FALSE)
