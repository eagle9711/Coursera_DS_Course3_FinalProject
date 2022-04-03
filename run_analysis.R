
#Download dataset:

fileUrlC3<- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
filename <- "C3-project.zip"
if (!file.exists(filename)){
  download.file(fileUrlC3, filename, method="curl")
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

#Load data:
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("id","functions"))

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")

x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")


#1. Merges the training and the test sets to create one data set.

xdata <- rbind(x_test, x_train)
ydata <- rbind(y_test, y_train)
subject <- rbind(subject_test, subject_train)
mergedData <- cbind(subject, ydata, xdata)


#2. Extracts only the measurements on the mean and standard deviation for each measurement. 

extractedData <- select(mergedData, subject, code, contains("mean"), contains("std"))

#3. Uses descriptive activity names to name the activities in the data set.

extractedData <- merge(activities, extractedData, by = "code")
extractedData <- relocate(extractedData, subject, .before=code)

#4. Appropriately labels the data set with descriptive variable names.

names(extractedData)[2] <- "activityCode"
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

finalData <- extractedData %>%
  group_by(subject, activityCode, activity) %>%
  summarise_all(funs(mean))
write.table(finalData, "finalData.txt", row.name=FALSE)








