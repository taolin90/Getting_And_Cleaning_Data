#dplyr package is used to manipulate data in grouping and chaining
library(dplyr)

filename <- "Coursera_DS_Assignment.zip"
#Checking if filename already exists in work directory
if (!file.exists(filename)){
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, filename, method="curl")
}  

#Checking if folder already exists in work directory
if (!file.exists("UCI HAR Dataset")) { 
    unzip(filename) 
}

#Read all txt files into data frames in R
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("num","featureName"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "Activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "Subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "Subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
colnames(x_train) <- features$featureName
colnames(x_test) <- features$featureName

#1. Merges the training and the test sets to create one data set.
#Combine train and test dataframe for x,y, and subject
x_combine <- rbind(x_train, x_test)
y_combine <- rbind(y_train, y_test)
subject_combine <- rbind(subject_train, subject_test)
merged <- cbind(subject_combine, y_combine, x_combine)

#2. Extracts only the measurements on the mean and standard deviation for each measurement.
#Use grep() to match the pattern for mean and std, extract matched variable names
features_mean_std <- grep("mean\\(\\)|std\\(\\)",features$featureName,value=TRUE)
variable <- union(c("Subject","code"), features_mean_std)
tidy_data<- subset(merged,select=variable) 

#3. Uses descriptive activity names to name the activities in the data set.
tidy_data$code <- activities[tidy_data$code, 2]

#4. Appropriately labels the data set with descriptive variable names.
#Use gsub() function to perform replacement all matches pattern for label name cleansing
names(tidy_data)[2] = "Activity"
names(tidy_data)<-gsub("^t", "Time", names(tidy_data))
names(tidy_data)<-gsub("^f", "Frequency", names(tidy_data))
names(tidy_data)<-gsub("mean()", "Mean", names(tidy_data))
names(tidy_data)<-gsub("std()", "STD", names(tidy_data))
names(tidy_data)<-gsub("freq()", "Frequency", names(tidy_data))
names(tidy_data)<-gsub("Acc", "Accelerometer", names(tidy_data))
names(tidy_data)<-gsub("Gyro", "Gyroscope", names(tidy_data))
names(tidy_data)<-gsub("BodyBody", "Body", names(tidy_data))
names(tidy_data)<-gsub("Mag", "Magnitude", names(tidy_data))

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#Use dplyr functions group_by() and summarise_all() to get average number for each variable by each activity and subject
tidy_data_sub <- tidy_data %>%
    group_by(Activity, Subject) %>%
    summarise_all(mean)

#Write tidy_data_sub to work directory
write.table(tidy_data_sub, "Tidy_data.txt", row.name=FALSE)

