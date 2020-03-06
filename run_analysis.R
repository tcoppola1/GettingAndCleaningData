
#download and unzip file
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
filename <- "UCIdata.zip"
dir <- "UCI HAR Dataset"


if(!file.exists(filename)){
  download.file(url,filename, mode = "wb") 
}

if(!file.exists(dir)){
  dir.create(dir)
}

unzip("UCIdata.zip", files = NULL, exdir=".", overwrite = TRUE)

#read the data and assign column names
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("activityID","Type"))
features <- read.table("UCI HAR Dataset/features.txt")  

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt",col.names = "subjectID")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt",col.names = features[,2])
y_test <- read.table("UCI HAR Dataset/test/y_test.txt",col.names = "activityID")

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt",col.names = "subjectID")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt",col.names = features[,2])
y_train <- read.table("UCI HAR Dataset/train/y_train.txt",col.names = "activityID")

# 1. Merges the training and the test sets to create one data set.
test_data <- cbind(subject_test,y_test,x_test)
train_data <- cbind(subject_train,y_train,x_train)
merged_data<-rbind(train_data,test_data)

# 2.Extracts only the measurements on the mean and standard deviation for each measurement.
extracted_data <-merged_data[,grepl("subjectID|activityID|mean|std",colnames(merged_data))]

# 3.Uses descriptive activity names to name the activities in the data set
final_data <- within(merge(extracted_data,activity_labels),rm("activityID"))

# 4.Appropriately labels the data set with descriptive variable names.
names(final_data) <- gsub("Acc", "Acceleration", names(final_data))
names(final_data) <- gsub("^t", "Time", names(final_data))
names(final_data) <- gsub("^f", "Frequency", names(final_data))
names(final_data) <- gsub("BodyBody", "Body", names(final_data))
names(final_data) <- gsub("mean", "Mean", names(final_data))
names(final_data) <- gsub("std", "Std", names(final_data))
names(final_data) <- gsub("Freq", "Frequency", names(final_data))
names(final_data) <- gsub("Mag", "Magnitude", names(final_data))

# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_data <- aggregate(. ~Type + subjectID, final_data, mean)
tidy_data <- tidy_data[order(tidy_data$Type,tidy_data$subjectID),]
write.table(tidy_data,file="tidydata.txt")