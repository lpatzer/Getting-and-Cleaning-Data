wd<-"C:/Users/patzerl/Documents/Coursera"
setwd(wd)

training <- read.table("UCI HAR Dataset/train/X_train.txt")
training[,562] <- read.table("UCI HAR Dataset/train/Y_train.txt")
training[,563] <- read.table("UCI HAR Dataset/train/subject_train.txt")

testing <- read.table("UCI HAR Dataset/test/X_test.txt")
testing[,562] <- read.table("UCI HAR Dataset/test/Y_test.txt")
testing[,563] <- read.table("UCI HAR Dataset/test/subject_test.txt")

activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")

## Part 4: Cleaning up column names
features <- read.table("UCI HAR Dataset/features.txt")
features[,2] <- gsub("^t", "Time", features[,2])
features[,2] <- gsub("^f", "Frequency", features[,2])
features[,2] <- gsub("-mean\\(\\)", "Mean", features[,2])
features[,2] <- gsub("-std\\(\\)", "StdDev", features[,2])
features[,2] <- gsub("-", "", features[,2])

## Part 1: Merge Data
merged_data <- rbind(training, testing)

## Part 2: Only mean and standard deviation columns
colsWeWant <- grep("Mean|Std|mean|std", features[,2])

features <- features[colsWeWant,]
colsWeWant <- c(colsWeWant, 562, 563)
merged_data <- merged_data[,colsWeWant]
colnames(merged_data) <- c(features$V2, "Activity", "Subject")
colnames(merged_data) <- tolower(colnames(merged_data))

## Part 3: Create meaningful activity names
currentActivity <- 1
for (currentActivityLabel in activityLabels$V2) {
  merged_data$activity <- gsub(currentActivity, currentActivityLabel, merged_data$activity)
  currentActivity <- currentActivity + 1
}

merged_data$activity <- as.factor(merged_data$activity)
merged_data$subject <- as.factor(merged_data$subject)

## Part 5: Creating tidy data
tidy <- aggregate(merged_data, by=list(activity = merged_data$activity, subject=merged_data$subject), mean)
tidy2<-tidy[,1:88]
write.table(tidy2, "C:/Users/patzerl/Documents/Coursera/Getting-and-Cleaning-Data/tidy.txt", sep="\t", row.name=FALSE)