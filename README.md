### Introduction

This project assignment does the following:
  1	Merges the training and the test sets to create one data set.
	2	Extracts only the measurements on the mean and standard deviation for each measurement. 
	3	Uses descriptive activity names to name the activities in the data set
	4	Appropriately labels the data set with descriptive variable names. 
	5	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

### Step 1: Loads the column headers

setwd("~/UCI HAR Dataset")
features <- read.table("features.txt")
cols <- nrow(features)

### Step 2: Creates the train data.frame

setwd("~/UCI HAR Dataset/train")
train_x <- read.table("X_train.txt")
for(i in 1:cols) {
  names(train_x)[i] <- paste(features[i, 2])
}
train_y <- read.table("y_train.txt")
train_z <- read.table("subject_train.txt")
names(train_y)[1]<-"activity"
names(train_z)[1]<-"subject"
train <- cbind(train_x, train_y, train_z)

### Step 3: Creates the test data.frame

setwd("~/UCI HAR Dataset/test")
test_x <- read.table("X_test.txt")
for(i in 1:cols) {
  names(test_x)[i] <- paste(features[i, 2])
}
test_y <- read.table("y_test.txt")
test_z <- read.table("subject_test.txt")
names(test_y)[1]<-"activity"
names(test_z)[1]<-"subject"
test <- cbind(test_x, test_y, test_z)

### Step 4: Merges the train and test data.frames and gets the relevant columns

df <- rbind(train, test)
mstd <- subset(df, select=c(1:6, 41:46, 81:86, 121:126, 161:166, 201:202, 214:215, 227:228, 240:241,
                            253:254, 266:271, 345:350, 424:429, 503:504, 516:517, 529:530, 542:543, 562:563))

### Step 5: Replaces the activity code with descriptive names

rownum <- nrow(mstd)
for(i in 1:rownum) {
  if (mstd[i, 67] == "1") {
    mstd[i, 67] <- "WALKING"
  } else if (mstd[i, 67] == "2") {
    mstd[i, 67] <- "WALKING_UPSTAIRS"
  } else if (mstd[i, 67] == "3") {
    mstd[i, 67] <- "WALKING_DOWNSTAIRS"
  } else if (mstd[i, 67] == "4") {
    mstd[i, 67] <- "SITTING"
  } else if (mstd[i, 67] == "5") {
    mstd[i, 67] <- "STANDING"
  } else if (mstd[i, 67] == "6") {
    mstd[i, 67] <- "LAYING"
  }
}

### Step 6: Gets the mean by activity and subject

mean_subj <- aggregate(mstd[, 1:66], list(mstd$subject), mean)
mean_act <- aggregate(mstd[, 1:66], list(mstd$activity), mean)

### Step 7: Merges the means and writes to a file

df2 <- rbind(mean_subj, mean_act)
names(df2)[1] <- paste("subject_activity")

write.table(df2, file = "tidy_data_mean.txt", sep = " ", row.name = FALSE)
