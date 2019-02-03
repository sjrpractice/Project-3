# ----------------------------------------------------------------------------------
# | PROGRAM NAME: JHU Data Science Project 3
# | DATE: February 2019
# | CREATED BY: sjr
# | PROJECT FILE: run_analysis.R
# ---------------------------------------------------------------------------------
# | PURPOSE: To satisfy the requirements for Part 3, Getting and Cleaning Data
# | 
# --------------------------------------------------------------------------------
# | COMMENTS: This project satisfies the following requirements--
# | 
# | 1. The submitted data set is tidy 
# | 2. The Github repo contains the required scripts.
# | 3. GitHub contains a code book that modifies and updates the available codebooks with the 
# |    data to indicate all the variables and summaries calculated, along with units, 
# |    and any other relevant information.
# | 4. The README that explains the analysis files is clear and understandable.
# | 
# --------------------------------------------------------------------------------
# | DATA USED:
# | - Wearble computing data available from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# | - Description of data at http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# | 
# -------------------------------------------------------------------------------
# | CONTENTS
# | 
# | Part 1: Merges the training and the test sets to create one data set
# | Part 2: Extracts only the measurements on the mean and standard deviation for each measurement
# | Part 3: Uses descriptive activity names to name the activities in the data set
# | Part 4: Appropriately labels the data set with descriptive variable names.
# | Part 5: From the data set in step 4, creates a second, independent tidy data set with the 
# |         average of each variable for each activity and each subject. 
# |
# -------------------------------------------------------------------------------
# | UPDATES: n/a
# | 
# --------------------------------------------------------------------------------

# PART 1: Merge the training and the test sets to create one data set.

# create input and output folders
folder_names <- c("raw_data", "output_data") 
sapply(folder_names, dir.create)

# read in raw data
subject_train <- read.table("raw_data/UCI HAR Dataset/train/subject_train.txt")
x_train <- read.table("raw_data/UCI HAR Dataset/train/x_train.txt")
y_train <- read.table("raw_data/UCI HAR Dataset/train/y_train.txt")
subject_test <- read.table("raw_data/UCI HAR Dataset/test/subject_test.txt")
x_test <- read.table("raw_data/UCI HAR Dataset/test/x_test.txt")
y_test <- read.table("raw_data/UCI HAR Dataset/test/y_test.txt")
feature_names <- read.table("raw_data/UCI HAR Dataset/features.txt")

# inspect raw data
View(subject_train)
View(x_train)
View(y_train)
View(x_test)
View(y_test)
View(feature_names)

# change column name for subject files
names(subject_train) <- "subjectNumber"
names(subject_test) <- "subjectNumber"

# change column name for measurement files
names(x_train) <- feature_names$V2
names(x_test) <- feature_names$V2

# change column name for label files
names(y_train) <- "activity"
names(y_test) <- "activity"

# create one dataset from all files
train <- cbind(subject_train, y_train, x_train)
test <- cbind(subject_test, y_test, x_test)
alldata <- rbind(train, test)

# inspect combined data
View(alldata)

# STEP 2: Extract only measurements on mean and standard deviation for each measurement

#find columns whose column name incluedes "mean()" or "std()"
mean_std_cols <- grepl("mean\\(\\)", names(alldata)) | grepl("std\\(\\)", names(alldata))

# maintain the columns for subjectNumber and activity
mean_std_cols[1:2] <- TRUE

# remove columns which are not needed
alldata <- alldata[, mean_std_cols]

# inspect new data
View(alldata)

# STEP 3: Use descriptive activity names to name the activities in the data set

# convert figures to text
alldata$activity <- factor(alldata$activity, labels=c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying"))

# inspect new data
View(alldata)

# STEP 4: Appropriately label the data set with descriptive variable names

# remove parentheses from label names
names <- gsub("()", "", names(alldata), fixed = TRUE)
names(alldata) <- names
str(alldata[,1:6])


# STEP 5:  From the data set in step 4, create a second, independent tidy data set with the 
#             average of each variable for each activity and each subject. 

# load reshape library
library(reshape2)

# repeat step 4 with second data set
new <- melt(alldata, id=c("subjectNumber","activity"))
tidy <- dcast(new, subjectNumber+activity ~ variable, mean)

# write tidy set to a file
write.table(tidy, "Project-3-data.csv",row.names=FALSE)

# inspect output
output <- read.csv("tidy4.csv")
View(output)

