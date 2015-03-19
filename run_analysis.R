## Preparation: download the file and unzip to local drive F:/getdata
## save as getdata-projectfiles.zip under F:/getdata
 setwd("F:/getdata")
 file_url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
 download.file(file_url,"getdata-projectfiles.zip")
 unzip("getdata-projectfiles.zip")

## Load the package dplyr
 library(dplyr)

### 1.Merges the training and the test sets to create one data set.

 setwd("F:/getdata/getdata-projectfile/UCI HAR Dataset")

 ## First Merges the X_test, X_train into one new data set named as X_test_plus_train
 
 X_test <- read.table("F:/getdata/UCI HAR Dataset/test/X_test.txt", quote="\"")
 X_train <- read.table("F:/getdata/UCI HAR Dataset/train/X_train.txt", quote="\"")
 X_test_plus_train <- bind_rows(X_test, X_train)

 ## labels the data set with descriptive variable names from feature.txt. 
 
 features <- read.table("F:/getdata/UCI HAR Dataset/features.txt", quote="\"")
 
 ## get the descriptive names of features, omits the "()" signs,replace "(" and "," with "-"
 
 no_brckts_ftr <- gsub("\\()|\\)", "", features$V2)
 descriptive_names_ftr <-  gsub("\\(|\\,", "\\-", no_brckts_ftr)
 
## names the column names by descriptive_names_ftr 
 
 colnames(X_test_plus_train)  <- descriptive_names_ftr

### 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
 
 mean_std_only <- X_test_plus_train[,grep("std|mean|Mean", colnames(X_test_plus_train))] 
 
### 3. Uses descriptive activity names to name the activities in the data set

 activity_labels <- read.table("F:/getdata/UCI HAR Dataset/activity_labels.txt", quote="\"")
 y_test <- read.table("F:/getdata/UCI HAR Dataset/test/y_test.txt", quote="\"")
 y_train <- read.table("F:/getdata/UCI HAR Dataset/train/y_train.txt", quote="\"")
## merge  y_test and y_train as y_test_train
 y_test_train <- bind_rows(y_test, y_train)
 
# convert the activity numbers to the activity names
y_test_train <- mutate(y_test_train, activity_names = activity_labels[V1,2])
mean_std_only_act_names <- bind_cols(select(y_test_train, activity_names), mean_std_only)
 
 
