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

 ## 
 ##  Merges all the related files of X_test and  X_train into one new data set named as X_total
 
 
 
 X_test <- read.table("F:/getdata/UCI HAR Dataset/test/X_test.txt", quote="\"")
 X_train <- read.table("F:/getdata/UCI HAR Dataset/train/X_train.txt", quote="\"")
 y_test <- read.table("F:/getdata/UCI HAR Dataset/test/y_test.txt", quote="\"")
 y_train <- read.table("F:/getdata/UCI HAR Dataset/train/y_train.txt", quote="\"")
 subject_test <- read.table("F:/getdata/UCI HAR Dataset/test/subject_test.txt", quote="\"")
 subject_train <- read.table("F:/getdata/UCI HAR Dataset/train/subject_train.txt", quote="\"")
 
 X_test_total <- bind_cols(subject_test, y_test, X_test)
 X_train_total <- bind_cols(subject_train, y_train, X_train)
 ## reorder the colnmber to make bind_rows working correctly
 colnames(X_test_total)[1:2] <- c("Subject", "Activity")
 colnames(X_train_total)[1:2] <- c("Subject", "Activity")
 
 X_total <- bind_rows(X_test_total, X_train_total)

 

### 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## plus 2 to bypass the first two cols (subject and activity). 
 
 features <- read.table("F:/getdata/UCI HAR Dataset/features.txt", quote="\"")
 mean_std_only <- X_total[,grep("std|mean|Mean", features$V2) + 2] 
 mean_std_X <- bind_cols(select(X_total,Subject, Activity), mean_std_only)
 
### 3. Uses descriptive activity names to name the activities in the data set
 
 activity_labels <- read.table("F:/getdata/UCI HAR Dataset/activity_labels.txt", quote="\"")

 # convert the activity numbers to the activity names
 
 X_mean_std_activity <- mutate(mean_std_X, Activity = activity_labels[Activity,2])         

### 4.Appropriately labels the data set with descriptive variable names.
 ## labels the data set with descriptive variable names from feature.txt.

 ## get the descriptive names of features, omits the "()" signs,replace "(" and "," with "-"
 
 no_brckts_ftr <- gsub("\\()|\\)", "", features$V2)
 descriptive_names_ftr <-  gsub("\\(|\\,", "\\-", no_brckts_ftr) 
 mean_std_names <- grep( "std|mean|Mean", descriptive_names_ftr)
 
 #names(master_merge) <- valid_column_names
 
 ## names the column names by descriptive_names_ftr 
 
 colnames(X_mean_std_activity)[3:88]  <- descriptive_names_ftr[mean_std_names]
 
### 5. From the data set in step 4, creates a second, independent tidy data set 
### with the average of each variable for each activity and each subject.
 
 ## arrange the data set by Subject, Activity
 X <- arrange(X_mean_std_activity, Subject, Activity)
 act <- arrange(activity_labels, V2)

 
 ## Ininial the for loop 
 m <- 1
 n <- 1
 ## loop count the mean
 while (m  <=  30){
         
         while (n  <= 6){
                 X1 <- X[which(X$Subject == m),]
                 X1_act<- X1[which(X1$Activity == act[n,2]),]
                 x11 <- apply(X1_act[,3:88], 2, mean)
                 x_tidy <- matrix(x11, nrow =1, ncol =86, byrow = FALSE)
                 
                 write.table(x_tidy, file = "tidy.csv", row.names = FALSE, col.names = FALSE, append = TRUE) 
                 #x113 <- data.frame(x112)
                 #colnames(x113) <- colnames(X_final)
                 
                #X_final <- bind_rows(X_final, x113)
                n <- n + 1
        }
       m <- m + 1  
       n <- 1  
 }

## write the colnames/rownames back
 x_tidy <- read.table("F:/getdata/tidy.csv", quote="\"")
 colnames(x_tidy) <- descriptive_names_ftr[mean_std_names]
 
 i =1
 t1 <-data.frame(Subject = rep(i, 6), Activity = rep(act$V2))
 i <- i +1
 
 while (i <= 30){
         t2 <-data.frame(Subject = rep(i, 6), Activity = rep(act$V2))
         t1 <-bind_rows(t1,t2)
         i <- i +1
         
 }
 x_tidy1 <- bind_cols(t1, x_tidy)
## tidy1.csv the final tidy data
 write.table(x_tidy1, file = "tidy1.txt", row.names = FALSE) 
 
 
 
