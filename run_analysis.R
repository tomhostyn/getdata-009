library (dplyr)
library(data.table)

# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average
# of each variable for each activity and each subject.


load_har_data_table <- function(wd = getwd()){

  uci_har_dir <- "UCI HAR Dataset"
  uci_har_path <- paste (wd, sep="/", uci_har_dir)
  
  uci_har_train_path <- paste (uci_har_path, sep="/", "train")
  uci_har_test_path <- paste (uci_har_path, sep="/", "test")
  
  features_path <- paste (uci_har_path, sep="/", "features.txt")
  activities_path <- paste (uci_har_path, sep="/", "activity_labels.txt")
  
  subject_train_path <- paste(uci_har_train_path, sep="/", "subject_train.txt")
  X_train_path <- paste(uci_har_train_path, sep="/", "X_train.txt")
  Y_train_path <- paste(uci_har_train_path, sep="/", "Y_train.txt")
  
  subject_test_path <- paste(uci_har_test_path, sep="/", "subject_test.txt")
  X_test_path <- paste(uci_har_test_path, sep="/", "X_test.txt")
  Y_test_path <- paste(uci_har_test_path, sep="/", "Y_test.txt")
  
  delayedAssign("features", data.table(read.table(features_path, stringsAsFactors=FALSE)))
  delayedAssign("activities", data.table(read.table(activities_path, stringsAsFactors=TRUE)))
  
  delayedAssign("subject_train", data.table(read.table(subject_train_path)))
  delayedAssign("X_train", data.table(read.table (X_train_path)))
  delayedAssign("Y_train", data.table(read.table (Y_train_path)))
  
  delayedAssign("subject_test", data.table(read.table(subject_test_path)))
  delayedAssign("X_test", data.table(read.table (X_test_path)))
  delayedAssign("Y_test", data.table(read.table (Y_test_path))) 
  
  # Load train dataset as datatables.
  # Assign labels as documented in the features file.
  # However, remove characters that cannot directly be used in r command line
  
  renamed <- features$V2
  
  setnames (X_train, renamed)
  setnames (subject_train, "Subject")
  setnames (Y_train, "ActivityID")

  #  Load test dataset as datatables.
  #  Assign labels as documented in the features file
  
  setnames (X_test, features$V2)
  setnames (subject_test, "Subject")
  setnames (Y_test, "ActivityID")

  #
  #  keep only mean and std columns in X_train and X_test
  #
  keep <- grep ("*mean\\(\\)*|*std*", features$V2) 
  X_train <- select(X_train, keep)
  X_test <- select(X_test, keep)
  
  #  append subject column and Y column to the features.
  har_train <- cbind(subject_train, Y_train, X_train )
  har_test <- cbind(subject_test, Y_test, X_test)
  
  #
  # Combine test and train datasets
  #
  
  har <- rbind (har_train, har_test)

  #
  # replace activity numbers with names
  #
  setnames(activities, c("ActivityID", "Activity"))
  setkey(activities, ActivityID)
  setkey(har, ActivityID)
  har <- merge(har, activities)
  select (DF, -ActivityID)
  
  #
  # rename columns. remove brackets, -
  #
  renamed <- names(har)
  renamed <- sub ("\\(\\)", "", renamed)
  renamed <- sub ("-", "_", renamed)
  renamed <- sub ("-", "_", renamed) 
  
  setnames(har, renamed)
  har
}


transform_har_data_table <- function (dt){
    
  #TO DO
  # activities number should be replaced by activity classes
  # make copy with averages
  
#   dt <- copy(dt)
#   
#   
#   dt <- copy(DT)
#   
#   dt[,tBodyAcc_mean_X_M:= mean(tBodyAcc_mean_X),by=Subject]
#   
#   
#   
#   dt %>% 
#     group_by(Subject)
#   
#   dt <- copy(DT)
#   dt <- group_by(dt, Subject)
#   transform (dt, 
#              tBodyAcc_mean_X_M = mean (tBodyAcc_mean_X)) 
#     
#   
#        
#     })
}
