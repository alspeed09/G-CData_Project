#### Download and unzip data ####
setwd("C:/Users/alspeed09/Desktop/Materias y cursos/Getting_data/G&CData_Project")
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
              "GACdata.zip")
unzip("GACdata.zip")

#### Reading the test data ####

## General data
Activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
Features_list   <- read.table("UCI HAR Dataset/features.txt")

## Test data
Test_subjects   <- read.table("UCI HAR Dataset/test/subject_test.txt")
Test_df         <- read.table("UCI HAR Dataset/test/X_test.txt")
Test_activty    <- read.table("UCI HAR Dataset/test/y_test.txt")

## Train data
Train_subjects  <- read.table("UCI HAR Dataset/train/subject_train.txt")
Train_df        <- read.table("UCI HAR Dataset/train/X_train.txt")
Train_activity  <- read.table("UCI HAR Dataset/train/y_train.txt")

#### Add variable names to test and training data ####
names(Test_df)  <- as.character(Features_list$V2)
names(Train_df) <- as.character(Features_list$V2)

#### Select only the "mean" and "std" columns in both dataframes ####
Test_df2 <- Test_df[, grep("mean|std", as.character(Features_list$V2))]
Test_df2 <- Test_df2[, -grep("meanFreq", names(Test_df2))]

Train_df2 <- Train_df[, grep("mean|std", as.character(Features_list$V2))]
Train_df2 <- Train_df2[, -grep("meanFreq", names(Train_df2))]

#### Add columns to each df identifying activity, subject and Type = "Test" or "Train" ####

## Add Type
Test_df2$Type  <- "Test"
Train_df2$Type <- "Train"

## Add Activity

Test_df2$Activity  <- Test_activty$V1
Train_df2$Activity <- Train_activity$V1

## Add subjects

Test_df2$Subject  <- Test_subjects$V1
Train_df2$Subject <- Train_subjects$V1

#### Merge test and train data ####

HAR_df <- rbind(Test_df2, Train_df2)

#### Use adecuate variable names ####
var_names <- strsplit(names(HAR_df), "-")[-c(67:69)]

for(i in 1:66) {
    if(length(var_names[[i]]) < 3) {
        length(var_names[[i]]) <- 3
    }
}

var_matrix <- do.call(rbind, var_names)
var_matrix[, 2] <- gsub(pattern = "[()]", replacement = "",var_matrix[, 2])
Newvar_names <- paste(var_matrix[, 2], var_matrix[, 1], var_matrix[, 3], sep = "_")
Newvar_names <- gsub(pattern = "_NA", replacement = "", Newvar_names)

names(HAR_df)[1:66] <- Newvar_names

#### Use descriptive activity names ####

for(i in 1:6) {
    HAR_df$Activity[HAR_df$Activity %in% Activity_labels$V1[i]] <- as.character(Activity_labels$V2[i])
}

#### Secon data set with average of each variable for each activity and subject ####

HAR_df_av <- aggregate(. ~ Subject + Activity, data = HAR_df[, -67], FUN = mean)

#### Reacomodate and save the data sets ####

## Write the HAR data set
HAR_df <- HAR_df[, c(69, 68, 67, 1:66)]
write.table(HAR_df, "HAR.txt", row.names = F)

## Write the summarized data set
write.table(HAR_df_av, "Average_HAR.txt", row.names = F)
