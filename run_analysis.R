# read features and activity_labels files.
features <- read.table("UCI HAR Dataset/features.txt")
activitylabels <- read.table("UCI HAR Dataset/activity_labels.txt",
                             col.names = c("nact","act"))

# read the files from test and the data is assigned to a variable with the same
# name and assign a name to the columns.
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt",
                           col.names = "subject")  
featuresraw_test <- read.table("UCI HAR Dataset/test/X_test.txt", 
                               col.names = features$V2)
activity_test <- read.table("UCI HAR Dataset/test/y_test.txt",
                            col.names = "activity")

# read the files from train and the data is assigned to a variable with the same
# name and assign a name to the columns.
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt",
                            col.names = "subject")  
featuresraw_train <- read.table("UCI HAR Dataset/train/X_train.txt",
                                col.names = features$V2)
activity_train <- read.table("UCI HAR Dataset/train/y_train.txt",
                             col.names = "activity")

# loading the dplyr package
library(dplyr)

# combine subject and activity files for both records and assign the dataset 
# to tidydata_test/train.
tidydata_test <- cbind(subject_test,activity_test,featuresraw_test)
tidydata_train <- cbind(subject_train,activity_train,featuresraw_train)

# combine both tidydata and assign the dataset to tidydata_1.
tidydata_1 <- merge(tidydata_test,tidydata_train,all = TRUE)


# select the mean and std for each measurement. 
tidydata_2 <- select(tidydata_1,1,2,
                     names(tidydata_1)[grepl("mean|std",names(tidydata_1)) & 
                                         !grepl("Freq|angle",names(tidydata_1))])

#replace the activity number with the name of the activity. 
actn <- tidydata_2$activity
for (i in 1:6) {
  actn <- replace(actn,grep(i,actn),activitylabels$act[activitylabels$nact==i])
}
tidydata_3 <- mutate(tidydata_2,activity = actn)

# clear variable names
colnames(tidydata_3) <- tolower(gsub("\\.","",names(tidydata_3)))

# classifies the data for subjects and activity ans calculates the average for 
# each variable.
split(tidydata_3[-1],tidydata_3$subject)-> splitind
for (i in 1:30) {
  splitind[[i]] <- split(splitind[[i]][-1],splitind[[i]]["activity"])
  for (n in 1:6) {
    splitind[[i]][[n]] <- lapply(splitind[[i]][[n]], mean)
  }
}

# crates a data frame with the average of all the variables and add the 
# activities to the names
dataf <- NULL
dataf <- data.frame(dataf)
dataf <- merge(dataf,as.data.frame(splitind[[1]]),all = TRUE) 
for (i in 1:30) {
  dataf <- merge(dataf,as.data.frame(splitind[[i]]),all = TRUE)
}

#creates the final tidy data.
tidydata_4 <- cbind(data.frame(subject = 1:30),dataf)

# clear variable names
names_td4 <- paste("mean",tolower(gsub("_|\\.","",names(tidydata_4)[-1])),sep = "")
colnames(tidydata_4) <- c("subject",names_td4)

# creates a file .txt 
write.table(tidydata_4,file = "./mean.txt",row.names = FALSE,col.names = TRUE)
