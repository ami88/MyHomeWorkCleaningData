# Beginning of the script
# I am already inside of the unzipped file that I downloaded
#> dir()
#[1] "activity_labels.txt" "features_info.txt"   "features.txt"        "README.txt"         
#[5] "run_analysis.R"      "test"                "train"              
library(dplyr)
library(reshape2)
# 1. Merges the training and the test sets to create one data set.
# Reading my sets of files
##---- Auxiliary files with activities and features
# Note for me: Not sure whether I will use it, but I put them anyway)
myacti = read.csv("activity_labels.txt", header=FALSE)
myfeat = read.csv("features.txt", header=FALSE, sep="")
dim(myacti) ; dim(myfeat)
#[1] 6 1
#[1] 561   2

##----- Training Set---------------
trainset <- read.csv("train/X_train.txt", sep="", header=FALSE)
trainsub <- read.csv("train/subject_train.txt", sep="", header=FALSE)
trainy <- read.csv("train/y_train.txt", sep="", header=FALSE)
trainall <- cbind(trainset,trainsub,trainy)
##----- Test Set---------------
testset <- read.csv("test/X_test.txt",  header=FALSE, sep="")
testsub <- read.csv("test/subject_test.txt", sep="", header=FALSE)
testy <- read.csv("test/y_test.txt", sep="", header=FALSE)
testall <- cbind(testset,testsub,testy)
#--------- Merge training and test sets together
allset <- rbind(trainall,testall)
# Given decent names to the columns
names(allset)[1:dim(trainset)[2]] <- as.character(myfeat$V2)
names(allset)[dim(trainset)[2] + 1] <- "subject"
names(allset)[dim(trainset)[2] + 2] <- "activity_label"

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# Vector with the index of all the columns that have "mean" or "std"
# in their names. 
idx <- c(grep("mean",names(allset)),grep("std",names(allset)))
mysel<- allset[,idx]
#> dim(mysel)
#[1] 10299    79

# 3. Uses descriptive activity names to name the activities in the data set
# I will try with merge
# 1st, I divide the colum in myacti in two columns:
# the label, and the activity
tempo <- strsplit(as.character(myacti$V1),' ') 
myactinew <- data.frame(do.call(rbind, tempo))
names(myactinew)[1] <- "activity_label"
names(myactinew)[2] <- "activity"
# Make sure that activity_label is numeric in both
myactinew$activity_label <- as.numeric(myactinew$activity_label)
allset$activity_label <- as.numeric(allset$activity_label)
allsetmerged <- merge(allset,myactinew,
                      by.x="activity_label", by.y="activity_label",
                      all.x = TRUE)
# Since we already have a column with the activity,
# we can remove the column with the activity identification
allset2 <- subset(allsetmerged, select = -activity_label)

# I suspect that the following steps are not done to all the data or only "mysel"
# I do the same to "mysel". I keep the other thing, just in case, I need to correct it.

mysel$activity_label <- allset$activity_label
mysel$subject <- allset$subject
myselmerged <- merge(mysel,myactinew,
                     by.x="activity_label", by.y="activity_label",
                     all.x = TRUE)
mysel2 <- subset(myselmerged, select = -activity_label)


# 4. Appropriately labels the data set with descriptive variable names.
# Already done

# 5. From the data set in step 4, creates a second, independent tidy data
# set with the average of each variable for each activity and each subject.

# Again, since I am not sure if I need to save in the final table all the variables or only
# those with *mean* and *std*, I do both in parallel.
mysel2mean <- aggregate(mysel2[,1:as.numeric(dim(mysel2)[2]-2)],
                        by=list(subject = mysel2$subject, activity = mysel2$activity),
                        mean)
allset2mean <- aggregate(allset[,1:as.numeric(dim(allset)[2]-2)],
                         by=list(subject = allset$subject, activity = allset$activity),
                         mean)
# Write the data frames in csv files
write.csv(mysel2mean, file = "tidy_selection.txt",row.names = FALSE)
write.csv(allset2mean, file = "tidy_all.txt",row.names = FALSE)

