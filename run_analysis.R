################################
##1. Merges the training and the test sets to create one data set
################################
##Loading datasets
testset <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
trainset <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
subjectTest <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
subjectTrain <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
activityTest <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
activityTrain <- read.table("./data/UCI HAR Dataset/train/y_train.txt")

##
fullset <- rbind(testset,trainset)
fullSubject <- rbind(subjectTest,subjectTrain)
fullActivity <- rbind(activityTest, activityTrain)

################################
##4. Appropriately labels the data set with descriptive variable names. 
################################
columnNames <- read.table("./data/UCI HAR Dataset/features.txt")
names(fullset) <- columnNames[,2]
names(fullSubject) <- "Subject"
names(fullActivity) <- "Activity"

fullset <- cbind(cbind(fullSubject,fullActivity),fullset)

################################
##2. Extracts only the measurements on the mean and standard deviation for each measurement. 
################################

##Column names that contain mean() or std() as part of the name. 
columnMean <- grep('mean()',names(fullset), fixed = T)
columnStd <- grep('std()',names(fullset), fixed = T)
columnStdAndMean <- c(columnMean, columnStd)

##Addition of columns 1 and 2 (Subject and Activity)
columnStdAndMeanExtended <- c(1,2,columnStdAndMean)

fullset<- fullset[,columnStdAndMeanExtended]

################################
##3. Uses descriptive activity names to name the activities in the data set
################################

activityLabels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")

##Set column names
colnames(activityLabels) = c("Activity", "ActivityLabel")
## Join by Activity
fullset <- join(fullset,activityLabels, by = "Activity")

################################
##5. tidy data set with the average of each variable for each activity and each subject.
################################
##In the new set Subject, Activity and Act. Label are considered as ID and the columns which index are stored in columnStdAndMean 
##are considered as measures
tidyset <- melt(fullset,
                id=c("Subject","Activity", "ActivityLabel"), 
                measures.vars= columnStdAndMean,
                variable.name = "Measure_name")


AverageTidyset <- dcast(tidyset, Subject +  ActivityLabel ~ Measure_name, mean  )

################################
##Final steps
################################
##Export as txt
write.table(AverageTidyset,file = "AverageMeanStd.txt", row.names = F)