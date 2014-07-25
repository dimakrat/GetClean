##  Course Project - Getting and Cleaning Data
##  1.Merges the training and the test sets to create one data set.
##  2.Extracts only the measurements on the mean and standard deviation for each measurement. 
##  3.Uses descriptive activity names to name the activities in the data set
##  4.Appropriately labels the data set with descriptive variable names. 
##  5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

## Set working directory 
setwd("/Users/dimakrat/Documents/Study Data Sience/Getting and Cleaning Data/Course Project/UCI HAR Dataset")

##################
## Getting data ##
##################
cNames = readLines("features.txt")

## read training data ##
trainX = read.table("./train/X_train.txt")
names(trainX) <- cNames

trainY = read.table("./train/Y_train.txt")
names(trainY) <- "actID"

trainSubj = read.table("./train/subject_train.txt")
names(trainSubj) <- "subject"

## read test data ##
testX = read.table("./test/X_test.txt")
names(testX) <- cNames

testY = read.table("./test/Y_test.txt")
names(testY) <- "actID"

testSubj = read.table("./test/subject_test.txt")
names(testSubj) <- "subject"

###########################################
## Combining tables to one table "combo" ##
###########################################

train <- cbind(trainX,trainSubj,trainY)
test <- cbind(testX,testSubj,testY)
combo <- rbind(test,train)  # test 292747 obs. 563 var. + train 7352 obs. 563 var.
                            # combo 10299 obs. 563 var.
rm(test,train,testX,testSubj,testY,trainX,trainSubj,trainY)

## read activity data ##
actLabels = read.table("activity_labels.txt")
names(actLabels) = c("actID","activity")

combo <- merge(combo, actLabels, by="actID")

###############################################
## Extract Mean value and Standard deviation ##
###############################################

dt <- combo[,grep("[Mm]ean|[Ss]td",names(combo))]

######################################
##    Uses descriptive names        ##
######################################

cNames <- names(dt)

## Remove leading numbers
cNames <- strsplit(cNames,"^[0-9]{1,3} ")
secondElement <- function(x){x[2]}
cNames <- sapply(cNames,secondElement)

# remove special characters  ##

cNames <- gsub("^t","Time.",cNames)             # 't' to 'Time'
cNames <- gsub("^f","Frequency.",cNames)        # 'f' to 'Frequency'
cNames <- gsub("\\,",".",cNames)                # ',' to '.'
cNames <- gsub("\\(\\)","",cNames)              # '()' to '.F'
cNames <- gsub("\\(|\\)|-",".",cNames)          # '(' ')' '-' to '.'
cNames <- gsub("\\.\\.",".",cNames)             # '..' to '.'
cNames <- gsub("\\.$","",cNames)                # remove '.' at end of string
cNames <- gsub(".tBody",".Time.Body",cNames)    # '.tBody' to '.Time.Body'

cNames <- tolower(cNames)

## Assign new names
names(dt) <- cNames
rm(cNames,actLabels)

## Add  columns  
dt <- cbind(dt,combo[c("subject", "activity")])

#####################################################
## Create second dataset with average measurements ##
#####################################################
colN <- ncol(dt)-2
dtMean <- aggregate(dt[,1:colN],by = dt[c("subject","activity")], FUN=mean)

rm(dt,colN, secondElement)
write.table(dtMean, file="dtMean.txt")
print("The script has done. Result in dtMean.txt")



