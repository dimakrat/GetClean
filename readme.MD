Course Project - Getting and Cleaning Data
========

### The Goal of Project ###
Practice importing, cleaning, and organizing data

### Original Discription ###

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

1. Merge the training and the test sets to create one data set.
2. Extract only the measurements on the mean and standard deviation for each measurement.
3. Use descriptive activity names to name the activities in the data set 
4. Appropriately label the data set with descriptive variable names. 
5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

### Data source ###
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

### Files ###
* CodeBook.MD - Description of variables
* README.md - this file
* run_analysis.R - R Script
./UCI HAR Dataset/dtMean.txt- tidy dat aset of t with the average of each variable for each activity and each subject

## Process

### Original data files ###
* 'features_info.txt': Shows information about the variables used on the feature vector.
* 'features.txt': List of all features.
* 'activity_labels.txt': Links the class labels with their activity name.
* 'train/X_train.txt': Training set.
* 'train/y_train.txt': Training labels.
* 'test/X_test.txt': Test set.
* 'test/y_test.txt': Test labels.

### Combining data ###

 Imports all tables by **read.table()**. Combines files from folder "train" to data.table **'train'** and from folder **'test'**.  Combines to data.table **'test'** by **cbind()**. The result is 2 data sets ** 'test'** with **292'747** obs. **563** var.  and **'train'** with **7'352** obs. **563** var. 
 Combine **test** and **train** by **rbind()**. The result is one set of raw data with **'combo'** with **10'299** obs. **563** var.
 
### Subseting data ###
Extracts only the measurements on the mean and standard deviation for each measurement by searching **Mean** and **Std** in names of virables.
The result data.table **dtMean** with **180** obs. of **88** variables.

### Uses appropriately labels and  discriptive names ###

Removes all numbers at beginning of names.

Change special symbols

* '^t' to 'Time'
* 'f' to 'Frequency'
* ',' to '.'
* '()' to '.F'
* '(' ')' '-' to '.'

Convert all names to lower case.

### Create second dataset with average measurements ###

Creates a second, independent tidy data set with the average of each variable for each activity and each subject. Uses **aggregate()** and **mean()** to group data by **subject** and **activity**


Dmitry Ermakov
25 July 2014

