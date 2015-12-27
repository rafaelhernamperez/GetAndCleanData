library("dplyr")

# This method inititates the execution of the project
run_analysis <- function() {

	# --------------------------------------------------------------------
	# STEP 1: Merges the training and the test sets to create one data set
	# --------------------------------------------------------------------
	print("Merging train data...")
	data <- mergeData("train")
	print(paste(nrow(data), " rows x ", ncol(data), " columns", sep=""))

	# Saves the current dataset to disc
	fileName <- paste("merge_data.txt", sep="")
	write.table(data, fileName, sep=" ", col.names=TRUE, row.names=FALSE)

	print("")
	print("Merging test data...")
	data <- mergeData("test")
	print(paste(nrow(data), " rows x ", ncol(data), " columns", sep=""))

	# Saves the current dataset to disc, adding them to the end of the file
	write.table(data, fileName, append=TRUE, sep=" ", col.names=FALSE, row.names=FALSE)

	# Load the train and test data joined
	print("")
	print("Joining train and test data")
	data <- read.table(fileName, sep=" ", header=TRUE)
	print(paste(nrow(data), " rows x ", ncol(data), " columns", sep=""))


	# ----------------------------------------------------------------------------------------------
	# STEP 2: Extracts only the measurements on the mean and standard deviation for each measurement
	# ----------------------------------------------------------------------------------------------
	print("")
	print("Extracting mean and standard deviation...")
	meanData <- select(data, activity_id, activity, subject, contains("mean"), contains("std"))
	print(paste(nrow(meanData), " rows x ", ncol(meanData), " columns", sep=""))

	# CONTROL POINT Saves the current dataset to disc
	write.table(meanData, "mean_data.txt", sep=" ", col.names=TRUE, row.names=FALSE)

	# -------------------------------------------------------------------------------------
	# STEP 5: From the data set in step 4, creates a second, independent tidy data set with 
	# the average of each variable for each activity and each subject
	# -------------------------------------------------------------------------------------
	print("")
	print("Grouping data by activity and subject...")
	# groupData <- group_by(meanData, activity, subject)
	groupData <- group_by(meanData, subject, activity)

	print("")
	print("Summarising data by activity and subject...")
	resultData <- summarise_each(groupData, funs(mean))
	print(paste(nrow(resultData), " rows x ", ncol(resultData), " columns", sep=""))

	# CONTROL POINT Saves the current dataset to disc with header
	write.table(resultData, "resultset.txt", sep=" ", col.names=TRUE, row.names=FALSE)

	# Saves the result to disc without header
	write.table(resultData, "result_data.txt", sep=" ", col.names=FALSE, row.names=FALSE)

	# Removes objects from memory in order to optimize resources
	rm(data, meanData, groupData)

	resultData	
}

# This method merges the different data in order to create a consolidated dataset
# This method is used to get the dataset for train and test datasets
# For that:
#    - Get the train main data (X_nnn.txt file)
#    - Get the column names, make its transformations and assign to the dataset
#    - Get the subject data (subject_nnn.txt file) and assign it to the dataset
#    - Get the activity IDs (y_nnn,txt file) and assign it to the dataset
#    - Get the activity names (activity_labels.txt)
#    - Join the dataset with the actiity names
# PARAMETERS:
#    - typeData: 'train' or 'test'
mergeData <- function(typeData) {
	# Prepare the input files
	#prepareInputFiles()
 
	# Get the main Data
	fileName <- paste("./UCI HAR Dataset/", typeData, "/X_", typeData, ".txt", sep="")
	mainData <- read.table(fileName, sep="", header=FALSE)

	# Get the column names and normalize 
	names(mainData) <- prepareColumnNames()

	# Get the Subject data and add it to the data frame
	fileName <- paste("./UCI HAR Dataset/", typeData, "/subject_", typeData, ".txt", sep="")
	subjectData <- read.table(fileName)

	mainData$subject <- subjectData[,1]

	# Get the Y data (activity IDs) and add it to the data frame
	fileName <- paste("./UCI HAR Dataset/", typeData, "/y_", typeData, ".txt", sep="")
	activityData <- read.table(fileName)
	names(activityData) <- c("activity_id")
	mainData$activity_id <- activityData$activity_id

	# -----------------------------------------------------------------------------
	# STEP 3 Uses descriptive activity names to name the activities in the data set
	# -----------------------------------------------------------------------------
	# Combine the activitiy names to the data frame (JOIN)
	activities <- read.table("./UCI HAR Dataset/activity_labels.txt", sep=" ", header=FALSE)
	names(activities) = c("id", "activity")

	mainData <- merge(mainData, activities, by.x = "activity_id", by.y = "id", all=TRUE)

	# Removes objects from memory in order to optimize resources
	rm(activityData, activities, subjectData)

	mainData
}

# This method reads the column names from features.txt file and
# transforms the names into a legible format in order to 
# assign them to the result data frame
# 
# -------------------------------------------------------------------------
# STEP 4: Appropriately labels the data set with descriptive variable names
# -------------------------------------------------------------------------
prepareColumnNames <- function() {
	# Read the features.txt file in order to get the original column names
	dfNames <- read.table("./UCI HAR Dataset/features.txt", sep=" ", header=FALSE)

	# New vector to store the names in a legible format
	colNames <- c()

	# Explore the names in order to transform them to legible format variable
	for (i in dfNames[,2]) {
		# Replaces punctuation characters with "_"
		# Punctuation characters: !"#$&'()*+,-./:;<=>?@[\]^_`{|}
		sName <- gsub("[[:punct:]]", "_", i)

		# Add the transformed name to the vector
		colNames <- c(colNames, sName)
	}

	# Look for duplicated names in the new vector. Transforms the duplicity
	# creating a new verion of the name
	for (i in 1:length(colNames)) {
		n <- 1  # number of ocurrences

		for (x in 1:length(colNames)) {
			# if duplicated, adds the number of occurrences as sufix
			if (colNames[x] == colNames[i] && x != i) {
				n <- n + 1
				colNames[x] = paste(colNames[x], "_", as.character(n), sep="")
				# print(paste("New name: ", colNames[x], sep=""))
			}
		}
	}

	# Return the vector with the correct names
	colNames
}


# This method prepares the files before its process and reading
# The original data can't be read, because the values are separated
# by one blank space (if it is negative) or two blank spaces (if
# it is positive). The separators are not homogeneous. The data is
# formatted in order to be legible for human, aligning the columns.
# read.table() needs an homogeneus separator, and this separator
# must have explicity a length of one character. 
prepareInputFiles <- function() {
	#filePrefixes <- c("body_acc", "body_gyro", "total_acc")
	#fileCoords <- c("x", "y", "z")
	typeData <- c("test", "train")

	for (td in typeData) {
		# Prepare raw data from files allocated at Internal Signals directory
		#fullPath <- paste("./Project/", td, "/Inertial Signals", sep="")
		#for (fp in filePrefixes) {
		#
		#	for (coord in fileCoords) {
		#		fileName <- paste(fp, "_", coord, "_", td, ".txt", sep="")
		#
		#		prepareFile(fullPath, fileName)
		#		print(paste("Preparing file... '", fullPath, "/", fileName, "'", sep=""))
		#	}
		#}

		# Process the X_ files
		fullPath <- paste("./UCI HAR Dataset/", td, sep="")
		fileName <- paste("X_", td, ".txt", sep="")
		#print("")
		print(paste("Preparing file... '", fullPath, "/", fileName, sep=""))
		#print("")
		#print("")

		prepareFile(fullPath, fileName)
	}	
}


# This function takes a file name, opens it, reads each line, 
# transform the separators to valid separators, and write a
# new file, with the correct format.
# In the original format, the separations between columns can
# be one or two blank spaces, depending of if then value is
# positive (two blank spaces) or negative (one blank space).
# The original format allows you to read the columns aligned,
# but the read file functions (like read.table()), requires
# only one character for separators (sep argument), and for
# this reason the reading of the file does not work
# The method generates a second version of the file, with the
# correct format, with the prefix "ok_"
#
# PARAMETERS:
#    - fullPath: String with the full path to the file to prepare 
prepareFile <- function(pathName, fileName) { 
	# Reads input file
	# Full path of the input file
	inputFile <- paste(pathName, "/", fileName, sep="")

	inputConn <- file(inputFile, "r")  # Establishes the connection to file
	inputData <- readLines(inputConn)  # Reads the content of file
	close(inputConn)  # Closes the connection to file

	outputFile <- paste(pathName, "/ok_", fileName, sep="")
	outputConn <- file(outputFile, "w")  # Establishes the connection to file

	# Transform each row, replacing separators with the correct separator
	for (i in 1:length(inputData)) {
		# Replaces 2 spaces with 1 space
		inputData[i] <- gsub('  ', ' ', inputData[i])

		# Adds CRLF char to the end of row
		# inputData[i] <- paste(inputData[i], "\n")

		# Writes and adds the current row to new file
		writeLines(inputData[i], outputConn) #, sep="")
	}
	
	close(outputConn)  # Closes the connection to the file
}

