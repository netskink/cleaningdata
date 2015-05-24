# One of the most exciting areas in all of data science right now is wearable computing - 
# see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to 
# develop the most advanced algorithms to attract new users. The data linked to from the course
# website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. 
# A full description is available at the site where the data was obtained: 
  
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

# Here are the data for the project: 
  
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

# You should create one R script called run_analysis.R that does the following. 

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average 
#    of each variable for each activity and each subject.

#install.packages("stringr")

#install.packages(googleVis)
library(googleVis)
library(stringr)

# options
thedatadir="UCI HAR Dataset"
#thedatadir = "data"
# path seperator
path_seperator = "/"

thetestdatadir=paste(thedatadir,"test",sep=path_seperator)
thetraindatadir=paste(thedatadir,"train",sep=path_seperator)

if (!file.exists(thedatadir)){
  # I did my code on mac, if you are on some other OS and this 
  # path does not work, spaces in file, or perhaps you renamed it
  # just change the dir name to data or whatever you called it
  # when you did your work.  In the case of directory markers,
  # make that change as well. I'm not sure how it works on windows
  stop("The UCI HAR Dataset data directory must be in this directory.")
}

#############
# 
#  Configure Options for repitivive runs
#
###########

# Can omit reading the two big files from disk into memory/session each run to save time.
# i.e., once its read into memory you can simply use it and you can skip rereading it.
#
readBigFiles = TRUE# This setting will read the two large files into memory. Required for first run.
#readBigFiles = FALSE# This setting will use the existing tables already loaded into memory.
if (!readBigFiles && !exists("X_test")) {
  stop("X_test is undefined. Set readBigFiles to true for next run. Subsquent runs can be done with readBigFiles set to false.")  
}
if (!readBigFiles && !exists("X_train")) {
  stop("X_train is undefined. Set readBigFiles to true for next run. Subsquent runs can be done with readBigFiles set to false.")  
}

# Read in the labels
filename=paste(thedatadir,"activity_labels.txt",sep=path_seperator)
activityLabels=read.fwf(
  file=filename,
  skip=0,                           # number of initial lines to skip
  widths=c(1, 19)                   # two columns of width 1 and 19
)
# remove leading/trailing spaces
activityLabels$V2= str_trim(activityLabels$V2)



# Read in the features.
# I used a little sed to right justify the one and two digit numbers
filename=paste(thedatadir,"features.txt",sep=path_seperator)
features=read.fwf(
  file=filename,
  skip=0,                           # number of initial lines to skip
  widths=c(3, 40)                   # two columns of width 1 and 19
)


###########
#
#  Test Data
#
###########
# Read in the X_test.
filename=paste(thetestdatadir,"X_test.txt",sep=path_seperator)
if (readBigFiles) {
  X_test=read.fwf(
    file=filename,
    skip=0,                           # number of initial lines to skip
    widths=c(rep(16,561))
  )
}

# Read in the subject_test.
filename=paste(thetestdatadir,"subject_test.txt",sep=path_seperator)
subject_test=read.fwf(
  file=filename,
  skip=0,                           # number of initial lines to skip
  widths=2
)


# Read in the Y_test.
filename=paste(thetestdatadir,"y_test.txt",sep=path_seperator)
y_test=read.fwf(
  file=filename,
  skip=0,                           # number of initial lines to skip
  widths=1
)



###########
#
#  Train Data
#
###########
# Read in the X_train.
filename=paste(thetraindatadir,"X_train.txt",sep=path_seperator)
if (readBigFiles) {
  X_train=read.fwf(
    file=filename,
    skip=0,                           # number of initial lines to skip
    widths=c(rep(16,561))
  )
}

# Read in the subject_train.
filename=paste(thetraindatadir,"subject_train.txt",sep=path_seperator)
subject_train=read.fwf(
  file=filename,
  skip=0,                           # number of initial lines to skip
  widths=2
)


# Read in the y_train.
filename=paste(thetraindatadir,"y_train.txt",sep=path_seperator)
y_train=read.fwf(
  file=filename,
  skip=0,                           # number of initial lines to skip
  widths=1
)


# check to see that the sizes read match the descriptive text.
# Provided 
NumOfInstances = 10299
NumOfAttributes = 561
if ( NumOfInstances != ( dim(subject_test)[1]+dim(subject_train)[1] ) ) {
  stop("mismatch in subject_test and subject_train with documentation!")  
}
if ( NumOfInstances != ( dim(X_test)[1]+dim(X_train)[1] ) ) {
  stop("mismatch in X_test and X_train with documentation!")  
}
if ( NumOfInstances != ( dim(y_test)[1]+dim(y_train)[1] ) ) {
  stop("mismatch in y_test and y_train with documentation!")  
}
if ( NumOfAttributes !=  dim(features)[1] ) {
  stop("mismatch in features with documentation!")  
}
if ( NumOfAttributes !=  dim(X_test)[2] ) {
  stop("mismatch in X_test with documentation!")  
}
if ( NumOfAttributes !=  dim(X_train)[2] ) {
  stop("mismatch in X_train with documentation!")  
}

# Initial table reads are correct.

# so what is the data?
# According to the documentation there are 30 people who do six differnt activities.
# The six activities are listed in activityLabels.
# The 30 people are identified by number and they are split into two groups randomly.
# Group 1 is the SMALLER test group.  The groups are not equal in size.
# They are determined by the following code.
group1 = unique(subject_test)
group2 = unique(subject_train)
# verify the ids are 1..30
if (! identical(sort(c(group1$V1,group2$V1)),seq(1,30)) ) {
  stop("There is an error in the data for the two groups identifiers. There are not 30 unique people.")
}
# there are approximately 300 measurements for each person.

# verify the activities in the y lavels. It should be 1-6
activitiesGroup1 = unique(y_test)
activitiesGroup2 = unique(y_train)
if (! identical(sort(c(activitiesGroup1$V1)), seq(1,6) ) )   {
  stop("There is an error in the data for the group 1 activities. There are not 6 unique activities.")
}
if (! identical(sort(c(activitiesGroup2$V1)), seq(1,6) ) )   {
  stop("There is an error in the data for the group 2 activities. There are not 6 unique activities.")
}

# The measurements do not have column labels.  Add them to the dataframe.
# Also, first convert the factors to characters and trim the spaces so we can
# use them as column names more easily later.
features$V2 = as.character(features$V2)
features$V2 = str_trim(features$V2)
names(X_test) = features$V2
names(X_train) = features$V2

# We are only interested in the ones which have mean and STD like measurements
# At the command line, grep -c shows there are:
# 33 std labels
# 46 mean labels
stdLabels = features$V2[grepl(".*std",features$V2)]
meanLabels = features$V2[grepl(".*mean",features$V2)]

# verify our labels count matches our grep count
if (33 != length(stdLabels)  )   {
  stop("The label count for std is wrong.")
}
if (46 != length(meanLabels)  )   {
  stop("The label count for mean is wrong.")
}

# form new dataframes of just the mean and std measurements
# for both sets of test and train data.
meanTestData = subset(X_test,select=meanLabels)
stdTestData = subset(X_test,select=stdLabels)
meanTrainData = subset(X_train,select=meanLabels)
stdTrainData = subset(X_train,select=stdLabels)


#
# The activities do not have column labels.  Add them to the dataframe.
# We need to fix them up before we can prepend them.
#
names(y_test) = "ActivityCode"
names(y_train) = "ActivityCode"

# Now, we can prepend the columns for the activities
# to each set of data.  
meanTestData = cbind(y_test,meanTestData)
stdTestData = cbind(y_test,stdTestData)
meanTrainData = cbind(y_train,meanTrainData)
stdTrainData = cbind(y_train,stdTrainData)

# The activity names need to be added as rows corresponding to the
# activityCode
names(activityLabels) = c("ActivityCode","Activity")


#
# The people doing the test need to be prepended. Do the same thing
#

# add a column heading
names(subject_test) = "PersonCode"
names(subject_train) = "PersonCode"


# Now, we can prepend the columns for the people
# to each set of data.  
meanTestData = cbind(subject_test,meanTestData)
stdTestData = cbind(subject_test,stdTestData)
meanTrainData = cbind(subject_train,meanTrainData)
stdTrainData = cbind(subject_train,stdTrainData)


# Now assign the labels row so each activity has a readable name.
meanTestData = merge(activityLabels,meanTestData, by.x="ActivityCode", by.y="ActivityCode")
stdTestData = merge(activityLabels,stdTestData, by.x="ActivityCode", by.y="ActivityCode")
meanTrainData = merge(activityLabels,meanTrainData, by.x="ActivityCode", by.y="ActivityCode")
stdTrainData = merge(activityLabels,stdTrainData, by.x="ActivityCode", by.y="ActivityCode")






#
# Now lets form one big table with all the data in one place.
#
# lessons learned
# This approach looked promising but did not work
# http://stackoverflow.com/questions/25204859/error-while-merging-data-frames-using-data-table-package
# This approach did work
# http://stackoverflow.com/questions/14657104/r-merge-by-variable-column-with-duplicated-entry

# convert from data.frame to data.table

meanTestData<- transform(meanTestData, ID2=ave(PersonCode, PersonCode, FUN=seq_along))
stdTestData<- transform(stdTestData, ID2=ave(PersonCode, PersonCode, FUN=seq_along))


# combine the mean and STD data frames first
stdmeanTestData = merge(stdTestData,meanTestData, all.x=TRUE,sort=FALSE)
#stdmeanTestData = merge(stdTestData2,meanTestData2, allow.cartesian=TRUE)
#stdmeanTestData = merge(stdTestData,meanTestData,by=c("PersonCode","Activity","ActivityCode"),all=TRUE)

#
# Checks to make sure data is valid
#

# is the combined size correctly
if (dim(stdmeanTestData)[1] != dim(stdTestData)[1]) {
  stop("combined table size differs for test data in terms of rows.")
}
# add 4 for the common columns, activity, activityCode, personCode, and ID2 for sorting.
if ((dim(stdmeanTestData)[2]+4) != dim(stdTestData)[2]+dim(meanTestData)[2]) {
  stop("combined table size differs for test data.")
}

# Now check to see cells are identical between the premerge tables and the merged one
# use 28 cause I like that number and its less than 33. In terms of rows, use 123
chkColName = colnames(stdTestData)[28]  
if ( stdmeanTestData[[chkColName]][123] != stdTestData[[chkColName]][123] ) {
  stop("the combined std/mean test data does not match the original std test data")
}
chkColName = colnames(meanTestData)[28]  
if ( stdmeanTestData[[chkColName]][123] != meanTestData[[chkColName]][123] ) {
  stop("the combined std/mean test data does not match the original mean test data")
}

# This method works. Do the same for the Train subsets to form a stdmeanTrainData frame
meanTrainData<- transform(meanTrainData, ID2=ave(PersonCode, PersonCode, FUN=seq_along))
stdTrainData<- transform(stdTrainData, ID2=ave(PersonCode, PersonCode, FUN=seq_along))
stdmeanTrainData = merge(stdTrainData,meanTrainData, all.x=TRUE,sort=FALSE)



#
# Checks to make sure data is valid
#

# is the combined size correctly
if (dim(stdmeanTrainData)[1] != dim(stdTrainData)[1]) {
  stop("combined table size differs for train data in terms of rows.")
}
# add 4 for the common columns, activity, activityCode, personCode, and ID2 for sorting.
if ((dim(stdmeanTrainData)[2]+4) != dim(stdTrainData)[2]+dim(meanTrainData)[2]) {
  stop("combined table size differs for train data.")
}

# Now check to see cells are identical between the premerge tables and the merged one
# use 28 cause I like that number and its less than 33. In terms of rows, use 123
chkColName = colnames(stdTrainData)[28]  
if ( stdmeanTrainData[[chkColName]][123] != stdTrainData[[chkColName]][123] ) {
  stop("the combined std/mean train data does not match the original std train data")
}
chkColName = colnames(meanTrainData)[28]  
if ( stdmeanTrainData[[chkColName]][123] != meanTrainData[[chkColName]][123] ) {
  stop("the combined std/mean train data does not match the original mean train data")
}

#
#
# Now combine the two sets of data using row bind. Remove the unwanted columns:
#   o ID2 which we used to merge/join
#   o ActivityCode since the hw requires only meaninful label column labels
#
#

stdmeanData = rbind(stdmeanTrainData,stdmeanTestData)
# verify the # of instances matches the README
if ( dim(stdmeanData)[1] != 10299 ) {
  stop("the combined std/mean data does not match the readme number of observations.")
}

# remove ID2 column
stdmeanData$ID2 = NULL

# remove ActivityCode
stdmeanData$ActivityCode = NULL


# verify the # of columns matches what we set out to do
# 33 for number of std cols (variables)
# 46 for number of mean cols (variables)
# 1 for Activity
# 1 for Person
if ( dim(stdmeanData)[2] != 33+46+1+1) {  
  stop("the combined std/mean data does not match our desired goal.")
}


# Now create the dataframe of the means for each person's activities
person1Walking = subset(stdmeanData,PersonCode==1 & Activity == 'WALKING')
cols=seq(3,81)
l=lapply(person1Walking[cols],mean)

# Init a dataframe like what we want and then delete all the rows
tdresult <- data.frame(Person=as.numeric(character()),
                 Activity=character(),  
                 stringsAsFactors=FALSE) 
tdresult = rbind(tdresult,subset(stdmeanData,PersonCode==1 & Activity == 'WALKING'))
rows_to_keep = 1:dim(tdresult)[1]
tdresult = tdresult[-rows_to_keep,]

for (person in 1:30) {
  for (activity in 1:6)  {
    # i fixed the need for trim at beginning    
#    al = str_trim(as.character(activityLabels[activity,2]))
    al = activityLabels[activity,2]
    personByActivity = subset(stdmeanData,PersonCode==person & Activity == al)
    l=lapply(personByActivity[cols],mean)
    mydf <- do.call("rbind", lapply(l, data.frame))
    mydf=t(mydf)
    mydf = cbind(Activity = activityLabels[activity,2], mydf)
    mydf = cbind(PersonCode = person, mydf)
    tdresult=rbind(tdresult,mydf)

  }
}

# Write the tiny data dataframe result for the grader
write.table(tdresult,row.name=FALSE,file="mytdresult.txt")

# Write the codebook


# Readback the result and examine to see that it is correct
check_result=read.table("mytdresult.txt",header=TRUE)
gvt = gvisTable(check_result)
plot(gvt)





