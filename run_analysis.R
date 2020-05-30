project<- function(){
  #reading data from the different txt files
  activityTest  <- read.table("test/y_test.txt",header = FALSE)
  activityTrain <- read.table("train/y_train.txt",header = FALSE)
  subjectTrain <- read.table("train/subject_train.txt",header = FALSE)
  subjectTest  <- read.table("test/subject_test.txt",header = FALSE)
  featuresTest  <- read.table("test/X_test.txt" ,header = FALSE)
  featuresTrain <- read.table("train/X_train.txt",header = FALSE)
  dataFeaturesNames <- read.table("features.txt",header=FALSE)
  activityLabels <- read.table("activity_labels.txt",header = FALSE)
  
  #binding data from test and training
  dataSubject <- rbind(subjectTrain, subjectTest)
  dataActivity<- rbind(activityTrain, activityTest)
  dataFeatures<- rbind(featuresTrain, featuresTest)
  
  #naming conveniently the data
  names(dataSubject)<-c("subject")
  names(dataActivity)<- c("activity_number")
  names(dataFeatures)<- dataFeaturesNames$V2
  names(activityLabels)<-c("activity_number","activity")
  
  #joining different data in a unique table
  dataCombine <- cbind(dataSubject, dataActivity)
  Data <- cbind(dataFeatures, dataCombine)
  
  #taking only data referring to mean or standard deviation
  subdataFeaturesNames<-dataFeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", dataFeaturesNames$V2)]
  selectedNames<-c(as.character(subdataFeaturesNames), "subject", "activity_number" )
  Data<-subset(Data,select=selectedNames)
  
  #merging previous data with table "activity_labels" by "activity_number"
  Data<-merge(Data,activityLabels,by="activity_number")
  
  #rename some coloumn from the data.table created
  names(Data)<-gsub("^t", "time", names(Data))
  names(Data)<-gsub("^f", "frequency", names(Data))
  names(Data)<-gsub("Acc", "Accelerometer", names(Data))
  names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
  names(Data)<-gsub("Mag", "Magnitude", names(Data))
  names(Data)<-gsub("BodyBody", "Body", names(Data))
  
  #deleting coloumn "activity_number" that was duplicated
  Data$activity_number<-NULL
  
 # creating an independent tidy data set with the average of each variable for each activity and each subject.
  Data2<-aggregate(. ~subject + activity, Data, mean)
  Data2<-Data2[order(Data2$subject,Data2$activity),]
  write.table(Data2, file = "tidydata.txt",row.name=FALSE)
  }
