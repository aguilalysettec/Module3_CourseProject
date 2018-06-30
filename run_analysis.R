library(dplyr)

#Read test and train data
X_test<-read.table("./UCI HAR Dataset/test/X_test.txt")
Y_test<-read.table("./UCI HAR Dataset/test/y_test.txt")
sub_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")
X_train<-read.table("./UCI HAR Dataset/train/X_train.txt")
Y_train<-read.table("./UCI HAR Dataset/train/y_train.txt")
sub_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")

#Read activity labels and features
act_labels<-read.table("./UCI HAR Dataset/activity_labels.txt")
var_names<-read.table("./UCI HAR Dataset/features.txt")

#Merge the training and the test sets to create one data set
X<-rbind(X_test,X_train)
Y<-rbind(Y_test,Y_train)
sub<-rbind(sub_test,sub_train)
data<-cbind(X,Y,sub)

#Extract only the measurements on the mean and standard deviation for each measurement
var<-var_names[grep("mean\\(\\)|std\\(\\)",var_names[,2]),]
X<-X[,var[,1]]

#Use descriptive activity names to name the activities in the data set
colnames(Y)<-"act_label"
Y$activity<-factor(Y$act_label,labels=as.character(act_labels[,2]))
activity<-Y[,-1]

#Appropriately label the data set with descriptive variable names
colnames(X)<-var_names[var[,1],2]

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
colnames(sub)<-"subject"
total<-cbind(X,activity,sub)
overall_mean<-total%>%group_by(activity,subject)%>%summarize_each(funs(mean))
write.table(overall_mean,file="./UCI HAR Dataset/tidydata.txt",row.names=FALSE,col.names=TRUE)
