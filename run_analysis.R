## prepare the TIDY DATASET from Human Activity Raw Dataset
## Part of the peer assignment for Getting and Cleaning Data course from John Hopkins University
##
## Author: Rakesh Mohan
## Date: 24-Apr-2014
##
## How to use?
##  Raw data must have been downloaded and available in a sub directory
##  1.) source(path-to-this-file)
##  2.) run()
##  or  optionally ..
##  3.) do_plot(SUBJECT_ID, FEATURE_OFFSET)

library(data.table)
setwd(".");

##
test_count <- -1;
#test_count <- 100;    ##!!!!! FOR TEST ONLY !!!!!

data_directory = "UCI HAR Dataset";

activities <- NULL;
features <- NULL;
full_dataset <- NULL;

tidy_activity_mean_and_std <- NULL;
tidy_subject_activity_avg <- NULL;


run <- function(force_refresh=FALSE) {
  fetchData(force_refresh);
  datainfo();
}

write_octave <- function() 
{
  octave_set <- as.data.table(full_dataset);
  octave_set[,activity:=NULL];
  write.table(octave_set, file="octave_training.txt", append=FALSE, quote=TRUE, row.names=FALSE, col.names=FALSE, eol="\n", sep="\t");
  
  train_y <- as.matrix(as.integer(full_dataset$activity=="WALKING"));
  write.table(train_y, file="octave_walking.txt", append=FALSE, quote=TRUE, row.names=FALSE, col.names=FALSE, eol="\n", sep="\t");
  
  train_y <- as.matrix(as.integer(full_dataset$activity=="WALKING_UPSTAIRS"));
  write.table(train_y, file="octave_upstairs.txt", append=FALSE, quote=TRUE, row.names=FALSE, col.names=FALSE, eol="\n", sep="\t");

  train_y <- as.matrix(as.integer(full_dataset$activity=="WALKING_DOWNSTAIRS"));
  write.table(train_y, file="octave_downstairs.txt", append=FALSE, quote=TRUE, row.names=FALSE, col.names=FALSE, eol="\n", sep="\t");

  train_y <- as.matrix(as.integer(full_dataset$activity=="SITTING"));
  write.table(train_y, file="octave_sitting.txt", append=FALSE, quote=TRUE, row.names=FALSE, col.names=FALSE, eol="\n", sep="\t");

  train_y <- as.matrix(as.integer(full_dataset$activity=="STANDING"));
  write.table(train_y, file="octave_standing.txt", append=FALSE, quote=TRUE, row.names=FALSE, col.names=FALSE, eol="\n", sep="\t");

  train_y <- as.matrix(as.integer(full_dataset$activity=="LAYING"));
  write.table(train_y, file="octave_laying.txt", append=FALSE, quote=TRUE, row.names=FALSE, col.names=FALSE, eol="\n", sep="\t");
  
}

write_tidy <- function()
{
  print("Saving the tidy dataset(s) ...");
  write.table(full_dataset, file="tidy_full_dataset.txt", append=FALSE, quote=TRUE, row.names=FALSE, col.names=TRUE, eol="\n", sep=",")
  write.table(tidy_activity_mean_and_std, file="tidy_activity_mean_and_std.txt", quote=FALSE, append=FALSE, row.names=FALSE, col.names=TRUE, eol="\n", sep=",")
  write.table(tidy_subject_activity_avg, file="tidy_subject_activity_avg.txt", quote=FALSE, append=FALSE, row.names=FALSE, col.names=TRUE, eol="\n", sep=",")
}

read_tidy <- function()
{
  print("Reading the tidy dataset(s) ...");
  if(file.exists("tidy_activity_mean_and_std.txt")) tidy_activity_mean_and_std <<- read.table(file="tidy_activity_mean_and_std.txt",header=TRUE, sep=",");
  if(file.exists("tidy_subject_activity_avg.txt")) tidy_subject_activity_avg <<- read.table(file="tidy_subject_activity_avg.txt", header=TRUE, sep=",");  
  if(file.exists("tidy_full_dataset.txt")) read.table(file="tidy_full_dataset.txt", quote="\"", header=TRUE, sep=",");  
}

downloadData <- function()
{
  zipname = "UCI HAR Dataset.zip"; 
  if(!file.exists(zipname))
  {
    print("Downloading the dataset ...");
    url = "http://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.zip";
    download.file(url, zipname, quiet=FALSE);
  }
  if(!file.exists(zipname))
  {
    print(paste("!!!ERROR!!! - Could not ensure download zip archive",zipname, sep=" - "));
    return(FALSE);
  }
  print("Unzipping the dataset ...");
  unzip(zipname, exdir=".", overwrite=TRUE);
  return(TRUE);
}

#Central function to arranage and load the data
fetchData <- function(force_refresh=FALSE) {
  directory <- data_directory;
  if(!file.exists(directory))
  {
    if(!downloadData())
    {
      return();
    }
  }
  
  filename <- "activity_labels.txt"
  filepath <- paste(directory, filename, sep="/")
  activities <<- read.table(filepath, col.names=c("activity_id", "activity"))
  activities <<- as.data.table(activities)
  setkey(activities, "activity_id")
  #names(activities) <<- c("activity_id", "activity")  ## Added descriptive field names

  filename <- "features.txt"
  filepath <- paste(directory, filename, sep="/")
  features <<- read.table(filepath, col.names=c("feature_id", "feature"))
  #names(features) <<- c("feature_id", "feature")  ## Added descriptive field names
  
  if(!force_refresh) read_tidy()    # Reads already saved tidy data
  if(is.null(full_dataset) || is.null(tidy_activity_mean_and_std) || is.null(tidy_subject_activity_avg)) {
    refresh_tidy()       # Prepares and saves the tidy data
  }
}

datainfo <- function() {
  info <- data.table(setname=character(), rowcount=integer(), colcount=integer())
  #info[1, c("setname","rowcount", "colcount") := list(setname="test set", rowcount=dim(activities)[1], colcount=dim(activities)[2])]
  
  info <- rbind(info, list("Activity Labels (activities)", dim(activities)[1], dim(activities)[2]))
  info <- rbind(info, list("Features (features)", dim(features)[1], dim(features)[2]))
  info <- rbind(info, list("Tidy Full Set", dim(full_dataset)[1], dim(full_dataset)[2]))
  info <- rbind(info, list("Tidy Dataset #1", dim(tidy_activity_mean_and_std)[1], dim(tidy_activity_mean_and_std)[2]))
  info <- rbind(info, list("Tidy Dataset #2", dim(tidy_subject_activity_avg)[1], dim(tidy_subject_activity_avg)[2]))
  info
}

#### MAIN FUNCTION TO TIDY UP THE RAW DATA AND SAVE LOCALLY
refresh_tidy <- function() {
  
  print("Refreshing the tidy dataset(s) ...");
  
  directory <- data_directory;
  
  ## IN SUB-DIRECTORY "train/"
  filename <- "subject_train.txt";
  filepath <- paste(directory, "train", filename, sep="/");
  subject <- read.table(filepath, col.names=c("subject_id"), nrows=test_count);
  
  filename <- "X_train.txt";
  filepath <- paste(directory, "train", filename, sep="/");
  X <- read.table(filepath, nrows=test_count);
  names(X) <- features$feature;               ## Added descriptive field names from features data.table
  
  filename <- "y_train.txt";
  filepath <- paste(directory, "train", filename, sep="/");
  y <- read.table(filepath, col.names=c("activity_id"), nrows=test_count);

  ## IN SUB-DIRECTORY "test/"
  filename <- "subject_test.txt";
  filepath <- paste(directory, "test", filename, sep="/");
  subject <- rbind(subject, read.table(filepath, col.names=c("subject_id"), nrows=test_count));
  
  filename <- "X_test.txt";
  filepath <- paste(directory, "test", filename, sep="/");
  temp <- read.table(filepath, nrows=test_count);
  names(temp) <- features$feature;               ## Added descriptive field names from features data.table
  X <- rbind(X, temp);
  temp <- NULL;
  
  filename <- "y_test.txt";
  filepath <- paste(directory, "test", filename, sep="/");
  y <- rbind(y, read.table(filepath, col.names=c("activity_id"), nrows=test_count));
  
  ocount <- nrow(X);
  
  full_dataset <<- cbind(subject, activity_id=y$activity_id, activity=rep("",ocount), X);
  full_dataset <<- as.data.table(full_dataset);
  full_dataset[,activity:=activities[activity_id,]$activity];    #Populate activity name looked up from activities

  #Extract Tidy Data Set #1 for ACTIVITY + MEAN + STD DEV fields
  mean_std_fields <- names(full_dataset)[grep("-std()|-mean()", names(full_dataset))];
  tidy_activity_mean_and_std <<- subset(full_dataset, select=c("activity",mean_std_fields));
  
  #Extract Tidy Data Set #1 for SUBJECT_ID + ACTIVITY + AVG fields
  #avg_fields <- names(full_dataset)[grep("-mean()", names(full_dataset))];
  tidy_subject_activity_avg <<- subset(full_dataset, select=c("subject_id","activity",mean_std_fields));
  #summarize all numeric columns, grouped by subject & activity
  library(plyr);
  tidy_subject_activity_avg <<- ddply(tidy_subject_activity_avg, .(subject_id,activity), numcolwise(mean));
  
  write_tidy();
}


#Utility Function to perform descriptive analysis and visualize the data for validations
do_plot <- function(subjid, feature_offset=7) {
  if(is.null(full_dataset)){run()}
  subject_set <- as.data.frame(subset(full_dataset,subject_id==subjid))  ## subset training dataset for subject's data
  sscount <- nrow(subject_set)
  
  plot.new()
  par(mar=c(5,4,4,8)+.1)

  plot(1:sscount, subject_set$activity_id, type='l',lwd=2, col="blue", xlab="", ylab="", yaxt="n", xlim=c(1,sscount), ylim=c(-10,6))
  #legend("topright", legend=rev(activities$activity), cex=.5, pt.cex = 1)
  
  #plot(1:sscount, subject_set[,c(2)], type='l', ylab="Activities", xlim=c(1,sscount), ylim=c(-10,6))
  axis(4, at=activities$activity_id,labels=activities$activity,col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
  mtext("Activities", side=4, padj=3, adj=0, line=1, cex.lab=1,las=1, col="blue")
  
  par(new=TRUE)
  
  #plot(1,type='n',xlim=c(1,10),ylim=c(-1,0.2),xlab='feature', ylab='value')
  plot(1:sscount, type='l', xlim=c(1,sscount),ylim=c(-1.6,1.6),xlab=paste('Summarized Observations (',as.character(nrow(subject_set)),')',sep=""),ylab='Feature Values')
#  axis(2)
  clrs <- c("red", "darkorange", "darkgreen", "skyblue", "green")
  ftr_legend <- c()
  col_names <-  names(subject_set)
  for (i in 1:3){
    #lines(data.frame.for.this.category, type='o', col=sample(rainbow(10)), lwd=2)
    #sset <- subject_set[activity==activity]
    lines(1:sscount, subject_set[,i+feature_offset], type='l', col=clrs[i], lwd=1)
    ftr_legend = c(ftr_legend, col_names[i+feature_offset])
    #lines(1:sscount, subject_set[,c(7)], type='l', col="#FF0000FF", lwd=1)
    #lines(1:sscount, subject_set[,c(8)], type='l', col="#0000FFFF", lwd=1)
    #lines(1:sscount, subject_set[,c(9)], type='l', col="#00FF00FF", lwd=1)
    #lines(1:sscount, subject_set[,7:9], type='l', col=c("#FF0000FF", "#0000FFFF", "#00FF00FF"), lwd=1)
  }
#print(ftr_legend)
  #legend(legend=)
  legend("bottomleft", legend=ftr_legend, cex=.75, pt.cex = 1, col=clrs, lwd=1, bty="n")
  title(paste("Human Activity Observations for Subject #",subjid))
  mtext(paste("features: ", paste(ftr_legend,collapse="; ")), side=3, las=1, padj=0, adj=0, line=0, cex=0.75, col="black")

  subject_set <<- subject_set
}