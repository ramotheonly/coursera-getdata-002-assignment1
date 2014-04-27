coursera-getdata-002-assignment1
================================

Peer Assignment for Getting and Cleaning Data course on John Hopkins Data Science track

Main script: run_analysis.R

How to run?:

 * > source("path-to-run_analysis.R)
 * > run()
 
Output: 2 tidy datasets are generated from the raw dataset

 * Tidy dataset #1: tidy_activity_mean_and_std
 * Tidy dataset #2: tidy_subject_activity_avg
 
How does the script work?

* Downloads the file from internet if zip archive is not available
* Unzips the archive in data source directory
* Reads the training dataset
* Merges the test dataset
* Sets the descriptive field names 
* creates to subsets for the tidy datasets