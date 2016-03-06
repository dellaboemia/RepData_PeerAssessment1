# Coursera Data Science Specialization
# Reproducible Research
# Peer Assessment Project 1
# process.R

# This script...

# Include requisite libraries
library(ggplot2)
library(plyr)
library(dplyr)

# Set script working directory and source functions
homeDir <-  Sys.getenv("PROJ_HOME")
rawScriptDir <- "/Code/Raw Scripts" 
setwd(file.path(homeDir, rawScriptDir))
source("func.R")

#############################################################################
##                    MEAN TOTAL NUMBER OF STEPS PER DAY                   ##
#############################################################################

# Extract complete cases from activities
completeCases <- activities[complete.cases(activities),]

# Summarizes activitities into steps by date
stepsByDay01 <- summate(x = completeCases, v = "date", f = "sum")

# Prepare, render and return histogram of total number of steps taken each day
stepsHist01 <- makeHist(stepsByDay01)

# Store histogram in appropriate figures folder. ver = "exploratory" will save the file in the exploratory figures folder
# ver = "final"  will save the plot in the final graphics folder
plotFile <- "stepsHist01.png"
ver <- "exploratory"
plotSave(stepsHist01, plotFile, ver)

# Calculate and report the mean and median total number of steps taken per day, place in data frame, then print it.
stepStats01 <- summary(stepsByDay01$steps)
print(stepStats02)



#############################################################################
##                    AVERAGE DAILY ACTIVITY PATTERN                       ##
#############################################################################

# Summarize activities into average steps taken per 5 minute interval
stepsByInterval <- summate(completeCases, "interval", "mean")

# Prepare, render and return time series plot of steps taken per interval
stepTimeSeries <- makeTimeSeries(stepsByInterval)

# Store line chart in approprite figures folder
plotFile <- "stepTimeSeries.png"
ver <- "exploratory"
plotSave(stepTimeSeries, plotFile, ver)

# Obtain the 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps?
maxInterval <- calcInterval(stepsByInterval)
print(paste("The 5-Minute time interval with the highest average number of steps begins at", maxInterval))



#############################################################################
##                    IMPUTING MISSING VALUES                              ##
#############################################################################

# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
missingValues <- sum(is.na(activities$steps))
print(paste("There are",missingValues,"rows with NAs in the steps variable"))

# Create a new activities data set with the missing values filled in.
newActivities <- impute(activities, stepsByInterval)

# Summarizes activitities into steps by date
stepsByDay02 <- summate(x = newActivities, v = "date", f = "sum")

# Prepare, render and return histogram of total number of steps taken each day
stepsHist02 <- makeHist(stepsByDay02)

# Store histogram in appropriate figures folder. ver = "exploratory" will save the file in the exploratory figures folder
# ver = "final"  will save the plot in the final graphics folder
plotFile <- "stepsHist02.png"
ver <- "exploratory"
plotSave(stepsHist02, plotFile, ver)

# Calculate and report the mean and median total number of steps taken per day, place in data frame, then print it.
stepStats02 <- summary(stepsByDay02$steps)
print(stepStats02)



#############################################################################
##                      DATA SET COMPARISON                                ##
#############################################################################

# Combine the raw and processed data frame into a single data frame with a new categorical variable for each row
# that identifies it as a raw or processed data.  All NA rows are removed. 
combined <- conjoin(stepsByDay01, stepsByDay02)


# Prepare, render and return a density plot comparing distributions of the raw and processed data 
density <- makeDensityPlot(combined)

# Store the density plot in approprite figures folder
plotFile <- "density.png"
ver <- "exploratory"
plotSave(density, plotFile, ver)


# Prepare, render and return a histogram plot comparing distributions of the raw and processed data 
stepsHist03 <- makeOverlaidHist(combined)

# Store the histogram in approprite figures folder
plotFile <- "stepHist03.png"
ver <- "exploratory"
plotSave(stepsHist03, plotFile, ver)



# Prepare, render and return a boxplot comparing distributions of the raw and processed data 
stepsBox <- makeBoxPlot(combined)

# Store the histogram in approprite figures folder
plotFile <- "stepBox.png"
ver <- "exploratory"
plotSave(stepsBox, plotFile, ver)



#############################################################################
##                      WEEKEND/WEEKDAY PATTERN ANALYSIS                   ##
#############################################################################

# Create a new Weekday / Weekend factor variable 
newActivities <- dayLabel(newActivities)

# Summarize the data by by day label, and 5-Minute Interval Time
stepsByIntervalWd <- summateWd(newActivities)

#Create comparative time-series 
stepTimeSeriesWd <- makeTimeSeriesWd(stepsByIntervalWd)

# Store the time series in approprite figures folder
plotFile <- "stepTimeSeriesWd.png"
ver <- "exploratory"
plotSave(stepTimeSeriesWd, plotFile, ver)