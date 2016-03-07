# Coursera Data Science Specialization
# Reproducible Research
# Peer Assessment Project 1
# Author: John James
# Date: March 5, 2016
# process.R

###############################################################################################################################
# INTRODUCTION
#
# It is now possible to collect a large amount of data about personal movement using activity monitoring 
# devices such as a Fitbit, Nike Fuelband, or Jawbone Up. This script performs an exploratory analysis of personal activity
# data to ascrtain patterns of behavior.
#
#
# QUESTION
#
# This analysis processes personal activity data obtained from a group  of anonymous individuals, collected on 5 minute intervals over the
# two month period from October thru November 2012.  This analysis seeks to address the following questions:
#
# 1. What is mean total number of steps taken per day?
# 2. What is the average daily activity pattern?
# 3. How do these observations change with imputed data?
# 4. Are there differences in activity patterns between weekdays and weekends?
#
#
# DATA
#
# This project makes use of data from a personal activity monitoring device. This device collects data at 
# 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual 
# collected during the months of October and November, 2012 and include the number of steps taken in 5 minute 
# intervals each day
#
# The data for this assignment was downloaded from the course web site: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
#
# The variables included in this dataset are:
#   steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
#   date: The date on which the measurement was taken in YYYY-MM-DD format
#   interval: Identifier for the 5-minute interval in which measurement was taken
#
# The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.
#
#
# ORGANIZATION
#
# The files for the analysis are organized as follows:
#   Data
#     - Raw Data - Data obtained from the course website.  This data has been read only and has not been manipulated
#     - Processed Data - Data with missing values imputed 
#   Figure
#     - Exploratory Figures - Figures used for exploratory analysis 
#     - Final Figures - Figures included in the final analysis
#   Code
#     - Raw Scripts - scripts used for experimentation purposes that are not considered final
#     - Final Scripts - final versions of scripts used for this analysis.  The scripts used are as follows
#           - load.R - functions used to load the raw and processed data into the environment
#           - func.R - functions used to analyze the data
#           - process.R - master script that runs the analysis from beginning to end 
#     - R Markdown - Markdown files used to report the analysis of the project
#   Text
#     - Text of the analysis / report
#
#
# EXPLORATORY ANALYSIS
#
# The script has been organized as follows. 
#     0.  User Instructions - Information required by the script from the user  
#     1.  Housekeeping - Initializes working directories, sources files, and indicates which parts of the analysis is to be performed.
#     2.  Load Data - Load the raw data into the environment 
#     3.  Mean Total - Calculates mean total steps per day and produces a histogram
#     4.  Activity Pattern - Prepares a time series of activity, in terms of steps per day for each 5 minute interval
#     5.  Impute Values - Missing values are replaced by the average number of steps per day across all days for the missing time interval
#     6.  Comparison - Compared density, histogram and box plots for the raw and processed data are prepared 
#     7.  Weekend/Weekday - Time series line charts are prepared for both weekday and weekends.
#
# Here we go!
##################################################################################################################################




#############################################################################
##                            USER INSTRUCTIONS                            ##
#############################################################################
# ---- initialize
# Step 1: Working Directory
# Note: This script assumes that your current working directory is your project home directory.  If you haven't 
# already done so, set your working directory accordingly.  All subordinate data and figure directories will be created 
# for you.  If using R Studio, you may set your working directory as follows: 
#           Go to menu bar
#           Click "Sessions"
#           Select "Set Working Directory"
#
# sTEP 2: Select Process Steps
# Please select the process steps you wish to execute.  The default is that all processing steps will be executed.  To 
# skip a process, set the flag to FALSE.  Note, that each process depends upon the prior process having been executed at
# least once.  
#
  loadData = TRUE
  meanAnalysis = FALSE
  activityPattern = FALSE
  imputeData = FALSE
  compare = FALSE
  wdAnalysis = FALSE
#
# Step 3: Set data directory, URL for the data source, and file names
#
  dataUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"   # Link to data source
  rawDataDir      <- "/Data/Raw Data"     # Directory where raw data is to be stored 
  dataZipFile     <- "activity.zip"       # Name of zip file (get this from the data source code book)
  dataCsvFile     <- "activity.csv"       # Name of csv file (get this from the data source code book)
  procDataFrame   <- "procActivities"     # Name of data frame in which the processed data is stored
#
#
# Step 4: Is this exploratory or final analysis
#  This determines the directories from which, scripts will be sourced and figures will be placed.
  exploratory = FALSE
# ---- end

  
#############################################################################
##                              HOUSEKEEPING                               ##
#############################################################################
# ---- housekeeping

# Store project top level working directory in the global environment
homeDir <- getwd()
Sys.setenv("PROJ_HOME"= homeDir)

# Set directory from which scripts will be sourced, then source them
if (exploratory) {scriptDir <- "/Code/Raw Scripts" } else {scriptDir <- "/Code/Final Scripts" }
#setwd(file.path(homeDir, scriptDir))

# Load custom functions files
source("func.R")
source("load.R")

# Set figure directory to which figures will be placed.
if (exploratory) {figDir  <- "/Figure/Exploratory Figures" } else {figDir <- "/Figure/Final Figures" }

# Include requisite libraries
library(ggplot2)
library(plyr)
library(dplyr)
library(gridExtra)
library(grid)
library(downloader)

# Reset working directory
setwd(file.path(homeDir))
# ---- end

#############################################################################
##                                LOAD DATA                                ##
#############################################################################
# Load raw data 
if (loadData) {

# ---- loadDataCall
  rawActivities <- loadZipData(dataUrl, rawDataDir, dataZipFile, dataCsvFile)
# ---- end
  
# ---- calcLoadStatsCall
  loadStats <- calcLoadStats(rawActivities)
  print(loadStats)
# ---- end

# ---- loadSummary
  print(summary(rawActivities$steps))
# ---- end

# ---- makeLoadHistCall
  loadHist <- makeLoadHist(rawActivities)
# ---- end
  
# ---- loadMissingData
  loadMissingData <- rawActivities[is.na(rawActivities$steps),]
# ---- end

# ---- loadMissingDataHistCall
  loadMissingDataHist <- missingDataHist(loadMissingData)
# ---- end  
}
#############################################################################
##                    MEAN TOTAL NUMBER OF STEPS PER DAY                   ##
#############################################################################

if (meanAnalysis){
  
# ---- completeCases
  # Extract complete cases from activities
  completeCases <- rawActivities[complete.cases(rawActivities),]
# ---- end
  
# ---- stepsByDay01Call
  # Summarizes activitities into steps by date
  stepsByDay01 <- summate(x = completeCases, v = "date", f = "sum")
# ---- end

# ---- computeStats01Call
  # Compute and store summary statistics of total steps per day
  stats01 <- computeStats(stepsByDay01)
# ---- end
  
# ---- makeHist01Call
  # Prepare, render and return histogram and summary statistics of total number of steps taken each day
  stepsHist01 <- makeHist(stepsByDay01, stats01)
# ---- end
  
# Store histogram in appropriate figures folder. 
  plotFile <- "stepsHist01.png"
  plotSave(stepsHist01, plotFile)
 
}


#############################################################################
##                    AVERAGE DAILY ACTIVITY PATTERN                       ##
#############################################################################

if (activityPattern) {

# ---- stepsByInterval01
  # Summarize activities into average steps taken per 5 minute interval
  stepsByInterval01 <- summate(completeCases, "interval", "mean")
# ---- end

# ----   makeTimeSeriesCall
  # Prepare, render and return time series plot of steps taken per interval
  stepTimeSeries01 <- makeTimeSeries(stepsByInterval01)
# ---- end  
  # Store line chart in approprite figures folder
  plotFile <- "stepTimeSeries01.png"
  plotSave(stepTimeSeries01, plotFile)


}

#############################################################################
##                    IMPUTING MISSING VALUES                              ##
#############################################################################

if (imputeData) {

# ---- missingValues  
  # Calculate and report the total number of missing values in the dataset
  missingValues <- sum(is.na(rawActivities$steps))
  print(paste("There are",missingValues,"rows with NAs in the steps variable"))
# ---- end

# ----  imputeCall
  # Create a new activities data set with the missing values filled in.
  procActivities <- impute(rawActivities, stepsByInterval01, "procActivities.csv")
# ---- end

# ---- stepsByDay02
  # Summarizes activitities into steps by date
  stepsByDay02 <- summate(x = procActivities, v = "date", f = "sum")
# ---- end
  
# ---- computeStatsCall  
  #Compute and store summary statistics of total steps per day
  stats02 <- computeStats(stepsByDay02)
# ---- end
  
# ----   stepsHist02
  # Prepare, render and return histogram of total number of steps taken each day
  stepsHist02 <- makeHist(stepsByDay02, stats02)
# ---- end
  
  # Store histogram in appropriate figures folder
  plotFile <- "stepsHist02.png"
  plotSave(stepsHist02, plotFile)

}

#############################################################################
##                      DATA SET COMPARISON                                ##
#############################################################################

if (compare) {

# ---- conjoinCall  
  # Combine the raw and processed data frame into a single data frame with a new categorical variable for each row
  # that identifies it as a raw or processed data.  All NA rows are removed. 
  combined <- conjoin(stepsByDay01, stepsByDay02)
# ---- end  

# ----  makeDensityPlotCall
  # Prepare, render and return a density plot comparing distributions of the raw and processed data 
  density <- makeDensityPlot(combined)
# ---- end


  # Store the density plot in approprite figures folder
  plotFile <- "density.png"
  plotSave(density, plotFile)
  
# ---- makeOverlaidHistCall  
  # Prepare, render and return a histogram plot comparing distributions of the raw and processed data 
  stepsHist03 <- makeOverlaidHist(combined)
# ---- end
  
  # Store the histogram in approprite figures folder
  plotFile <- "stepHist03.png"
  plotSave(stepsHist03, plotFile)

# ---- compareStatsCall    
  # Compute the comparative difference in summary statistics of total steps per day between the raw and processed data
  difference <- compareStats(stats01, stats02)
# ---- end
    
# ---- makeBoxPlotCall  
  # Prepare, render and return a boxplot comparing distributions of the raw and processed data 
  stepsBox <- makeBoxPlot(combined, difference)
# ---- end
  
  # Store the boxpot in approprite figures folder
  plotFile <- "stepBox.png"
  plotSave(stepsBox, plotFile)

}

#############################################################################
##                      WEEKEND/WEEKDAY PATTERN ANALYSIS                   ##
#############################################################################

if (wdAnalysis) {
  
# ---- dayLabelCAll
  # Create a new Weekday / Weekend factor variable 
  procActivities <- dayLabel(procActivities)
# ---- end

# ---- summateWdCall
  # Summarize the data by day label, and 5-Minute Interval Time
  stepsByIntervalWd <- summateWd(procActivities)
# ---- end
  
# ---- makeTimeSeriesWdCall
    #Create comparative time-series 
  stepTimeSeriesWd <- makeTimeSeriesWd(stepsByIntervalWd)
# ---- end
  
  # Store the time series in approprite figures folder
  plotFile <- "stepTimeSeriesWd.png"
  plotSave(stepTimeSeriesWd, plotFile)

}