# Coursera Data Science Specialization
# Reproducible Research
# Peer Assessment Project 1
# func.R

summate <- function(x, v="date", f="sum") {
  # This function groups and summarizes the data 
  #
  # Args:
  #   x: the dataframe to be summarized
  #   v: the variable by which the data frame shall be grouped.  Valid values are "date" and "interval"
  #   f: the summary function to be used.  Valid values are "sum" and "mean"
  #
  # Returns: Data frame grouped and summarized by the requested statistic
  
  # Validate arguments
  if (missing(x)) { stop("Data frame to be summarized must be provided") }
  if (!any(v == c("date", "interval"))) { stop("Invalid variable name") }
  if (!any(f == c("sum", "mean"))) { stop("Invalid function name") }
  
  # Perform summation
  if (v == "date" & f == "sum") { 
    df <- x %>% group_by(date) %>% summarize(steps = sum(steps))
  } else if (v == "interval" & f == "sum") {
    df <- x %>% group_by(interval) %>% summarize(steps = sum(steps))
  } else if (v == "date" & f == "mean") {
    df <- x %>% group_by(date) %>% summarize(steps = mean(steps))
  } else {
    df <- x %>% group_by(interval) %>% summarize(steps = mean(steps))
  }
  
  return(df)
}



makeHist <- function(x) {
  # Produces a histogram of the total number of steps by day
  #
  # Args:     x - A dataframe containing the total number of steps taken by day.
  #
  # Returns:  ggplot object containing the histogram plot
  
  # Validate arguments.
  if (missing(x)) { stop("Data argument must be provided") }
  
  # Prepare and render histogram to screen
  h <- ggplot(x, aes(x=steps)) +
    geom_histogram(aes(fill = ..count..), na.rm = TRUE) +
    scale_fill_gradient("Frequency", low = "green", high = "red") +
    xlab("Steps Per Day") +
    ylab("Frequency") +
    labs(title = "Total Steps Taken Each Day") +
    theme_bw(base_family = "serif")
  suppressMessages(print(h))
  return(h)
}




plotSave <- function(x, f, ver = "exploratory"){
  # Saves plots in png format to the exploratory figures or the final figures directories.
  #
  # Args:     
  #   x:    a ggplot object  
  #   f:    name of file in which the plot will be saved 
  #   ver:  the version of the plot.  Valid values are: "exploratory", or "final"
  #
  # Returns:  
  #   Plot saved in designated figure folder.
  
  # Validate arguments
  if (missing(x)) { stop("Plot object not specified") }
  if (missing(f)) { stop("Filename not specified") }
  
  # Set working directory based upon version (ver) of the plot
  homeDir <- Sys.getenv("PROJ_HOME")
  if (ver == "final") { 
    plotDir <- "/Figure/Final Figures" 
  } else { 
    plotDir <- "/Figure/Exploratory Figures" 
  }
  
  # Create plot directory if does not already exist and set working directory
  if (!dir.exists(file.path(homeDir, plotDir))) {
    dir.create(file.path(homeDir, plotDir), recursive = TRUE)
  }
  setwd(file.path(homeDir, plotDir))
  
  # Save plot
  suppressMessages(ggsave(filename = f, plot = x))
  
  # Reset working directory
  homeDir <- Sys.getenv("PROJ_HOME")
  setwd(homeDir)
}




makeTimeSeries <- function(x) {
  # Produces a line chart time series of of the 5-minute interval (x-axis) and the 
  # average number of steps taken, averaged across all days (y-axis)
  #
  # Args:     x - A dataframe containing the average number of steps taken by interval across all days
  #
  # Returns:  ggplot object containing the timeseries plot
  
  # Validate arguments.
  if (missing(x)) { stop("Data argument must be provided") }
  
  # Prepare and render line chart to screen
  l <- ggplot(x, aes(x=interval, y=steps)) +
    geom_line(colour="blue") +
    xlab("5-Minute Interval") +
    ylab("Average Steps") +
    labs(title = "Average Daily Activity Pattern") +
    theme_bw(base_family = "serif")
  suppressMessages(print(l))
  return(l)
}



calcInterval <- function(x) {
  # The function determines Which 5-minute interval, on average across all the days in the dataset, 
  # contains the maximum number of steps?  
  #
  # Args:   x - a dataframe containing the average number of steps per 5-minute interval
  #
  # Returns; Character variable containing the 5-minute interval with highest average number of steps
  
  steps <- x$steps
  maxSteps <- max(steps)
  maxRow <- filter(x, steps == maxSteps)
  maxInterval <- maxRow$interval
  maxInterval <- gsub('^([0-9]{1,2})([0-9]{2})$', '\\1:\\2', sprintf('%04d',maxInterval))
  return(maxInterval)
}



impute <- function(x, stepsByInterval) {
  # This function imputes missing step count data by substituting the mean for the associated 
  # 5-minute time interval across all days.
  #
  # Args:
  #   x - the original activities data frame
  #   stepsByInterval - a dataframe containing the average number of steps per 5-minute interval
  #
  # Returns: new activities data frame with missing values imputed
  
  # Validate arguments 
  if (missing(x)) { stop("Activities file must be provided") }
  if (missing(stepsByInterval)) { stop("stepsByInterval dataframe must be provided")}
  
  # Make a copy of the activities data frame passed as an argument
  newdf = data.frame(x)

  # The approach here is to merge the newActivities and  stepsByInterval data frames. This will 
  # add a column to each row in newActivities with the average steps for the time interval.  
  newdf <- merge(newdf, stepsByInterval, by = "interval")
  
  # Next, we use the ifelse construct to identify missing values and to replace them with the average value 
  # in the column just added.  
  newdf <- transform(newdf, steps.x = ifelse(is.na(steps.x), steps.y, steps.x))
  
  #Remove the extra averages column and reset the column names.
  newdf$steps.y <- NULL
  names(newdf) <- c("interval", "steps", "date")
  
  # Set the directory where this processed data set is to be saved.
  homeDir <- Sys.getenv("PROJ_HOME")
  dataDir <- "/Data/Processed Data"

  # Create data directory if does not already exist and set working directory
  if (!dir.exists(file.path(homeDir, dataDir))) {
    dir.create(file.path(dataDir, plotDir), recursive = TRUE)
  }
  setwd(file.path(homeDir, dataDir))
  
  # Save data set
  write.csv(newdf, file="newActivities.csv", sep=",")
  
  # Reset working directory
  homeDir <- Sys.getenv("PROJ_HOME")
  setwd(homeDir)
  
  return(newdf)
  
}


conjoin <- function(x, y) {
  # This function conjoins the raw and processed versions of the activities file.  A categorical variable is 
  # created for each row, indicating to which original file the row belongs. All NA rows are removed from the 
  # combined data frame.
  #
  # Args:
  #   x - the original activities data frame
  #   y - the processed activities file with missing values imputed
  #
  # Returns: a ggplot, boxplot object
  
  # Add categorical variable to both files.
  x$source <- "Raw Data"
  y$source <- "Processed Data"
  
  # Bind both data frames
  z = rbind(x,y)
  
  # Remove all NA rows
  z  <- z[complete.cases(z),]
  
  return(z)
}
  

makeDensityPlot <- function(x) {
  # Produces overlapping, semi-transparent density plots grouped by data source. 
  #
  # Args:     x - A combined data frame including both raw and processed data
  #
  # Returns:  ggplot object containing the density plot
  
  # Validate arguments.
  if (missing(x)) { stop("Data argument must be provided") }
  
  # Prepare and render density plots to screen
  d <- ggplot(x, aes(x=steps, fill=source)) +
    geom_density(alpha=.3) +
    ylab("Density") +
    xlab("Total Steps per Day") +
    scale_fill_discrete(name="Data Source") +
    labs(title = "Raw versus Processed Data Density Total Average Daily Steps") 
  suppressMessages(print(d))
  return(d)
}



makeOverlaidHist <- function(x) {
  # Produces overlapping, semi-transparent histogram plots grouped by data source. 
  #
  # Args:     x - A combined data frame including both raw and processed data
  #
  # Returns:  ggplot object containing the histogram plot
  
  # Validate arguments.
  if (missing(x)) { stop("Data argument must be provided") }
  
  # Find the mean of each group
  meansData <- ddply(x, "source", summarise, stepsMean=mean(steps))
  
  
  # Prepare and render density plots to screen
  d <- ggplot(x, aes(x=steps, fill=source), environment = environment()) +
    geom_histogram(alpha=.3, position="identity") +
    geom_vline(data=meansData, aes(xintercept=stepsMean,  colour=source),
             linetype="dashed", size=1) +
    ylab("Frequency") +
    xlab("Total Steps per Day") +
    scale_fill_discrete(name="Data Source") +
    scale_colour_discrete(name="Data Source") +
    labs(title = "Raw versus Processed Histogram of Total Average Daily Steps")
  suppressMessages(print(d))
  return(d)
}


makeBoxPlot <- function(x) {
  # Produces side-by-side box plots grouped by data source. 
  #
  # Args:     x - A combined data frame including both raw and processed data
  #
  # Returns:  ggplot object containing the histogram plot
  
  # Validate arguments.
  if (missing(x)) { stop("Data argument must be provided") }
  
  # Prepare and render boxplots to screen
  b <- ggplot(x, aes(x=source, y=steps, fill=source)) +
    geom_boxplot() +
    guides(fill=FALSE) +
    ylab("Steps") +
    xlab("Data Source") +
    scale_fill_discrete(name="Data Source") +
    labs(title = "Raw versus Processed Distribution of Total Average Daily Steps")
  suppressMessages(print(b))
  return(b)
}

dayLabel <- function(x) {
  # Create a new factor variable in the dataset with two levels -- "weekday" and
  # "weekend" indicating whether a given date is a weekday or weekend day. 
  #
  # Args:     x - the processed activities data frame
  #
  # Returns:  dataframe modified to include the factor variable
  
  # Validate arguments.
  if (missing(x)) { stop("Data argument must be provided") }

  # Convert date 
  x$date <- as.Date(x$date, format="%Y-%m-%d")
  
  # Create vector of weekend days
  weekend <- c("Saturday", "Sunday")

  #Evaluate weekday and create a logical vector, with factor levels, labels
  x$dayLabel <- factor((weekdays(x$date) %in%  weekend), 
                     levels=c(TRUE, FALSE), labels=c('weekend', 'weekday'))
  return(x)

}



summateWd <- function(x) {
  # This function groups and summarizes the data by day label, and mean steps per 5-minute interval 
  #
  # Args:
  #   x: the dataframe to be summarized
  #
  # Returns: Data frame grouped and summarized by the requested statistic
  
  # Validate arguments
  if (missing(x)) { stop("Data frame to be summarized must be provided") }

  # Perform summation
  df <- x %>% group_by(dayLabel, interval) %>% summarize(steps = mean(steps))

  return(df)
}





makeTimeSeriesWd <- function(x) {
  # Produces a line chart time series of the 5-minute interval (x-axis) and the 
  # average number of steps taken, averaged across all days (y-axis) for both weekend and weekdays
  #
  # Args:     x - A dataframe containing the average number of steps taken by interval across all days
  #
  # Returns:  ggplot object containing the timeseries plot
  
  # Validate arguments.
  if (missing(x)) { stop("Data argument must be provided") }
  
  # Prepare and render line chart to screen
  l <- ggplot(x, aes(x=interval, y=steps, group=dayLabel, colour=dayLabel)) +
    geom_line() +
    xlab("5-Minute Interval") +
    ylab("Average Steps") +
    labs(title = "Average Daily Activity Pattern") +
    theme_bw(base_family = "serif") +
    scale_colour_discrete(name="") 
  suppressMessages(print(l))
  return(l)
}
