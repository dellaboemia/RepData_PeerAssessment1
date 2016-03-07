# Coursera Data Science Specialization
# Reproducible Research
# Peer Assessment Project 1
# func.R

# ---- calcLoadStatsFunc
calcLoadStats <- function(x) {
  # This function calculates various statistics for the loaded file an stores them in a data frame
  #
  # Args: 
  #   x: the raw data set
  #
  # Returns: Renders a data frame containing the total observations, complete cases, missing values and the percent of the data that is missing
  
  # Validate arguments
  if (missing(x)) { stop("Data frame to be summarized must be provided") }
  
  # Calculate basic statistics of data set.
  total     <- nrow(rawActivities)
  complete  <- nrow(na.omit(rawActivities))
  missing   <- total - complete
  pctMissing <- round(missing/total * 100,2)
  
  # Prepare summary statistics dataframe
  Measure <- c("Total", "Complete", "Missing", "Pct Missing")
  Values  <- c(total, complete, missing, pctMissing)
  df      <- data.frame(Measure, Values)
  
  return(df)

}
# ---- end

# ---- makeLoadHistFunc
makeLoadHist <- function(x) {
  # Produces a histogram of the steps taken for each 5-minute interval
  #
  # Args:     x - A dataframe containing the total number of steps taken by day.
  #
  # Returns:  ggplot object containing the histogram plot
  
  # Validate arguments.
  if (missing(x)) { stop("Data argument must be provided") }
  
  # Prepare histogram
  h <- ggplot(x, aes(x=steps)) +
    geom_histogram(aes(fill = ..count..), na.rm = TRUE) +
    scale_fill_gradient("Frequency", low = "green", high = "red") +
    xlab("Steps Per 5-minute Interval") +
    ylab("Frequency") +
    labs(title = "Steps per 5-Minute Interval") +
    theme_bw(base_family = "sans")

  # Render histogram and summary statistics to screen
  suppressMessages(print(h))
  
  # Return combined graphic.
  return(h)
}
# ---- end


# ---- loadMissingDataHistFunc
missingDataHist <- function(x) {
  # This function prepares and renders a histogram of the number of NA values for each 5 minute time interval. 
  #
  # Args: 
  #   x: the dataset containing the NA values
  #
  # Returns: Renders a ggplot histogram showing the distribution of NA values over the range of 5 minute intervals
  
  # Validate arguments
  if (missing(x)) { stop("Data frame to be summarized must be provided") }
  
  # Prepare histogram
  h <- ggplot(x, aes(x=interval)) +
    geom_histogram(aes(fill = ..count..), na.rm = TRUE) +
    scale_fill_gradient("Frequency", low = "green", high = "red") +
    xlab("5-Minute Interval") +
    ylab("Frequency") +
    labs(title = "Distribution of NA Values") +
    theme_bw(base_family = "sans") 

  # Render histogram 
  suppressMessages(print(h))
  
  # Return combined graphic.
  return(h)
}
# ---- end




# ---- summateFunc
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
# ---- end

# ---- computeStatsFunc
computeStats <- function(x) {
  # This function calculates summary statistics and stores the data in a data.table for later use. 
  #
  # Args:
  #   x: the dataframe to be summarized
  #
  # Returns: Data table of summary statistics for the dataset.  
  
  # Validate arguments
  if (missing(x)) { stop("Data frame to be summarized must be provided") }
  
  # Gather summary statistics
  min   <- min(x$steps)
  q1    <- unname(quantile(x$steps,probs=0.25))
  med   <- median(x$steps)
  mn    <- mean(x$steps)
  q3    <- unname(quantile(x$steps,probs=0.75))
  mx    <- max(x$steps)
  
  # Prepare summary statistics table
  No      <- c(1,2,3,4,5,6)
  Measure <- c("Minimum", "25th Percentile", "Median", "Mean", "75th Percentile", "Maximum")
  Steps   <- c(min, q1, med, mn, q3, mx)
  stbl    <- data.frame(No, Measure, Steps)

  return(stbl)

}
# ---- end


# ---- makeHistFunc
makeHist <- function(x,s) {
  # Produces a histogram of the total number of steps by day with a summary statistics table
  #
  # Args:     x - A dataframe containing the total number of steps taken by day.
  #           s - the summary statistics to be annoted on the histogram
  #
  # Returns:  ggplot object containing the histogram plot
  
  # Validate arguments.
  if (missing(x)) { stop("Data argument must be provided") }

  # Prepare summary statistics table plot
  s <- select(s, Measure, Steps)
  tt  <- ttheme_minimal(base_size = 10)
  g   <- tableGrob(s, theme=tt, rows=NULL)

  # Prepare histogram
  h <- ggplot(x, aes(x=steps)) +
    geom_histogram(aes(fill = ..count..), na.rm = TRUE) +
    scale_fill_gradient("Frequency", low = "green", high = "red") +
    xlab("Steps Per Day") +
    ylab("Frequency") +
    labs(title = "Total Steps Taken Each Day") +
    theme_bw(base_family = "sans") +
    annotation_custom(g, xmin=0, xmax = 8000, ymin = 5, ymax = 8)

  # Render histogram and summary statistics to screen
  suppressMessages(print(h))

  # Return combined graphic.
  return(h)
}
# ---- end



# ---- plotSaveFunc
plotSave <- function(x, f){
  # Saves plots in png format to the exploratory figures or the final figures directories.
  #
  # Args:     
  #   x:    a ggplot object  
  #   f:    name of file in which the plot will be saved 
  #
  # Returns:  
  #   Plot saved in designated figure folder.
  
  # Validate arguments
  if (missing(x)) { stop("Plot object not specified") }
  if (missing(f)) { stop("Filename not specified") }
  
  # Set working directory based upon version (ver) of the plot
  homeDir <- Sys.getenv("PROJ_HOME")

  # Create plot directory if does not already exist and set working directory
  if (!dir.exists(file.path(homeDir, figDir))) {
    dir.create(file.path(homeDir, figDir), recursive = TRUE)
  }
  setwd(file.path(homeDir, figDir))
  
  # Save plot
  suppressMessages(ggsave(filename = f, plot = x))
  
  # Reset working directory
  homeDir <- Sys.getenv("PROJ_HOME")
  setwd(homeDir)
}
# ---- end



# ---- makeTimeSeriesFunc
makeTimeSeries <- function(x) {
  # Produces a line chart time series of of the 5-minute interval (x-axis) and the 
  # average number of steps taken, averaged across all days (y-axis)
  #
  # Args:     x - A dataframe containing the average number of steps taken by interval across all days
  #
  # Returns:  ggplot object containing the timeseries plot
  
  # Validate arguments.
  if (missing(x)) { stop("Data argument must be provided") }
  
  # Calculate the time of maximum step activity
  maxInterval <- calcInterval(x)
  
  # Prepare and render line chart to screen
  l <- ggplot(x, aes(x=interval, y=steps)) +
    geom_line(colour="blue") +
    xlab("5-Minute Interval") +
    ylab("Average Steps") +
    labs(title = "Average Daily Activity Pattern") +
    theme_bw(base_family = "sans") + 
    annotate("text", x=835,y=220, 
             label = paste("5-Min interval with highest average number of steps begins at", maxInterval), 
             colour="red")
  
  suppressMessages(print(l))
  
  return(l)
}
# ---- end


# ---- calcIntervalFunc
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
# ---- end


# ---- imputeFunc
impute <- function(x, stepsByInterval, f) {
  # This function imputes missing step count data by substituting the mean for the associated 
  # 5-minute time interval across all days.
  #
  # Args:
  #   x - the original activities data frame
  #   stepsByInterval - a dataframe containing the average number of steps per 5-minute interval
  #   f - the name of the file in which the imputed data frame will be saved
  #
  # Returns: new activities data frame with missing values imputed
  
  # Validate arguments 
  if (missing(x)) { stop("Activities file must be provided") }
  if (missing(stepsByInterval)) { stop("stepsByInterval dataframe must be provided")}
  if (missing(f)) { stop("The filename for the imputed data frame must be provided")}
  
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
  dataDir <- "/Data/Processed Data"

  # Create data directory if does not already exist and set working directory
  if (!dir.exists(file.path(homeDir, dataDir))) {
    dir.create(file.path(homeDir, dataDir), recursive = TRUE)
  }
  setwd(file.path(homeDir, dataDir))
  
  # Save data set
  write.csv(newdf, file=f)
  
  # Reset working directory
  setwd(homeDir)
  
  return(newdf)
  
}
# ---- end

# ---- conjoinFunc
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
  x$source_order <- 1
  y$source <- "Processed Data"
  y$source_order <- 2

  # Bind both data frames
  z = rbind(x,y)
  
  # Remove all NA rows
  z  <- z[complete.cases(z),]
  
  return(z)
}
# ---- end  


# ---- makeDensityPlotFunc
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
    labs(title = "Raw versus Processed Data Density Average Total Daily Steps") 
  suppressMessages(print(d))
  return(d)
}

# ---- end


# ---- makeOverlaidHistFunc
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
    labs(title = "Raw versus Processed Histogram of Average Total Daily Steps")
  suppressMessages(print(d))
  return(d)
}
# ---- end


# ---- compareStatsFunc
compareStats <- function(x,y) {
  # Produces a table containing the difference in summary statistics between two tables containing summary statistics. 
  #
  # Args:     x - A data table containing summary statistics
  #           y - A data table containing summary statistics
  #
  # Returns:  a data table comparison of the summary statistics contained in x and y
  
  # Validate arguments.
  if (missing(x)) { stop("Summary statistic data table x must be provided") }
  if (missing(y)) { stop("Summary statistic data table y must be provided") }
  
  
  # Create the comparative table
  z <- merge(x,y, by = "Measure")
  z <- mutate(z, 
         Difference = round((z$Steps.y - z$Steps.x),2),
         Pct_Difference = paste(round(((z$Steps.y-z$Steps.x)/z$Steps.x)*100,2),"%"))
  arrange(z, No.x)
  z <- select(z, No.x, Measure, Steps.x, Steps.y, Difference, Pct_Difference)
  names(z) <- c("No", "Measure", "Raw", "Processed", "Difference", "Pct Difference")
  
  return(z)
  
}
# ---- end
  

# ---- makeBoxPlotFunc
makeBoxPlot <- function(x,s) {
  # Produces side-by-side box plots grouped by data source. 
  #
  # Args:     x - A combined data frame including both raw and processed data
  #           s - Data frame with summary statistics
  #
  # Returns:  ggplot object containing the histogram plot
  
  # Validate arguments.
  if (missing(x)) { stop("Data argument must be provided") }
  if (missing(s)) { stop("Summary statistics must be provided") }
  
  # Reorder factors so that statistics for raw data appears on the left
  x$source_id <- reorder(x$source, x$source_order)
  
  # Prepare and render boxplots to screen
  b <- ggplot(x, aes(x=source_id, y=steps, fill=source)) +
    geom_boxplot() +
    guides(fill=FALSE) +
    ylab("Steps") +
    xlab("Data Source") +
    scale_fill_discrete(name="Data Source") +
    labs(title = "Average Total Daily Steps") +
    theme_classic()  
  
  # Prepare summary statistics table
  s   <- arrange(s,No)
  s   <- s %>% select(everything(), - contains("No"))
  tt  <- ttheme_minimal(base_size = 10)
  t   <- tableGrob(s, theme=tt, rows=NULL)
  
  # Plot both graphs
  x   <- grid.arrange(b,t, nrow=2)
  print(x)

  return(x)
}
# ---- end


# ---- dayLabelFunc
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
                     levels=c(TRUE, FALSE), labels=c('Weekend', 'Weekday'))
  return(x)

}
# ---- end


# ---- summateWdFunc
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
# ---- end



# ---- makeTimeSeriesWdFunc
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
    theme_bw(base_family = "sans") +
    scale_colour_discrete(name="") 
  suppressMessages(print(l))
  return(l)
}
# ---- end
