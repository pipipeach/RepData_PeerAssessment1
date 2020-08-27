library(dplyr)
library(ggplot2)
# unzip the file
zipF <- "/Users/ruoyingtao/Desktop/R_Files/RepData_PeerAssessment1/activity.zip"
unzip(zipF)

# load the data
all_data <- read.csv("activity.csv")

# ignore the missing values in the dataset
activity <- subset(all_data, is.na(all_data$steps) == FALSE)

# create a new variable "day", which transform the "date" variables into class "POSIXct"
activity <- mutate (activity, day=as.POSIXct(date)) 
# group the data by "day"
days <- group_by(activity, day)

# calculate the total number of steps taken per day 
total_steps <- summarize(days, total = sum(steps, na.rm = TRUE))
# make a histogtam of the total number of steps taken each day
hist(total_steps$total, 
     main="Histogram of total number of steps per day", 
     xlab="Total number of steps in a day")
# calculate mean and median of the total number of steps taken per day
summary(total_steps)

# group the data by "interval"
intervals <- group_by(activity, interval)
# calculate the average number of steps taken at any 5 minute interval, averaged across all days
mean_steps <- summarize(intervals, mean = mean(steps, na.rm = TRUE))
# make time series plot
plot(mean_steps$mean ~ mean_steps$interval, type='l', 
     main="Average number of steps over all days", xlab="Interval", 
     ylab="Average number of steps")

# calculate the total number of missing values in the dataset
sum(is.na(all_data))

# use the mean of that 5 minute interval to impute the missing values
# create a new dataset with the missing values filled in 
copy <- all_data
for (i in 1:nrow(copy)){
        if (is.na(copy$steps[i])){
                interval_value <- copy$interval[i]
                mean_for_interval <- mean_steps[mean_steps$interval == interval_value,]
                copy$steps[i] <- mean_for_interval$mean
        }
} 
# check_na <- subset(copy, is.na(copy$steps) == TRUE)
# this subset should have 0 observations

# group the new dataset by day
copy <- mutate (copy, day=as.POSIXct(date)) 
copy_days <- group_by(copy, day)

# make a histogram of the total number of steps taken each day
copy_total_steps <- summarize(copy_days, total = sum(steps))
hist(copy_total_steps$total, 
     main="Histogram of total number of steps per day (imputed)", 
     xlab="Total number of steps in a day")

# calculate the mean and median total number of steps taken per day
# does not differ from the first part of the assignment
options(digits = 15)  
summary(copy_total_steps)

# create a new factor variable in the dataset with two levels : weekday and weekend
copy <- mutate (copy, weekday = weekdays(as.Date(copy$date)))
copy$weekday[copy$weekday  %in% c('Saturday','Sunday') ] <- "weekend"
copy$weekday[copy$weekday != "weekend"] <- "weekday"
copy$weekday <- as.factor(copy$weekday)

# group the dataset by interval and weekday
copy_intervals <- group_by(copy, interval, weekday)
copy_mean_steps <- summarize(copy_intervals, mean = mean(steps, na.rm = TRUE))

# make a panel plot containing a time series plot of the 5 minute interval and the average number 
# of steps taken, averaged across all weekdays or weekend days. 
qplot(interval, mean, 
      data = copy_mean_steps, 
      type = 'l', 
      geom=c("line"),
      xlab = "Interval", 
      ylab = "Number of steps", 
      main = "") +
        facet_wrap(~ weekday, ncol = 1)







