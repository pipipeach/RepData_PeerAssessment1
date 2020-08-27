library(dplyr)
zipF <- "/Users/ruoyingtao/Desktop/R_Files/RepData_PeerAssessment1/activity.zip"
unzip(zipF)

all_data <- read.csv("activity.csv")
activity <- subset(all_data, is.na(all_data$steps) == FALSE)

activity <- mutate (activity, day=as.POSIXct(date)) 
days <- group_by(activity, day)

total_steps <- summarize(days, total = sum(steps, na.rm = TRUE))
hist(total_steps$total, 
     main="Histogram of total number of steps per day", 
     xlab="Total number of steps in a day")
summary(total_steps)

intervals <- group_by(activity, interval)
mean_steps <- summarize(intervals, mean = mean(steps, na.rm = TRUE))
plot(mean_steps$mean ~ mean_steps$interval, type='l', 
     main="Average number of steps over all days", xlab="Interval", 
     ylab="Average number of steps")

sum(is.na(all_data))

copy <- all_data
for (i in 1:nrow(copy)){
        if (is.na(copy$steps[i])){
                interval_value <- copy$interval[i]
                mean_for_interval <- mean_steps[mean_steps$interval == interval_value,]
                copy$steps[i] <- mean_for_interval$mean
        }
} 
check_na <- subset(copy, is.na(copy$steps) == TRUE)
# this subset should have 0 observations

copy <- mutate (copy, day=as.POSIXct(date)) 
copy_days <- group_by(copy, day)

copy_total_steps <- summarize(copy_days, total = sum(steps))
hist(copy_total_steps$total, 
     main="Histogram of total number of steps per day (imputed)", 
     xlab="Total number of steps in a day")

options(digits = 15)  
summary(copy_total_steps)









