
#  set the working directory
setwd("~/Data_Science/coursera-repdata/project1")
library(ggplot2)
library (dplyr)
library (markdown)
library (knitr)
#  Loading and preprocessing the data
#  Show any code that is needed to
#  Load the data (i.e. 𝚛𝚎𝚊𝚍.𝚌𝚜𝚟())
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")

## Process/transform the data (if necessary) into a format suitable for your analysis

data$date <- as.Date(data$date)  # convert to Date object

# # What is mean total number of steps taken per day?
# # For this part of the assignment, you can ignore the missing values in the dataset.
# # Calculate the total number of steps taken per day
# If you do not understand the difference between a histogram and a barplot,
# research the difference between them. 
# Make a histogram of the total number of steps taken each day
# Calculate and report the mean and median of the total number of steps taken per day

t_steps <- data %>%
  group_by(date) %>%
  summarise(steps=sum(steps, na.rm=TRUE))
qplot(t_steps$steps, binwidth=1000, xlab="total steps each day")
mean(t_steps$steps, na.rm=TRUE)
median(t_steps$steps, na.rm=TRUE)


# What is the average daily activity pattern?
# 
# Make a time series plot (i.e.𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute iatervil (x-
# axis) and the average number of steps taken, averaged across all days (y-axis)

average <- data %>%
  group_by(interval) %>%
  summarise(steps=mean(steps, na.rm=TRUE))

ggplot(data=average, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken")
# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
average[which.max(average$steps),]
# Imputing missing values
# Note that there are a number of days/intervals where there are missing values (coded as 𝙽𝙰)
# The presence of missing days may introduce bias into some calculations or summaries of the data.
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s
missing <- is.na(data$steps)
table(missing)
# Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, 
# you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# Replace each missing value with a zero

for (i in 1:dim(data)[1]){
  ifelse ((!is.na(data$steps[i])),  data$steps[i]<- data$steps[i], data$steps[i]<- 0)
}

# Create a new dataset that is equal to the original dataset but with the missing data filled in.
# Make a histogram of the total number of steps taken each day and 
# Calculate and report the mean and median total number of steps taken per day.

t2_steps <- data %>%
  group_by(date) %>%
  summarise(steps=sum(steps, na.rm=TRUE))
qplot(t_steps$steps, binwidth=1000, xlab="total steps each day")
mean(t_steps$steps, na.rm=TRUE)
median(t_steps$steps, na.rm=TRUE)


# Do these values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total daily number of steps?


# Are there differences in activity patterns between weekdays and weekends?
# For this part the 𝚠𝚎𝚎𝚔𝚍𝚊𝚢𝚜() function may be of some he
#  Use the dataset with the filled-in missing values for this part.
# 
# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
# indicating whether a given date is a weekday or weekend day.
# Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = ���") o
# f the 5-minute interval (x-axis) and the average number of steps taken, 
# averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository 
# to see an example of what this plot should look like using simulated data.
# #  load data

  data <- mutate(data, day.of.week = weekdays(date)) 

  data.weekend <- filter (data, day.of.week %in% c("Saturday", "Sunday"))
  data.weekend <- mutate(data.weekend , weekend_or_weekday = 'weekend')
  
  data_weekday <- filter (data, day.of.week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
  data_weekday <- mutate(data_weekday, weekend_or_weekday = 'weekday')


data <- rbind(data_weekday, data.weekend)


## ------------------------------------------------------------------------
averages <- aggregate(steps ~ interval + weekend_or_weekday, data = data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(weekend_or_weekday ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")

