library(dplyr)
library(ggplot2)
library(cowplot)

##### Part 1: Load and preprocess the data. 

# 1. Load data
data <- read.csv('activity.csv')

# 2. Process/transform the data (if necessary) 


##### Part 2: What is mean total number of steps taken per day?

# 1. Calculate the total number of steps taken per day
sum_data <- data %>% group_by(date) %>% 
        summarize(Sum_Steps = sum(steps, na.rm=TRUE))

# 2. Make a histogram of the total number of steps taken each day
hist(sum_data$Sum_Steps,
     main="Total Number of\nSteps Taken Each Day",
     xlab="Steps Taken",
     ylab='Frequency of Steps Taken',
     ylim=c(0, 30),
     col="darkmagenta"
     )

# 3. Calculate the mean and median of the total of steps taken per day. 
mean <- mean(sum_data$Sum_Steps, na.rm = TRUE)
median <- median(sum_data$Sum_Steps, na.rm = TRUE)
print(paste('Mean of total of steps taken per day is', mean))
print(paste('Median of total of steps taken per day is', median))


##### Part 3: What is the average daily activity pattern?

# 1. Make a time series plot of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)
interval_data <- data %>% group_by(interval) %>% 
        summarize(Mean_Interval = mean(steps, na.rm=TRUE))

p <- ggplot(interval_data, aes(x=interval, y=Mean_Interval)) + 
            geom_line(color="turquoise4") + theme_minimal() + 
            labs(x="Five-Minute Intervals", 
                 y="Mean Number of Steps", 
                 title="Mean Number of Steps for\nEach Five-Minute Interval") 
print(p)

# 2. Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
max <- max(interval_data$Mean_Interval, na.rm = TRUE)
print(paste('Max of mean of total of steps taken each five-minute interval is'
            , max))


##### Part 4: Imputing missing values

# 1. Calculate and report the total number of missing values in the dataset 
frac_missing <- sum(is.na(data$steps))  / length(data$steps)
print(paste('Proportion of missing values is', frac_missing))

# 2. Devise a strategy for filling in all of the missing values in the dataset. 
print("Missing values don't seem to be randomly distributed, and without more ") 
print("information about why they are missing, removing entries with missing ")
print("values seems to be the best approach.") 

# 3. Create a new dataset with the missing data filled in.
new_data <- data[complete.cases(data), ]

# 4. Make a histogram of the total number of steps taken each day.

# Calculate total number of steps 
new_sum_data <- new_data %>% group_by(date) %>% 
                summarize(Sum_Steps = sum(steps, na.rm=TRUE))

# Plot histogram 
hist(new_sum_data$Sum_Steps,
     main="Total Number of Steps Taken\nEach Day",
     xlab="Steps Taken",
     ylab='Frequency of Steps Taken',
     ylim=c(0, 30),
     col="royalblue4"
)

# 5. Calculate and report the mean and median total number of steps taken per day. 
new_mean <- mean(new_sum_data$Sum_Steps, na.rm = TRUE)
new_median <- median(new_sum_data$Sum_Steps, na.rm = TRUE)
print(paste('Mean of total of steps taken per day is', new_mean))
print(paste('Median of total of steps taken per day is', new_median))

# 6. Do these values differ from the estimates from the first part? 
# What is the impact of imputing missing data on the estimates? 

perc_mean_diff <- (mean - new_mean) / mean(mean, new_mean) * 100 
print(paste('The percent difference between the first mean and the second is ', 
            perc_mean_diff, '%', sep=''))
perc_median_diff <- (median - new_median) / mean(median, new_median) * 100 
print(paste('The percent difference between the first median and the second is ',
            perc_median_diff, '%', sep=''))

print('After dropping the missing values, the mean dropped about 15%, and')
print(' the median dropped by about 3.5%.')


##### Part 5: Are there differences in activity between weekdays and weekends?

# 1. Create a new factor variable for “weekday” and “weekend”.

# Convert date column to POSIXlt class 
new_data$day <- strftime(new_data$date, "%A")

# Find weekend values 
weekend <- c('Sunday', 'Saturday')

# Create mask to find weekend days 
mask <- new_data$day %in% weekend

# Create factor column 'day_category'
new_data$day_category <- factor(mask, 
                                levels=c(FALSE, TRUE),
                                labels = c("weekday", "weekend"))

# 2. Make a panel plot containing a time series plot of the 5-minute interval 
# and average number of steps taken, averaged across all the days. 
new_interval_data <- new_data %>% group_by(day_category, interval) %>% 
        summarize(Mean_Interval = mean(steps))

# Separate data into weekend and weekday data frames 
weekend_data <- new_interval_data[new_interval_data$day_category %in% c('weekend'), ]
weekday_data <- new_interval_data[new_interval_data$day_category %in% c('weekday'), ]

# Create panel plot

# Set figure size 
options(repr.plot.width = 12, repr.plot.height = 12) 

# Create weekday data plot
p1 <- ggplot(weekday_data, aes(x=interval, y=Mean_Interval)) + 
        geom_line(color="deepskyblue4") + theme_minimal() + 
        labs(x="Five-Minute Intervals", 
             y="Mean Number of Steps", 
             title="Mean Number of Steps for\nEach Five-Minute Interval\non Weekdays") 

# Create weekend data plot
p2 <- ggplot(weekend_data, aes(x=interval, y=Mean_Interval)) + 
        geom_line(color="darkolivegreen4") + theme_minimal() + 
        labs(x="Five-Minute Intervals", 
             y="Mean Number of Steps", 
             title="Mean Number of Steps for\nEach Five-Minute Interval\non Weekends") 

# Plot on panel 
plot_grid(p1, p2, ncol = 1, nrow = 2)