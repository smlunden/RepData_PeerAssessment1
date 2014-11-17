---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

If zip file exists, unzip and read file. Display first few data points to confirm.

```r
if (file.exists("activity.zip")) {
    raw_data <- read.csv(unz("activity.zip", "activity.csv"))
    raw_data$full_date <- strptime(raw_data$date, "%Y-%m-%d")
    head(raw_data)
} else {
    stop("you need to put your activity.zip file right here next to this script...")
}
```

```
##   steps       date interval  full_date
## 1    NA 2012-10-01        0 2012-10-01
## 2    NA 2012-10-01        5 2012-10-01
## 3    NA 2012-10-01       10 2012-10-01
## 4    NA 2012-10-01       15 2012-10-01
## 5    NA 2012-10-01       20 2012-10-01
## 6    NA 2012-10-01       25 2012-10-01
```


## What is mean total number of steps taken per day?
Remove NAs, and then calculate sum of steps per day.

```r
filled_data <- raw_data[complete.cases(raw_data$steps),]
steps_per_day <- setNames(aggregate(filled_data$steps ~ filled_data$date, factor(filled_data$date), sum), c('date', 'steps'))
```


Calculate mean of all steps per day.

```r
mean(steps_per_day$steps)
```

```
## [1] 10766.19
```
Also calculate mean per day of steps.

```r
mean_steps_per_day <- setNames(aggregate(filled_data$steps ~ filled_data$date, factor(filled_data$date), mean), c('date', 'mean_steps'))
head(mean_steps_per_day)
```

```
##         date mean_steps
## 1 2012-10-02    0.43750
## 2 2012-10-03   39.41667
## 3 2012-10-04   42.06944
## 4 2012-10-05   46.15972
## 5 2012-10-06   53.54167
## 6 2012-10-07   38.24653
```

Calculate median steps per day.

```r
median(steps_per_day$steps)
```

```
## [1] 10765
```
Also calculate median per day of steps.

```r
median_steps_per_day <- setNames(aggregate(filled_data$steps ~ filled_data$date, factor(filled_data$date), median), c('date', 'median_steps'))
head(median_steps_per_day)
```

```
##         date median_steps
## 1 2012-10-02            0
## 2 2012-10-03            0
## 3 2012-10-04            0
## 4 2012-10-05            0
## 5 2012-10-06            0
## 6 2012-10-07            0
```

Plot histogram of steps per day.

```r
hist(steps_per_day$steps, main = "Histogram of Steps per Day", xlab = "Steps per Day")
```

![plot of chunk plotHist](figure/plotHist-1.png) 

## What is the average daily activity pattern?

Find steps per interval

```r
steps_per_interval <- setNames(aggregate(filled_data$steps ~ filled_data$interval, factor(filled_data$interval), mean), c('interval', 'mean_steps'))
```
Plot average steps per interval

```r
plot(steps_per_interval, type="l")
```

![plot of chunk plotInterval](figure/plotInterval-1.png) 
Return interval of maximum number of steps.

```r
steps_per_interval[steps_per_interval$mean_steps == max(steps_per_interval$mean_steps), "interval"]
```

```
## [1] 835
```
## Imputing missing values
Calculate number of rows with NA value for steps.

```r
length(which(is.na(raw_data$steps)))
```

```
## [1] 2304
```
Replace NAs with mean value for that interval

```r
replace_data <- merge(raw_data, steps_per_interval, all = TRUE)
replace_data$steps[is.na(replace_data$steps)] <- replace_data$mean_steps[is.na(replace_data$steps)]
```
Compare steps per day mean and median

```r
new_steps_per_day <- setNames(aggregate(replace_data$steps ~ replace_data$date, factor(replace_data$date), sum), c('date', 'steps'))
mean(new_steps_per_day$steps)
```

```
## [1] 10766.19
```

```r
median(new_steps_per_day$steps)
```

```
## [1] 10766.19
```
Plot histogram of steps per day with filled-in na values.

```r
hist(new_steps_per_day$steps, main = "Histogram of Steps per Day (with filled-in NA values)", xlab = "Steps per Day")
```

![plot of chunk plotNAHist](figure/plotNAHist-1.png) 
## Are there differences in activity patterns between weekdays and weekends?
Create new column for weekend/weekday designation.

```r
weekend = c("Saturday", "Sunday")
replace_data$weekday <- ifelse(weekdays(replace_data$full_date) %in% weekend, "weekend", "weekday")
```
