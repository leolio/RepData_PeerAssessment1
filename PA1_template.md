---
title: "Peer Assessment 1"
output: html_document
---

##Loading and preprocessing the data


```r
unzip(zipfile="activity.zip")
data <- read.csv(file="activity.csv")
```

## What is mean total number of steps taken per day?
For this part, we ignore the missing values in the dataset.

```r
steps_by_date <-tapply(data$steps, data$date, sum)
hist(steps_by_date, xlab="Number of steps", main="Total number of steps taken each day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 



```r
mean(steps_by_date, na.rm=TRUE)
```

```
## [1] 10766
```

```r
median(steps_by_date, na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
steps_by_interval <-tapply(data$steps, data$interval, function(x) mean(x, na.rm=TRUE))
plot(steps_by_interval, type = "l", main="Average number of steps", xlab="5-minute interval", ylab="Average number of steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


```r
# index of max step by interval
names(steps_by_interval[which.max(steps_by_interval)])
```

```
## [1] "835"
```

## Imputing missing values

```r
# number of NA (rows)
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
a<-tapply(data$steps, data$interval, function(x) sum(!is.na(x)))
b<-tapply(data$steps, data$date, function(x) sum(!is.na(x)))
# number of non NA by interval
sum(a==0)
```

```
## [1] 0
```

```r
# number of non NA by date
sum(b==0)
```

```
## [1] 8
```

```r
new_data <- data
mean_by_interval <- tapply(new_data$steps, new_data$interval, function(x) mean(x, na.rm=TRUE))

for (i in seq(1,nrow(new_data))) {
  if (is.na(new_data$steps[i])) {
    new_data$steps[i] <- mean_by_interval[as.character(new_data$interval[i])]
  }
}


# TRUE steps_by_date
steps_by_date <-tapply(new_data$steps, new_data$date, sum)
hist(steps_by_date)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

```r
mean(steps_by_date)
```

```
## [1] 10766
```

```r
median(steps_by_date)
```

```
## [1] 10766
```

## Are there differences in activity patterns between weekdays and weekends?


```r
new_data$date <- strptime(new_data$date, format="%Y-%m-%d")
new_data$weekday <- factor(weekdays(new_data$date) %in% c("Saturday", "Sunday"), labels=c("weekday", "weekend"))
```
