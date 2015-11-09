# Reproducible Research: Peer Assessment 1
## Loading and preprocessing the data

The data that is going to be used in this assignment is stored in a .csv file in the 
working directory.
The following code will be used to load the data

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(plyr)
```

```
## -------------------------------------------------------------------------
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
## -------------------------------------------------------------------------
## 
## Attaching package: 'plyr'
## 
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```r
library(lattice)
data<-read.csv("activity.csv")
```

It's necessary to modify the data into a format more suitable for the analysis. 
In this case was necessary to count the total number of steps taken in each day, which can be made using the following code.

```r
sum_data<-ddply(data,.(date),summarize, sum = sum(steps,na.rm=TRUE))
```

## What is mean total number of steps taken per day?

The histogram of the total total number of steps taken per day is:

```r
hist(sum_data$sum,main='Histogram of the total number of steps',xlab='Total number of steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

The following code will be used to calculate the mean and median.

```r
mean<-mean(sum_data$sum)
median<-median(sum_data$sum)
```
The mean total number os steps taken per day is 9354.2295082 and the median total number os steps taken per day is 10395.

## What is the average daily activity pattern?
The average daily activity pattern can be seen in the following plot.

```r
data_interval<-ddply(data,.(interval),summarize, mean= mean(steps,na.rm=TRUE))
plot(data_interval$interval,data_interval$mean,main="Average number of steps by interval",type = "l",xlab='Interval',ylab='Average number of steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

The following code will be used to calculate which is the 5-minute interval that contains the maximum number of steps on average.

```r
interval_max<-data_interval$interval[which.max(data_interval$mean)]
```

The 5-minute interval that contains the maximum number of steps on average across all the days in the dataset is 835.

## Imputing missing values


```r
total_na <- sum(is.na(data$steps))
```
The total number of missing values in the dataset (i.e. the total number of rows with NAs) is  2304.

The above code will be used to filling in all of the missing values in the dataset.
The strategy consists on substitute the NA values by the mean for that 5-minute interval.
The new data set _data_without_NAs_  is equal to the original dataset but with the missing data filled in.

```r
data_without_NAs<-data
data_without_NAs[is.na(data_without_NAs$steps),names(data_without_NAs) %in% c("steps")]<-as.integer(
floor(mapvalues(data_without_NAs[is.na(data_without_NAs$steps),]$interval, from = data_interval$interval, to = data_interval$mean)))
```

After filling in the NAs it's necessary to count the total number of steps taken in each day.

```r
sum_data_withous_NAs<-ddply(data_without_NAs,.(date),summarize, sum = sum(steps))
```

The histogram of the total total number of steps taken per day of the new data set is:

```r
hist(sum_data_withous_NAs$sum,main='Histogram of the total number of steps',xlab='Total number of steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

The following code will be used to calculate the mean and median of the new data set.

```r
mean_without_NAs<-mean(sum_data_withous_NAs$sum)
median_without_NAs<-median(sum_data_withous_NAs$sum)
```
The mean total number os steps taken per day of the new data set is 1.074977\times 10^{4} and the median total number os steps taken per day is 10641.

As we can see these values differ from the estimates from the first part of the assignment because we filled in the missing values with the mean of the interval.

With the missing values filled in the mean and median of the data set are higher than the original ones. 

## Are there differences in activity patterns between weekdays and weekends?

This code will add another factor variable (_weekday_) to the data set.

```r
data_without_NAs$date <-as.Date(format(data_without_NAs$date,format="%Y-%m-%d"))
weekdays1 <- c('sábado', 'domingo')

data_without_NAs$weekday <- factor((!(weekdays(data_without_NAs$date) %in% weekdays1)), 
         levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
```

Before ploting the the average number of steps taken in weekdays and weekends days across by interval it is necessary to create a new data set groupped by interval with the mean of each interval by 
weekdays and weekends.

```r
mean_data_new<-ddply(data_without_NAs,.(interval,weekday),summarize, mean = mean(steps,na.rm=TRUE))
```

Finaly we can construct the plot with the average number of steps taken across all weekday days or weekend days.

```r
xyplot(mean ~ interval | factor(weekday), data = mean_data_new, type = "l",ylab="Number of steps",xlab="Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png) 

As can be seen there are plenty differences between the number of steps taken in each interval. For instante in weekends the average number if steps is more homogeneous that in weekdays. 

