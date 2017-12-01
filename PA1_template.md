---
output:
  html_document: 
    keep_md: yes
  pdf_document: default
---
Reproducing-research-assignment1
===========================================

## R preparations
 In this report, code will be demonstrated to show how the results have coming through. So set the default of echo to be true throughout the project.

```r
library(knitr)
opts_chunk$set(echo=TRUE)
```

## Loading and pre-processing the data
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

### Reading the data
1. Load the data (i.e. 𝚛𝚎𝚊𝚍.𝚌𝚜𝚟())
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
dt <- read.csv("activity.csv", colClasses = c("numeric","character","integer"))
str(dt)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

### What is mean total number of steps taken per day?

#### For this part of the assignment, you can ignore the missing values in the dataset.

- Calculate the total number of steps taken per day
- If you do not understand the difference between a histogram and a bar plot, research the difference between them. Make a histogram of the total number of steps taken each day
- Calculate and report the mean and median of the total number of steps taken per day

1. Calculate the total number of steps per day 

```r
steps.date <- aggregate(steps~date, dt, sum)
head(steps.date)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```
2. Use ggplot for making histogram

```r
library(ggplot2)
g <- ggplot(steps.date, aes(x = steps))
g <- g + geom_histogram(fill= "darkblue", binwidth = 1000) + labs(title = "Histogram of steps per day", x = "steps per day")
print(g)
```

![](PA1_template_files/figure-html/histogram1-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

Mean of total steps 


```r
mean(steps.date$steps)
```

```
## [1] 10766.19
```

Median of total steps


```r
median(steps.date$steps)
```

```
## [1] 10765
```

###What is the average daily activity pattern?

Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
Which 5-minute interval, on average across all the days in the data set, contains the maximum number of steps?


```r
steps.interval <- aggregate(steps~interval, dt, sum)
head(steps.interval)
```

```
##   interval steps
## 1        0    91
## 2        5    18
## 3       10     7
## 4       15     8
## 5       20     4
## 6       25   111
```

```r
plot(steps.interval, type = "l")
```

![](PA1_template_files/figure-html/step2-1.png)<!-- -->

## Imputing missing values

##### Note that there are a number of days/intervals where there are missing values (coded as 𝙽𝙰). The presence of missing days may introduce bias into some calculations or summaries of the data.

- Calculate and report the total number of missing values in the data set (i.e. the total number of rows with 𝙽𝙰s)
- Devise a strategy for filling in all of the missing values in the data set. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. 
- Create a new data set that is equal to the original data set but with the missing data filled in. 
- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

1. First, calculate the total number of missing values in this dataset


```r
sum(is.na(dt))
```

```
## [1] 2304
```

2. The strategy for filling in all of missing values in the datset is to use mean of the day.


```r
mean(steps.date$steps)/length(unique(dt$date))
```

```
## [1] 176.4949
```
3. Take the approach to fill in a missing NA with the average number of steps in the same 5-min interval.

4. Create a new data set as the original and use tapply for filling in the missing values with the average number of steps per 5-minute interval:


```r
data_full <- dt
nas <- is.na(data_full$steps)
avg_interval <- tapply(data_full$steps, data_full$interval, mean, na.rm=TRUE, simplify=TRUE)
data_full$steps[nas] <- avg_interval[as.character(data_full$interval[nas])]
```

Check the results

```r
sum(is.na(data_full$steps))
```

```
## [1] 0
```

5. Calculate the number of steps taken in each 5-minute interval per day using dplyr and group by interval. Use ggplot for making the histogram:


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
steps_full <- data_full %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print
```

```
## # A tibble: 61 x 2
##          date    steps
##         <chr>    <dbl>
##  1 2012-10-01 10766.19
##  2 2012-10-02   126.00
##  3 2012-10-03 11352.00
##  4 2012-10-04 12116.00
##  5 2012-10-05 13294.00
##  6 2012-10-06 15420.00
##  7 2012-10-07 11015.00
##  8 2012-10-08 10766.19
##  9 2012-10-09 12811.00
## 10 2012-10-10  9900.00
## # ... with 51 more rows
```

6. Make a histogram of the total number of steps taken each day 


```r
g <- ggplot(steps_full, aes(x = steps))
g <- g + geom_histogram(fill= "darkblue", binwidth = 1000) + labs(title = "Histogram of steps per day", x = "steps per day")
print(g)
```

![](PA1_template_files/figure-html/histogram2-1.png)<!-- -->

7. Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

 
Calculate the mean and median

```r
mean_steps_full <- mean(steps_full$steps, na.rm = TRUE)
median_steps_full <- median(steps_full$steps, na.rm = TRUE)
```

mean

```r
mean_steps_full
```

```
## [1] 10766.19
```

median

```r
median_steps_full
```

```
## [1] 10766.19
```

The impact of imputing missing data with the average number of steps in the same 5-min interval is that both the mean and the median are equal to the same value: 10766.

## Are there differences in activity patterns between weekdays and weekends?
#### For this part the 𝚠𝚎𝚎𝚔𝚍𝚊𝚢𝚜() function may be of some help here. Use the dataset with the filled-in missing values for this part.

- Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

- Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

1. Create new factor variable - "weekday" and "weekend"

```r
Sys.setlocale("LC_TIME", "en_US") 
```

```
## [1] "en_US"
```

```r
weekdays(Sys.Date()+0:6)
```

```
## Warning in as.POSIXlt.POSIXct(Sys.time()): unknown timezone 'default/Asia/
## Tokyo'
```

```
## [1] "Friday"    "Saturday"  "Sunday"    "Monday"    "Tuesday"   "Wednesday"
## [7] "Thursday"
```

```r
data_full$weekday <- weekdays(as.Date(data_full$date))

data_full$weekend <- ifelse(data_full$weekday=="Saturday"|data_full$weekday=="Sunday",1,0)
```

2. Calculate average according to weekend and intervals

```r
data_full$weekend <- as.factor(data_full$weekend)

dt_weekend <- data_full %>%
        dplyr::group_by(interval, weekend) %>%
        dplyr::summarise(mean_steps = mean(steps))
summary(dt_weekend)
```

```
##     interval      weekend   mean_steps     
##  Min.   :   0.0   0:288   Min.   :  0.000  
##  1st Qu.: 588.8   1:288   1st Qu.:  2.047  
##  Median :1177.5           Median : 28.133  
##  Mean   :1177.5           Mean   : 38.988  
##  3rd Qu.:1766.2           3rd Qu.: 61.263  
##  Max.   :2355.0           Max.   :230.378
```


```r
plot <- ggplot(dt_weekend, aes(interval, mean_steps, color = weekend))
plot + geom_line() + facet_grid(weekend~.) + labs(x = "Intervals", y = "Average Steps", title = "Activity Patterns")
```

![](PA1_template_files/figure-html/ggplot_weekend-1.png)<!-- -->

From the two plots, it seems that the test object is more active earlier in the day during weekdays compared to weekends. However, he or she may be more active throughout the weekends compared with weekdays. 


```
