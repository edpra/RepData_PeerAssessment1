# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

* "activity.zip" contains file "activity.csv" which has the data for us to process.  We unzip the file here since it's already in the forked repository.


```r
    unzip("activity.zip")
    df_activity <- read.csv("activity.csv", header=TRUE)
    df_activity$date <- as.Date(df_activity$date)
```

* Here are the first few lines of the activity data set that we are about to process.


```r
    # summary(df_activity)
    head(df_activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

* * * * *
## What is mean total number of steps taken per day?

1. We use dplyr to compute activity data.  First let's display a histogram showing **total number of steps taken per day**.

2. Here is the histogram of the total number of steps taken each day.

```r
    library(ggplot2)
    library(dplyr)
```

Below is the 

```r
    df <- df_activity %>% 
        group_by(date) %>% 
        summarize(all_steps = sum(steps, na.rm=TRUE))
    # cannot use ggplot due to is.integer(group) not TRUE error so use hist()
    hist(df$all_steps, col="lightgray", 
         xlab = "Steps per day", ylab = "How Often Steps Occur",
         main = "Histogram of total number of steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Following are the **mean** and **median** of the total number of steps taken per day.


```r
    df %>% summarize(mean(all_steps), median(all_steps))
```

```
## # A tibble: 1 × 2
##   `mean(all_steps)` `median(all_steps)`
##               <dbl>               <int>
## 1           9354.23               10395
```

* The mean and median do *match* up with the histogram above.

* * * * *
## What is the average daily activity pattern?

1. Here is the time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
    df <- df_activity %>%
        group_by(interval) %>%
        summarise(mean_steps = mean(steps, na.rm=TRUE))
    # cannot use ggplot due to is.integer(group) not TRUE error so use plot()
    plot(df, type="l", ylab="Average number of steps", xlab = "5-minute Intervals",
         main="Average number of steps per interval", col="darkgreen")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Below is the **interval, on average across all days** in the dataset, that contains the maximum number of steps.  We find this value by finding the maximum steps value of the interval data.


```r
    df %>% slice(which.max(mean_steps)) %>% select(interval)
```

```
## # A tibble: 1 × 1
##   interval
##      <int>
## 1      835
```

* The interval numeric value **835** maps to **8:35 am**.

* * * * *
## Imputing missing values

1. The **total number of row with missing values** is found by counting rows with steps having NA value.


```r
    df_activity %>% filter(is.na(steps)) %>% count()
```

```
## # A tibble: 1 × 1
##       n
##   <int>
## 1  2304
```

2. Let's *replace all* **missing values** *with the* **mean steps** value of the corresponding interval for all days.  Here are the first rows with NA values before replacement.


```r
    head(df_activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
    df_imputed <- df_activity %>% group_by(interval) %>%
        mutate(steps = replace(steps, is.na(steps), 
                               as.integer(round(mean(steps, na.rm=TRUE)))))
```

* Here are the first rows with the NA values replaced.


```r
    # summary(df_imputed)
    head(df_imputed)
```

```
## Source: local data frame [6 x 3]
## Groups: interval [6]
## 
##   steps       date interval
##   <int>     <date>    <int>
## 1     2 2012-10-01        0
## 2     0 2012-10-01        5
## 3     0 2012-10-01       10
## 4     0 2012-10-01       15
## 5     0 2012-10-01       20
## 6     2 2012-10-01       25
```

3. Now we are recreating the histogram as we did earlier after loading data.  Below is the histogram of the total number of steps taken each day.


```r
    df <- df_imputed %>% 
        group_by(date) %>% 
        summarize(all_steps = sum(steps, na.rm=TRUE))
    # cannot use ggplot due to is.integer(group) not TRUE error so use hist()
    hist(df$all_steps, col="lightgray", 
         xlab = "Steps per day", ylab = "How Often Steps Occur",
         main = "Histogram of total number of steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

4. Following are the **mean** and **median** of the total number of steps taken per day.


```r
    df %>% summarize(mean(all_steps), median(all_steps))
```

```
## # A tibble: 1 × 2
##   `mean(all_steps)` `median(all_steps)`
##               <dbl>               <int>
## 1          10765.64               10762
```

* The mean and median values differ slightly from the value of the earlier histogram.  This histogram has more steps taken per day as shown in the increase of the frequency. This is expected since we imputed the dataset with average values across all days for each interval. This causes the distribution of steps taken per day resemble the histogram earlier.

* * * * *
## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.  Note that we are using imputed data.


```r
dfi <- df_imputed %>% 
    group_by(date) %>% 
    mutate(wd_or_we = ifelse (any(weekdays(date) %in% c("Saturday", "Sunday")), 
                        "weekend", "weekday")) %>%
    group_by(wd_or_we, interval) %>%
    summarize(mean_steps = mean(steps))
dfi
```

```
## Source: local data frame [576 x 3]
## Groups: wd_or_we [?]
## 
##    wd_or_we interval mean_steps
##       <chr>    <int>      <dbl>
## 1   weekday        0 2.28888889
## 2   weekday        5 0.40000000
## 3   weekday       10 0.15555556
## 4   weekday       15 0.17777778
## 5   weekday       20 0.08888889
## 6   weekday       25 1.57777778
## 7   weekday       30 0.75555556
## 8   weekday       35 1.15555556
## 9   weekday       40 0.00000000
## 10  weekday       45 1.73333333
## # ... with 566 more rows
```

2. Below is the panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
    # This ggplot works and the is.integer(group) not TRUE error would not pop up
    # if xlab, ylab, and other buildups are not used
    ggplot(dfi, aes(interval, mean_steps)) +
        ggtitle("Time Series Plot of Mean Steps by Interval of Imputed Data") +
        facet_grid(. ~ wd_or_we) +
        geom_line(size = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

* The diagrams show that there are more steps per day during weekends than during weekdays.  This could be due to an active lifestyle during weekends.  There is a spike between 8:00am and 9:00am during weekdays.  This could be due to early morning activities that occur less during weekends.
