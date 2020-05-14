---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---




## Loading and preprocessing the data
- Package(s) loading

```r
library(dplyr)
library(ggplot2)
library(knitr)
```

- Data loading

```r
act_data<- read.csv("activity.csv")
```

- Data Processing

```r
# Coverting date column from factor to DATE
act_data$date<-as.Date(act_data$date)

# Showing first few rows of the imputed dataset
kable(head(act_data))
```



 steps  date          interval
------  -----------  ---------
    NA  2012-10-01           0
    NA  2012-10-01           5
    NA  2012-10-01          10
    NA  2012-10-01          15
    NA  2012-10-01          20
    NA  2012-10-01          25

## What is mean total number of steps taken per day?

- Total number of steps taken per day

```r
# Grouping steps count by Date, then adding them accross groups.
totalStepsPerDay<- act_data %>% group_by(date) %>%
    summarise(steps=sum(steps,na.rm=TRUE))
kable(totalStepsPerDay, caption = "Total number of steps taken per day")
```



Table: Total number of steps taken per day

date          steps
-----------  ------
2012-10-01        0
2012-10-02      126
2012-10-03    11352
2012-10-04    12116
2012-10-05    13294
2012-10-06    15420
2012-10-07    11015
2012-10-08        0
2012-10-09    12811
2012-10-10     9900
2012-10-11    10304
2012-10-12    17382
2012-10-13    12426
2012-10-14    15098
2012-10-15    10139
2012-10-16    15084
2012-10-17    13452
2012-10-18    10056
2012-10-19    11829
2012-10-20    10395
2012-10-21     8821
2012-10-22    13460
2012-10-23     8918
2012-10-24     8355
2012-10-25     2492
2012-10-26     6778
2012-10-27    10119
2012-10-28    11458
2012-10-29     5018
2012-10-30     9819
2012-10-31    15414
2012-11-01        0
2012-11-02    10600
2012-11-03    10571
2012-11-04        0
2012-11-05    10439
2012-11-06     8334
2012-11-07    12883
2012-11-08     3219
2012-11-09        0
2012-11-10        0
2012-11-11    12608
2012-11-12    10765
2012-11-13     7336
2012-11-14        0
2012-11-15       41
2012-11-16     5441
2012-11-17    14339
2012-11-18    15110
2012-11-19     8841
2012-11-20     4472
2012-11-21    12787
2012-11-22    20427
2012-11-23    21194
2012-11-24    14478
2012-11-25    11834
2012-11-26    11162
2012-11-27    13646
2012-11-28    10183
2012-11-29     7047
2012-11-30        0

- Histogram of the total number of steps taken each day

```r
# Calculating mean and median for the plot
mean1<-round(mean(totalStepsPerDay$steps),2)
median1<-round(median(totalStepsPerDay$steps),2)
# Histogram  generation
ggplot(totalStepsPerDay, aes(steps))+
    geom_histogram(binwidth = 500,alpha=0.7, col="black",aes( fill=..count..,))+
    geom_density(aes(y=..count.. *500))+
    geom_vline(xintercept = mean1,color="red",size=1.5,alpha=0.5)+
    geom_text(aes(x=8000, label=paste("Mean\n",mean1), y=8.5))+
    geom_vline(xintercept = median1,color="darkgreen",size=1.5,alpha=0.5)+
    geom_text(aes(x=12000, label=paste("Median\n",median1), y=8.5))+
    labs(title = "Histogram: Total number of steps taken each day", x="Daily steps", y="Frequency")+
    theme(legend.position = c(0.95,0.85), plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/Histogram of the total number of steps taken each day-1.png)<!-- -->

- Mean and median of the total number of steps taken per day

```r
# Mean calculation
mean1<-round(mean(totalStepsPerDay$steps),2)
print(paste("Mean = ",mean1))
```

```
## [1] "Mean =  9354.23"
```

```r
# Median calculation
median1<-round(median(totalStepsPerDay$steps),2)
print(paste("Median = ",median1))
```

```
## [1] "Median =  10395"
```

## What is the average daily activity pattern?
- Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
# Grouping steps count by interval, then calculating means across groups
timeSeriesData<- act_data %>% group_by(interval) %>% summarise(steps=mean(steps,na.rm=TRUE))
# Time series plot
ggplot(timeSeriesData, aes(x=interval, y=steps))+
    geom_line(color="red")+
    labs(title = "Time series plot: Average number of steps taken by 5 Minute interval ",
         x="Five minute interval Identifier", y="Steps count averaged over all days")+
    theme( plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/time series plor-1.png)<!-- -->

- 5-minute interval with the maximum number of steps (average across all the days in the dataset)

```r
maxinterval<- timeSeriesData[which.max(timeSeriesData$steps),]
names(maxinterval)<- c("Interval identifier", "Maximun average steps")
kable(maxinterval)
```



 Interval identifier   Maximun average steps
--------------------  ----------------------
                 835                206.1698

## Imputing missing values
-  Total number of missing values in the original dataset 

```r
kable(colSums(is.na(act_data)), caption = "Number of Missnig values per column")
```



Table: Number of Missnig values per column

               x
---------  -----
steps       2304
date           0
interval       0

- Replacing missing values with mean of steps across all days for that specific 5 minute interval

```r
# Grouping steps count by interval, then replacing missing steps count value with mean of steps across all days for that specific 5 minute interval
imputed_data<- act_data %>% group_by(interval) %>% 
    mutate(steps= replace(steps, is.na(steps), mean(steps,na.rm=TRUE) ))
# Checking missing value number in the new formatted dataset
kable(colSums(is.na(imputed_data)),caption = "Number of Missnig values per column")
```



Table: Number of Missnig values per column

             x
---------  ---
steps        0
date         0
interval     0

- New imputed dataset

```r
# Showing first few rows of the imputed dataset
kable(head(imputed_data))
```

     steps  date          interval
----------  -----------  ---------
 1.7169811  2012-10-01           0
 0.3396226  2012-10-01           5
 0.1320755  2012-10-01          10
 0.1509434  2012-10-01          15
 0.0754717  2012-10-01          20
 2.0943396  2012-10-01          25

- Imputed data - Histogram, mean and median

```r
# Grouping steps count by Date, Then adding steps count across groups
totalStepsPerDay_imputed<- imputed_data %>%
    group_by(date) %>%
    summarise(steps=sum(steps,na.rm=TRUE))
# calculating mean and median for the histogram
mean2<-round(mean(totalStepsPerDay_imputed$steps),2)
median2<-round(median(totalStepsPerDay_imputed$steps),2)
# Histogram  generation
ggplot(totalStepsPerDay_imputed, aes(steps))+
    geom_histogram(binwidth = 500,alpha=0.7, col="black",aes( fill=..count..,))+
    geom_density(aes(y=..count.. *500))+
    geom_vline(xintercept = mean2,color="red",size=1.5,alpha=0.5)+
    geom_text(aes(x=8000, label=paste("Mean\n",mean2), y=8.5))+
    geom_vline(xintercept = median2,color="darkgreen",size=1.5,alpha=0.5)+
    geom_text(aes(x=13000, label=paste("Median\n",median2), y=8.5))+
    labs(title = "Histogram: Total number of steps taken each day", x="Daily steps", y="Frequency")+
    theme(legend.position = c(0.95,0.85), plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/Imputed data - Histogram, mean and median-1.png)<!-- -->

THe mean and median value is same. That is why, in the histogram, the red mean line and green median line merged and created a brown line.  


```r
# Mean 
print(paste("Mean = ",mean2))
```

```
## [1] "Mean =  10766.19"
```

```r
# Median
print(paste("Median = ",median2))
```

```
## [1] "Median =  10766.19"
```

According to the histogram and the calculated mean and median values, Imputing missing values in the dataset made the steps count variable more normalized. Also, the mean and median value became same.

## Are there differences in activity patterns between weekdays and weekends?
- New factor variable in the dataset with two levels – “weekday” and “weekend” 

```r
# Adding new column to specify the weekdays based on the dates
imputed_data$weekday <- weekdays(imputed_data$date)

# Adding new column named weekdayType to specify whether the date is a weekday or a weekend
imputed_data$weekdayType<-ifelse(weekdays(imputed_data$date) %in% c("Saturday","Sunday"), "Weekend", "Weekday")

# Showing first few rows of the modified data 
kable(head(imputed_data))
```

     steps  date          interval  weekday   weekdayType 
----------  -----------  ---------  --------  ------------
 1.7169811  2012-10-01           0  Monday    Weekday     
 0.3396226  2012-10-01           5  Monday    Weekday     
 0.1320755  2012-10-01          10  Monday    Weekday     
 0.1509434  2012-10-01          15  Monday    Weekday     
 0.0754717  2012-10-01          20  Monday    Weekday     
 2.0943396  2012-10-01          25  Monday    Weekday     

- Panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)


```r
# Grouping steps count by weekdayType and interval accordingly, Then mean of steps count across groups
timeSeriesData2<- imputed_data %>% group_by(weekdayType,interval) %>% summarise(steps = mean(steps, na.rm=TRUE))

# Panel plot
ggplot(timeSeriesData2, aes(x=interval, y=steps, color=weekdayType))+
    facet_wrap(weekdayType~.,ncol = 1)+
    geom_line()+
    labs(title = "Time series plot: Average number of steps taken by 5 Minute interval ",
         x="Five minute interval Identifier", y="Steps count averaged over all days")+
    theme( plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/panel plot-1.png)<!-- -->

