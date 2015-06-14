# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

First I will clean any remaining variable in the environment:

```r
rm(list = ls(all.names = TRUE))
```
Then if the **activity.csv** file is not present in the current directory, one has to download it from [Activity Dataset for Assignment 1](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) and place it in the current directory.
As described in the exercise, this dataset has recorded **steps** in a 5-minute **interval** for a specified **date**.
I load the file, trasform the date into a date object and do some preliminary inspection:

```r
activity = read.csv("activity.csv", header = T, stringsAsFactors = FALSE)
activity$date = as.Date(activity$date)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(activity)
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
summary(activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```


## What is mean total number of steps taken per day?

First we calculate the total number of steps per day and store it in the variable **stepsByDate**. We will use the library *dplyr* examined in the 3rd course of the specialization.

```r
require(dplyr)
stepsByDate = activity %>% 
              group_by(date) %>% 
              summarize(totalSteps = sum(steps, na.rm = TRUE)) %>% 
              collect()
```
Then we use the package *ggplot* examined in the 4th course of the specialization to plot the histogram.
We use a bindwidth of 1000 and colorize the bars by the number of measurements in each group of the histogram.
The mean and median are calculated in the variable *meanSteps* and *medianSteps* accordingly and are shown on the graph and the graph legend

```r
require(ggplot2)
meanSteps = round(mean(stepsByDate$totalSteps))
medianSteps = median(stepsByDate$totalSteps)
ggplot(stepsByDate) + 
        geom_histogram(binwidth = 1100, aes(x = totalSteps, fill = ..count..)) + 
        labs(x = "Total Steps taken") + 
        ggtitle("Total number of steps taken per day") + 
        geom_vline(aes(xintercept = meanSteps, colour = "red"), size = 2, linetype = "longdash") + 
        geom_vline(aes(xintercept = medianSteps, colour = "green"), size = 2, linetype = "longdash", show_guide = TRUE) + 
        scale_colour_manual(name = "Steps", values = c('red' = 'red', 'green' = 'green'), labels = c(paste("Mean:", meanSteps), paste("Median:", medianSteps)), guide = 'legend')
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

## What is the average daily activity pattern?




## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
