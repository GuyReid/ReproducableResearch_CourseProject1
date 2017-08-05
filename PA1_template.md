# Activity Monitoring - Reproducable Research Project 1
Guy Reid  
3 August 2017  


This R Markdown file attempts to answer to the following questions of Project 1 of Reproducible Research, based on activity monitoring data downloaded from the following source "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

1.What is mean and median total number of steps taken per day?

2.What is the average daily activity pattern?

3.Imputing missing values

4.Are there differences in activity patterns between weekdays and weekends?

Step 1 Load the Data into a suitable format assuming that the zip file has been downloaded and unzipped to the current working directory, then using dplyr create a subset summing the number of steps aggregated by date remebering to remove NA values.


```r
library(knitr)
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
setwd("C:/Users/guyre_000/Documents")## Change as appropriate
cls = c("integer", "character", "integer")
dataset <- read.csv("activity.csv", head=TRUE, colClasses=cls, na.strings="NA")
DailySteps <- dataset %>% group_by(date) %>% summarise(steps=sum(steps,na.rm=TRUE))
```

Plot 1 Histogram of total steps per day


```r
with(DailySteps, hist(steps,xlab="Steps",main="Total Steps per day"))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Calculate mean and median steps per day


```r
mean(DailySteps$steps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(DailySteps$steps)
```

```
## [1] 10395
```
Average steps taken per day, create aggregated subset showing the average per day followed by plotting the 

```r
AVGDailySteps <- dataset %>% group_by(interval) %>% summarise(steps=mean(steps,na.rm=TRUE))
with(AVGDailySteps,plot(interval,steps,type="l", xlab = "time interval", ylab = "Average steps", main = "Average steps taken over all days vs n time interval"),col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
Determine the time interval where the  maximum number of steps occurs


```r
AVGDailySteps[AVGDailySteps$steps == max(AVGDailySteps$steps),1]
```

```
## # A tibble: 1 × 1
##   interval
##      <int>
## 1      835
```
Imputing Missing Values - Determine number of rows with NAs


```r
na <- dataset[dataset$steps %in% NA,]
dim(na)
```

```
## [1] 2304    3
```
Imputing missing values by assuming on occasions where NA exists, there was no activity so NA = 0


```r
na$steps <- 0
dataset2 <- rbind(dataset[complete.cases(dataset),],na)
##Test to confirm no NA values exist after imputing
sum(is.na(dataset2$steps))
```

```
## [1] 0
```
Create Histogram with new Dataset with NAs imputed, followed by mean and median calculations


```r
DailySteps2 <- dataset2 %>% group_by(date) %>% summarise(steps=sum(steps,na.rm=TRUE))
hist(DailySteps2$steps, 
    main = "Total Steps per Day Complete", 
    xlab = "Number of Steps per Day", 
    ylab = "Interval",
    col="green",
    breaks=50)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
mean(DailySteps2$steps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(DailySteps2$steps)
```

```
## [1] 10395
```
Are there differences between weekend and weekday activity?
Define weekdays and weekends based on day of week 
Define mean steps per interval
Create panel plot using GGplot making the distinction between weekday and weekend activity


```r
library(ggplot2)
dataset2$date <- as.Date(dataset2$date,"%Y-%m-%d")
dataset2$week <- ifelse(weekdays(dataset2$date) %in% c("Saturday","Sunday"),"weekend","weekday")
dataset2$week <- as.factor(dataset2$week)
newsteps <- dataset2 %>% group_by(week,interval) %>% summarise(steps=mean(steps))
g <- ggplot(newsteps,aes(interval,steps,group=week))
g + geom_line() + facet_grid(week~.)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
Are there differences in activity patterns between weekdays and weekends?



