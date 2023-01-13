Reproducible Research - Week 2 - Project 1
----------------------------------

Loading and pre-processing the data
----------------------------------

01. Reading csv Data


``` r
activityData <- read.csv("activity.csv")
```

Total number of steps taken per day
-------------------------------------------------

01.  Calculating the total number of steps taken per day

``` r
activityData$date <- as.Date(activityData$date)
p <- as.POSIXct(as.Date(activityData$date))
d <- unclass(as.POSIXlt(activityData$date))
activityData$yday <- d$yday

steps_num <- tapply(activityData$steps, activityData$yday, sum)
```
02.  Making a histogram of the total number of steps taken each day

```r
hist(steps_num, breaks = 10, col="cyan", main = "Number of steps taken - daily", xlab = "Number of steps")
```


![](https://raw.githubusercontent.com/AngeloTen/RepData_PeerAssessment1/master/instructions_fig/histogram.png)

03.  Calculating the mean and median of the total number of steps taken per day

``` r
summary(steps_num)
```
  Mean = 10766
  <br>Median = 10765

What is the average daily activity pattern?
-------------------------------------------

01.  Making a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days

``` r
mean_steps <- tapply(activityData$steps, activityData$interval, mean, na.rm = TRUE)
plot(mean_steps,type ="l", col="purple", main = "Time Series Plot \n average number of steps taken \naveraged across all days", 
     xlab="Interval", ylab="Average steps per day")
```

![](https://raw.githubusercontent.com/AngeloTen/RepData_PeerAssessment1/master/instructions_fig/timeseriesplot.png)

02.  Which 5-minute interval, on average across all the days in the data set, contains the maximum number of steps?

``` r
mean_steps[mean_steps>200]

```

    ##    835
    ## 206.1698 
Max Interval = 835

Imputing missing values
-----------------------

01.  Calculating the total number of missing values in the data set

``` r
sum(is.na(act$steps))
```

    ## [1] 2304
Total number of missing values = 2304

02.  Filling in all of the missing values in the data set
03.  Creating new data set

``` r
for(i in 1:length(activityData$steps)) 
{
  if (is.na(activityData$steps[i]))
  {activityData$steps[i] <- mean_steps[as.character(activityData$interval[i])]
  }
}
```
04.  Making new histogram of the total number of steps taken each day
<br> calculating the mean and median total number of steps taken per day.

``` r
steps_num <- tapply(activityData$steps, activityData$yday, sum)
hist(steps_num, breaks = 10, col="blue", main = "Number of steps taken - Daily",xlab = "number of steps", ylim = c(0,25))
summary(steps_num)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
     41    9819   10766   10766   12811   21194 
```
  Mean = 10766
  <br>Median = 10766
<br> compared to previous plot, <br>No difference in mean but slight difference in median(+1)

![](https://raw.githubusercontent.com/AngeloTen/RepData_PeerAssessment1/master/instructions_fig/newHist.png)

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

01.  Creating a new factor variable in the data set with two levels – “weekday” and “weekend”

``` r
activityData$wday<- weekdays(p)
for (i in 1:length(activityData$steps))
{
  if(activityData$wday[i]=="Saturday"|activityData$wday[i]=="Sunday")
  {
    activityData$day[i] <- "weekend"
  } else  
  {
    activityData$day[i] <- "weekday"
  }
}
```

02.  Making a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

``` r
activity_split <- split(activityData, activityData$day)
ave_steps_weekday <- with(activity_split$weekday,tapply(steps, interval, mean))
ave_steps_weekday <- data.frame(ave = ave_steps_weekday, interval = as.numeric(dimnames(ave_steps_weekday)[[1]]),day="weekday")
ave_steps_weekend <- with(activity_split$weekend,tapply(steps, interval, mean))
ave_steps_weekend <- data.frame(ave = ave_steps_weekend, interval = as.numeric(dimnames(ave_steps_weekend)[[1]]),day="weekend")
ave_steps <- rbind(ave_steps_weekend,ave_steps_weekday)

library(lattice)

y<- ave_steps$ave
x<- ave_steps$interval
f<-ave_steps$day
png("FinalPlot23.png")
xyplot(y~x|f, layout=c(1,2), type="l", xlab = "5-minutes Interval", ylab = "Number of Steps")
```

![](https://raw.githubusercontent.com/AngeloTen/RepData_PeerAssessment1/master/instructions_fig/panelPlot.png)