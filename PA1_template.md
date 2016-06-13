This is the R markdown file for the first project in Reproduciable
Research Course.

Dataset is downloaded and saved locally, before read the data, first set
the working directory to the corresponding folder containing the data
file.

Step 1. Loading and preprocessing the data
------------------------------------------

    setwd("C:/A_nnealing/2016Spring/Data_Science_Serial/DS-Course5/project/repdata")
    mydt <- read.csv("activity.csv", stringsAsFactors = FALSE)
    str(mydt)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

Step 2. Calculate the total and mean number of steps per day without NA value
-----------------------------------------------------------------------------

    total <- aggregate(mydt$steps, by=list(date=factor(mydt$date)), FUN=sum, na.rm = TRUE)
    head(total)

    ##         date     x
    ## 1 2012-10-01     0
    ## 2 2012-10-02   126
    ## 3 2012-10-03 11352
    ## 4 2012-10-04 12116
    ## 5 2012-10-05 13294
    ## 6 2012-10-06 15420

    hist(total$x,xlab = "Number of steps", main = "Avergae Daily Steps")

![](figure/total-1.png)

    meanS <- mean(total$x)
    medS <- median(total$x)

The mean and median of the total number of steps taken per day is
9354.2295082 and 10395 respectively.

Step 3. Estimate average daily activity pattern
-----------------------------------------------

    daily <- aggregate(mydt$steps, by=list(factor(mydt$interval)),FUN=mean,na.rm = TRUE)
    plot(daily$Group.1,daily$x, xlab = "Interval", ylab = "Number of steps", main = "Number of steps in one day")
    points(daily$Group.1,daily$x,type = "l")

![](figure/daily-1.png)

    max_day <- daily$Group.1[daily$x == max(daily$x)]

On average across all the days, the 5-minite interval of 835 contains
the maximum number of steps.

Step 4. Imputing missing values
-------------------------------

To impute the missing values, the record of steps with NA will be filled
using the average step numbers in the same 5-minute interval.

    steps <- vector(,dim(mydt)[1])
    for(i in 1:dim(mydt)[1]){
      if(is.na(mydt$steps[i])){
        avg <- daily$x[daily$Group.1 == mydt$interval[i]]
        steps[i] <- avg
      }
      else{
        steps[i] <- mydt$steps[i]
      }
    }
    new_dt <- data.frame(steps,date = mydt$date,time = mydt$interval)

Step 5. Estimate the total number of steps per day using new dataset with imputed step values
---------------------------------------------------------------------------------------------

    new_total <- aggregate(new_dt$steps, by=list(date=factor(new_dt$date)), FUN=sum)
    hist(new_total$x, xlab = "Number of steps", main = "Avergae Daily Steps with Imputed Values")

![](figure/newtotal-1.png)

    new_meanS <- mean(new_total$x)
    new_medS <- median(new_total$x)

With imputed data, the mean and median of the total number of steps
taken per day is 1.076618910^{4} and 1.076618910^{4} respectively.

Comparing with the results from the original dataset, imputing the NA
value with the average steps in the same interval increases the total
number of steps per day.

Step 6. Differences in activity patterns between weekdays and weekends
----------------------------------------------------------------------

    weekday <- weekdays(as.Date(new_dt$date))
    for(i in 1:length(weekday)){
      if(weekday[i] != "Saturday" && weekday[i] != "Sunday"){
        weekday[i] <- "weekday"
      }
      else{
        weekday[i] <- "weekend"
      }
    }
    new_dt["weekday"] <- as.factor(weekday)
    f <- split(new_dt,new_dt$weekday)
    day <- as.data.frame(f[1])[,c(1,3)]
    end <- as.data.frame(f[2])[,c(1,3)]
    day_step <- aggregate(day$weekday.steps, by=list(time = factor(day$weekday.time)),FUN=mean)
    end_step <- aggregate(end$weekend.steps, by=list(time = factor(end$weekend.time)),FUN=mean)

    par(mfrow = c(2,1))
    plot(end_step$time,end_step$x,main = "Weekend",xlab = "Interval", ylab = "Number of steps")
    points(end_step$time,end_step$x,type = "l")
    plot(day_step$time,day_step$x,main = "Weekday",xlab = "Interval", ylab = "Number of steps")
    points(day_step$time,day_step$x,type = "l")

![](figure/weekday-1.png)
