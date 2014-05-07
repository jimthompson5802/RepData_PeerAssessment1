# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Read the csv file into a dataframe and provide a summary of the data.

```r
df <- read.csv(file = "./activity.csv", colClasses = c("integer", "character", 
    "integer"))
cat("Number of rows and columns:", dim(df))
```

```
## Number of rows and columns: 17568 3
```

```r
cat("Data frame overview")
```

```
## Data frame overview
```

```r
str(df)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


Keep only complete observations

```r
df <- na.omit(df)
cat("Number of complete rows and columns:", dim(df))
```

```
## Number of complete rows and columns: 15264 3
```



## What is mean total number of steps taken per day?
Calculate the total number steps per day.

```r
steps.day <- by(df$steps, df$date, sum)
head(steps.day)
```

```
## df$date
## 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##        126      11352      12116      13294      15420      11015
```



```r
hist(steps.day, main = "Histogram of Total Number of Steps", xlab = "Total Number of Daily Steps")
```

![plot of chunk HistTotalDailySteps](figure/HistTotalDailySteps.png) 


Calculate mean and median total number of steps per day.

```r
mean.total.steps <- mean(steps.day)
median.total.steps <- median(steps.day)
```


* Mean total number of steps taken per day is **1.0766 &times; 10<sup>4</sup>**
* Median total number of steps taken per day is **10765**


## What is the average daily activity pattern?
Now calculate the average interval activty over the course of a day.

```r
## calculate average number of steps in an interval over all days
interval.steps <- by(df$steps, df$interval, mean)
head(interval.steps)
```

```
## df$interval
##       0       5      10      15      20      25 
## 1.71698 0.33962 0.13208 0.15094 0.07547 2.09434
```


Plot of activity over daily interval

```r
## convert character interval names to numeric values
interval <- as.integer(rownames(interval.steps))

## generate line chart
plot(interval, interval.steps, type = "l", main = "Average Daily Activity Pattern", 
    ylab = "Average number of steps", xlab = "Interval")
```

![plot of chunk ActivityPattern](figure/ActivityPattern.png) 


Determine interval with the maximum value.

```r
max.steps.idx <- which(interval.steps == max(interval.steps))
```


Five-minute interval "835" has the maximum average number of steps of 206.1698.

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
