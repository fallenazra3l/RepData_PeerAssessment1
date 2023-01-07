---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Loading and preprocessing the data  


Use unz to extract activity.csv from compressed .zip, read in as a data frame

```r
activityData <- read.csv(unz("activity.zip", filename = "activity.csv"), header = TRUE)
summary(activityData)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```
The summary shows a large number of NA's but they'll be dealt with later on 

## What is mean total number of steps taken per day?



```r
# Scoop up all the dates
dates <- unique(activityData$date)

# Calculate totals in a vector parallel to dates
totals <- numeric()
for(i in 1:length(dates)) {
  totals <- c(totals, 
             sum(activityData[activityData$date==dates[i],1], na.rm = TRUE))
}

# Histogram of totals
hist(totals,
     main = "Total steps taken per day",
     xlab = "Total steps",
     ylab = "Frequency (Days)")
```

![](PA1_files/figure-html/meansteps-1.png)<!-- -->

```r
# Final calculations
meanTotal <- mean(totals)
medianTotal <- median(totals)
```

na.rm set to TRUE to remove all NA values from mean calculation 

The mean of total steps per day is 9354.2295082 steps  

The median of total steps per day is 1.0395\times 10^{4} steps  

## What is the average daily activity pattern?

```r
## Calculate total steps at each time interval across every day 

# Collect every time interval value 
intervals <- unique(activityData$interval)

# Calculate totals in a vector parallel to intervals
totalIntSteps <- numeric()
for(i in 1:length(intervals)) {
  totalIntSteps <- c(totalIntSteps, 
                    sum(activityData[activityData$interval==intervals[i],1], 
                    na.rm = TRUE))
}

# Divide every sum by the total number of days
# Mean for that interval over every day 
meanIntSteps <- totalIntSteps/length(dates)

# Plot mean steps over all intervals
plot(intervals, meanIntSteps, 
     type="l",
     main = "Mean steps in each time interval",
     xlab = "Time intervals (24 hr-clock HHMM)",
     ylab = "Mean steps")
```

![](PA1_files/figure-html/meanactivity-1.png)<!-- -->

```r
# Find peak activity time (intervals vector @ index of max steps)
intervals[which.max(meanIntSteps)]
```

```
## [1] 835
```
Peak activity time is observed around the interval 835  

## Imputing missing values  
All NA's will be replaced with mean steps for that time interval 

```r
# Copy the data into a new DF for editing
newData <- activityData
# loop through all NA's
for(i in 1:nrow(activityData)) {
  # Replace NA's with mean steps for the correct time interval
  if(is.na(activityData[i,1])) {
    newData[i,1] <- meanIntSteps[intervals == newData[i,3]]
  }
}

# Quick check 
# head(newData)

# Calculate totals once more (no longer need remove na)
totals <- numeric()
for(i in 1:length(dates)) {
  totals <- c(totals, 
             sum(newData[newData$date==dates[i],1]))
}


# Create histogram of totals
hist(totals,
     main = "Total steps taken per day",
     xlab = "Total steps",
     ylab = "Frequency (Days)")
```

![](PA1_files/figure-html/missingVals-1.png)<!-- -->

```r
# Final calculations
meanTotal <- mean(totals)
medianTotal <- median(totals)
```
The mean of total steps per day is 1.0581014\times 10^{4} steps  

The median of total steps per day is 1.0395\times 10^{4} steps 

The mean value has risen. Replacement of NA's with mean interval value across 
all days resulted in a slightly more normal distribution, specifically this
appears to be due to a greater frequency of days where total steps numbered 
between 5000-10000

## Are there differences in activity patterns between weekdays and weekends?
