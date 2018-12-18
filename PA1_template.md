---
title: "Reproducible Research Project 1"
author: "victorfrederick"
date: "December 18, 2018"
output: html_document
keep_md: true
---

## Loading and preprocessing the data
```{r echo=TRUE}
library(ggplot2)
library(dplyr)
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
        temp <- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
        unzip(temp)
        unlink(temp)
}
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
```{r echo=TRUE} 
# Calculate total steps per day
steps <- aggregate(steps ~ date, activity, sum)
# Make histogram
hist(steps$steps, main = paste("Total Steps by Day"), col="lightgreen", xlab="Steps")
# Find mean & median
mean(steps$steps)
median(steps$steps)
```

## What is the average daily activity pattern?
```{r echo=TRUE}
# Calculate and plot interval
interval <- aggregate(steps ~ interval, activity, mean) 
plot(interval$interval,interval$steps, type="l", xlab="Interval", ylab="Steps",main="Steps per Day by Interval")
# Find which interval has maximum steps
max <- interval[which.max(interval$steps),1]
```

## Imputing missing values
```{r echo=TRUE}
# Create table for dplyr
ACT <- tbl_df(activity)
# Find column
ACT %>% filter(is.na(steps)) %>% summarize(missing_values = n())
```

```{r echo=TRUE}
# Imput NA into new column
activity$CompleteSteps <- ifelse(is.na(activity$steps), round(interval$steps[match(activity$interval, interval$interval)],0), activity$steps)
```

```{r echo=TRUE}
# Create new dataset
activityFull <- data.frame(steps=activity$CompleteSteps, interval=activity$interval, date=activity$date)
```

```{r histogram2, echo=TRUE, fig.width=10, warning=FALSE}
# Create histogram
StepsPerDayFull <- aggregate(activityFull$steps, list(activityFull$date), FUN=sum)
colnames(StepsPerDayFull) <- c("Date", "Steps")
g <- ggplot(StepsPerDayFull, aes(Steps))
g+geom_histogram(boundary=0, binwidth=2500, col="darkgreen", fill="lightgreen")+ggtitle("Total Steps per Day No Missing Values")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))+scale_x_continuous(breaks=seq(0,25000,2500))+scale_y_continuous(breaks=seq(0,26,2))
```

```{r echo=TRUE}
# Find new mean & median
mean(StepsPerDayFull$Steps)
median(StepsPerDayFull$Steps)
```

## Are there differences in activity patterns between weekdays and weekends?
```{r echo=TRUE}
# Create formatted date variable
activityFull$RealDate <- as.Date(activityFull$date, format = "%Y-%m-%d")
# Create weekdays variable
activityFull$weekday <- weekdays(activityFull$RealDate)
# Weekday vs Weekend variable
activityFull$DayType <- ifelse(activityFull$weekday=='Saturday' | activityFull$weekday=='Sunday', 'weekend','weekday')
```

```{r timeplot2, echo=TRUE, fig.width=10, warning=FALSE}
# Create table
StepsPerTimeDT <- aggregate(steps~interval+DayType,data=activityFull,FUN=mean,na.action=na.omit)
# Time variable
StepsPerTimeDT$time <- interval$interval/100
# Plot
j <- ggplot(StepsPerTimeDT, aes(time, steps))
j+geom_line(col="darkblue")+ggtitle("Weekday & Weekend Steps")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))+facet_grid(DayType ~ .)
```