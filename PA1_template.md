---
title: "Reproducible Research Project 1"
author: "victorfrederick"
date: "December 23, 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Synopsis
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Prepare R environment for analysis and load data
```{r echo=TRUE}
library(plyr)
library(dplyr)
library(ggplot2)
activity <- read.csv("activity.csv", header = TRUE)
```

## Question 1: What is the mean total number of steps taken per day?

Calculate the total number of steps taken per day
```{r echo=TRUE}
steps <- aggregate(steps ~ date, activity, sum)
head(steps)
```

Create histogram
```{r echo=TRUE}
hist(steps$steps, main = paste("Total Steps by Day"), col="lightgreen", xlab="Steps")
```

Find mean & median
```{r echo=TRUE}
mean(steps$steps)
median(steps$steps)
```
As the data shows, the mean and median total number of steps taken per day are 10766 and 10765 respectively.


# Question 2: What is the average daily activity pattern?

Make time series plot of the 5-minute interval and the average number of steps
```{r echo=TRUE}
interval <- aggregate(steps ~ interval, activity, mean) 
plot(interval$interval,interval$steps, type="l", xlab="Interval", ylab="Steps",main="Steps per Day by Interval")
```

Find which 5-minute interval contains the maximum number of steps
```{r echo=TRUE}
max <- interval[which.max(interval$steps),1]
max
```


# Question 3: Imput missing values

Calculate and report total number of missing values in the dataset
```{r echo=TRUE}
ACT <- tbl_df(activity)
ACT %>% filter(is.na(steps)) %>% summarize(missing_values = n())
```
The total number of missing values is 2304.

Fill in missing values and create new dataset
```{r echo=TRUE}
activity$CompleteSteps <- ifelse(is.na(activity$steps), round(interval$steps[match(activity$interval, interval$interval)],0), activity$steps)
activityFull <- data.frame(steps=activity$CompleteSteps, interval=activity$interval, date=activity$date)
```

Create histogram of new dataset
```{r histogram2, echo=TRUE, fig.width=10, warning=FALSE}
StepsPerDayFull <- aggregate(activityFull$steps, list(activityFull$date), FUN=sum)
colnames(StepsPerDayFull) <- c("Date", "Steps")
g <- ggplot(StepsPerDayFull, aes(Steps))
g+geom_histogram(boundary=0, binwidth=2500, col="darkgreen", fill="lightgreen")+ggtitle("Total Steps per Day No Missing Values")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))+scale_x_continuous(breaks=seq(0,25000,2500))+scale_y_continuous(breaks=seq(0,26,2))
```

Find new mean and median
```{r echo=TRUE}
mean(StepsPerDayFull$Steps)
median(StepsPerDayFull$Steps)
```
The mean and median, 10766.64 and 10762, of the total number of steps taken per day from our newly created dataset without missing values differs slightly from the original dataset's mean and median, 10766.19 and 10765. Therefore, the impact is little but enough for variation.


# Question 4: Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels, weekday and weekend
```{r echo=TRUE}
activityFull$RealDate <- as.Date(activityFull$date, format = "%Y-%m-%d")
activityFull$weekday <- weekdays(activityFull$RealDate)
activityFull$DayType <- ifelse(activityFull$weekday=='Saturday' | activityFull$weekday=='Sunday', 'weekend','weekday')
```

Make panel plot containing a time series plot of the 5-minute interval and average number of steps taken across weekdays and weekends
```{r timeplot2, echo=TRUE, fig.width=10, warning=FALSE}
StepsPerTimeDT <- aggregate(steps~interval+DayType,data=activityFull,FUN=mean,na.action=na.omit)
StepsPerTimeDT$time <- interval$interval/100
j <- ggplot(StepsPerTimeDT, aes(time, steps))
j+geom_line(col="darkblue")+ggtitle("Weekday & Weekend Steps")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))+facet_grid(DayType ~ .)
```

The plots show us that more on average more steps are taken during the weekend leading to the supposition that the person whose data was used has a sedentary weekday occupation