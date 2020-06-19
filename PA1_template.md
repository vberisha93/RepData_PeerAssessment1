---
title: "Reproducible Research - Project 1"
author: "Valdrin"
date: "6/17/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Assignment
This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

## Loading and Preprocessing Data
```{r}

#Libraries
library(ggplot2)
library(plyr)
library(knitr)

#set Working Directory
setwd("C:/Users/vberi/Documents/R_Files/Coursera/Data_Science_Foundations_Using_R/Course_5_Reproducible_Research/Project1")

#Loading and Processing the Data
activity <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?
```{r}
# What is mean total number of steps taken per day?
activityTable <- aggregate(activity$steps ~ activity$date, FUN=sum, )
colnames(activityTable)<- c("Date", "Steps")
hist(activityTable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day")

sumTable <- aggregate(activity$steps ~ activity$date, FUN=sum, )
colnames(sumTable)<- c("Date", "Steps")

#Mean of Steps
as.integer(mean(sumTable$Steps))

#Median of Steps
as.integer(median(sumTable$Steps))
```


## What is the average daily activity pattern?
```{r}
library(ggplot2)
library(plyr)
library(knitr)
#Loading and Processing the Data
activity <- read.csv("activity.csv")

#data without nas
clean <- activity[!is.na(activity$steps),]

#create average number of steps/ interval
intervalTable <- ddply(clean, .(interval), summarize, Avg = mean(steps))

#Create line plot of average number of steps/ interval
p <- ggplot(intervalTable, aes(x=interval, y=Avg), xlab = "Interval", ylab="Average Number of Steps")
p + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")

#Maximum steps by interval
maxSteps <- max(intervalTable$Avg)

#Which interval contains the maximum average number of steps
intervalTable[intervalTable$Avg==maxSteps,1]
```

## Imputing Missing Values
```{r}
NA.vals <- sum(is.na(activity$steps))

StepsPerInterval <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)

# split activity data by interval
activity.split <- split(activity, activity$interval)

# fill in missing data for each interval
for(i in 1:length(activity.split)){
    activity.split[[i]]$steps[is.na(activity.split[[i]]$steps)] <- StepsPerInterval[i]
}

activity.imputed <- do.call("rbind", activity.split)
activity.imputed <- activity.imputed[order(activity.imputed$date) ,]

StepsPerDay.imputed <- tapply(activity.imputed$steps, activity.imputed$date, sum)
hist(StepsPerDay.imputed, xlab = "Number of Steps", main = "Histogram: Steps per Day (Imputed data)")

MeanPerDay.imputed <- mean(StepsPerDay.imputed, na.rm = TRUE)
MedianPerDay.imputed <- median(StepsPerDay.imputed, na.rm = TRUE)
```

## Are there differences in activity patterns between weekdays and weekends?
```{r}

activity.imputed$day <- ifelse(weekdays(as.Date(activity.imputed$date)) == 
                        "Saturday" | weekdays(as.Date(activity.imputed$date)) == 
                        "Sunday", "weekend", "weekday")

# Calculate average steps per interval for weekends
StepsPerInterval.weekend <- tapply(activity.imputed[activity.imputed$day == 
                        "weekend" ,]$steps, activity.imputed[activity.imputed$day == 
                        "weekend" ,]$interval, mean, na.rm = TRUE)

# Calculate average steps per interval for weekdays
StepsPerInterval.weekday <- tapply(activity.imputed[activity.imputed$day == 
                        "weekday" ,]$steps, activity.imputed[activity.imputed$day == 
                        "weekday" ,]$interval, mean, na.rm = TRUE)

# Set a 2 panel plot
par(mfrow=c(1,2))

# Plot 1
plot(as.numeric(names(StepsPerInterval.weekday)), 
     StepsPerInterval.weekday, 
     xlab = "Interval", 
     ylab = "# Steps", 
     main = "Weekdays", 
     type = "l")

# Plot 2
plot(as.numeric(names(StepsPerInterval.weekend)), 
     StepsPerInterval.weekend, 
     xlab = "Interval", 
     ylab = "# Steps", 
     main = "Weekends", 
     type = "l")

```


