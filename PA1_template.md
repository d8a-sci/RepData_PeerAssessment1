# Reproducible Research: Peer Assessment 1
  

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute
intervals through out the day. The data consists of two months of data from an anonymous individual collected 
during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.  

The code uses (some of) the libaries below.


```r
# Get libraries
library(ggplot2,		quietly = TRUE, warn.conflicts=FALSE)
library(plyr,			quietly = TRUE, warn.conflicts=FALSE)
library(dplyr,			quietly = TRUE, warn.conflicts=FALSE)
library(reshape2,		quietly = TRUE, warn.conflicts=FALSE)
library(data.table,		quietly = TRUE, warn.conflicts=FALSE)
library(tidyr,			quietly = TRUE, warn.conflicts=FALSE)
library(lubridate,		quietly = TRUE, warn.conflicts=FALSE)
library(qdapTools,		quietly = TRUE, warn.conflicts=FALSE)
library(RColorBrewer,		quietly = TRUE, warn.conflicts=FALSE)
```


## Loading and preprocessing the data

Below is the code that is used to

1. Find/define the data sources
1. Load the data
1. Pre-process the data



```r
# Define data source
source_data	<- "./data/activity.csv"
source_zip	<- "./data/activity.zip"

# Create folder 'data' if it doesn't exist
if ( !file.exists("./data") ) {
	dir.create("./data")
}

# Extract the source file if it doesn't exist yet,
# download zip file first if needed
if ( !file.exists(source_data) ) {
	if ( !file.exists(source_zip) ) {
		url	<- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
		if ( download.file(url, method="curl", destfile="./data/activity.zip") > 0 ) {
			stop("Download error")
		}
	}
	if ( length(unzip(zipfile="./data/activity.zip", exdir="./data")) == 0 ) {
		stop("Unzip error")
	}
}
```


```r
# Load the data
# Columns: steps, date, interval
df	<- read.csv(source_data, header=TRUE, sep=",", stringsAsFactors = FALSE)

# Add column datetime, pad() needs sort=FALSE!
df	<- cbind(df,
		 datetime=parse_date_time(paste(df$date,
						pad(df$interval,4,type="numeric",sort=FALSE)),
						"ymd_hm"))
```

   


## What is mean total number of steps taken per day?
Here we calculate the total mean of steps/day; missing values in the dataset are ignored.


```r
by_date		<- group_by(df,date)
summed		<- summarise(by_date,
			     steps.sum=sum(steps, na.rm=TRUE),
			     steps.mean=mean(steps, na.rm=TRUE),
			     steps.median=median(steps, na.rm=TRUE))
total_mean	<- mean(summed$steps.sum)
total_median	<- median(summed$steps.sum)
```

Here is a histogram for the mean steps per day.

```r
# Create plot
hist(summed$steps.sum, col="red", main="Total steps taken each day", xlab="# steps")
```

![](PA1_template_files/figure-html/hist-1.png) 

The mean number of steps taken per day is 9354.2. The median is 10395.   
   


## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
