  ## Reproducible Research Week 2 Project

## Loading and preprocessing the data
  # download file from web
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity.zip", 
                mode="wb")
  # unzip data and read 
  unzip("activity.zip")
  stepdata <- read.csv("activity.csv", header = TRUE)
  head(stepdata)

  
## 1. Calculate total number of steps taken each day
    #install.packages("magrittr")
    #install.packages("dplyr")
    #install.packages("Hmisc")
  library(magrittr)
  library(dplyr)
  library(Hmisc)
  databydate <- stepdata %>% select(date, steps) %>% group_by(date) %>% summarize(tsteps= sum(steps)) %>%na.omit()
  hist(databydate$tsteps, xlab = "Total daily Steps",main="Histogram of Total Steps by day", breaks = 20)
  
  
## 2. Calculate and report the mean and median of the total number of steps taken per day
  mean(databydate$tsteps)
    #R/ [1] 10766.19
  median(databydate$tsteps)
    #R/ [1] 10765
  
## 4. Time series plot
    #install.packages("ggplot2")  
  library(ggplot2)
  databyinterval <- stepdata%>% select(interval, steps) %>% na.omit() %>% group_by(interval) %>% 
    summarize(tsteps= mean(steps)) 
  ggplot(databyinterval, aes(x=interval, y=tsteps))+ geom_line()
  
## 5.The 5-minute interval that, on average, contains the maximum number of steps
  databyinterval[which(databyinterval$tsteps== max(databyinterval$tsteps)),]
  
# Imputing missing values
  # 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
    # generate listing of NA's
    missingVals <- sum(is.na(data))
    missingVals
    #R/ [1] 0
  # 2. Devise a strategy for filling in all of the missing values in the dataset.
    library(magrittr)
    library(dplyr)
    replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
    meandata <- stepdata%>% group_by(interval) %>% mutate(steps= replacewithmean(steps))
    head(meandata)
    
  # 3. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
    FullSummedDataByDay <- aggregate(meandata$steps, by=list(meandata$date), sum)
    names(FullSummedDataByDay)[1] ="date"
    names(FullSummedDataByDay)[2] ="totalsteps"
    head(FullSummedDataByDay,15)
    
  # Summary of new data : mean & median
    summary(FullSummedDataByDay)
    
  # Making a histogram
    hist(FullSummedDataByDay$totalsteps, xlab = "Steps", ylab = "Frequency", main = "Total Daily Steps", breaks = 20)
    
  # 4. Compare the mean and median of Old and New data
    oldmean <- mean(databydate$tsteps, na.rm = TRUE)
    newmean <- mean(FullSummedDataByDay$totalsteps)
      # Old mean and New mean
        #oldmean
          #R/ [1] 10766.19
        #newmean
          #R/ [1] 10766.19
    
    oldmedian <- median(databydate$tsteps, na.rm = TRUE)
    newmedian <- median(FullSummedDataByDay$totalsteps)
      # Old median and New median
        #old median
          #R/ [1] 10765
        #new median
          #R/ [1] 10766.19
    
  # Are there differences in activity patterns between weekdays and weekends?
    meandata$date <- as.Date(meandata$date)
    meandata$weekday <- weekdays(meandata$date)
    meandata$weekend <- ifelse(meandata$weekday=="Saturday" | meandata$weekday=="Sunday", "Weekend",
                               "Weekday" )
    
    library(ggplot2)
    meandataweekendweekday <- aggregate(meandata$steps , by= list(meandata$weekend, meandata$interval)
                                        , na.omit(mean))
    names(meandataweekendweekday) <- c("weekend", "interval", "steps")
    ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + geom_line()+
      facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
      ggtitle("Comparison of Average Number of Steps in Each Interval")
    