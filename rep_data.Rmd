---
title: "repdata"
author: "Anvar K"
date: "October 18, 2016"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)


```

## R Markdown

This is an R Markdown document of Activity Analysis


```{r cached_chunk, cache = TRUE}
library(data.table)
file_name<-"activity.csv"
df<-read.csv(file_name, )
df$date<-as.Date(df$date)

```

Histogram of the total number of steps taken each day:

```{r}
df3 <-  aggregate(.~date, df,FUN=sum)
hist(df3$steps, n = 50, col = "green", xlab = "Steps", main = "Number of Steps")

```

```{r}

m<-mean(df3$steps, na.rm = TRUE)
med<-median(df3$steps, na.rm = TRUE)
```

Mean value of steps taken per day is: `r m `
Median value of steps taken per day is: " `r med`

```{r}
df2 <-  aggregate(.~interval, df,FUN=mean, na.rm=TRUE, na.action=na.pass)
plot(rownames(df2), df2$steps, type = "l", xlab = "Intervals", ylab = "Average Number of Steps", main = "Average Number of Steps over Intervals", col = "blue", lwd = 2)
```

```{r}
mx_steps<-max(df2$steps, na.rm = TRUE)
mx_interval<- df2$interval[df2$steps == mx_steps]

```

Maximum steps' interval (averaged over all days) is `r mx_interval` and the maximum value is `r mx_steps`


```{r}
num_of_na<-sum(is.na(df$steps))
```

Number of "NA"s in 'Steps' column: `r num_of_na `


####################################################


Now replacing "NA" values in Number of Steps by mean values in the same interval and creating a new dataset df4:
```{r}
df4<-df
for (i in 1:17568) {if (is.na(df[i,1])) {df4[i,1] <- df2$steps[df2$interval == df4[i,3]]}}
df_3 <-  aggregate(.~date, df4,FUN=sum)
df_2 <-  aggregate(.~interval, df4,FUN=mean)
```
Comparison of df and df4 dataframes:

```{r}
head(df)
head(df4)

##Counting "NA"
num_of_na<-sum(is.na(df4$steps))

```
Number of "NA"s in 'Steps' column of the new data set: `r num_of_na `


########################################
New data set histogram, median, and mean


```{r}

num_of_na<-sum(is.na(df$steps))
```

Histogram of the total number of steps taken each day:

```{r}
hist(df_3$steps, n = 50, col = "green", xlab = "Steps", main = "Number of Steps after NA replacement")


```
```{r}

m2<-mean(df_3$steps)
med2<-median(df_3$steps)
```

Mean value of steps taken per day is: `r m2 `

Median value of steps taken per day is: `r med2`



```{r}

tot<-sum(df$steps, na.rm = TRUE)
tot2<-sum(df4$steps)
s_d<-sd(df2$steps, na.rm = TRUE)
s_d2<-sd(df_2$steps)
s_d3<-sd(df3$steps, na.rm = TRUE)
s_d4<-sd(df_3$steps)
```

Total value of the steps of the initial and the new data sets are: `r tot` and `r tot2`
Standard deviations by interval are: `r s_d` and `r s_d2`
Standard deviations by date are: `r s_d3` and `r s_d4`

Thus, there is no change in meadian, mean and standard deviation by interval we replaced them by mean values in the intervals? But there is an increase in total number of steps and some decrease in standard deviation of number of steps by date. 


##################################################################
Adding new two-level factor variable week_day to the df4 data set

```{r}
df4$week_day<-lapply(df4$date, weekdays)

for (i in 1:17568) {if (df4[i,4] == "Saturday" ||df4[i,4] == "Sunday") {df4[i,4] <- "weekend" } 
  else {df4[i,4]<- "weekday"}}
head(df4)
```


Now splitting into two sets: df5 weekend and df6 weekday

```{r}

df5<-df4[FALSE,]
df6<-df4[FALSE,]
for (i in 1:17568) {if (df4[i,4] == "weekend") {df5[i,] <- df4[i,] } 
  else {df6[i,] <- df4[i,] }}

df5<-na.omit(df5)
df6<-na.omit(df6)

```
Averaging over intervals:

```{r}
df5<-df5[,-4]
df6<-df6[,-4]

df7 <-  aggregate(.~interval, df5,FUN=mean )
df8 <-  aggregate(.~interval, df6,FUN=mean )

par(mfrow=c(2,1))
plot(df7$interval, df7$steps, type = "l", col = "red", lwd = 2, main = "Weekend", ylab = "Number of Steps", xlab = "Interval")
plot(df8$interval, df8$steps, type = "l", col = "blue", lwd = 2, main = "Weekday",ylab = "Number of Steps", xlab = "Interval")

```