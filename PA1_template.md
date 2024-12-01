Reproducible Research Project
By James Marino
=========================================================================================

Install libraries

``` r
library(dplyr)
```


Load data from csv into DataFrame

``` r
raw_data <- read.csv("C:/Users/jjmar/OneDrive/Desktop/Reproducible Research Project/activity.csv")
head(raw_data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

Get new dataframe that is subset of raw dataframe without rows with NA values in Steps column

``` r
#Remove NA values in Steps column
steps_df_no_NAs <- subset(raw_data, complete.cases(steps))
```

Histogram of steps per day

``` r
steps_per_day <- steps_df_no_NAs %>%  select(date, steps) %>% group_by(date) %>% summarise(TotalSteps = sum(steps))
steps_per_day_col <- steps_per_day$TotalSteps 
hist(steps_per_day_col, xlab="Total Steps per Day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

Mean and Median Steps per Day

``` r
cat("Mean steps per day is ",mean(steps_per_day_col))
```

```
## Mean steps per day is  10766.19
```

``` r
cat("Median steps per day is ", median(steps_per_day_col))
```

```
## Median steps per day is  10765
```

Time-series of average number of 5 minute intervals per day

``` r
steps_per_day_5min_interval <- steps_df_no_NAs %>% filter(interval == 5) %>% select(date, steps) %>% group_by(date) %>% mutate(date = as.Date(date))

with(steps_per_day_5min_interval, plot(date, steps, type = 'l'))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

Max Steps Per Day for 5 min interval

``` r
max_steps_per_day <- max(steps_per_day_5min_interval$steps)

max_steps_per_day_date <- steps_per_day_5min_interval %>% filter(steps == max_steps_per_day) 

max_steps_per_day_date$date
```

```
## [1] "2012-10-10"
```

Calculate number of missing rows in dataset

``` r
sum(is.na(raw_data))
```

```
## [1] 2304
```

Strategy for imputing missing data is to use the mean steps per day as the missing amounts


``` r
avg_steps_per_day <- steps_df_no_NAs %>%  select(date, steps) %>% group_by(date) %>% summarise(AvgSteps = mean(steps))

merged_df <- raw_data %>% inner_join(avg_steps_per_day, by="date") %>% mutate(steps = coalesce(steps, AvgSteps)) %>% select(steps, date) %>% group_by(date) %>% summarise(TotalSteps = sum(steps))

steps_per_day_col_noNas <- merged_df$TotalSteps 

cat("Mean steps per day is ",mean(steps_per_day_col_noNas))
```

```
## Mean steps per day is  10766.19
```

``` r
cat("Median steps per day is ", median(steps_per_day_col_noNas))
```

```
## Median steps per day is  10765
```

Create a new factor variable for whether the day is a weekday or weekend

``` r
df_w_weekday_column <- steps_df_no_NAs %>%
  mutate(weekday_column = weekdays(as.Date(date)),
         weekend_indicator = as.factor(ifelse(weekday_column %in% c("Saturday", "Sunday"), 1, 0)))

head(df_w_weekday_column)
```

```
##     steps       date interval weekday_column weekend_indicator
## 289     0 2012-10-02        0        Tuesday                 0
## 290     0 2012-10-02        5        Tuesday                 0
## 291     0 2012-10-02       10        Tuesday                 0
## 292     0 2012-10-02       15        Tuesday                 0
## 293     0 2012-10-02       20        Tuesday                 0
## 294     0 2012-10-02       25        Tuesday                 0
```




``` r
interval_5_df <- df_w_weekday_column %>% filter(interval == 5) %>% mutate(date = as.Date(date))
weekday_df <- interval_5_df %>% filter(weekend_indicator == 0) %>% select(date,steps) %>% group_by(date) %>% 
                summarise(steps = mean(steps))

weekend_df <- interval_5_df %>% filter(weekend_indicator == 1) %>% select(date,steps) %>% group_by(date) %>% 
                summarise(steps = mean(steps))

par(mfrow = c(2,1))
with(weekday_df, plot(date, steps, type = 'l'))
with(weekend_df, plot(date, steps, type = 'l'))
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)
