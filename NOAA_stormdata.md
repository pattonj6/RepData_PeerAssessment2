# NOAA Stormdata Tracking between 1950-2011

## Synopsis

In this report we are analyzing the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database.  The database has tracking for events in the US 50 states as well as 22 other locations like US territorities  (e.g. Guam, American Samoa).  Tornados, heat, and wind are the top 3 events that measured the highest number of combined injuries + fatalities.  We combined injuries and fatalities into a single metric to determine the ranking.  Extreme heat-related events appear to 

## Loading and Processing the Data


```r
library(lubridate)
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(data.table)
```

```
## 
## Attaching package: 'data.table'
## 
## The following objects are masked from 'package:dplyr':
## 
##     between, last
## 
## The following objects are masked from 'package:lubridate':
## 
##     hour, mday, month, quarter, wday, week, yday, year
```

```r
library(tidyr)
```

```
## Warning: package 'tidyr' was built under R version 3.2.3
```

```r
library(knitr)
## loading for US State Facts and Figures
library(datasets)

## output wider graphs
opts_chunk$set(out.width='750px', dpi=200)

if (!file.exists("./data")) {
    dir.create("./data")
}
```

Download, unzip, and read in the data.  This was done on Windows7 OS, so downloaded in 'wb' mode.


```r
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileurl, destfile = "./data/repdata%2Fdata%2FStormData.csv.bz2", mode = 'wb')

data <- read.csv("./data/repdata%2Fdata%2FStormData.csv.bz2", stringsAsFactors = FALSE)
```

Take a preliminary look at the data.


```r
str(data)
```

```
## 'data.frame':	902297 obs. of  37 variables:
##  $ STATE__   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_DATE  : chr  "4/18/1950 0:00:00" "4/18/1950 0:00:00" "2/20/1951 0:00:00" "6/8/1951 0:00:00" ...
##  $ BGN_TIME  : chr  "0130" "0145" "1600" "0900" ...
##  $ TIME_ZONE : chr  "CST" "CST" "CST" "CST" ...
##  $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57 ...
##  $ COUNTYNAME: chr  "MOBILE" "BALDWIN" "FAYETTE" "MADISON" ...
##  $ STATE     : chr  "AL" "AL" "AL" "AL" ...
##  $ EVTYPE    : chr  "TORNADO" "TORNADO" "TORNADO" "TORNADO" ...
##  $ BGN_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ BGN_AZI   : chr  "" "" "" "" ...
##  $ BGN_LOCATI: chr  "" "" "" "" ...
##  $ END_DATE  : chr  "" "" "" "" ...
##  $ END_TIME  : chr  "" "" "" "" ...
##  $ COUNTY_END: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ COUNTYENDN: logi  NA NA NA NA NA NA ...
##  $ END_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ END_AZI   : chr  "" "" "" "" ...
##  $ END_LOCATI: chr  "" "" "" "" ...
##  $ LENGTH    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
##  $ WIDTH     : num  100 150 123 100 150 177 33 33 100 100 ...
##  $ F         : int  3 2 2 2 2 2 2 1 3 3 ...
##  $ MAG       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: chr  "K" "K" "K" "K" ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: chr  "" "" "" "" ...
##  $ WFO       : chr  "" "" "" "" ...
##  $ STATEOFFIC: chr  "" "" "" "" ...
##  $ ZONENAMES : chr  "" "" "" "" ...
##  $ LATITUDE  : num  3040 3042 3340 3458 3412 ...
##  $ LONGITUDE : num  8812 8755 8742 8626 8642 ...
##  $ LATITUDE_E: num  3051 0 0 0 0 ...
##  $ LONGITUDE_: num  8806 0 0 0 0 ...
##  $ REMARKS   : chr  "" "" "" "" ...
##  $ REFNUM    : num  1 2 3 4 5 6 7 8 9 10 ...
```

```r
head(data)
```

```
##   STATE__           BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE
## 1       1  4/18/1950 0:00:00     0130       CST     97     MOBILE    AL
## 2       1  4/18/1950 0:00:00     0145       CST      3    BALDWIN    AL
## 3       1  2/20/1951 0:00:00     1600       CST     57    FAYETTE    AL
## 4       1   6/8/1951 0:00:00     0900       CST     89    MADISON    AL
## 5       1 11/15/1951 0:00:00     1500       CST     43    CULLMAN    AL
## 6       1 11/15/1951 0:00:00     2000       CST     77 LAUDERDALE    AL
##    EVTYPE BGN_RANGE BGN_AZI BGN_LOCATI END_DATE END_TIME COUNTY_END
## 1 TORNADO         0                                               0
## 2 TORNADO         0                                               0
## 3 TORNADO         0                                               0
## 4 TORNADO         0                                               0
## 5 TORNADO         0                                               0
## 6 TORNADO         0                                               0
##   COUNTYENDN END_RANGE END_AZI END_LOCATI LENGTH WIDTH F MAG FATALITIES
## 1         NA         0                      14.0   100 3   0          0
## 2         NA         0                       2.0   150 2   0          0
## 3         NA         0                       0.1   123 2   0          0
## 4         NA         0                       0.0   100 2   0          0
## 5         NA         0                       0.0   150 2   0          0
## 6         NA         0                       1.5   177 2   0          0
##   INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP WFO STATEOFFIC ZONENAMES
## 1       15    25.0          K       0                                    
## 2        0     2.5          K       0                                    
## 3        2    25.0          K       0                                    
## 4        2     2.5          K       0                                    
## 5        2     2.5          K       0                                    
## 6        6     2.5          K       0                                    
##   LATITUDE LONGITUDE LATITUDE_E LONGITUDE_ REMARKS REFNUM
## 1     3040      8812       3051       8806              1
## 2     3042      8755          0          0              2
## 3     3340      8742          0          0              3
## 4     3458      8626          0          0              4
## 5     3412      8642          0          0              5
## 6     3450      8748          0          0              6
```

```r
summary(data)
```

```
##     STATE__       BGN_DATE           BGN_TIME          TIME_ZONE        
##  Min.   : 1.0   Length:902297      Length:902297      Length:902297     
##  1st Qu.:19.0   Class :character   Class :character   Class :character  
##  Median :30.0   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :31.2                                                           
##  3rd Qu.:45.0                                                           
##  Max.   :95.0                                                           
##                                                                         
##      COUNTY       COUNTYNAME           STATE              EVTYPE         
##  Min.   :  0.0   Length:902297      Length:902297      Length:902297     
##  1st Qu.: 31.0   Class :character   Class :character   Class :character  
##  Median : 75.0   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :100.6                                                           
##  3rd Qu.:131.0                                                           
##  Max.   :873.0                                                           
##                                                                          
##    BGN_RANGE          BGN_AZI           BGN_LOCATI       
##  Min.   :   0.000   Length:902297      Length:902297     
##  1st Qu.:   0.000   Class :character   Class :character  
##  Median :   0.000   Mode  :character   Mode  :character  
##  Mean   :   1.484                                        
##  3rd Qu.:   1.000                                        
##  Max.   :3749.000                                        
##                                                          
##    END_DATE           END_TIME           COUNTY_END COUNTYENDN    
##  Length:902297      Length:902297      Min.   :0    Mode:logical  
##  Class :character   Class :character   1st Qu.:0    NA's:902297   
##  Mode  :character   Mode  :character   Median :0                  
##                                        Mean   :0                  
##                                        3rd Qu.:0                  
##                                        Max.   :0                  
##                                                                   
##    END_RANGE          END_AZI           END_LOCATI       
##  Min.   :  0.0000   Length:902297      Length:902297     
##  1st Qu.:  0.0000   Class :character   Class :character  
##  Median :  0.0000   Mode  :character   Mode  :character  
##  Mean   :  0.9862                                        
##  3rd Qu.:  0.0000                                        
##  Max.   :925.0000                                        
##                                                          
##      LENGTH              WIDTH                F               MAG         
##  Min.   :   0.0000   Min.   :   0.000   Min.   :0.0      Min.   :    0.0  
##  1st Qu.:   0.0000   1st Qu.:   0.000   1st Qu.:0.0      1st Qu.:    0.0  
##  Median :   0.0000   Median :   0.000   Median :1.0      Median :   50.0  
##  Mean   :   0.2301   Mean   :   7.503   Mean   :0.9      Mean   :   46.9  
##  3rd Qu.:   0.0000   3rd Qu.:   0.000   3rd Qu.:1.0      3rd Qu.:   75.0  
##  Max.   :2315.0000   Max.   :4400.000   Max.   :5.0      Max.   :22000.0  
##                                         NA's   :843563                    
##    FATALITIES          INJURIES            PROPDMG       
##  Min.   :  0.0000   Min.   :   0.0000   Min.   :   0.00  
##  1st Qu.:  0.0000   1st Qu.:   0.0000   1st Qu.:   0.00  
##  Median :  0.0000   Median :   0.0000   Median :   0.00  
##  Mean   :  0.0168   Mean   :   0.1557   Mean   :  12.06  
##  3rd Qu.:  0.0000   3rd Qu.:   0.0000   3rd Qu.:   0.50  
##  Max.   :583.0000   Max.   :1700.0000   Max.   :5000.00  
##                                                          
##   PROPDMGEXP           CROPDMG         CROPDMGEXP       
##  Length:902297      Min.   :  0.000   Length:902297     
##  Class :character   1st Qu.:  0.000   Class :character  
##  Mode  :character   Median :  0.000   Mode  :character  
##                     Mean   :  1.527                     
##                     3rd Qu.:  0.000                     
##                     Max.   :990.000                     
##                                                         
##      WFO             STATEOFFIC         ZONENAMES            LATITUDE   
##  Length:902297      Length:902297      Length:902297      Min.   :   0  
##  Class :character   Class :character   Class :character   1st Qu.:2802  
##  Mode  :character   Mode  :character   Mode  :character   Median :3540  
##                                                           Mean   :2875  
##                                                           3rd Qu.:4019  
##                                                           Max.   :9706  
##                                                           NA's   :47    
##    LONGITUDE        LATITUDE_E     LONGITUDE_       REMARKS         
##  Min.   :-14451   Min.   :   0   Min.   :-14455   Length:902297     
##  1st Qu.:  7247   1st Qu.:   0   1st Qu.:     0   Class :character  
##  Median :  8707   Median :   0   Median :     0   Mode  :character  
##  Mean   :  6940   Mean   :1452   Mean   :  3509                     
##  3rd Qu.:  9605   3rd Qu.:3549   3rd Qu.:  8735                     
##  Max.   : 17124   Max.   :9706   Max.   :106220                     
##                   NA's   :40                                        
##      REFNUM      
##  Min.   :     1  
##  1st Qu.:225575  
##  Median :451149  
##  Mean   :451149  
##  3rd Qu.:676723  
##  Max.   :902297  
## 
```

This was moderately helpful, we are interested in health and economic impacts by events.  I see the columns of interest.

Let's start to explore the data in the columns, here are some basic questions I have.


```r
length(unique(data$EVTYPE)) -> disaster.type.count
range(data$FATALITIES) -> max.death.count
range(data$INJURIES) -> max.injury.count

length(unique(data$STATE)) -> states.impacted.count
unique(data$STATE) -> states.impacted
setdiff(states.impacted, state.abb) -> impacted.outside.50.states
```

There are 22 locations outside the US 50 states that are included in this data set.  I see Guam, Puerto Rico, Virgin Islands....several others I don't recognize, immediately.  For this analysis we are asked to look at just the United States, which means I will need to remove all of the extra locations in the data, rather than filter just for U.S.  "DC" appears to be Washington, DC, so I will include that one.

convert from data.frame to my preferred data.table.


```r
dt_data <- data.table(data)

## check for NA values
na.rows.injuries <- sum(is.na(dt_data$INJURIES))
na.rows.deaths <- sum(is.na(dt_data$FATALITIES))
```

There are no NA rows....good!

## Health impact analysis

Calculate: 
--Remove non-US states.  Keep Washington, DC.
--The total sum and averages of deaths and injuries by event type.
--Also calculate a total sum of fatal+injure by event type.


```r
impacted.outside.50.states.no.DC <- impacted.outside.50.states[!impacted.outside.50.states %in% 
                                                 "DC"]

dt_data_nonUS_removed <- dt_data[which(!dt_data$STATE %in% 
                                           impacted.outside.50.states.no.DC),]

dt_data_sums <- dt_data_nonUS_removed[ ,.(sum.fatalities=sum(FATALITIES), avg.fatalities=mean(FATALITIES),
                                          sum.injuries=sum(INJURIES), avg.injuries=mean(INJURIES),
                                          sum.fatal.or.injure=sum(FATALITIES,INJURIES)),
                                       by=EVTYPE][order(-sum.fatal.or.injure)][sum.fatal.or.injure >= 100]
```

I filtered out rows of data < 100 combined injuries or fatalities.  There are misspelled EVTYPE labels 
and lables that don't all fit into 1 common heading, so end up as a separate observation with 
no injuries or fatalities.  A perfect data set would combine in these misspelled EVTYPE labels
and other categories so that averages are correctly counted.  I am going to ignore this consolidation work
for this exercise.

Let's take a look at the sum.fatal.or.injure in dot plot format.  I chose this since EVTYPE is a long discrete list, and I want to see the overall ranking in easy-to-read labels.


```r
a <- ggplot(dt_data_sums, aes(log(sum.fatal.or.injure), reorder(EVTYPE,sum.fatal.or.injure))) +
                geom_point(size=3)
print(a)
```

<img src="NOAA_stormdata_files/figure-html/unnamed-chunk-7-1.png" title="" alt="" width="750px" />

Tornado (by 2.5x orders of magnitude), Excessive Heat, and TSTM Wind are the top 3 injury and fatality events, since 1950.  I'd like to plot all of the injury, fatality, and average data on the same plot, so I need to tidy the data.  Data is ordered by sum.fatal.or.injure.  I know it is going to deprecate my duplicate levels, this is ok for this plot.


```r
dt_data_sums %>% gather(type, count, -EVTYPE) -> tidy.data
nameorder <- tidy.data$EVTYPE[order(tidy.data[which(tidy.data$type == "sum.fatal.or.injure"),])]
tidy.data$EVTYPE <- factor(tidy.data$EVTYPE, levels = nameorder)
```

```
## Warning in `levels<-`(`*tmp*`, value = if (nl == nL) as.character(labels)
## else paste0(labels, : duplicated levels in factors are deprecated
```


```r
b <- ggplot(tidy.data, aes(count, EVTYPE)) + 
    geom_point(size=4, aes(colour=type)) +
    scale_x_log10() +
    ggtitle("storm event versus injury/fatality")
print(b)
```

```
## Warning in `levels<-`(`*tmp*`, value = if (nl == nL) as.character(labels)
## else paste0(labels, : duplicated levels in factors are deprecated
```

```
## Warning in `levels<-`(`*tmp*`, value = if (nl == nL) as.character(labels)
## else paste0(labels, : duplicated levels in factors are deprecated
```

<img src="NOAA_stormdata_files/figure-html/unnamed-chunk-9-1.png" title="" alt="" width="750px" />

I love this dot plot!  Clearly, tornado by itself is on top for the total historical sum.fatal.or.injure metric.  However, if you study the avg.injuries(blue) and avg.fatalities(green) dots you'll see that all "heat" categories and hurricane/typhoon stand out above the baseline data.  ("Glaze"" and "Wild Fires" also show elevated levels from baseline, but further down the chart.) This means that on average, when these events occur, they injure or kill more people than even tornados.  But since 1950, tornados have injured or killed a larger total, likely do to a few signficantly bad events.

Top priority would be placed on tornados, heat, and hurricane/typhoons.  I suspect heat would jump to the top of the list if we combined all 4 "heat" categories into a single line item.  (If I wasn't limited to 3 figures I would do this analysis!)

## Economic Impact Analysis

Calculate:
--Total sum and average values using PROPDMG and CROPDMG columns by event type.  Even though CROPDMG is a separate column, I still consider it a direct economic impact event and can be combined with PROPDMG to examine overall economic impact. 
--Remove non-US states and rows with incorrect "EXP" sympols or values.


```r
range(dt_data_nonUS_removed$PROPDMG) -> prop.damage.range
unique(dt_data_nonUS_removed$PROPDMGEXP) -> prop.damage.exp
range(dt_data_nonUS_removed$CROPDMG) -> crop.damage.range
unique(dt_data_nonUS_removed$CROPDMGEXP) -> crop.damage.exp
```

The exponent column is supposed to just have "B", "M", "K", or "H".  I'd like to do a quick
check on a few rows with the other values to figure out why they are there.


```r
dt_data_rows_wrong_exp <- dt_data_nonUS_removed[which(dt_data_nonUS_removed$PROPDMGEXP %in% 
                                            c("+", "5", "6", "?", "4", "2", "3", "7", "-")),]
```

Is there a notes section at the bottom of the data?  Why are numbers in the EXP column?


```r
tail(dt_data_nonUS_removed$PROPDMGEXP)
```

```
## [1] "K" "K" "K" "K" "K" "K"
```

```r
identical(crop.damage.exp, prop.damage.exp)
```

```
## [1] FALSE
```

```r
identical(dt_data_nonUS_removed$CROPDMGEXP, dt_data_nonUS_removed$PROPDMGEXP)
```

```
## [1] FALSE
```

The remarks column has some notes on total damage estimates that sort of match up with the
numbers in the EXP columns, but this only happens on a few of them.  It appears these numeric values are due to rounding not to 3 significant figures, or just incorrect input of data.  

How many rows are like this?


```r
print(prop.damage.exp)
```

```
##  [1] "K" "M" ""  "B" "m" "+" "0" "5" "6" "?" "4" "2" "3" "h" "7" "H" "-"
## [18] "1" "8"
```

```r
print(crop.damage.exp)
```

```
## [1] ""  "M" "K" "m" "B" "?" "0" "k" "2"
```

```r
prop.damage.exp.incorrect <- prop.damage.exp[!prop.damage.exp %in% 
                                                 c("K","M","B","m","h","H", "", "0")]

crop.damage.exp.incorrect <- crop.damage.exp[!crop.damage.exp %in% 
                                                 c("k","K","M","B","m","h","H", "", "0")]

nrow(dt_data_nonUS_removed[which(dt_data_nonUS_removed$PROPDMGEXP %in% 
                                                            prop.damage.exp.incorrect),])
```

```
## [1] 98
```

```r
nrow(dt_data_nonUS_removed[which(dt_data_nonUS_removed$CROPDMGEXP %in% 
                                                            crop.damage.exp.incorrect),])
```

```
## [1] 8
```

98+8 rows with incorrect EXP values....ok, not many.  I am going to remove them from the analysis.

Now I am going to filter out the rows that contain incorrect "EXP" values.
Go back to original dt_data.


```r
dt_data_w_exp_rows_removed <- dt_data_nonUS_removed[which(!dt_data_nonUS_removed$PROPDMGEXP %in% 
                                           prop.damage.exp.incorrect),]
dt_data_w_exp_rows_removed <- dt_data_w_exp_rows_removed[which
                                            (!dt_data_w_exp_rows_removed$CROPDMGEXP %in% 
                                                crop.damage.exp.incorrect),]

print(unique(dt_data_w_exp_rows_removed$PROPDMGEXP))
```

```
## [1] "K" "M" ""  "B" "m" "0" "h" "H"
```

```r
print(unique(dt_data_w_exp_rows_removed$CROPDMGEXP))
```

```
## [1] ""  "M" "K" "m" "B" "0" "k"
```

OK, good, we removed the rows with incorrect "EXP" values. Now, create a function to translate character mulitpliers into numeric.


```r
exp.magnitude <- function(x) {
    ifelse(x == "K", 10^3,
    ifelse(x == "M", 10^6,
    ifelse(x == "" , 1,
    ifelse(x == "B", 10^9,
    ifelse(x == "m", 10^6,
    ifelse(x == "0" , 1,
    ifelse(x == "h", 10^2,
    ifelse(x == "H", 10^2, NA))))))))
}
```

Create the new columns with numeric multiplier, execute the multiplication, create a prop+crop summary column.


```r
dt_data_w_exp_rows_removed[ , prop.damage.multiplier := exp.magnitude(PROPDMGEXP)]
dt_data_w_exp_rows_removed[ , crop.damage.multiplier := exp.magnitude(CROPDMGEXP)]

dt_data_w_exp_rows_removed[ , prop.damage.total := prop.damage.multiplier*PROPDMG]
dt_data_w_exp_rows_removed[ , crop.damage.total := crop.damage.multiplier*CROPDMG]
dt_data_w_exp_rows_removed[ , damage.total := prop.damage.total+crop.damage.total]
```

Now generate the statistics.  Sum and averages for property and crop, plus sum/averages for prop+crop, all by event type.  Since I am plotting on a dot plot, I filtered out rows to make the plot readable.


```r
dt_data_econ <- dt_data_w_exp_rows_removed[ ,.(sum.property=sum(prop.damage.total), 
                                               avg.property=mean(prop.damage.total), 
                                         sum.crop=sum(crop.damage.total), 
                                         avg.crop=mean(crop.damage.total), 
                                         sum.prop.and.crop=sum(damage.total)),
                                      by=EVTYPE][order(-sum.prop.and.crop)][sum.prop.and.crop >100000000]
```

I'd like to plot all of the property, crop, and combined data on the same plot, so I need to tidy the data.  Data is ordered by sum.prop.or.crop.  I know it is going to deprecate my duplicate levels, this is ok for this plot.


```r
dt_data_econ %>% gather(type, count, -EVTYPE) -> tidy.econ.data
nameorder <- tidy.econ.data$EVTYPE[order(tidy.econ.data[which(tidy.econ.data$type == "sum.prop.and.crop"),])]
tidy.econ.data$EVTYPE <- factor(tidy.econ.data$EVTYPE, levels = nameorder)
```

```
## Warning in `levels<-`(`*tmp*`, value = if (nl == nL) as.character(labels)
## else paste0(labels, : duplicated levels in factors are deprecated
```


```r
b <- ggplot(tidy.econ.data, aes(count, EVTYPE)) + 
    geom_point(size=4, aes(colour=type)) +
    scale_x_log10() + 
    xlab("US dollar amount") +
    ggtitle("storm event vs. property/crop damage loss ($)")
print(b)
```

```
## Warning in `levels<-`(`*tmp*`, value = if (nl == nL) as.character(labels)
## else paste0(labels, : duplicated levels in factors are deprecated
```

```
## Warning in `levels<-`(`*tmp*`, value = if (nl == nL) as.character(labels)
## else paste0(labels, : duplicated levels in factors are deprecated
```

<img src="NOAA_stormdata_files/figure-html/unnamed-chunk-19-1.png" title="" alt="" width="750px" />

Top 3 for economic impact are flood, hurrican/typhoon, and tornado.  Interesting to note that the majority of economic loss is in property, evidenced by the overlap of red(sum.property) and pink(sum.prop.and.crop) datapoints for most events.  Temperature related events (drought, extreme heat/cold, frost) all show higher crop damage than property damage - this intuitively makes sense.  The catastropic events like flood, hurricane/typhoon, tornados are the ones that cause large property damage.
