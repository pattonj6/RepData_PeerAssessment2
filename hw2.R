## 2.28.16 jpatton
## Homework Assignment 2 - Reproducible Research
## 1. Across the United States, which types of events 
## (as indicated in the EVTYPE variable) are most harmful 
## with respect to population health?
## 2. Across the United States, which types of events have 
## the greatest economic consequences?


library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)
## loading for US State Facts and Figures
library(datasets)

## sesssionInfo() -> session.info

if (!file.exists("./data")) {
    dir.create("./data")
}

fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileurl, destfile = "./data/repdata%2Fdata%2FStormData.csv.bz2", mode = 'wb')

data <- read.csv("./data/repdata%2Fdata%2FStormData.csv.bz2", stringsAsFactors = FALSE)

str(data)
head(data)
summary(data)

## start to explore the data
## basic questions
length(unique(data$EVTYPE)) -> disaster.type.count
range(data$FATALITIES) -> max.death.count
range(data$INJURIES) -> max.injury.count

length(unique(data$STATE)) -> states.impacted.count
unique(data$STATE) -> states.impacted
setdiff(states.impacted, state.abb) -> impacted.outside.50.states

## convert from data.frame to data.table
dt_data <- data.table(data)

## check for NA values
na.rows.injuries <- sum(is.na(dt_data$INJURIES))
na.rows.deaths <- sum(is.na(dt_data$FATALITIES))

## calculate: 
## --The sum and averages of deaths and injuries by event type.
## --Also calculate a total sum of fatal+injure by event type.
## We can ignore property damage for now, this is not "population health".
dt_data_sums <- dt_data[ ,.(sum.fatalities=sum(FATALITIES), avg.fatalities=mean(FATALITIES), 
         sum.injuries=sum(INJURIES), avg.injuries=mean(INJURIES), 
         sum.fatal.or.injure=sum(FATALITIES,INJURIES)),
         by=EVTYPE][order(-sum.fatal.or.injure)][sum.fatal.or.injure >= 100]

## I filtered out rows of data < 100 combined injuries or fatalities.  There are misspelled EVTYPE labels
## and lables that don't all fit into 1 common heading, so land as a separate observation with 
## no injuries or fatalities.  A perfect data set would combine in these misspelled EVTYPE labels
## and other categories so that averages are correctly counted.  I am going to ignore this work
## for this exercise, and only focus on the top most health harmful events.

a <- ggplot(dt_data_sums, aes(log(sum.fatal.or.injure), reorder(EVTYPE,sum.fatal.or.injure))) +
                geom_point(size=3)
print(a)

## I'd like to plot all of the data on the same plot, so I need to tidy the data.
## Data is ordered by sum.fatal.or.injure.
dt_data_sums %>% gather(type, count, -EVTYPE) -> tidy.data
nameorder <- tidy.data$EVTYPE[order(tidy.data[which(tidy.data$type == "sum.fatal.or.injure"),])]
tidy.data$EVTYPE <- factor(tidy.data$EVTYPE, levels = nameorder)

b <- ggplot(tidy.data, aes(log(count), EVTYPE)) + 
    geom_point(size=4, aes(colour=type))
print(b)

## I love this dot plot!  Clearly, tornado by itself is on top for the fatality+injury metric.  
## But I notice that "heat" is represented multiple times by different names.  I wonder what the 
## data looks like if I combine rows for "heat", "wind", and "cold"?

##############
dt_data_grep_rows <- dt_data[grep("(heat|wind|cold)", dt_data$EVTYPE, ignore.case = TRUE),]

dt_data_combo_sums <- dt_data[ , .(sum.fatalities=sum(FATALITIES), avg.fatalities=mean(FATALITIES), 
                            sum.injuries=sum(INJURIES), avg.injuries=mean(INJURIES), 
                            sum.fatal.or.injure=sum(FATALITIES,INJURIES)), 
                            by=.(grep("heat",EVTYPE), grep("wind", EVTYPE), grep("cold",EVTYPE))]


## calculate the weighted averages of deaths and injuries by event type.
## We can ignore property damage for now, this is not "population health".
dt_data_sums <- dt_data[ ,.(avg.fatal.per.event=sum(FATALITIES), avg.fatalities=mean(FATALITIES), 
                            sum.injuries=sum(INJURIES), avg.injuries=mean(INJURIES), 
                            sum.fatal.or.injure=sum(FATALITIES,INJURIES)),
                         by=EVTYPE][order(-sum.fatal.or.injure)]

#################

range(dt_data$PROPDMG) -> prop.damage.range
unique(dt_data$PROPDMGEXP) -> prop.damage.exp
range(dt_data$CROPDMG) -> crop.damage.range
unique(dt_data$CROPDMGEXP) -> crop.damage.exp

## The exponent column is supposed to just have "B", "M", "K", or "H".  I'd like to do a quick
## check on a few rows with the other values to figure out why they are there.

dt_data_rows_wrong_exp <- dt_data[which(dt_data$PROPDMGEXP %in% 
                                            c("+", "5", "6", "?", "4", "2", "3", "7", "-")),]

## Is there a notes section at the bottom of the data?  Why are numbers in the EXP column?
tail(dt_data$PROPDMGEXP)
identical(crop.damage.exp, prop.damage.exp) -> notes.the.same
identical(dt_data$CROPDMGEXP, dt_data$PROPDMGEXP) -> col.notes.the.same

## The remarks column has some notes on total damage estimates that match up with the
## numbers in the EXP columns.  It appears these numeric values are due to rounding not to
## 3 significant figures, or just incorrect input of data.  

print(prop.damage.exp)
print(crop.damage.exp)
prop.damage.exp.incorrect <- prop.damage.exp[!prop.damage.exp %in% 
                                                 c("K","M","B","m","h","H", "", "0")]

crop.damage.exp.incorrect <- crop.damage.exp[!crop.damage.exp %in% 
                                                 c("k","K","M","B","m","h","H", "", "0")]

prop.row.count.with.incorrect.exp <- nrow(dt_data[which(dt_data$PROPDMGEXP %in% 
                                                            prop.damage.exp.incorrect),])
crop.row.count.with.incorrect.exp <- nrow(dt_data[which(dt_data$CROPDMGEXP %in% 
                                                            crop.damage.exp.incorrect),])

## 98+8 rows with incorrect EXP values.

## calculate values just using PROPDMG and CROPDMG columns and keeping all rows.
## I know this is not correct (missing mag multiplier), but I want to see the baseline data.
## --The sum and averages of property and crop damage by event type.
## --Also calculate a total sum of property+crop by event type.
dt_data_econ_sums_no_mult <- dt_data[ ,.(sum.property=sum(PROPDMG), avg.property=mean(PROPDMG), 
                            sum.crop=sum(CROPDMG), avg.crop=mean(CROPDMG), 
                            sum.prop.or.crop=sum(PROPDMG,CROPDMG)),
                            by=EVTYPE][order(-sum.prop.or.crop)][sum.prop.or.crop >1500]

c <- ggplot(dt_data_econ_sums_no_mult, aes(log(sum.prop.or.crop), reorder(EVTYPE,sum.prop.or.crop))) +
    geom_point(size=3)
print(c)

## Tornado damage is again on top, with flood, wind, and hail being the next highest.
## This seems to make sense.  Heat is not at the top like it was for human health impact.

## Now I am going to filter out the rows that contain incorrect "EXP" values.
## Go back to original dt_data.
dt_data_w_exp_rows_removed <- dt_data[which(!dt_data$PROPDMGEXP %in% 
                                           prop.damage.exp.incorrect),]
dt_data_w_exp_rows_removed <- dt_data_w_exp_rows_removed[which
                                            (!dt_data_w_exp_rows_removed$CROPDMGEXP %in% 
                                                crop.damage.exp.incorrect),]

print(unique(dt_data_w_exp_rows_removed$PROPDMGEXP))
print(unique(dt_data_w_exp_rows_removed$CROPDMGEXP))

## function to determine weekday and weekend
## data.table reference generation and use of function

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

dt_data_w_exp_rows_removed[ , prop.damage.multiplier := exp.magnitude(PROPDMGEXP)]
dt_data_w_exp_rows_removed[ , crop.damage.multiplier := exp.magnitude(CROPDMGEXP)]

dt_data_w_exp_rows_removed[ , prop.damage.total := prop.damage.multiplier*PROPDMG]
dt_data_w_exp_rows_removed[ , crop.damage.total := crop.damage.multiplier*CROPDMG]
dt_data_w_exp_rows_removed[ , damage.total := prop.damage.total+crop.damage.total]



dt_data_econ <- dt_data_w_exp_rows_removed[ ,.(sum.property=sum(prop.damage.total), 
                                               avg.property=mean(prop.damage.total), 
                                         sum.crop=sum(crop.damage.total), 
                                         avg.crop=mean(crop.damage.total), 
                                         sum.prop.or.crop=sum(prop.damage.total, crop.damage.total)),
                                      by=EVTYPE][order(-sum.prop.or.crop)][sum.prop.or.crop >10000000]


d <- ggplot(dt_data_econ, aes(log(sum.prop.or.crop), reorder(EVTYPE,sum.prop.or.crop))) +
    geom_point(size=3)
print(d)


