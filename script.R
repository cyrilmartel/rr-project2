library (dplyr)

##read the .csv file
data <- read.csv ("repdata-data-StormData.csv", row.names = 37, stringsAsFactors = F)

##picks only interesting columns
data2 <- data[c(1,2,7,8,21:28)]
rm(data)

##turns EVTYPE into lowercase
data2$EVTYPE <- tolower(data2$EVTYPE)

##############################################################################
##only rows with damages to health
health <- filter(data2, data2$FATALITIES !=0 | data2$INJURIES !=0)

##creates event column to avoid messing with EVTYPE
health <- mutate(health, event = EVTYPE)

##cleans the data by grouping them in categories (event column)
health$event[agrep("torna", health$event)] <- "tornado"
health$event[agrep("aval", health$event)] <- "avalanche"
health$event[agrep("fl", health$event)] <- "flood"
health$event[agrep("hurri", health$event)] <- "hurricane"
health$event[agrep("thun", health$event)] <- "thunderstorm"
health$event[agrep("tstm", health$event)] <- "thunderstorm"
health$event[agrep("rain", health$event)] <- "rain"
health$event[agrep("wind", health$event)] <- "wind"
health$event[agrep("snow", health$event)] <- "snow"
health$event[agrep("heat", health$event)] <- "heat"
health$event[agrep("hypothermia", health$event)] <- "hypothermia"
health$event[agrep("current", health$event)] <- "rip currents"
health$event[agrep("storm surge", health$event)] <- "storm surge"
health$event[agrep("ice road", health$event)] <- "ice on road"
health$event[agrep("ice on road", health$event)] <- "ice"
health$event[agrep("waterspout", health$event)] <- "tornado"

##creates a health2 dataset to avoid messing with health
health2 <- health
health2$event <- as.factor(health2$event)
levels(health2$event)

##create year colum
library(lubridate)
health2$BGN_DATE <- gsub("0:00:00","", health2$BGN_DATE)
health2$BGN_DATE <- mdy(health2$BGN_DATE)
health2 <- mutate (health2, year = format(health2$BGN_DATE, "%Y"))


##sums fatalities and injuries by type of event
health2 <- group_by(health2, year, event)
totalfatalities <- summarize(health2, sum(FATALITIES), sum(INJURIES))
colnames(totalfatalities)[c(3,4)] <- c("fatalities","injuries")
totalfatalities <- arrange(totalfatalities, year, event)
fatperyear <-  summarize(group_by(health2, year), sum(FATALITIES), sum(INJURIES))
colnames(fatperyear)[c(2,3)] <- c("fatperyear", "injperyear")
totalfatalities <- merge (totalfatalities, fatperyear)
totalfatalities <- mutate (totalfatalities, pctfat = 100*fatalities/fatperyear)
totalfatalities <- mutate (totalfatalities, pctinj = 100*injuries/injperyear)

library (ggplot2)

ggplot(totalfatalities, aes(year,fatalities)) + geom_bar(stat = "identity") + facet_wrap(~ event)

##looks only at data from 1995 (20 years back, rip currents start in 1994)
totalfatalities$year <- as.numeric(totalfatalities$year)
df95 <- totalfatalities[totalfatalities$year > 1994 & totalfatalities$pctfat > 5,]
ggplot(df95, aes(year,fatalities)) + geom_bar(stat = "identity") + facet_wrap(~ event)
ggplot(df95, aes(year,pctfat, fill = event)) + geom_bar(stat = "identity")

##summarizes data from last 20 years by fatalities and injuries in absolute numbers

summaryhealth <- arrange(summarize(group_by(df95,event), fatalities = sum(fatalities), injuries = sum(injuries)),desc(fatalities))


#########################################################


##only rows with damages to property
property <- filter(data2,PROPDMG !=0 | CROPDMG !=0)

##creates event column to avoid messing with EVTYPE
property <- mutate(property, event = EVTYPE)

##cleans the data by grouping them in categories (event column)
property$event[agrep("torna", property$event)] <- "tornado"
property$event[agrep("aval", property$event)] <- "avalanche"
property$event[agrep("fl", property$event)] <- "flood"
property$event[agrep("hurri", property$event)] <- "hurricane"
property$event[agrep("thun", property$event)] <- "thunderstorm"
property$event[agrep("tstm", property$event)] <- "thunderstorm"
property$event[agrep("rain", property$event)] <- "rain"
property$event[agrep("wind", property$event)] <- "wind"
property$event[agrep("snow", property$event)] <- "snow"
property$event[agrep("heat", property$event)] <- "heat"
property$event[agrep("hypothermia", property$event)] <- "hypothermia"
property$event[agrep("current", property$event)] <- "rip currents"
property$event[agrep("storm surge", property$event)] <- "storm surge"
property$event[agrep("ice road", property$event)] <- "ice on road"
property$event[agrep("ice on road", property$event)] <- "ice"
property$event[agrep("waterspout", property$event)] <- "tornado"

##creates a property2 dataset to avoid messing with property
property2 <- property
property2$event <- as.factor(property2$event)
levels(property2$event)

##replaces K,M and B by 1000, 1000000 and 1000000000
property2$PROPDMGEXP <- gsub("K", 1000, property2$PROPDMGEXP)
property2$PROPDMGEXP <- gsub("M", 1000000, property2$PROPDMGEXP)
property2$PROPDMGEXP <- gsub("B", 1000000000, property2$PROPDMGEXP)
property2$CROPDMGEXP <- gsub("K", 1000, property2$CROPDMGEXP)
property2$CROPDMGEXP <- gsub("M", 1000000, property2$CROPDMGEXP)
property2$CROPDMGEXP <- gsub("B", 1000000000, property2$CROPDMGEXP)

##mutates PROPDMG
property2$PROPDMGEXP <- as.numeric(property2$PROPDMGEXP)
property2$CROPDMGEXP <- as.numeric(property2$CROPDMGEXP)
property2 <- mutate(property2, PROPDMG = PROPDMG*PROPDMGEXP)
property2 <- mutate(property2, CROPDMG = PROPDMG*CROPDMGEXP)
property2[is.na(property2)] <- 0

library(lubridate)
property2$BGN_DATE <- gsub("0:00:00","", property2$BGN_DATE)
property2$BGN_DATE <- mdy(property2$BGN_DATE)
property2 <- mutate (property2, year = format(property2$BGN_DATE, "%Y"))


property2 <- group_by(property2, year, event)
totalprop <- summarize(property2, propdmg = sum(PROPDMG), cropdmg = sum(CROPDMG))
totalprop <- arrange(totalprop, year, event)
propperyear <-  summarize(group_by(property2, year), propperyear = sum(PROPDMG), cropperyear = sum(CROPDMG))
totalprop <- merge (totalprop, propperyear)
totalprop <- mutate (totalprop, pctfat = 100*propdmg/propperyear)
totalprop <- mutate (totalprop, pctinj = 100*cropdmg/cropperyear)

library (ggplot2)

ggplot(totalprop, aes(year,propdmg)) + geom_bar(stat = "identity") + facet_wrap(~ event)

##looks only at data from 1995 (20 years back, rip currents start in 1994)
totalprop$year <- as.numeric(totalprop$year)
df95prop <- totalprop[totalprop$year > 1994,]
ggplot(df95prop, aes(year,propdmg)) + geom_bar(stat = "identity") + facet_wrap(~ event)
ggplot(df95prop, aes(year,pctfat, fill = event)) + geom_bar(stat = "identity")

##summarizes data from last 20 years by fatalities and injuries in absolute numbers

summaryprop <- arrange(summarize(group_by(df95prop,event), propdmg = sum(propdmg), cropdmg = sum(cropdmg)),propdmg)
