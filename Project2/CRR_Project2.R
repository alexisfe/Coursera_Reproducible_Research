library(ggplot2)
library(reshape2)

data_url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
data_file = "repdata-data-StormData.csv"

storm_df <- read.csv(data_file)

# Remove summary rows
# storm_df2 <- subset(storm_df, !grepl("[summary][a-z]*", tolower(storm_df$EVTYPE)))

# storm_df2[grep("summary[a-z0-9]+", tolower(storm_df$EVTYPE)), "EVTYPE"] 

# Question 1
# Across the United States, which types of events (as indicated in the EVTYPE variable) 
# are most harmful with respect to population health?

storm_df2 <- storm_df
storm_df2$EVTYPE <- toupper(storm_df2$EVTYPE)
storm_df2$EVTYPE <- gsub("/", " ", storm_df2$EVTYPE)
storm_df2$EVTYPE <- gsub("TSTM", "THUNDERSTORM", storm_df2$EVTYPE)
storm_df2$EVTYPE <- gsub("EXCESSIVE ", "", storm_df2$EVTYPE)
storm_df2$EVTYPE <- gsub("HIGH ", "", storm_df2$EVTYPE)
storm_df2$EVTYPE <- gsub("EXTREME ", "", storm_df2$EVTYPE)
storm_df2$EVTYPE <- gsub("FLASH ", "", storm_df2$EVTYPE)
storm_df2$EVTYPE <- gsub("HEAVY ", "", storm_df2$EVTYPE)
storm_df2$EVTYPE <- gsub("STRONG ", "", storm_df2$EVTYPE)
storm_df2$EVTYPE <- gsub(" WAVE", "", storm_df2$EVTYPE)

#Aggregate event types by both fatalities and injuries
harm_evtype <- aggregate(cbind(FATALITIES, INJURIES) ~ EVTYPE, data=storm_df2, sum)

#Drop rows without records
harm_evtype <- subset(harm_evtype, FATALITIES > 0 | INJURIES > 0)

harm_evtype <- melt(harm_evtype, id.var = c('EVTYPE'), variable.name = 'HARM')

harm_evtype_ordered <- aggregate(value ~ EVTYPE, data=harm_evtype, sum)

harm_evtype_ordered <- harm_evtype_ordered[order(harm_evtype_ordered$value, decreasing=TRUE),] 

ggplot(harm_evtype, aes(x=EVTYPE, y=value, fill=HARM)) + 
  geom_bar(stat="identity") + 
  scale_x_discrete(limits=harm_evtype_ordered$EVTYPE[1:10])

ggplot(subset(harm_evtype, HARM == "FATALITIES"), aes(x=EVTYPE, y=value)) + 
  geom_bar(stat="identity") + 
  scale_x_discrete(limits=subset(harm_evtype_ordered, HARM == "FATALITIES")$EVTYPE[1:10])

#Order event types by either fatalities or injuries
fatalities_evtype <- fatalities_evtype[order(fatalities_evtype$FATALITIES, decreasing=TRUE),] 
injuries_evtype <- injuries_evtype[order(injuries_evtype$INJURIES, decreasing=TRUE),] 

ggplot(fatalities_evtype, aes(x=EVTYPE, y=FATALITIES)) + geom_bar(stat="identity") + scale_x_discrete(limits=fatalities_evtype$EVTYPE)
ggplot(injuries_evtype, aes(x=EVTYPE, y=INJURIES)) + geom_bar(stat="identity") + scale_x_discrete(limits=injuries_evtype$EVTYPE)

# Question 2
# Across the United States, which types of events have the greatest economic consequences?

storm_df$PROPERTY_DAMAGE <- ifelse(storm_df$PROPDMGEXP == "K", storm_df$PROPDMG*1e3,
                                   ifelse(storm_df$PROPDMGEXP == "M", storm_df$PROPDMG*1e6, 
                                          ifelse(storm_df$PROPDMGEXP == "B", storm_df$PROPDMG*1e9, storm_df$PROPDMG)))

storm_df$CROP_DAMAGE <- ifelse(storm_df$CROPDMGEXP == "K", storm_df$CROPDMG*1e3,
                               ifelse(storm_df$CROPDMGEXP == "M", storm_df$CROPDMG*1e6,
                                      ifelse(storm_df$CROPDMGEXP == "B", storm_df$CROPDMG*1e9, storm_df$CROPDMG)))

storm_df[, c("EVTYPE", "PROPERTY_DAMAGE", "CROP_DAMAGE")]

arrange(myCars, cyl, desc(disp))

propdmg_evtype <- aggregate(PROPERTY_DAMAGE ~ EVTYPE, data=storm_df, sum)
cropdmg_evtype <- aggregate(CROP_DAMAGE ~ EVTYPE, data=storm_df, sum)

