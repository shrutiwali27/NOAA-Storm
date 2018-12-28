#Downloading the data
## ----cache=TRUE----------------------------------------------------------
if (! file.exists('stormData.csv.bz2')){
  download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2',destfile = 'stormData.csv.bz2',method = 'curl',mode = 'w')
  
}

if(!exists('stormData')){
  strmDataZip <- 'stormData.csv.bz2'
  stormDataFile <- bzfile(description=strmDataZip, open="r")
  stormData <- read.csv(stormDataFile,fill = TRUE,header = T)  
  close(stormDataFile)
}
------------------------------------------------------------------------
#Data Processing

require(ggplot2)
require(reshape2)
require(plyr)

stormDataEOPH <- as.data.frame(cbind(stormData$EVTYPE,stormData$FATALITIES+stormData$INJURIES))
names(stormDataEOPH) <- c('EVENT.TYPE','fatalities.and.injuries')

stormDataEOPH$EVENT.TYPE <- as.factor(stormDataEOPH$EVENT.TYPE)
levels(stormDataEOPH$EVENT.TYPE) <- levels(stormData$EVTYPE)


summary <- ddply(.data = stormDataEOPH,.(EVENT.TYPE),summarize,sum(fatalities.and.injuries))
names(summary)[2] <- 'fatalities.and.injuries'
summary$EVENT.TYPE  <- as.factor(summary$EVENT.TYPE)
levels(summary$EVENT.TYPE) <- levels(stormData$EVTYPE)
ord.summary <- summary[order(summary$fatalities.and.injuries,decreasing = T),]
m <- mean(ord.summary$fatalities.and.injuries)
susS <- subset(ord.summary,fatalities.and.injuries>m)

median <- median(unique(stormDataEOPH$fatalities.and.injuries))
subData <- subset(stormDataEOPH,fatalities.and.injuries>median)


ggplot(susS,aes(EVENT.TYPE,fatalities.and.injuries)) + geom_point(aes(colour=EVENT.TYPE)) 
+ theme(legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle('fatilities and injuries from major disasters') 

#---------------------------------------------------------------------------------
##the most severe weather events and their impact on the total number of fatalities and injuries
  
top20 <- head(susS,20) 
rownames(top20) <- 1:20
print(top20)

#-------------------------------------------------------------------------------------
##the events that cased the most damage on the economy

#Data Processing
stormDataEOE <- as.data.frame(cbind(stormData$EVTYPE,stormData$PROPDMG+stormData$CROPDMG))
names(stormDataEOE) <- c('Event.Type','economic.damages')

summary <- ddply(.data = stormDataEOE,.(Event.Type),summarize,sum(economic.damages))
names(summary)[2] <- "economic.damages"
summary$Event.Type  <- as.factor(summary$Event.Type)
levels(summary$Event.Type) <- levels(stormData$EVTYPE)
ord.summary <- summary[order(summary$economic.damages,decreasing = T),]
m <- mean(ord.summary$economic.damages)
susS <- subset(ord.summary,economic.damages>m)

#Results

ggplot(susS,aes(Event.Type,economic.damages)) + geom_point(aes(colour=Event.Type)) + theme(legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('Economic damage caused by major disasters') 

#-----------------------------------------------------------------------------------------
##a list of 20 events that cause the most economy damages sorted decreasingly

top20 <- head(susS,20)
rownames(top20) <- 1:20
print(top20)
