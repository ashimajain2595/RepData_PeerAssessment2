filename <- "StormData.csv.bz2"

# Download and unzip  dataset
if (!file.exists(filename)){
  fileurl <- "https://d396qusza40orc.cloudfront.net/repdata/data/StormData.csv.bz2"
  download.file(fileURL, filename)
}  
if (!file.exists("StormData.csv")) { 
  bzip2(filename) 
}

data <- read.csv('repdata-data-StormData.csv')
names(data) <- tolower(names(data))
length(levels(data$evtype))
evtype <- tolower(levels(data$evtype))

data$damagesource <- NA
data[grepl("precipitation|rain|hail|drizzle|wet|percip|burst|depression|fog|wall cloud", data$evtype, ignore.case = TRUE), "damageSource"] <- "Precipitation & Fog"
data[grepl("wind|storm|wnd|hurricane|typhoon", data$evtype, ignore.case = TRUE), "damagesource"] <- "Wind & Storm"
data[grepl("slide|erosion|slump", data$evtype, ignore.case = TRUE), "damagesource"] <- "Landslide & Erosion"
data[grepl("warmth|warm|heat|dry|hot|drought|thermia|temperature record|record temperature|record high", data$evtype, ignore.case = TRUE), "damageSource"] <- "Heat & Drought" 
data[grepl("cold|cool|ice|icy|frost|freeze|snow|winter|wintry|wintery|blizzard|chill|freezing|avalanche|glaze|sleet", data$evtype, ignore.case = TRUE), "damageSource"] <- "Snow & Ice"
data[grepl("flood|surf|blow-out|swells|fld|dam break", data$evtype, ignore.case = TRUE), "damagesource"] <- "Flooding & High Surf"
data[grepl("seas|high water|tide|tsunami|wave|current|marine|drowning", data$evtype, ignore.case = TRUE), "damagesource"] <- "High seas"
data[grepl("dust|saharan", data$evtype, ignore.case = TRUE), "damagesource"] <- "Dust & Saharan winds"  
data[grepl("tstm|thunderstorm|lightning", data$evtype, ignore.case = TRUE), "damagesource"] <- "Thunderstorm & Lightning"
data[grepl("tornado|spout|funnel|whirlwind", data$evtype, ignore.case = TRUE), "damagesource"] <- "Tornado"
data[grepl("fire|smoke|volcanic", data$evtype, ignore.case = TRUE), "damagesource"] <- "Fire & Volcanic activity"
data <- data[complete.cases(data[, "damagesource"]), ]
data$damagesource <- as.factor(data$damagesource)

toPowerTen <- function(n){
  if(n %in% c('h','H'))
    return(2)
  else if(n %in% c('k','K'))
    return(3)
  else if(n %in% c('m', 'M'))
    return(6)
  else if(n %in% c('b','B'))
    return(9)
  else if(n %in% c('','-','?','+'))
    return(0)
  else if(is.numeric(n))
    return(n)
}

computeValue <- function(n, exp){
  if(is.numeric(n))
     n <- as.numeric(n * 10^(toPowerTen(exp)))
  else
     n <-0
  n
}               

data$propdamage <- mapply(computeValue, data$propdmg, data$propdmgexp)
data$cropdamage <- mapply(computeValue, data$cropdmg, data$cropdmgexp)
storage.mode(data$propdamage) <- "integer"
storage.mode(data$cropdamage) <- "integer"
data <- data[complete.cases(data[, "propdamage"]), ]
data <- data[complete.cases(data[, "cropdamage"]), ]
                                   
library(plyr)
humandamage<- ddply(data, .(damagesource), summarize, fatalities = sum(fatalities), injuries = sum(injuries))
fatal <- head(humandamage[order(humandamage$fatalities, decreasing = T), ], 10)
rownames(fatal)<-NULL
injure <- head(humandamage[order(humandamage$injuries, decreasing = T), ], 10)
rownames(injure)<-NULL

econdamage <- ddply(data, .(damagesource), summarize, propdamage = sum(as.numeric(propdamage)), cropdamage = sum(as.numeric(cropdamage)))
property <- econdamage[order(econdamage$propdamage, decreasing = T), ]
rownames(property)<-NULL
crop <- econdamage[order(econdamage$cropdamage, decreasing = T), ]
rownames(crop) <- NULL

par(mfrow=c(1,2),cex=0.7, mar = c(12, 4, 3, 2))
barplot(fatal$fatalities, names.arg=fatal$damagesource,las=3, col="blue", main="Event Types with top 11 Fatalities")
barplot(injure$injuries, names.arg=injure$damagesource,las=3, col="blue", main="Event Types with top 11 Injuries")

par(mfrow=c(1,2),cex=0.7, mar = c(12, 4, 3, 2))
barplot(property$propdamage, names.arg=property$damagesource,las=3, col="blue", main="Event Types with top 11 Property damage")
barplot(crop$cropdamage, names.arg=crop$damagesource,las=3, col="blue", main="Event Types with top 11 Crop damage")
