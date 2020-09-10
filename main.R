

# eCommerceAction.option
# 99984 poeple
install.packages('tidyverse')
library('tidyverse')
install.packages('lubridate')
library('lubridate')
library(dplyr)
library(xlsx)

###################### test section ##########################
gstoreCount <- gstoreWeblog %>% summarise(n = n())
eventCount <- eventTable %>% summarise(n = n())

gt <- gstoreWeblog
###################### add date ###############################
final <- gt %>% mutate(year = year(timestamp) ,month= month(timestamp),quarter=quarter(timestamp))

ggt <- gt %>% group_by(eventInfo.eventAction) %>% summarise(count = n())
ggt <- gt %>% group_by(page.pageTitle) %>% summarise(count = n())
ggt <- gt %>% group_by('page.pagePath') %>% summarise(count = n())
ggt <- gt %>% group_by(deviceCategory) %>% summarise(count = n())
gt
ggt <- gt %>% filter(channelGrouping=='referral')
ggt
ggt <- as.data.frame(ggt)

View(ggt)
ggt <- gt %>% group_by(medium) %>% summarise(count = n())
ggt <- arrange(ggt,desc(count))
ggt

ggt1 <- gt %>% subset(is.na(newVisits))
ggt <- ggt1 %>% group_by(channelGrouping) %>% summarise(count = n(), rate=round((count/count(ggt1)*100),1))
ggt <- arrange(ggt,desc(count))
ggt

ggt1 <- gt %>% subset(!is.na(newVisits))
ggt <- ggt1 %>% group_by(channelGrouping) %>% summarise(count = n(), rate=round((count/count(ggt1)*100),1))
ggt <- arrange(ggt,desc(count))
ggt

sg <- ggt1 %>% subset(channelGrouping=='Social')
sgt <- sg %>% group_by(deviceCategory) %>% summarise(count = n(), rate=round((count/count(sg)*100),1))
sgt <- arrange(sgt,desc(count))
sgt
################ data read & define ##################
GstoreWeblog <- read.csv('Gstore_WebLog.csv')
gstoreWeblog <- GstoreWeblog
eventTable <- read.xlsx('eventTable.xlsx',1,encoding = 'UTF-8')
View(gstoreWeblog)
View(eventTable)
options(dplyr.summarise.inform = FALSE)
source('functionSet.R')
################ begin coding ###################
getNewVisitChannleCount(gt,TRUE)
getNewVisitChannleCount(gt,FALSE)

getChannelCount(ggt1,'Social')
getChannelCount(ggt1,'Organic Search')
getChannelCount(ggt1,'Referral')

getHitCountByDevice(gt)

count(gt %>% filter(deviceCategory=='mobile') %>% group_by(case) %>% summarise(count = n()))
checkHitnumberByUser(gt,'mobile',150)
# checkHitnumberByChannel(gt,'mobile',150)
count(gt %>% filter(deviceCategory=='tablet') %>% group_by(case) %>% summarise(count = n()))
checkHitnumberByUser(gt,'tablet',150)
# checkHitnumberByChannel(gt,'tablet',150)
count(gt %>% filter(deviceCategory=='desktop') %>% group_by(case) %>% summarise(count = n()))
checkHitnumberByUser(gt,'desktop',150)
# checkHitnumberByChannel(gt,'desktop',150)

getCountrylistByPament(gt,TRUE)
getCountrylistByPament(gt,FALSE)

nonPS <- gt %>% subset(eCommerceAction.option == 'Billing and Shipping' & page.pageTitle=='Shopping Cart')

nonPS <- gt %>% subset(page.pageTitle == 'Shopping Cart')

okps <- final %>% subset(eCommerceAction.option=='Payment')
ystat <- okps %>% group_by(year) %>% summarise(count = n())
ystat <- arrange(ystat,desc(count))
ystat
mstat <- okps %>% group_by(month) %>% summarise(count = n())
mstat <- arrange(mstat,desc(count))
mstat

nops <- final
nystat <- nops %>% group_by(year) %>% summarise(count = n())
nystat <- arrange(nystat,desc(count))
nystat
nmstat <- nops %>% group_by(month) %>% summarise(count = n())
nmstat <- arrange(nmstat,desc(count))
nmstat

ystat
nystat

mstat
nmstat

