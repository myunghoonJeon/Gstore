library(dplyr)
library(xlsx)

# data read & define
# GstoreWeblog <- read.csv('Gstore_WebLog.csv')
# gstoreWeblog <- GstoreWeblog
# eventTable <- read.xlsx('eventTable.xlsx',1,encoding = 'UTF-8')
View(gstoreWeblog)
# View(eventTable)
options(dplyr.summarise.inform = FALSE)


gstoreCount <- gstoreWeblog %>% summarise(n = n())
eventCount <- eventTable %>% summarise(n = n())


gt <- gstoreWeblog
gt

ggt <- gt %>% group_by(eventInfo.eventAction) %>% summarise(count = n())
ggt <- gt %>% group_by(page.pageTitle) %>% summarise(count = n())
ggt <- gt %>% group_by(page.pagePath) %>% summarise(count = n())
ggt

et <- eventTable %>% group_by(country) %>% summarise(count = n())
et

sample <- gt %>% subset(case=='0000174067426171406_1478846641' )
sp <- sample %>% select(timestamp,page.pageTitle,page.pagePathLevel1,page.pagePathLevel2,page.pagePathLevel3,page.pagePathLevel4)
sp <- as.data.frame(sp)
sp
