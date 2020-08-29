#1
library(readxl)
library(tidyverse)
#2
setwd(getwd())
data.frame<-read_excel('counties.xlsx',na='-')
#3
filter(data.frame,city=='New Taipei City',Year=='1998')
class(data.frame$Year)
#4
data.frame<-mutate(data.frame,Year=as.numeric(Year))
data.frame<-rename(data.frame,year=Year)
#5
data.frame<-rename(data.frame,area=Area)
data.frame<-mutate(data.frame,area=factor(area))
nlevels(data.frame$area)
#6
new.data.frame<-filter(data.frame,!city%in%c('Total','Taiwan'))
#7
another.data.frame<-filter(new.data.frame,year=='2015')
#8
plot(x=another.data.frame$self_funding,y=another.data.frame$elder_share,xlab='Self Funding',ylab='Elder Share')


