library(tidyverse)

#讀取資料
setwd(getwd())
source('W4_Solution.R')

#將mydata中city屬於Taiwan的部分抓出,命名為sub_Taiwan
sub_Taiwan<-filter(mydata,city=='Taiwan')

#畫各題要求的histogram資料
ggplot(sub_data2015,aes(x=pop_density))+geom_histogram(fill='red',color='black')
ggplot(sub_data2015,aes(x=youth_un))+geom_histogram(bins=15,fill='red',color='black')
ggplot(sub_Taiwan,aes(x=year,y=pop_change))+geom_col(fill='red',color='black')


