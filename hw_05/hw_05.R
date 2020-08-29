library(readxl)
library(tidyverse)


setwd(getwd())
taxdata<-read_excel('tax_data.xls')

taxdata%>%
  select(house,land_val,land_vat,year)%>%
  gather(key='tax',value='tax_revenue','house','land_val','land_vat')%>%
  ggplot(aes(x=year,y=tax_revenue/1000000,color=tax))+
  geom_line()+
  labs(x='',y='Tax Revenue(in million NTD)')


ggsave('tax_revenue.png',width=20,height=15,units='cm')
  
  


  
  






