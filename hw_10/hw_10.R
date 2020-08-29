library(tidyverse)
library(maps)

#建立工作目錄和讀取檔案
setwd(getwd())
pop.m<-read_rds('pop_m.rds')

#part(a)
pop.m%>%
  group_by(year)%>%
  summarise_at(vars(-Location,-'Children 0-18',-'65+'),median)%>%
  view()

#part(b)
pop.m%>%
  filter_at(vars(-Location,-year),any_vars(.>0.3))%>%
  view()

#建立變數
world_map<-map_data('world')
BRICS<-map_data('world',region=c('Brazil','Russia','India','China','South Africa') )
PIIGS<-map_data('world',region=c('Portugal','Italy','Ireland','Greece','Spain') )

#畫圖
ggplot(world_map,aes(x=long,y=lat,group=group))+
  geom_polygon(fill='white',color='black')+
  geom_polygon(data=BRICS,fill='yellow',color='black')+
  geom_polygon(data=PIIGS,fill='green',color='black')


