library(tidyverse)
library(ggmap)
library(broom)
library(rgdal)
library(readxl)


#建立工作目錄
setwd(getwd())

#資料讀入
tw_map<-readOGR(dsn='mapdata201905210454',layer='TOWN_MOI_1080509')%>%
  tidy()
acc<-read_excel('accident_s_106.xlsx')

#畫圖  
ggplot(tw_map,aes(x=long,y=lat,group=group))+
  geom_polygon(color='red',fill='white')+
  scale_x_continuous(limits=c(118,124))+
  scale_y_continuous(limits=c(21,27))

#讀取Google Api
register_google(key='xxx')
#查詢經緯度
geocode('臺北火車站')


#Map 1畫圖
qmap('Taoyuan',zoom=11,color='bw')+
  geom_point(data=acc,aes(x=lon,y=lat,color=type),alpha=0.5)+
  ggsave('map1.png',width=30,height=14,units='cm')

#Map 2畫圖
qmap('Taoyuan',zoom=12,color='bw',legend='bottomright')+
  stat_density2d(data=acc,aes(x=lon,y=lat,fill=..level..,alpha=..level..),
                 bins=6,geom='polygon')+
  scale_fill_gradient(low='yellow',high='red')+
  scale_alpha_continuous(guide=FALSE)+
  ggsave('map2.png',width=30,height=14,units='cm')
  

