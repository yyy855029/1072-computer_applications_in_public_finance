library(tidyverse)
library(ggmap)
library(broom)
library(rgdal)
library(rgeos)


#建立工作目錄
setwd(getwd())

#資料讀入和處理
tw_map<-readOGR(dsn='mapdata201905100344',layer='COUNTY_MOI_1080509')
tw_map@data<-tw_map@data%>%
  mutate(COUNTYNAME=iconv(COUNTYNAME,from='UTF-8',to='big5'))
tw_map.ft<-tidy(tw_map,region='COUNTYCODE')%>%
  filter(long>=118&long<=123&lat<=27&lat>=21)

#檢查資料結構
str(tw_map@data)
str(tw_map.ft)

#資料合併
countmap.m<-inner_join(tw_map.ft,tw_map@data,by=c('id'='COUNTYCODE'))

#資料讀入和處理
ob<-read_csv('obesity.csv',locale=locale(encoding='big5'),skip=1,
             col_names=c('county','有效樣本數','男','女','percent'))%>%
  mutate(county=replace(county,county=='桃園縣','桃園市'))

#檢查資料結構
str(ob)
str(countmap.m)
levels(factor(ob$county))
levels(factor(countmap.m$COUNTYNAME))

#資料合併
merged.map<-inner_join(ob,countmap.m,by=c('county'='COUNTYNAME'))
merged.map<-arrange(merged.map,group,order)

#讀取Google Api,此處用別人的key
register_google(key='xxx')

#畫圖
qmap('Taiwan',zoom=7,color='bw',maptype='roadmap',legend='bottomright')+
  geom_polygon(data=merged.map,aes(x=long,y=lat,group=group,
                                   fill=percent),alpha=0.5,color='white')+
  scale_fill_distiller(palette='RdYlBu')+
  scale_alpha_continuous(guide=FALSE)+
  labs(title='Obesity and Overweight Rates in TW',fill='(%)')
  ggsave('Obesity and Overweight Rates in TW.png',width=15,height=20,units='cm')





  

