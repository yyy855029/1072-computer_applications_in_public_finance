library(tidyverse)

setwd(getwd())

#part one

#讀取資料
indicators<-carData::UN98%>%select(-region)

#法一
ind1.m<-vector('double',length=ncol(indicators))
for(i in seq_along(indicators)){
  ind1.m[i]<-mean(indicators[[i]],na.rm=TRUE)
}

#法二
ind2.m<-map_dbl(indicators,mean,na.rm=TRUE)


#part two

#資料讀取+清理
pop<-list()
for(i in 2012:2017){
  j<-i-2011
  pop[[j]]<-read_csv(str_c('us_percent_age_data_',i,'.csv'),
                     skip=2,skip_empty_rows=TRUE)
  pop[[j]]<-pop[[j]]%>%
    select(-c(Footnotes,Total))%>%
    filter(!is.na(`65+`))%>%
    mutate(year=i)
}

#資料合併
pop<-bind_rows(pop[1:6])

#畫圖
ggplot(pop,aes(x=year,y=`65+`))+
  geom_line()+
  labs(x='year',y='65+')+
  facet_wrap(~Location,nrow=6)+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))

#圖片存取
ggsave('2012-2017 US 65+ percent age.png',width=40,height=20,units='cm')

