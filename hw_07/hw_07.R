library(tidyverse)

#讀取資料

setwd(getwd())
elec<-read_csv('elec.csv')
pop12<-read_csv('pop2012.csv')
pop16<-read_csv('pop2016.csv')
state<-read_csv('state_info.csv')
edu<-read_csv('edu.csv')

#資料處理

pop.m<-bind_rows(pop12,pop16,.id='year')%>%
  mutate(year=as.numeric(year),
         year=if_else(year==1,2012,2016))

elec.m<-left_join(elec,state,by=c('state'='Postal'))

combined<-left_join(elec.m,pop.m,by=c('year'='year','State'='Location'))%>%
  left_join(edu,by=c('State'='state'))

#資料視覺化

combined%>%
  ggplot(aes(x=R_share,y=elderly,color=factor(year)))+
  geom_point()+
  scale_color_manual(limits=c(2012,2016),
                     labels=c('2012(D Won)','2016(R Won)'),
                     values=c('navyblue','maroon'),
                     name='Election Year')+
  labs(x='Share of Votes for Republican candidate',y='Elderly Population')
  

combined%>%
  ggplot(aes(x=R_share,y=per.bac,color=factor(year)))+
  geom_point()+
  scale_color_manual(limits=c(2012,2016),
                     labels=c('2012(D Won)','2016(R Won)'),
                     values=c('navyblue','maroon'),
                     name='Election Year')+
  labs(x='Share of Votes for Republican candidate',y='Education Attainment')
  

  
