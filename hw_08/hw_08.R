library(tidyverse)

#讀取資料
setwd(getwd())
transcript<-read_file('transcript.txt')
songs<-read_csv('billboard_lyrics.csv')

#統計字詞頻率 
str_count(transcript,'黃捷')
str_count(transcript,'韓國瑜')
str_count(transcript,'發大財')
str_count(transcript,'發財')

songs%>%
  mutate(word_count=str_count(Lyrics,'money'))%>%
  group_by(Year)%>%
  summarise(mean=mean(word_count,na.rm=TRUE))%>%
  ggplot(aes(x=Year,y=mean))+
  geom_col()+
  labs(x='',y='Average Occurence per Song',
       title='Average occurence of the word \"money\" in the lyrics 
 of Billboard Top 100 Songs, 1965-2015')+
  theme_bw()
