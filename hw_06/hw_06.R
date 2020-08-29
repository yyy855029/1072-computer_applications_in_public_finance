library(tidyverse)

#讀取資料
setwd(getwd())
load('dot_plot2019.RData')

sub_2015_city%>%
  mutate(area=fct_collapse(city,
  North=c("Taipei City","New Taipei City", "Keelung City","Taoyuan City","Hsinchu City","Hsinchu County","Yilan County"),
  Central=c("Miaoli County","Taichung City","Changhua County","Nantou County","Yunlin County"),
  South=c("Chiayi City", "Chiayi County","Tainan City","Kaohsiung City","Pingtung County"),
  East=c("Hualien County","Taitung County"),
  Islands=c("Penghu County","Kinmen County", "Lianjian County")
  ))%>%
  mutate(city=fct_drop(city))%>%
  arrange(desc(area),avg_nh)%>%
  mutate(city=fct_inorder(city))%>%
  ggplot(aes(x=avg_nh,y=city,color=area))+
  geom_point(shape=24)+
  labs(x='LTC facilities per 10,000 seniors',y='',color='Area')+
  theme_bw()

#儲存圖片 
ggsave('Taiwan LTC facilities per 10,000 seniors.png',width=10,height=20,units='cm')
  

  