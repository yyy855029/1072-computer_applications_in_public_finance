library(tidyverse)
library(ggmap)
library(broom)
library(rgdal)
library(rgeos)
library(readxl)

#建立工作目錄
setwd(getwd())

#建立空的list
cause<-list()
cancer<-list()

#存入資料
for(i in 97:106){
  j<-i-96
  cause[[j]]<-
    read_csv(str_c('死因統計/','dead',i,'.csv',sep=''))
  cancer[[j]]<-
    read_csv(str_c('癌症死因統計/','cancer',i,'.csv',sep=''))
}

#patr1
#合併資料
c.cause<-bind_rows(cause)%>%
  mutate(sex=factor(sex),age=factor(age_code),
         year=year+1911)
c.cancer<-bind_rows(cancer)%>%
  mutate(sex=factor(sex),age=factor(age_code),
         year=year+1911)

#讀取資料
h.cancer<-read_csv('歷年惡性腫瘤發生率(攝護腺癌、肺癌 、大腸直腸癌、女性乳癌、子宮頸癌).csv',
                   skip=1,col_names=c('year','攝護腺癌','氣管.支氣管和肺癌',
                                      '結腸.直腸和肛門癌','女性乳癌','子宮頸癌'))


#2008~2017臺灣平均每年十大主要死因圓餅圖

#資料處理
c.cause%>%
  filter(cause!='41')%>%
  group_by(cause)%>%
  summarise(number=sum(N))%>%
  arrange(desc(number))%>%
  head(10)%>%
  mutate(share=number/sum(number)*100,
         cause=fct_recode(cause,'惡性腫瘤'='06','心臟疾病（高血壓性疾病除外)'='16',
                          '腦血管疾病'='17','肺炎'='21',
                          '糖尿病'='09','事故傷害'='38',
                          '慢性下呼吸道疾病'='23','高血壓性疾病'='15',
                          '慢性肝病及肝硬化'='28','腎炎、腎病症候群及腎病變'='32'))%>%
#畫圖  
  ggplot(aes(x='',y=share,fill=reorder(cause,share)))+
  geom_bar(stat="identity", width=1)+
  coord_polar('y',start=0)+
  scale_fill_manual(values=c('#A1887F','#E57373','#F06292',
                             '#BA68C8','#7986CB','#4DD0E1',
                             '#AED581','#FFF176','#FFB74D',
                             '#FF8A65'))+
  geom_text(aes(label=paste0(round(share,1),'%')),
            position=position_stack(vjust=0.5),
            vjust=-0.6,hjust=0.4,size=2)+
  labs(x=NULL,y=NULL,fill=NULL,title='2008~2017年臺灣平均每年十大主要死因')+
  theme_void()+
  ggsave('2008~2017臺灣平均每年十大主要死因.png',width=30,height=14,units='cm')


#2008~2017臺灣女性平均每年十大主要死因圓餅圖

#資料處理
c.cause%>%
  filter(sex=='2',cause!='41')%>%
  group_by(cause)%>%
  summarise(number=sum(N))%>%
  arrange(desc(number))%>%
  head(10)%>%
  mutate(share=number/sum(number)*100,
         cause=fct_recode(cause,'惡性腫瘤'='06','心臟疾病（高血壓性疾病除外)'='16',
                          '糖尿病'='09','腦血管疾病'='17',
                          '肺炎'='21','高血壓性疾病'='15',
                          '腎炎、腎病症候群及腎病變'='32','事故傷害'='38',
                          '慢性下呼吸道疾病'='23','敗血症'='03'))%>%
#畫圖  
  ggplot(aes(x='',y=share,fill=reorder(cause,share)))+
  geom_bar(stat="identity", width=1)+
  coord_polar('y',start=0)+
  scale_fill_manual(values=c('#A1887F','#E57373','#F06292',
                             '#BA68C8','#7986CB','#4DD0E1',
                             '#AED581','#FFF176','#FFB74D',
                             '#FF8A65'))+
  geom_text(aes(label=paste0(round(share,1),'%')),
            position=position_stack(vjust=0.5),
            vjust=-0.6,hjust=0.4,size=2)+
  labs(x=NULL,y=NULL,fill=NULL,title='2008~2017年臺灣女性平均每年十大主要死因')+
  theme_void()+
  ggsave('2008~2017臺灣女性平均每年十大主要死因.png',width=30,height=14,units='cm')


#2008~2017臺灣男性平均每年十大主要死因圓餅圖

#資料處理
c.cause%>%
  filter(sex=='1',cause!='41')%>%
  group_by(cause)%>%
  summarise(number=sum(N))%>%
  arrange(desc(number))%>%
  head(10)%>%
  mutate(share=number/sum(number)*100,
         cause=fct_recode(cause,'惡性腫瘤'='06','心臟疾病（高血壓性疾病除外)'='16',
                          '腦血管疾病'='17','肺炎'='21',
                          '事故傷害'='38','糖尿病'='09',
                          '慢性下呼吸道疾病'='23','慢性肝病及肝硬化'='28',
                          '蓄意自我傷害（自殺）'='39','高血壓性疾病'='15'))%>%
  #畫圖
  ggplot(aes(x='',y=share,fill=reorder(cause,share)))+
  geom_bar(stat="identity", width=1)+
  coord_polar('y',start=0)+
  scale_fill_manual(values=c('#A1887F','#E57373','#F06292',
                             '#BA68C8','#7986CB','#4DD0E1',
                             '#AED581','#FFF176','#FFB74D',
                             '#FF8A65'))+
  geom_text(aes(label=paste0(round(share,1),'%')),
            position=position_stack(vjust=0.5),
            vjust=-0.6,hjust=0.4,size=2)+
  labs(x=NULL,y=NULL,fill=NULL,title='2008~2017年臺灣男性平均每年十大主要死因')+
  theme_void()+
  ggsave('2008~2017臺灣男性平均每年十大主要死因.png',width=30,height=14,units='cm')


#2008~2017年臺灣平均每年十大癌症死亡人數長條圖

#資料處理
c.cancer%>%
  filter(cause!=33)%>%
  group_by(cause)%>%
  summarise(number=sum(N)/10)%>%
  arrange(desc(number))%>%
  head(10)%>%
  mutate(cause=fct_recode(cause,'氣管.支氣管和肺癌'='13','肝和肝類膽管癌'='08',
                          '結腸.直腸和肛門癌'='07','口腔癌'='01',
                          '胃癌'='05','女性乳房癌'='19',
                          '胰臟癌'='10','食道癌'='04',
                          '前列腺(攝護腺)癌'='23','非何杰金氏淋巴瘤'='31'))%>%
  #畫圖  
  ggplot(aes(x=reorder(cause,number),y=number,fill=reorder(cause,number)))+
  geom_col(width=0.5,show.legend=FALSE)+
  scale_fill_manual(values=c('#A1887F','#E57373','#F06292',
                             '#BA68C8','#7986CB','#4DD0E1',
                             '#AED581','#FFF176','#FFB74D',
                             '#FF8A65'))+
  labs(title='2008~2017年臺灣平均每年十大癌症死亡人數',x=NULL,y='人數')+
  ylim(0,9000)+
  coord_flip()+
  geom_text(aes(label=number),hjust=-0.2)+
  theme_bw()+
  ggsave('2008~2017年臺灣平均每年十大癌症死亡人數.png',width=30,height=14,units='cm')



#2008~2017年臺灣女性平均每年十大癌症死亡人數長條圖

#資料處理
c.cancer%>%
  filter(sex=='2',cause!=33)%>%
  group_by(cause)%>%
  summarise(number=sum(N)/10)%>%
  arrange(desc(number))%>%
  head(10)%>%
  mutate(cause=fct_recode(cause,'氣管.支氣管和肺癌'='13','肝和肝類膽管癌'='08',
                          '結腸.直腸和肛門癌'='07','女性乳房癌'='19',
                          '胃癌'='05','胰臟癌'='10',
                          '子宮頸及部位未明示子宮癌'='20','卵巢癌'='22',
                          '非何杰金氏淋巴瘤'='31','白血病'='32'))%>%
#畫圖
  ggplot(aes(x=reorder(cause,number),y=number,fill=reorder(cause,number)))+
  geom_col(width=0.5,show.legend=FALSE)+
  scale_fill_manual(values=c('#A1887F','#E57373','#F06292',
                             '#BA68C8','#7986CB','#4DD0E1',
                             '#AED581','#FFF176','#FFB74D',
                             '#FF8A65'))+
  labs(title='2008~2017年臺灣女性平均每年十大癌症死亡人數',x=NULL,y='人數')+
  ylim(0,3500)+
  coord_flip()+
  geom_text(aes(label=number),hjust=-0.2)+
  theme_bw()+
  ggsave('2008~2017年臺灣女性平均每年十大癌症死亡人數.png',width=30,height=14,units='cm')


#2008~2017年臺灣男性平均每年十大癌症死亡人數長條圖

#資料處理
c.cancer%>%
  filter(sex=='1',cause!=33)%>%
  group_by(cause)%>%
  summarise(number=sum(N)/10)%>%
  arrange(desc(number))%>%
  head(10)%>%
  mutate(cause=fct_recode(cause,'氣管.支氣管和肺癌'='13','肝和肝類膽管癌'='08',
                          '結腸.直腸和肛門癌'='07','口腔癌'='01',
                          '食道癌'='04','胃癌'='05',
                          '前列腺(攝護腺)癌'='23','胰臟癌'='10',
                          '非何杰金氏淋巴瘤'='31','白血病'='32'))%>%
#畫圖
  ggplot(aes(x=reorder(cause,number),y=number,fill=reorder(cause,number)))+
  geom_col(width=0.5,show.legend=FALSE)+
  scale_fill_manual(values=c('#A1887F','#E57373','#F06292',
                             '#BA68C8','#7986CB','#4DD0E1',
                             '#AED581','#FFF176','#FFB74D',
                             '#FF8A65'))+
  labs(title='2008~2017年臺灣男性平均每年十大癌症死亡人數',x=NULL,y='人數')+
  ylim(0,6000)+
  coord_flip()+
  geom_text(aes(label=number),hjust=-0.2)+
  theme_bw()+
  ggsave('2008~2017年臺灣男性平均每年十大癌症死亡人數.png',width=30,height=14,units='cm')


#2008~2017年臺灣三大癌症死亡總數變化折線圖

#資料處理
c.cancer%>%
  filter(cause%in%c('07','08','13'))%>%
  mutate(cause=fct_recode(cause,'結腸.直腸和肛門癌'='07','肝和肝類膽管癌'='08',
                          '氣管.支氣管和肺癌'='13'),
         year=factor(year))%>%
  group_by(year,cause)%>%
  summarise(number=sum(N))%>%
  arrange(cause)%>%
  #畫圖
  ggplot(aes(x=year,y=number,color=cause,group=cause))+
  geom_line()+
  geom_point()+
  labs(x=NULL,y=NULL,color='癌症類別',title='2008~2017年臺灣三大癌症死亡總數變化')+
  ggsave('2008~2017年臺灣三大癌症死亡總數變化.png',width=30,height=14,units='cm')


#2000~2015年臺灣主要癌症(肺癌、大腸癌)發生率折線圖

#資料處理
h.cancer%>%
  select(year,colnames(h.cancer)[3],colnames(h.cancer)[4])%>%
  gather(key='item',value='number','氣管.支氣管和肺癌','結腸.直腸和肛門癌')%>%
  #畫圖  
  ggplot(aes(x=year,y=number,color=item))+
  geom_line()+
  geom_point()+
  theme_gray(base_size=12)+
  labs(x=NULL,y=NULL,caption='單位:每十萬人',color='癌症類別',title='2000~2015年臺灣主要癌症發生率')+
  ggsave('2000~2015年臺灣主要癌症發生率.png',width=30,height=14,units='cm')


#2000~2015年臺灣女性乳癌、子宮頸癌發生率折線圖

#資料處理
h.cancer%>%
  select(year,colnames(h.cancer)[5],colnames(h.cancer)[6])%>%
  gather(key='item',value='number','女性乳癌','子宮頸癌')%>%
  #畫圖  
  ggplot(aes(x=year,y=number,color=item))+
  geom_line()+
  geom_point()+
  theme_gray(base_size=12)+
  labs(x=NULL,y=NULL,caption='單位:每十萬人',color='癌症類別',title='2000~2015年臺灣女性乳癌、子宮頸癌發生率')+
  ggsave('2000~2015年臺灣女性乳癌、子宮頸癌發生率.png',width=30,height=14,units='cm')




#patr2
#癌症各年死亡人數圖(三大+其他)

#資料處理
b.cancer <- bind_rows(cancer)
cancer1 <- b.cancer %>%
  filter(cause=="01") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="口腔癌")
cancer2 <- b.cancer %>%
  filter(cause=="02") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="主唾液腺癌")
cancer3 <- b.cancer %>%
  filter(cause=="03") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="鼻咽癌")
cancer4 <- b.cancer %>%
  filter(cause=="04") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="食道癌")
cancer5 <- b.cancer %>%
  filter(cause=="05") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="胃癌")
cancer6 <- b.cancer %>%
  filter(cause=="06") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="小腸癌")
cancer7 <- b.cancer %>%
  filter(cause=="07") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="結腸、直腸和肛門癌")
cancer8 <- b.cancer %>%
  filter(cause=="08") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="肝和肝類膽管癌")
cancer9 <- b.cancer %>%
  filter(cause=="09") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="膽囊和其他膽道癌")
cancer10 <- b.cancer %>%
  filter(cause=="10") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="胰臟癌")
cancer11 <- b.cancer %>%
  filter(cause=="11") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="鼻腔、中耳和副鼻竇癌")
cancer12 <- b.cancer %>%
  filter(cause=="12") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="喉癌")
cancer13 <- b.cancer %>%
  filter(cause=="13") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="氣管、支氣管和肺癌")
cancer14 <- b.cancer %>%
  filter(cause=="14") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="胸腺癌")
cancer15 <- b.cancer %>%
  filter(cause=="15") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="心臟、縱隔和胸(肋)膜癌")
cancer16 <- b.cancer %>%
  filter(cause=="16") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="骨和關節軟骨癌")
cancer17 <- b.cancer %>%
  filter(cause=="17") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="黑色素瘤和其他皮膚癌")
cancer18 <- b.cancer %>%
  filter(cause=="18") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="間皮和軟組織癌")
cancer19 <- b.cancer %>%
  filter(cause=="19") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="女性乳房癌")
cancer20 <- b.cancer %>%
  filter(cause=="20") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="子宮頸及部位未明示子宮癌")
cancer21 <- b.cancer %>%
  filter(cause=="21") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="子宮體癌")
cancer22 <- b.cancer %>%
  filter(cause=="22") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="卵巢癌")
cancer23 <- b.cancer %>%
  filter(cause=="23") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="攝護腺癌")
cancer24 <- b.cancer %>%
  filter(cause=="24") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="男性生殖器官癌")
cancer25 <- b.cancer %>%
  filter(cause=="25") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="腎臟癌")
cancer26 <- b.cancer %>%
  filter(cause=="26") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="膀胱癌")
cancer27 <- b.cancer %>%
  filter(cause=="27") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="腦癌")
cancer28 <- b.cancer %>%
  filter(cause=="28") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="腦膜及中樞神經系統其他部份癌")
cancer29 <- b.cancer %>%
  filter(cause=="29") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="甲狀腺癌")
cancer30 <- b.cancer %>%
  filter(cause=="30") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="何杰金氏症")
cancer31 <- b.cancer %>%
  filter(cause=="31") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="非何杰金氏淋巴瘤")
cancer32 <- b.cancer %>%
  filter(cause=="32") %>%
  select(cause,N,year) %>%
  group_by(year) %>%
  summarise(N=sum(N)) %>%
  mutate(cause="白血病")
t.cancer <- bind_rows(cancer1,cancer2,cancer3,cancer4,cancer5,cancer6,cancer7,cancer8,cancer9,cancer10,cancer11,cancer12,cancer13,cancer14,cancer15,cancer16,cancer17,cancer18,cancer19,cancer20,cancer21,cancer22,cancer23,cancer24,cancer25,cancer26,cancer27,cancer28,cancer29,cancer30,cancer31,cancer32)

#繪圖
highest <- filter(t.cancer,cause=="肝和肝類膽管癌"|cause=="氣管、支氣管和肺癌"|cause=="結腸、直腸和肛門癌")
ggplot(highest, aes(x=year,y=N))+geom_line()+facet_wrap(~ cause, nrow=1)+labs(title="三大癌症各年死亡人數(97年~106年)",x="年份",y="死亡人數")
ggsave("top3.png",width=30,height=14,units="cm")
little <- filter(t.cancer,N<3000)
ggplot(little, aes(x=year,y=N))+geom_line()+facet_wrap(~ cause, nrow=3)+theme(axis.text.x = element_text(angle=45, vjust=0.5))+labs(title="其他癌症各年死亡人數(97年~106年)",x="年份",y="死亡人數")
ggsave("others.png",width=30,height=14,units="cm")


#2008年~2015年男性癌症發生率

#資料處理
m.rate <- read_csv("癌症發生統計.csv", locale=locale(encoding="big5"))
m.rate <- m.rate %>%
  select(癌症診斷年,性別,癌症別,`年齡標準化發生率 (每10萬人口)`) %>%
  rename(sex=性別,year=癌症診斷年,cause=癌症別,rate=`年齡標準化發生率 (每10萬人口)`)
m.rate <- filter(m.rate,sex=="男",year>=2008,year<=2015,cause!="全癌症")
m.rate <- m.rate %>%
  group_by(year,cause) %>%
  summarise(rate=sum(rate)) %>%
  arrange(desc(rate)) %>%
  filter(cause=="肝及肝內膽管"|cause=="結直腸"|cause=="肺、支氣管及氣管"|cause=="口腔、口咽及下咽"|cause=="攝護腺"|cause=="胃"|cause=="食道"|cause=="皮膚") %>%
  mutate(cause=fct_inorder(cause))

#繪圖
ggplot(m.rate,aes(x=year,y=rate,colour=cause))+geom_line()+geom_point()+labs(title="2008年~2015年男性癌症發生率",x="",y="發生率(每十萬人口)")+scale_color_discrete(name="癌症種類")
ggsave("男性癌症發生率.png",width=30,height=14,units="cm")




#2008年~2015年女性癌症發生率

#資料處理
f.rate <- read_csv("癌症發生統計.csv", locale=locale(encoding="big5"))
f.rate <- f.rate %>%
  select(癌症診斷年,性別,癌症別,`年齡標準化發生率 (每10萬人口)`) %>%
  rename(sex=性別,year=癌症診斷年,cause=癌症別,rate=`年齡標準化發生率 (每10萬人口)`)
f.rate <- filter(f.rate,sex=="女",year>=2008,year<=2015,cause!="全癌症")
f.rate <- f.rate %>%
  group_by(year,cause) %>%
  summarise(rate=sum(rate)) %>%
  arrange(desc(rate)) %>%
  filter(rate>110) %>%
  mutate(cause=fct_inorder(cause))

#繪圖
ggplot(f.rate,aes(x=year,y=rate,colour=cause))+geom_line()+geom_point()+labs(title="2008年~2015年女性癌症發生率",x="",y="發生率(每十萬人口)")+scale_color_discrete(name="癌症種類")
ggsave("女性癌症發生率.png",width=30,height=14,units="cm")




#2015年食道癌發生性別比

#資料整理
mouth <- read_csv("癌症發生統計.csv", locale=locale(encoding="big5"))
mouth <- mouth %>%
  rename(sex=性別,year=癌症診斷年,cause=癌症別,count=癌症發生數,age=平均年齡) %>%
  select(year,sex,cause,age,count)
mouth <- filter(mouth,cause=="食道",sex=="女"|sex=="男")
male_female <- mouth %>%
  group_by(year,sex) %>%
  summarise(count=sum(count))
ggplot(male_female,aes(x=year,y=count,colour=sex))+geom_line()+geom_point()+labs(title="1979年~2015年食道癌發生數",x="",y="每年發生人數")+scale_color_discrete(name="性別")
ggsave("食道癌發生數(線).png",width=30,height=14,units="cm")
recent <- male_female %>%
  filter(year==2015) %>%
  mutate(share=count/5173*100)

#繪圖
ggplot(recent,aes(x=year,y=count,fill=sex))+
  geom_col()+
  coord_polar('y',start=0)+
  theme_void()+
  geom_text(aes(label=paste0(round(share,1),'%')),
            position=position_stack(vjust=0.5),
            vjust=0.5,hjust=0.5,size=4)+
  scale_fill_manual(values=c('#F8BBD0',"steelblue"),name='性別')+
  labs(title='2015年食道癌發生性別比')


#地圖1(肺癌)
#數值資料讀入和處理
lung.data<-read_csv('癌症發生統計.csv',locale=locale(encoding='big5'),
                    skip=1,col_names=c('year','sex','city',
                                       'cause','age','count',
                                       'avgage','medage','percent'))

lung.data<-lung.data%>%
  filter(year==2015,cause=='肺、支氣管及氣管',
         sex=='不分男女',city!='台閩地區')%>%
  select(year,city,cause,percent)%>%
  mutate(city=replace(city,city%in%c('台北市','台中市','台南市','台東縣'),
                      c('臺北市','臺中市','臺南市','臺東縣')))


#地圖資料讀入和處理
tw_map<-readOGR(dsn='mapdata201905100344',layer='COUNTY_MOI_1080509')
tw_map@data<-tw_map@data%>%
  mutate(COUNTYNAME=iconv(COUNTYNAME,from='UTF-8',to='big5'))
tw_map.ft<-tidy(tw_map,region='COUNTYCODE')%>%
  filter(long>=118&long<=123&lat<=27&lat>=21)

#資料合併
countmap.m<-inner_join(tw_map.ft,tw_map@data,by=c('id'='COUNTYCODE'))
merged.lung<-inner_join(lung.data,countmap.m,by=c('city'='COUNTYNAME'))
merged.lung<-arrange(merged.lung,group,order)

#讀取Google Api
register_google(key="xxx",write=TRUE)

#畫圖
qmap('Taiwan',zoom=7,maptype='roadmap',legend='bottomright')+
  geom_polygon(data=merged.lung,aes(x=long,y=lat,group=group,
                                    fill=percent),alpha=0.5,color='white')+
  scale_fill_distiller(palette='RdYlBu')+
  scale_alpha_continuous(guide=FALSE)+
  labs(title='2015年臺灣各縣市肺癌發生率',fill='單位:每十萬人')+
  ggsave('2015年臺灣各縣市肺癌發生率.png',width=30,height=14,units='cm')


#地圖2(女性乳癌)
#數值資料讀入和處理
breast.data<-read_csv('癌症發生統計.csv',locale=locale(encoding='big5'),
                      skip=1,col_names=c('year','sex','city',
                                         'cause','age','count',
                                         'avgage','medage','percent'))

breast.data<-breast.data%>%
  filter(year==2015,cause=='女性乳房',
         sex=='不分男女',city!='台閩地區')%>%
  select(year,city,cause,percent)%>%
  mutate(city=replace(city,city%in%c('台北市','台中市','台南市','台東縣'),
                      c('臺北市','臺中市','臺南市','臺東縣')))


#地圖資料讀入和處理
tw_map<-readOGR(dsn='mapdata201905100344',layer='COUNTY_MOI_1080509')
tw_map@data<-tw_map@data%>%
  mutate(COUNTYNAME=iconv(COUNTYNAME,from='UTF-8',to='big5'))
tw_map.ft<-tidy(tw_map,region='COUNTYCODE')%>%
  filter(long>=118&long<=123&lat<=27&lat>=21)

#資料合併
countmap.m<-inner_join(tw_map.ft,tw_map@data,by=c('id'='COUNTYCODE'))
merged.breast<-inner_join(breast.data,countmap.m,by=c('city'='COUNTYNAME'))
merged.breast<-arrange(merged.breast,group,order)

#讀取Google Api
register_google(key="xxx",write=TRUE)

#畫圖
qmap('Taiwan',zoom=7,maptype='roadmap',legend='bottomright')+
  geom_polygon(data=merged.breast,aes(x=long,y=lat,group=group,
                                      fill=percent),alpha=0.5,color='white')+
  scale_fill_distiller(palette='RdYlBu')+
  scale_alpha_continuous(guide=FALSE)+
  labs(title='2015年臺灣各縣市女性乳癌發生率',fill='單位:每十萬人')+
  ggsave('2015年臺灣各縣市女性乳癌發生率.png',width=30,height=14,units='cm')


#地圖3(大腸癌)
#數值資料讀入和處理
sausage.data<-read_csv('癌症發生統計.csv',locale=locale(encoding='big5'),
                       skip=1,col_names=c('year','sex','city',
                                          'cause','age','count',
                                          'avgage','medage','percent'))

sausage.data<-sausage.data%>%
  filter(year==2015,cause=='結直腸',
         sex=='不分男女',city!='台閩地區')%>%
  select(year,city,cause,percent)%>%
  mutate(city=replace(city,city%in%c('台北市','台中市','台南市','台東縣'),
                      c('臺北市','臺中市','臺南市','臺東縣')))


#地圖資料讀入和處理
tw_map<-readOGR(dsn='mapdata201905100344',layer='COUNTY_MOI_1080509')
tw_map@data<-tw_map@data%>%
  mutate(COUNTYNAME=iconv(COUNTYNAME,from='UTF-8',to='big5'))
tw_map.ft<-tidy(tw_map,region='COUNTYCODE')%>%
  filter(long>=118&long<=123&lat<=27&lat>=21)

#資料合併
countmap.m<-inner_join(tw_map.ft,tw_map@data,by=c('id'='COUNTYCODE'))
merged.sausage<-inner_join(sausage.data,countmap.m,by=c('city'='COUNTYNAME'))
merged.sausage<-arrange(merged.sausage,group,order)

#讀取Google Api
register_google(key="xxx",write=TRUE)

#畫圖
qmap('Taiwan',zoom=7,maptype='roadmap',legend='bottomright')+
  geom_polygon(data=merged.sausage,aes(x=long,y=lat,group=group,
                                       fill=percent),alpha=0.5,color='white')+
  scale_fill_distiller(palette='RdYlBu')+
  scale_alpha_continuous(guide=FALSE)+
  labs(title='2015年臺灣各縣市大腸癌發生率',fill='單位:每十萬人')+
  ggsave('2015年臺灣各縣市大腸癌發生率.png',width=30,height=14,units='cm')


#地圖4(食道癌)
#數值資料讀入和處理
eat.data<-read_csv('癌症發生統計.csv',locale=locale(encoding='big5'),
                   skip=1,col_names=c('year','sex','city',
                                      'cause','age','count',
                                      'avgage','medage','percent'))

eat.data<-eat.data%>%
  filter(year==2015,cause=='食道',
         sex=='不分男女',city!='台閩地區')%>%
  select(year,city,cause,percent)%>%
  mutate(city=replace(city,city%in%c('台北市','台中市','台南市','台東縣'),
                      c('臺北市','臺中市','臺南市','臺東縣')))


#地圖資料讀入和處理
tw_map<-readOGR(dsn='mapdata201905100344',layer='COUNTY_MOI_1080509')
tw_map@data<-tw_map@data%>%
  mutate(COUNTYNAME=iconv(COUNTYNAME,from='UTF-8',to='big5'))
tw_map.ft<-tidy(tw_map,region='COUNTYCODE')%>%
  filter(long>=118&long<=123&lat<=27&lat>=21)

#資料合併
countmap.m<-inner_join(tw_map.ft,tw_map@data,by=c('id'='COUNTYCODE'))
merged.eat<-inner_join(eat.data,countmap.m,by=c('city'='COUNTYNAME'))
merged.eat<-arrange(merged.eat,group,order)

#讀取Google Api
register_google(key="xxx",write=TRUE)

#畫圖
qmap('Taiwan',zoom=7,maptype='roadmap',legend='bottomright')+
  geom_polygon(data=merged.eat,aes(x=long,y=lat,group=group,
                                   fill=percent),alpha=0.5,color='white')+
  scale_fill_distiller(palette='RdYlBu')+
  scale_alpha_continuous(guide=FALSE)+
  labs(title='2015年臺灣各縣市食道癌發生率',fill='單位:每十萬人')+
  ggsave('2015年臺灣各縣市食道癌發生率.png',width=30,height=14,units='cm')


#地圖5(口腔癌)
#數值資料讀入和處理
mouth.data<-read_csv('癌症發生統計.csv',locale=locale(encoding='big5'),
                     skip=1,col_names=c('year','sex','city',
                                        'cause','age','count',
                                        'avgage','medage','percent'))

mouth.data<-mouth.data%>%
  filter(year==2015,cause=='口腔、口咽及下咽',
         sex=='不分男女',city!='台閩地區')%>%
  select(year,city,cause,percent)%>%
  mutate(city=replace(city,city%in%c('台北市','台中市','台南市','台東縣'),
                      c('臺北市','臺中市','臺南市','臺東縣')))


#地圖資料讀入和處理
tw_map<-readOGR(dsn='mapdata201905100344',layer='COUNTY_MOI_1080509')
tw_map@data<-tw_map@data%>%
  mutate(COUNTYNAME=iconv(COUNTYNAME,from='UTF-8',to='big5'))
tw_map.ft<-tidy(tw_map,region='COUNTYCODE')%>%
  filter(long>=118&long<=123&lat<=27&lat>=21)

#資料合併
countmap.m<-inner_join(tw_map.ft,tw_map@data,by=c('id'='COUNTYCODE'))
merged.mouth<-inner_join(mouth.data,countmap.m,by=c('city'='COUNTYNAME'))
merged.mouth<-arrange(merged.mouth,group,order)

#讀取Google Api
register_google(key="xxx",write=TRUE)

#畫圖
qmap('Taiwan',zoom=7,maptype='roadmap',legend='bottomright')+
  geom_polygon(data=merged.mouth,aes(x=long,y=lat,group=group,
                                     fill=percent),alpha=0.5,color='white')+
  scale_fill_distiller(palette='RdYlBu')+
  scale_alpha_continuous(guide=FALSE)+
  labs(title='2015年臺灣各縣市口腔癌發生率',fill='單位:每十萬人')+
  ggsave('2015年臺灣各縣市口腔癌發生率.png',width=30,height=14,units='cm')


#地圖6(肝癌)
#數值資料讀入和處理
liver.data<-read_csv('癌症發生統計.csv',locale=locale(encoding='big5'),
                     skip=1,col_names=c('year','sex','city',
                                        'cause','age','count',
                                        'avgage','medage','percent'))

liver.data<-liver.data%>%
  filter(year==2015,cause=='肝及肝內膽管',
         sex=='不分男女',city!='台閩地區')%>%
  select(year,city,cause,percent)%>%
  mutate(city=replace(city,city%in%c('台北市','台中市','台南市','台東縣'),
                      c('臺北市','臺中市','臺南市','臺東縣')))


#地圖資料讀入和處理
tw_map<-readOGR(dsn='mapdata201905100344',layer='COUNTY_MOI_1080509')
tw_map@data<-tw_map@data%>%
  mutate(COUNTYNAME=iconv(COUNTYNAME,from='UTF-8',to='big5'))
tw_map.ft<-tidy(tw_map,region='COUNTYCODE')%>%
  filter(long>=118&long<=123&lat<=27&lat>=21)

#資料合併
countmap.m<-inner_join(tw_map.ft,tw_map@data,by=c('id'='COUNTYCODE'))
merged.liver<-inner_join(liver.data,countmap.m,by=c('city'='COUNTYNAME'))
merged.liver<-arrange(merged.liver,group,order)

#讀取Google Api
register_google(key="xxx",write=TRUE)

#畫圖
qmap('Taiwan',zoom=7,maptype='roadmap',legend='bottomright')+
  geom_polygon(data=merged.liver,aes(x=long,y=lat,group=group,
                                     fill=percent),alpha=0.5,color='white')+
  scale_fill_distiller(palette='RdYlBu')+
  scale_alpha_continuous(guide=FALSE)+
  labs(title='2015年臺灣各縣市肝癌發生率',fill='單位:每十萬人')+
  ggsave('2015年臺灣各縣市肝癌發生率.png',width=30,height=14,units='cm')


#繪製癌症平均發生年齡(依癌症種類區分)圖
#讀取資料
data<-read_csv('癌症發生統計.csv',locale=locale(encoding='big5'),
               skip=1,col_names=c('year','sex','city',
                                  'cause','age','count',
                                  'avgage','medage','percent'))
#資料整理
age1 <- data %>%
  select(year,cause,city,avgage,sex) %>%
  filter(city=="台閩地區",sex=="不分男女") %>%
  filter(cause=="口腔、口咽及下咽"|cause=="女性乳房"|cause=="胃"|cause=="食道"|cause=="肺、支氣管及氣管"|cause=="胰"|cause=="子宮頸"|cause=="肝及肝內膽管"|cause=="攝護腺"|cause=="結直腸"|cause=="卵巢、輸卵管及寬韌帶")
#繪圖
ggplot(age1,aes(x=year,y=avgage,colour=cause))+geom_line()+geom_point()+scale_color_discrete(name="癌症種類")+labs(x="年份",y="平均年齡",title="癌症平均發生年齡(依癌症種類區分)")
ggsave('癌症發生年齡(癌症種類).png',width=30,height=14,units='cm')


#繪製癌症平均發生年齡(依性別區分)圖
#資料整理
age2 <- data %>%
  select(year,cause,city,sex,avgage) %>%
  filter(city=="台閩地區",cause=="全癌症",sex!="不分男女")
#繪圖
ggplot(age2,aes(x=year,y=avgage,colour=sex))+geom_line()+geom_point()+labs(x="年份",y="平均年齡",title="癌症平均發生年齡(依性別區分)")+scale_color_discrete(name="性別")
ggsave('癌症平均發生年齡(男女).png',width=30,height=14,units='cm')


#繪製2013年各縣市嚼檳榔率圖
#讀取資料
xdata <- read_excel("102年18歲以上人口目前嚼檳榔率-依縣市別分.xlsx",
                    skip=1,col_names=c('city','N','male','female','per'))
#資料整理
xdata<-xdata %>%
  mutate(per=as.numeric(per)) %>%
  mutate(per=per/100)%>%
  arrange(desc(per)) %>%
  filter(per>0.038)
#繪圖
ggplot(xdata,aes(x=fct_inorder(city),y=per))+geom_col(fill="navyblue")+labs(x="縣市",y="",title="2013年各縣市嚼檳榔率")+geom_text(aes(label=per),vjust=-0.3)
ggsave('2013年各縣市嚼檳榔率.png',width=30,height=14,units='cm')
