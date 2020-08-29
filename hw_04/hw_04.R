library(tidyverse)

setwd(getwd())
source('W4_Solution.R')

#part 1
sub_data2015%>%
  group_by(area)%>%
  mutate(rank=min_rank(desc(tax_incidence)))%>%
  arrange(area,rank)%>%
  view()


#part 2
sub_data2015<-mutate(sub_data2015,pos=(pop_change>0))

ggplot(sub_data2015,aes(x=reorder(city,pop_change),y=pop_change,fill=pos))+
  geom_col(color='black')+
  theme(axis.text.x=element_text(angle=60,hjust=1))+
  labs(x='City',y='Population Change(%)')+
  scale_fill_manual(values=c('#CCEEFF','#FFDDDD'),guide=FALSE)
