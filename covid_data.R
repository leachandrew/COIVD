#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive")
print(getwd())

#library(tidyr)
#library(dplyr)
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(scales)
library(ggthemes)
library(gghighlight)
library(roll)
source("andrew_base.R")

blakes_blue<-"#4477AA"

blake_theme<-function(){
  theme_hc(20)+
    theme(plot.subtitle = element_text(color="grey10",size=rel(.7)),
          plot.title = element_text(face="bold"),
          plot.caption = element_text(color="grey50",size=rel(.5)),
          legend.title = element_text(color="grey10",size=rel(.5)),
          legend.text = element_text(color="grey10",size=rel(.5)),
          axis.title = element_text(size=rel(.8)),
          axis.ticks = element_blank(),
          panel.spacing = unit(2,"lines"),
          legend.position = "none",
          plot.margin = margin(t = .5, r = 1, b = .25, l = 1,unit= "cm"),
          axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
          
    )
}



# IMPORT RAW DATA: Johns Hopkins Github data
us_cases<-read.csv("https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv?raw=true") %>% 
  filter(iso2=="US",!grepl("Princess",Province_State)) %>% select(-c(1:6)) %>% pivot_longer(-c(1:11),names_to = "date",values_to="cases") %>% clean_names()%>%
  mutate(date=gsub("X","",date), date=mdy(date)) %>% arrange(date)%>%
  group_by(country_region,province_state,date) %>% summarize(lat=(max(lat)-min(lat))/2,long=(max(long)-min(long))/2,cases=sum(cases)) 


global_cases <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")%>%
 pivot_longer(-c(1:4),names_to = "date",values_to="cases") %>% clean_names()%>%
  filter(!grepl("Princess",province_state)) %>%
  mutate(date=gsub("X","",date), date=mdy(date)) %>% arrange(date) %>% 
  filter(country_region!="US")%>% bind_rows(us_cases)




#pivot to long net of first 11 columns


graph_data<-global_cases %>% filter(country_region=="US")%>%
  group_by(province_state) %>% mutate(last_cases=last(cases),test=(last(cases)/cases[n()-3])^(1/3),
                                      new_cases=cases-lag(cases),new_7d=roll_mean(new_cases,7))%>%
  mutate(test=last(new_7d/lag(new_7d,14))) 


#nv_data<-us_cases%>%filter(province_state=="Nevada")
                                                             

cutoff<-2


ggplot(filter(graph_data,date>ymd("2020-04-01"),test>cutoff)) +
  #geom_ribbon(aes(x = hour_start, ymin = min_vol, ymax = max_vol,fill="A"),alpha=.25, linetype = 0)+
  geom_rect(aes(xmin = Sys.Date()-days(14), xmax =Sys.Date(),ymin = -Inf, ymax = Inf,
                fill = "Last 14 days"),alpha = .8)+
  
  geom_line(size=1.2,aes(date,new_7d,group=province_state,colour="7 day moving average of new cases"))+
  geom_point(size=1.5,aes(date,pmax(new_cases,0),group=province_state,shape="Daily new cases"),color=blakes_blue)+
  facet_wrap(~province_state,scales="free_y",nrow = 2)+
  blake_theme()+ theme(panel.spacing = unit(.5,"lines"))+
  theme(legend.position = "bottom")+
  #annotate("text", x = as.Date(Sys.Date()-days(7)), y=0, label = "Last 14 days",size=1,hjust=0.5,vjust=0)+
  scale_x_date(date_breaks = "1 month",date_labels="%d %b\n%Y")+
  expand_limits(y = 0)+ 
  scale_colour_manual("",values = c("grey30"))+
  scale_fill_manual("",values = c("grey70"))+
  scale_shape_manual("",values = c(20))+
  labs(y="New Confirmed COVID-19 Cases (linear scale)",x="",
             title="New US COVID-19 Cases",
             subtitle=paste("States shown have more than a ",cutoff,"x increase in the 7 day moving average of reported new cases from ",format(Sys.Date()-days(14),"%b %d")," to ",format(Sys.Date(),"%b %d"),sep=""),
             caption="Source: Novel Coronavirus (COVID-19) cases provided by JHU CSSE (https://systems.jhu.edu/research/public-health/ncov/), graph by Andrew Leach\n
       Note: days with negative case growth (i.e. data revisions) are shown as zeros, but raw case counts are used for moving averages.")
ggsave("covid_us.png",width = 16,height = 10)


top_10<-c("California","Texas", "New York","Florida", "Illinois","Pennsylvania","Ohio","Michigan",
          "Georgia","North Carolina")

top_data<-graph_data %>% ungroup() %>% filter(province_state %in% top_10) %>%
  mutate(province_state = factor(province_state,levels=top_10))
ggplot(filter(top_data,date>ymd("2020-04-01"))) +
  #geom_ribbon(aes(x = hour_start, ymin = min_vol, ymax = max_vol,fill="A"),alpha=.25, linetype = 0)+
  geom_rect(aes(xmin = Sys.Date()-days(14), xmax =Sys.Date(),ymin = -Inf, ymax = Inf,
                fill = "Last 14 days"),alpha = .8)+
  geom_line(size=1.2,aes(date,new_7d,group=province_state,colour="7 day moving average of new cases"))+
  geom_point(size=1.5,aes(date,pmax(new_cases,0),group=province_state,shape="Daily new cases"),color=blakes_blue)+
  facet_wrap(~province_state,scales="free_y",nrow = 2)+
  blake_theme()+ theme(panel.spacing = unit(.5,"lines"))+
  expand_limits(y = 0)+
  theme(legend.position = "bottom")+
  scale_x_date(date_breaks = "1 month",date_labels="%d %b\n%Y")+
  scale_colour_manual("",values = c("grey30"))+
  scale_fill_manual("",values = c("grey70"))+
  scale_shape_manual("",values = c(20))+
  labs(y="New Confirmed COVID-19 Cases (linear scale)",x="",
       title="New US COVID-19 Cases",
       subtitle=paste("States shown are Top-10 states by population, data from ",format(Sys.Date(),"%b %d, %Y"),sep=""),
       caption="Source: Novel Coronavirus (COVID-19) cases provided by JHU CSSE (https://systems.jhu.edu/research/public-health/ncov/), graph by Andrew Leach\n
       Note: days with negative case growth (i.e. data revisions) are shown as zeros, but raw case counts are used for moving averages.")
ggsave("covid_us2.png",width = 16,height = 10)







ggplot(filter(us_cases,date>ymd("2020-04-01"))) +
  #geom_ribbon(aes(x = hour_start, ymin = min_vol, ymax = max_vol,fill="A"),alpha=.25, linetype = 0)+
  geom_line(size=1.2,aes(date,cases,group=province_state,colour="7 day moving average of new cases"))+
  facet_geo(~province_state,scales="free_y")+
  
  geom_point(size=1.5,aes(date,pmax(new_cases,0),group=province_state,shape="Daily new cases"),color=blakes_blue)+
  facet_geo(~province_state,scales="free_y",nrow = 2)+
  #blake_theme()+ 
  theme(legend.position = "bottom")+
  #annotate("text", x = as.Date(Sys.Date()-days(7)), y=0, label = "Last 14 days",size=1,hjust=0.5,vjust=0)+
  scale_x_date(date_breaks = "1 month",date_labels="%d %b\n%Y")+
  expand_limits(y = 0)+ 
  #scale_y_continuous(trans = 'log10',
  #                   breaks = trans_breaks('log10', function(x) 10^x),
  #                   labels = trans_format('log10', math_format(10^.x)),
  #                   limits = c(100,10^6))+
  scale_colour_manual("",values = c("grey30"))+
  scale_fill_manual("",values = c("grey70"))+
  scale_shape_manual("",values = c(20))+
  labs(y="New Confirmed COVID-19 Cases (linear scale)",x="",
       title="New US COVID-19 Cases",
       subtitle=paste("States shown have more than a ",cutoff,"x increase in the 7 day moving average of reported new cases from ",format(Sys.Date()-days(14),"%b %d")," to ",format(Sys.Date(),"%b %d"),sep=""),
       caption="Source: Novel Coronavirus (COVID-19) cases provided by JHU CSSE (https://systems.jhu.edu/research/public-health/ncov/), graph by Andrew Leach\n
       Note: days with negative case growth (i.e. data revisions) are shown as zeros, but raw case counts are used for moving averages.")
ggsave("covid_us.png",width = 16,height = 10)



provs<-c("British Columbia","Alberta","Saskatchewan","Manitoba","Ontario",
         "Quebec","New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador",
         "Northwest Territories","Yukon")  

cda_data<-global_cases %>% filter(country_region=="Canada")%>%
  mutate(province_state=factor(province_state,levels=provs),
         province_state=fct_other(province_state,drop=c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador"),other_level="Atlantic Provinces"),
         province_state=fct_other(province_state,drop=c("Northwest Territories","Yukon"),other_level="Territories"))%>%
  group_by(province_state,date) %>% summarize(cases=sum(cases))%>%
  group_by(province_state)%>%
  mutate(last_cases=last(cases),test=(last(cases)/cases[n()-3])^(1/3),
                 new_cases=cases-lag(cases),new_7d=roll_mean(new_cases,7))%>%
  mutate(test=last(new_7d/lag(new_7d,14))) %>% ungroup()


ggplot(filter(cda_data)) +
  #geom_ribbon(aes(x = hour_start, ymin = min_vol, ymax = max_vol,fill="A"),alpha=.25, linetype = 0)+
  geom_rect(aes(xmin = Sys.Date()-days(14), xmax =Sys.Date(),ymin = -Inf, ymax = Inf,
                fill = "Last 14 days"),alpha = .8)+
  
  geom_line(size=1.2,aes(date,new_7d,group=province_state,colour="7 day moving average of new cases"))+
  geom_point(size=1.5,aes(date,pmax(new_cases,0),group=province_state,shape="Daily new cases"),color=blakes_blue)+
  facet_wrap(~province_state,scales="free_y",nrow = 2)+
  blake_theme()+ theme(panel.spacing = unit(.5,"lines"))+
  theme(legend.position = "bottom")+
  #annotate("text", x = as.Date(Sys.Date()-days(7)), y=0, label = "Last 14 days",size=1,hjust=0.5,vjust=0)+
  scale_x_date(date_breaks = "1 month",date_labels="%b")+
  expand_limits(y = 0)+ 
  scale_colour_manual("",values = c("grey30"))+
  scale_fill_manual("",values = c("grey70"))+
  scale_shape_manual("",values = c(20))+
  labs(y="New Confirmed COVID-19 Cases (linear scale)",x="",
       title="New Canadian COVID-19 Cases",
       #subtitle=paste("States shown have more than a ",cutoff,"x increase in the 7 day moving average of reported new cases from ",format(Sys.Date()-days(14),"%b %d")," to ",format(Sys.Date(),"%b %d"),sep=""),
       caption="Source: Novel Coronavirus (COVID-19) cases provided by JHU CSSE (https://systems.jhu.edu/research/public-health/ncov/). Graph by Andrew Leach")
ggsave("can_covid_long.png",width = 16,height = 10)

ggplot(filter(cda_data,date>ymd("2020-04-01"),province_state=="Alberta")) +
  #geom_ribbon(aes(x = hour_start, ymin = min_vol, ymax = max_vol,fill="A"),alpha=.25, linetype = 0)+
  geom_rect(aes(xmin = Sys.Date()-days(14), xmax =Sys.Date(),ymin = -Inf, ymax = Inf,
                fill = "Last 14 days"),alpha = .8)+
  
  geom_line(size=1.2,aes(date,new_7d,group=province_state,colour="7 day moving average of new cases"))+
  geom_point(size=1.5,aes(date,pmax(new_cases,0),group=province_state,shape="Daily new cases"),color=blakes_blue)+
  #facet_wrap(~province_state,scales="free_y",nrow = 2)+
  blake_theme()+ theme(panel.spacing = unit(.5,"lines"))+
  theme(legend.position = "bottom")+
  #annotate("text", x = as.Date(Sys.Date()-days(7)), y=0, label = "Last 14 days",size=1,hjust=0.5,vjust=0)+
  scale_x_date(date_breaks = "1 month",date_labels="%d %b\n%Y")+
  expand_limits(y = 0)+ 
  scale_colour_manual("",values = c("grey30"))+
  scale_fill_manual("",values = c("grey70"))+
  scale_shape_manual("",values = c(20))+
  labs(y="New Confirmed COVID-19 Cases (linear scale)",x="",
       title="Alberta's COVID-19 Cases",
       #subtitle=paste("States shown have more than a ",cutoff,"x increase in the 7 day moving average of reported new cases from ",format(Sys.Date()-days(14),"%b %d")," to ",format(Sys.Date(),"%b %d"),sep=""),
       caption="Source: Novel Coronavirus (COVID-19) cases provided by JHU CSSE (https://systems.jhu.edu/research/public-health/ncov/). Graph by Andrew Leach")
ggsave("ab_covid.png",width = 16,height = 10)

