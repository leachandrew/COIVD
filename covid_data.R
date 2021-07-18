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
    theme(plot.subtitle = element_text(color="grey10",size=rel(.5)),
          plot.title = element_text(face="bold"),
          plot.caption = element_text(color="grey50",size=rel(.5)),
          legend.title = element_text(color="grey10",size=rel(.5)),
          legend.text = element_text(color="grey10",size=rel(.5)),
          strip.text = element_text(size=rel(.5)),
          axis.title = element_text(size=rel(.5)),
          axis.text = element_text(size=rel(.5)),
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


# IMPORT RAW DATA: Johns Hopkins Github data
us_deaths<-read.csv("https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv?raw=true") %>% 
  filter(iso2=="US",!grepl("Princess",Province_State)) %>% select(-c(1:6)) %>% pivot_longer(-c(1:11),names_to = "date",values_to="deaths") %>% clean_names()%>%
  mutate(date=gsub("X","",date), date=mdy(date)) %>% arrange(date)%>%
  group_by(country_region,province_state,date) %>% summarize(lat=(max(lat)-min(lat))/2,long=(max(long)-min(long))/2,deaths=sum(deaths)) 


global_deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")%>%
  pivot_longer(-c(1:4),names_to = "date",values_to="deaths") %>% clean_names()%>%
  filter(!grepl("Princess",province_state)) %>%
  mutate(date=gsub("X","",date), date=mdy(date)) %>% arrange(date) %>% 
  filter(country_region!="US")%>% bind_rows(us_deaths)


global_cases<-global_cases %>% left_join(global_deaths)

#pivot to long net of first 11 columns


graph_data<-global_cases %>% filter(country_region=="US")%>%
  group_by(province_state) %>% mutate(last_cases=last(cases),test=(last(cases)/cases[n()-3])^(1/3),
                                      new_cases=cases-lag(cases),new_7d=roll_mean(new_cases,7),
                                      last_deaths=last(deaths),
                                      new_deaths=deaths-lag(deaths),deaths_7d=roll_mean(new_deaths,7)
                                      )%>%
  mutate(test=last(new_7d/lag(new_7d,14))) 





#nv_data<-us_cases%>%filter(province_state=="Nevada")
                                                             

cutoff<-1


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
  scale_x_date(date_breaks = "1 month",date_labels="%d\n%b")+
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
  scale_x_date(date_breaks = "1 month",date_labels="%d\n%b")+
  scale_colour_manual("",values = c("grey30"))+
  scale_fill_manual("",values = c("grey70"))+
  scale_shape_manual("",values = c(20))+
  labs(y="New Confirmed COVID-19 Cases (linear scale)",x="",
       title="New US COVID-19 Cases",
       subtitle=paste("States shown are Top-10 states by population, data from ",format(Sys.Date(),"%b %d, %Y"),sep=""),
       caption="Source: Novel Coronavirus (COVID-19) cases provided by JHU CSSE (https://systems.jhu.edu/research/public-health/ncov/), graph by Andrew Leach\n
       Note: days with negative case growth (i.e. data revisions) are shown as zeros, but raw case counts are used for moving averages.")
ggsave("covid_us3.png",width = 16,height = 10)



download.file("https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/totals/nst-est2019-01.xlsx",mode="wb",destfile = "us_pops.xlsx")
us_pop<-read_excel("us_pops.xlsx",skip = 3)%>% rename(state=1) %>% clean_names()%>%
pivot_longer(-c(1:3),values_to="population",names_to="year")%>%
  mutate(year=gsub("x","",year),
         state=gsub("\\.","",state),
         )%>% filter(year==2019)

cda_all<-global_cases %>% filter(country_region=="Canada")%>%
  group_by(date) %>% summarize(cases=sum(cases),province_state="Canada")%>%
  mutate(population=37590000,last_cases=last(cases),test=(last(cases)/cases[n()-3])^(1/3),
         new_cases=cases-lag(cases),new_7d=roll_mean(new_cases,7),active=roll_sum(new_cases,14))%>%
  mutate(test=last(new_7d/lag(new_7d,14)),active_pop=active/population*10^6,canada_pop=new_7d/population*10^6,canada_raw=new_cases/population*10^6) %>% ungroup()%>%
  select(date,can_cases=canada_raw,canada_pop,canada_active=active,canada_active_pop=active_pop)

ab_all<-global_cases %>% filter(province_state=="Alberta")%>%
  # group_by(date) %>% summarize(cases=sum(cases),province_state="Canada")%>%
  mutate(population=4472800,last_cases=last(cases),test=(last(cases)/cases[n()-3])^(1/3),
         new_cases=cases-lag(cases),new_7d=roll_mean(new_cases,7),active=roll_sum(new_cases,14))%>%
  mutate(test=last(new_7d/lag(new_7d,14)),active_pop=active/population*10^6,ab_pop=new_7d/population*10^6,ab_raw=new_cases/population*10^6) %>% ungroup()%>%
  select(date,ab_cases=ab_raw,ab_pop,ab_active=active,ab_active_pop=active_pop)

on_all<-global_cases %>% filter(province_state=="Ontario")%>%
  # group_by(date) %>% summarize(cases=sum(cases),province_state="Canada")%>%
  mutate(population=14677900,last_cases=last(cases),test=(last(cases)/cases[n()-3])^(1/3),
         new_cases=cases-lag(cases),new_7d=roll_mean(new_cases,7),active=roll_sum(new_cases,14))%>%
  mutate(test=last(new_7d/lag(new_7d,14)),active_pop=active/population*10^6,on_pop=new_7d/population*10^6,on_raw=new_cases/population*10^6) %>% ungroup()%>%
  select(date,on_cases=on_raw,on_pop,on_active=active,on_active_pop=active_pop)

qc_all<-global_cases %>% filter(province_state=="Quebec")%>%
  # group_by(date) %>% summarize(cases=sum(cases),province_state="Canada")%>%
  mutate(populatiqc=8494500,last_cases=last(cases),test=(last(cases)/cases[n()-3])^(1/3),
         new_cases=cases-lag(cases),new_7d=roll_mean(new_cases,7),active=roll_sum(new_cases,14))%>%
  mutate(test=last(new_7d/lag(new_7d,14)),active_pop=active/populatiqc*10^6,qc_pop=new_7d/populatiqc*10^6,qc_raw=new_cases/populatiqc*10^6) %>% ungroup()%>%
  select(date,qc_cases=qc_raw,qc_pop,qc_active=active,qc_active_pop=active_pop)


graph_data<-global_cases %>% filter(country_region=="US")%>%
  group_by(province_state) %>% mutate(last_cases=last(cases),test=(last(cases)/cases[n()-3])^(1/3),
                                      new_cases=cases-lag(cases),new_7d=roll_mean(new_cases,7),active=roll_sum(new_cases,14))%>%
  mutate(test=last(new_7d/lag(new_7d,14))) %>%left_join(us_pop,by=c("province_state"="state"))%>%
  mutate(population=population/10^6) %>% left_join(cda_all) %>% left_join(ab_all)%>% left_join(on_all)%>% left_join(qc_all)


#get worst 10 US states

filter_set<-graph_data %>% group_by(province_state)%>% summarize(last_cases=last(new_7d/population))%>%
  ungroup()%>% arrange(last_cases) %>% tail(10)

cutoff<-min(tail(sort(unique(graph_data$test)),10))
pt_size<-0

filter_set<-c(filter_set$province_state,"Alberta","Quebec","Ontario")

ggplot(filter(graph_data,date>ymd("2020-04-01"),province_state %in% filter_set)) +
  #geom_ribbon(aes(x = hour_start, ymin = min_vol, ymax = max_vol,fill="A"),alpha=.25, linetype = 0)+
  geom_rect(aes(xmin = Sys.Date()-days(14), xmax =Sys.Date(),ymin = -Inf, ymax = Inf,
                fill = "Last 14 days"),alpha = .8)+
  #geom_point(size=1,stroke=0,aes(date,ab_cases,colour="Alberta",shape="Alberta"))+
  #geom_point(size=1,stroke=0,aes(date,qc_cases,colour="Quebec",shape="Quebec"))+
  #geom_point(size=1,stroke=0,aes(date,on_cases,colour="Ontario",shape="Ontario"))+
  #geom_point(size=1,stroke=0,aes(date,pmax(new_cases,0)/population,colour="State",shape="State"))+
  geom_line(size=.75,aes(date,ab_pop,colour="Alberta"))+
  geom_line(size=.75,aes(date,qc_pop,colour="Quebec"))+
  geom_line(size=.75,aes(date,on_pop,colour="Ontario"))+
  geom_line(size=.75,aes(date,new_7d/population,group=province_state,colour="State"))+
  
  
  facet_wrap(~province_state,scales="fixed",nrow = 2)+
  blake_theme()+ theme(panel.spacing = unit(.5,"lines"))+
  theme(legend.position = "bottom")+
  #annotate("text", x = as.Date(Sys.Date()-days(7)), y=0, label = "Last 14 days",size=1,hjust=0.5,vjust=0)+
  scale_x_date(date_breaks = "2 month",date_labels="%b")+
  expand_limits(y = 0)+ 
  #scale_colour_manual("",values = c("#FF0000",blakes_blue,colors_ua10()[1],"#FF0000",blakes_blue,colors_ua10()[1]))+
  
  scale_colour_manual("",values = colors_tableau10())+
  
  scale_fill_manual("",values = c("grey70"))+
  #scale_shape_manual("",values = c(20,20,20,20,20))+
  #blank_shape
  scale_shape_manual("",values = c(16,16,16,16,16))+
  
  
  labs(y="New Confirmed COVID-19 Cases per Million Residents",x="",
       title="New COVID-19 Cases in US States Compared to Canada",
       subtitle=paste("Daily new cases and 7-day moving average new cases states with more than a ",sprintf("%.2f", cutoff),"x increase in the 7 day moving average of reported new cases from ",format(Sys.Date()-days(14),"%b %d")," to ",format(Sys.Date(),"%b %d"),sep=""),
       caption="Source: Novel Coronavirus (COVID-19) cases provided by JHU CSSE (https://systems.jhu.edu/research/public-health/ncov/), graph by Andrew Leach\n
       Note: days with negative case growth (i.e. data revisions) are shown as zeros, but raw case counts are used for moving averages.")
ggsave("covid_us_pop.png",width = 16,height = 10)



filter_set<-c("Florida","Texas","South Dakota","Alberta")

ggplot(filter(graph_data,date>ymd("2020-04-01"),province_state %in% filter_set)) +
  #geom_ribbon(aes(x = hour_start, ymin = min_vol, ymax = max_vol,fill="A"),alpha=.25, linetype = 0)+
  #geom_rect(aes(xmin = Sys.Date()-days(14), xmax =Sys.Date(),ymin = -Inf, ymax = Inf,
  #              fill = "Last 14 days"),alpha = .8)+
  #geom_point(size=1,stroke=0,aes(date,ab_cases,colour="Alberta",shape="Alberta"))+
  #geom_point(size=1,stroke=0,aes(date,qc_cases,colour="Quebec",shape="Quebec"))+
  #geom_point(size=1,stroke=0,aes(date,on_cases,colour="Ontario",shape="Ontario"))+
  #geom_point(size=1,stroke=0,aes(date,pmax(new_cases,0)/population,colour="State",shape="State"))+
  geom_line(size=.75,aes(date,ab_pop,colour="Alberta"))+
  #geom_line(size=.75,aes(date,qc_pop,colour="Quebec"))+
  #geom_line(size=.75,aes(date,on_pop,colour="Ontario"))+
  geom_line(size=.75,aes(date,new_7d/population,group=province_state,colour=province_state))+
  
  
  #facet_wrap(~province_state,scales="fixed",nrow = 2)+
  blake_theme()+ theme(panel.spacing = unit(.5,"lines"))+
  theme(legend.position = "bottom")+
  #annotate("text", x = as.Date(Sys.Date()-days(7)), y=0, label = "Last 14 days",size=1,hjust=0.5,vjust=0)+
  scale_x_date(date_breaks = "2 month",date_labels="%b")+
  expand_limits(y = 0)+ 
  #scale_colour_manual("",values = c("#FF0000",blakes_blue,colors_ua10()[1],"#FF0000",blakes_blue,colors_ua10()[1]))+
  
  scale_colour_manual("",values = colors_tableau10())+
  
  scale_fill_manual("",values = c("grey70"))+
  #scale_shape_manual("",values = c(20,20,20,20,20))+
  #blank_shape
  scale_shape_manual("",values = c(16,16,16,16,16))+
  
  
  labs(y="New Confirmed COVID-19 Cases per Million Residents",x="",
       title="New COVID-19 cases in US states Drew Barnes thinks we should emulate compared to Alberta",
       #subtitle=paste("Daily new cases and 7-day moving average new cases states with more than a ",sprintf("%.2f", cutoff),"x increase in the 7 day moving average of reported new cases from ",format(Sys.Date()-days(14),"%b %d")," to ",format(Sys.Date(),"%b %d"),sep=""),
       caption="Source: Novel Coronavirus (COVID-19) cases provided by JHU CSSE (https://systems.jhu.edu/research/public-health/ncov/), graph by Andrew Leach\n
       Note: days with negative case growth (i.e. data revisions) are shown as zeros, but raw case counts are used for moving averages.")
ggsave("covid_barnes_pop.png",width = 16,height = 10)



graph_data<-global_cases %>% filter(country_region=="US")%>%
  group_by(province_state) %>% mutate(last_cases=last(cases),test=(last(cases)/cases[n()-3])^(1/3),
                                      new_cases=cases-lag(cases),new_7d=roll_mean(new_cases,7),active=roll_sum(new_cases,14))%>%
  mutate(test=last(new_7d/lag(new_7d,14))) %>%left_join(us_pop,by=c("province_state"="state"))%>%
  mutate(population=population/10^6) %>% left_join(cda_all) %>% left_join(ab_all)%>% left_join(on_all)%>% left_join(qc_all)



top_data<-graph_data %>% ungroup() %>% filter(province_state %in% top_10) %>%
  mutate(province_state = factor(province_state,levels=top_10))


ggplot(filter(top_data,date>=ymd("2020-02-01"))) +
  geom_rect(aes(xmin = Sys.Date()-days(14), xmax =Sys.Date(),ymin = -Inf, ymax = Inf,
                fill = "Last 14 days"),alpha = .8)+
  
  #geom_point(size=1,stroke=0,aes(date,ab_cases,colour="Alberta",shape="Alberta"))+
  #geom_point(size=1,stroke=0,aes(date,qc_cases,colour="Quebec",shape="Quebec"))+
  #geom_point(size=1,stroke=0,aes(date,on_cases,colour="Ontario",shape="Ontario"))+
  #geom_point(size=1,stroke=0,aes(date,pmax(new_cases,0)/population,colour="State",shape="State"))+
  geom_line(size=.75,aes(date,ab_pop,colour="Alberta"))+
  geom_line(size=.75,aes(date,qc_pop,colour="Quebec"))+
  geom_line(size=.75,aes(date,on_pop,colour="Ontario"))+
  geom_line(size=.75,aes(date,new_7d/population,group=province_state,colour="State"))+
  
  
  facet_wrap(~province_state,nrow = 2)+
  blake_theme()+ theme(panel.spacing = unit(.5,"lines"))+
  theme(legend.position = "bottom")+
  #annotate("text", x = as.Date(Sys.Date()-days(7)), y=0, label = "Last 14 days",size=1,hjust=0.5,vjust=0)+
  scale_x_date(date_breaks = "3 months",date_labels="%b")+
  expand_limits(y = 0)+ 
  scale_colour_manual("",values = colors_tableau10())+
  
  scale_fill_manual("",values = c("grey70"))+
  #scale_shape_manual("",values = c(20,20,20,20,20))+
  #blank_shape
  scale_shape_manual("",values = c(16,16,16,16,16))+
  labs(y="New Confirmed COVID-19 Cases per Million Residents",x="",
       title="New COVID-19 Cases in US States Compared to Canada",
       subtitle=paste("7-day moving average new cases for the 10 largest US states by population compared to select Canadian provinces. Data current to ",format(Sys.Date(),"%b %d, %Y"),".",sep=""),
       caption="Source: Novel Coronavirus (COVID-19) cases provided by JHU CSSE (https://systems.jhu.edu/research/public-health/ncov/), graph by Andrew Leach\n
       Note: days with negative case growth (i.e. data revisions) are shown as zeros, but raw case counts are used for moving averages.")
ggsave("covid_us_top_pop.png",width = 16,height = 10)


ggplot(filter(top_data,date>Sys.Date()-months(3)-days(10))) +
  geom_rect(aes(xmin = Sys.Date()-days(14), xmax =Sys.Date(),ymin = -Inf, ymax = Inf,
                fill = "Last 14 days"),alpha = .8)+
  
  #geom_point(size=1,stroke=0,aes(date,ab_cases,colour="Alberta",shape="Alberta"))+
  #geom_point(size=1,stroke=0,aes(date,qc_cases,colour="Quebec",shape="Quebec"))+
  #geom_point(size=1,stroke=0,aes(date,on_cases,colour="Ontario",shape="Ontario"))+
  #geom_point(size=1,stroke=0,aes(date,pmax(new_cases,0)/population,colour="State",shape="State"))+
  geom_line(size=.75,aes(date,ab_pop,colour="Alberta"))+
  geom_line(size=.75,aes(date,qc_pop,colour="Quebec"))+
  geom_line(size=.75,aes(date,on_pop,colour="Ontario"))+
  geom_line(size=.75,aes(date,new_7d/population,group=province_state,colour="State"))+
  
  
  facet_wrap(~province_state,nrow = 2)+
  blake_theme()+ theme(panel.spacing = unit(.5,"lines"))+
  theme(legend.position = "bottom")+
  #annotate("text", x = as.Date(Sys.Date()-days(7)), y=0, label = "Last 14 days",size=1,hjust=0.5,vjust=0)+
  scale_x_date(date_breaks = "1 month",date_labels="%b")+
  expand_limits(y = 0)+ 
  scale_colour_manual("",values = colors_tableau10())+
  
  scale_fill_manual("",values = c("grey70"))+
  #scale_shape_manual("",values = c(20,20,20,20,20))+
  #blank_shape
  scale_shape_manual("",values = c(16,16,16,16,16))+
  labs(y="New Confirmed COVID-19 Cases per Million Residents",x="",
       title="New COVID-19 Cases in US States Compared to Canada",
       subtitle=paste("7-day moving average new cases for the 10 largest US states by population compared to select Canadian provinces. Data current to ",format(Sys.Date(),"%b %d, %Y"),".",sep=""),
       caption="Source: Novel Coronavirus (COVID-19) cases provided by JHU CSSE (https://systems.jhu.edu/research/public-health/ncov/), graph by Andrew Leach\n
       Note: days with negative case growth (i.e. data revisions) are shown as zeros, but raw case counts are used for moving averages.")
ggsave("covid_us_top_pop_short.png",width = 16,height = 10)



#active cases per capita

ggplot(filter(top_data,date>=ymd("2020-04-01"))) +
  geom_rect(aes(xmin = Sys.Date()-days(14), xmax =Sys.Date(),ymin = -Inf, ymax = Inf,
                fill = "Last 14 days"),alpha = .8)+
  geom_line(size=1.2,aes(date,active/population/10,group=province_state,colour="State"))+
  geom_line(size=1.2,aes(date,canada_active_pop/10,colour="Canada"))+
  #geom_point(size=1.5,aes(date,can_cases,colour="Canada",shape="Canada"))+
  geom_line(size=1.2,aes(date,ab_active_pop/10,colour="Alberta"))+
  #geom_point(size=1.5,aes(date,ab_cases,colour="Alberta",shape="Alberta"))+
  #geom_point(size=1.5,aes(date,pmax(new_cases,0)/population,colour="State",shape="State"))+
  facet_wrap(~province_state,nrow = 2)+
  blake_theme()+ theme(panel.spacing = unit(.5,"lines"))+
  theme(legend.position = "bottom")+
  #annotate("text", x = as.Date(Sys.Date()-days(7)), y=0, label = "Last 14 days",size=1,hjust=0.5,vjust=0)+
  scale_x_date(date_breaks = "1 month",date_labels="%b")+
  expand_limits(y = 0)+ 
  scale_colour_manual("",values = c("#0D3692","#ff0000","black"))+
  #scale_colour_viridis("",discrete = T,option = "B",direction = -1)+
  scale_fill_manual("",values = c("grey70"))+
  scale_shape_manual("",values = c(20,20,20))+
  labs(y="Approximate Active COVID-19 Cases per 100k Population",x="",
       title="New COVID-19 Cases in US States Compared to Canada",
       subtitle=paste("14-day sum of new cases per 100k population for the 10 largest US states by population. Data current to ",format(Sys.Date(),"%b %d, %Y"),".",sep=""),
       caption="Source: Novel Coronavirus (COVID-19) cases provided by JHU CSSE (https://systems.jhu.edu/research/public-health/ncov/), graph by Andrew Leach\n
       Note: Approximated active cases only.")
ggsave("covid_us_top_pop_active.png",width = 16,height = 10)




load("prov_pop.Rdata")
prov_pop<-prov_pop %>%mutate(Prov_name=gsub(" \\(4\\)","",Prov_name))

provs<-c("British Columbia","Alberta","Saskatchewan","Manitoba","Ontario",
         "Quebec","New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador",
         "Northwest Territories","Yukon","Nunavut")  

pop_proj<-read_csv("pop_proj.csv",col_types = cols(.default = "c")) %>%
  pivot_longer(-c(Year),names_to = "region",values_to = "pop")%>%
  mutate(prov=as.factor(region),pop=as.numeric(gsub(",","",pop))) %>% filter(Year==2020)


cda_data<-global_cases %>% filter(country_region=="Canada",province_state!="Repatriated Travellers")%>%
  left_join(pop_proj%>%clean_names(),by=c("province_state"="prov"))%>%
  mutate(province_state=factor(province_state,levels=provs),
         province_state=fct_other(province_state,drop=c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador"),other_level="Atlantic Provinces"),
         province_state=fct_other(province_state,drop=c("Northwest Territories","Yukon","Nunavut"),other_level="Territories"))%>%
  group_by(province_state,date) %>% summarize(cases=sum(cases),
                                              prov_pop=sum(pop*1000),
                                              )%>%
  group_by(province_state)%>%
  mutate(last_cases=last(cases),test=(last(cases)/cases[n()-3])^(1/3),
                 new_cases=cases-lag(cases),new_7d=roll_mean(new_cases,7))%>%
  mutate(test=last(new_7d/lag(new_7d,14))) %>% ungroup() %>%arrange(date)


ggplot(filter(cda_data,date>ymd("2020-04-01"))) +
  #geom_ribbon(aes(x = hour_start, ymin = min_vol, ymax = max_vol,fill="A"),alpha=.25, linetype = 0)+
  geom_rect(aes(xmin = Sys.Date()-days(14), xmax =Sys.Date(),ymin = -Inf, ymax = Inf,
                fill = "Last 14 days"),alpha = .8)+
  
  geom_line(size=1.2,aes(date,new_7d,group=province_state,colour="7 day moving average of new cases"))+
  #geom_point(size=1.5,aes(date,pmax(new_cases,0),group=province_state,shape="Daily new cases"),color=blakes_blue)+
  facet_wrap(~province_state,scales="fixed",nrow = 2)+
  blake_theme()+ theme(panel.spacing = unit(.5,"lines"))+
  theme(legend.position = "bottom")+
  #annotate("text", x = as.Date(Sys.Date()-days(7)), y=0, label = "Last 14 days",size=1,hjust=0.5,vjust=0)+
  scale_x_date(date_breaks = "1 month",date_labels="%d\n%b")+
  expand_limits(y = 0)+ 
  scale_colour_manual("",values = c("grey30"))+
  scale_fill_manual("",values = c("grey70"))+
  scale_shape_manual("",values = c(20))+
  labs(y="New Confirmed COVID-19 Cases (linear scale)",x="",
       title="New Canadian COVID-19 Cases",
       #subtitle=paste("States shown have more than a ",cutoff,"x increase in the 7 day moving average of reported new cases from ",format(Sys.Date()-days(14),"%b %d")," to ",format(Sys.Date(),"%b %d"),sep=""),
       caption="Source: Novel Coronavirus (COVID-19) cases provided by JHU CSSE (https://systems.jhu.edu/research/public-health/ncov/). Graph by Andrew Leach")
ggsave("can_covid.png",width = 16,height = 10)

ggplot(filter(cda_data,date>ymd("2020-04-01"))) +
  #geom_ribbon(aes(x = hour_start, ymin = min_vol, ymax = max_vol,fill="A"),alpha=.25, linetype = 0)+
  geom_rect(aes(xmin = Sys.Date()-days(14), xmax =Sys.Date(),ymin = -Inf, ymax = Inf,
                fill = "Last 14 days"),alpha = .8)+
  
  geom_line(size=1.2,aes(date,new_7d/prov_pop*10^6,group=province_state,colour="7 day moving average of new cases"))+
  #geom_point(size=1.5,aes(date,pmax(new_cases/prov_pop*10^6,0),group=province_state,shape="Daily new cases"),color=blakes_blue)+
  facet_wrap(~province_state,nrow = 2,scales = "fixed")+
  blake_theme()+ theme(panel.spacing = unit(.5,"lines"))+
  theme(legend.position = "bottom")+
  #annotate("text", x = as.Date(Sys.Date()-days(7)), y=0, label = "Last 14 days",size=1,hjust=0.5,vjust=0)+
  scale_x_date(date_breaks = "3 month",date_labels="%b %Y")+
  expand_limits(y = 0)+ 
  scale_colour_manual("",values = c("grey30"))+
  scale_fill_manual("",values = c("grey70"))+
  scale_shape_manual("",values = c(20))+
  labs(y="New Confirmed COVID-19 Cases per Million People",x="",
       title="New Canadian COVID-19 Cases Per Million People",
       #subtitle=paste("States shown have more than a ",cutoff,"x increase in the 7 day moving average of reported new cases from ",format(Sys.Date()-days(14),"%b %d")," to ",format(Sys.Date(),"%b %d"),sep=""),
       caption="Source: Novel Coronavirus (COVID-19) cases provided by JHU CSSE (https://systems.jhu.edu/research/public-health/ncov/), population data via StatCan, graph by Andrew Leach")
ggsave("can_covid_pop.png",width = 16,height = 10)

ggplot(filter(cda_data,date>max(date)-months(1))) +
  #geom_ribbon(aes(x = hour_start, ymin = min_vol, ymax = max_vol,fill="A"),alpha=.25, linetype = 0)+
  geom_rect(aes(xmin = Sys.Date()-days(14), xmax =Sys.Date(),ymin = -Inf, ymax = Inf,
                fill = "Last 14 days"),alpha = .8)+
  
  geom_line(size=1.2,aes(date,new_7d/prov_pop*10^6,group=province_state,colour="7 day moving average of new cases"))+
  #geom_point(size=1.5,aes(date,pmax(new_cases/prov_pop*10^6,0),group=province_state,shape="Daily new cases"),color=blakes_blue)+
  facet_wrap(~province_state,nrow = 2)+
  blake_theme()+ theme(panel.spacing = unit(.5,"lines"))+
  theme(legend.position = "bottom")+
  #annotate("text", x = as.Date(Sys.Date()-days(7)), y=0, label = "Last 14 days",size=1,hjust=0.5,vjust=0)+
  scale_x_date(date_breaks = "1 week",date_labels="%d\n%b")+
  expand_limits(y = 0)+ 
  scale_colour_manual("",values = c("grey30"))+
  scale_fill_manual("",values = c("grey70"))+
  scale_shape_manual("",values = c(20))+
  labs(y="New Confirmed COVID-19 Cases per Million People",x="",
       title="New Canadian COVID-19 Cases Per Million People",
       subtitle=paste("7-day moving average of reported new cases from ",format(Sys.Date()-months(1),"%B %d")," to ",format(Sys.Date(),"%B %d, %Y"),sep=""),
       caption="Source: Novel Coronavirus (COVID-19) cases provided by JHU CSSE (https://systems.jhu.edu/research/public-health/ncov/), population data via StatCan, graph by Andrew Leach")
ggsave("can_covid_pop_short.png",width = 16,height = 10,dpi=300)

ggplot(filter(cda_data,date>ymd("2020-04-01"))) +
  #geom_ribbon(aes(x = hour_start, ymin = min_vol, ymax = max_vol,fill="A"),alpha=.25, linetype = 0)+
  geom_rect(aes(xmin = Sys.Date()-days(14), xmax =Sys.Date(),ymin = -Inf, ymax = Inf,
                fill = "Last 14 days"),alpha = .8)+
  
  geom_line(size=1.2,aes(date,new_7d/prov_pop*10^6,group=province_state,colour="7 day moving average of new cases"))+
  #geom_point(size=1.5,aes(date,pmax(new_cases/prov_pop*10^6,0),group=province_state,shape="Daily new cases"),color=blakes_blue)+
  facet_wrap(~province_state,nrow = 2,scales = "fixed")+
  blake_theme()+ theme(panel.spacing = unit(.5,"lines"))+
  theme(legend.position = "bottom")+
  #annotate("text", x = as.Date(Sys.Date()-days(7)), y=0, label = "Last 14 days",size=1,hjust=0.5,vjust=0)+
  scale_x_date(date_breaks = "1 month",date_labels="%d\n%b")+
  expand_limits(y = 0)+ 
  scale_colour_manual("",values = c("grey30"))+
  scale_fill_manual("",values = c("grey70"))+
  scale_shape_manual("",values = c(20))+
  labs(y="New Confirmed COVID-19 Cases per Million People",x="",
       title="New Canadian COVID-19 Cases Per Million People",
       #subtitle=paste("States shown have more than a ",cutoff,"x increase in the 7 day moving average of reported new cases from ",format(Sys.Date()-days(14),"%b %d")," to ",format(Sys.Date(),"%b %d"),sep=""),
       caption="Source: Novel Coronavirus (COVID-19) cases provided by JHU CSSE (https://systems.jhu.edu/research/public-health/ncov/), population data via StatCan, graph by Andrew Leach")
ggsave("can_covid_pop.png",width = 16,height = 10)

ggplot(filter(cda_data,date>ymd("2021-02-01"),province_state %in% c("British Columbia","Alberta","Ontario","Quebec"))) +
  #geom_ribbon(aes(x = hour_start, ymin = min_vol, ymax = max_vol,fill="A"),alpha=.25, linetype = 0)+
  #geom_rect(aes(xmin = Sys.Date()-days(14), xmax =Sys.Date(),ymin = -Inf, ymax = Inf,
  #              fill = "Last 14 days"),alpha = .8)+
  
  geom_line(size=1.2,aes(date,new_7d/prov_pop*10^6,group=province_state,colour=province_state))+
  #geom_point(size=1.5,aes(date,pmax(new_cases/prov_pop*10^6,0),group=province_state,shape="Daily new cases"),color=blakes_blue)+
  #facet_wrap(~province_state,nrow = 2)+
  blake_theme()+ theme(panel.spacing = unit(.5,"lines"))+
  theme(legend.position = "bottom")+
  #annotate("text", x = as.Date(Sys.Date()-days(7)), y=0, label = "Last 14 days",size=1,hjust=0.5,vjust=0)+
  scale_x_date(date_breaks = "1 week",date_labels="%b %d")+
  expand_limits(y = 0)+ 
  scale_colour_manual("",values = colors_ua10())+
  scale_fill_manual("",values = c("grey70"))+
  scale_shape_manual("",values = c(20))+
  labs(y="Confirmed COVID-19 cases per million people",x="",
       title="Canadian COVID-19 cases per million people",
       subtitle=paste("7-day moving average of reported new cases from ",format(ymd("2021-02-01"),"%B %d")," to ",format(Sys.Date(),"%B %d, %Y"),sep=""),
       caption="Source: Novel Coronavirus (COVID-19) cases provided by JHU CSSE (https://systems.jhu.edu/research/public-health/ncov/), population data via StatCan, graph by Andrew Leach")
ggsave("provs_covid_pop_short.png",width = 16,height = 10)




ggplot(filter(cda_data,date>=ymd("2020-03-01"))) +
  #geom_ribbon(aes(x = hour_start, ymin = min_vol, ymax = max_vol,fill="A"),alpha=.25, linetype = 0)+
  geom_rect(aes(xmin = Sys.Date()-days(14), xmax =Sys.Date(),ymin = -Inf, ymax = Inf,
                fill = "Last 14 days"),alpha = .8)+
  
  geom_line(size=1.2,aes(date,new_7d,group=province_state,colour="7 day moving average of new cases"))+
  geom_point(size=1.5,aes(date,pmax(new_cases,0),group=province_state,shape="Daily new cases"),color=blakes_blue)+
  facet_wrap(~province_state,scales="free_y",nrow = 2)+
  blake_theme()+ theme(panel.spacing = unit(.5,"lines"))+
  theme(legend.position = "bottom")+
  #annotate("text", x = as.Date(Sys.Date()-days(7)), y=0, label = "Last 14 days",size=1,hjust=0.5,vjust=0)+
  scale_x_date(date_breaks = "1 month",date_labels="%d\n%b")+
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
  scale_x_date(date_breaks = "1 month",date_labels="%d\n%b")+
  expand_limits(y = 0)+ 
  scale_colour_manual("",values = c("grey30"))+
  scale_fill_manual("",values = c("grey70"))+
  scale_shape_manual("",values = c(20))+
  labs(y="New Confirmed COVID-19 Cases (linear scale)",x="",
       title="Alberta's COVID-19 Cases",
       #subtitle=paste("States shown have more than a ",cutoff,"x increase in the 7 day moving average of reported new cases from ",format(Sys.Date()-days(14),"%b %d")," to ",format(Sys.Date(),"%b %d"),sep=""),
       caption="Source: Novel Coronavirus (COVID-19) cases provided by JHU CSSE (https://systems.jhu.edu/research/public-health/ncov/). Graph by Andrew Leach")
ggsave("ab_covid.png",width = 16,height = 10)





ggplot(filter(cda_data,date>ymd("2020-04-01"),province_state=="Alberta")%>%
       mutate(change_14d=(new_7d/lag(new_7d,14))^(1/14)-1))+
  #geom_ribbon(aes(x = hour_start, ymin = min_vol, ymax = max_vol,fill="A"),alpha=.25, linetype = 0)+
  geom_rect(aes(xmin = Sys.Date()-days(14), xmax =Sys.Date(),ymin = -Inf, ymax = Inf,
                fill = "Last 14 days"),alpha = .8)+
  #geom_line(size=1.2,aes(date,new_7d,group=province_state,colour="7 day moving average of new cases"))+
  geom_line(size=1.2,aes(date,change_14d,group=province_state,colour="7 day moving average of new cases"))+
  #geom_point(size=1.5,aes(date,pmax(new_cases,0),group=province_state,shape="Daily new cases"),color=blakes_blue)+
  #facet_wrap(~province_state,scales="free_y",nrow = 2)+
  blake_theme()+ theme(panel.spacing = unit(.5,"lines"))+
  theme(legend.position = "bottom")+
  #annotate("text", x = as.Date(Sys.Date()-days(7)), y=0, label = "Last 14 days",size=1,hjust=0.5,vjust=0)+
  scale_x_date(date_breaks = "1 month",date_labels="%d\n%b")+
  scale_y_continuous(labels = scales::percent)+
  #expand_limits(y = 0)+ 
  scale_colour_manual("",values = c("grey30"))+
  scale_fill_manual("",values = c("grey70"))+
  scale_shape_manual("",values = c(20))+
  labs(y="New Confirmed COVID-19 Cases (linear scale)",x="",
       title="Alberta's COVID-19 Cases",
       #subtitle=paste("States shown have more than a ",cutoff,"x increase in the 7 day moving average of reported new cases from ",format(Sys.Date()-days(14),"%b %d")," to ",format(Sys.Date(),"%b %d"),sep=""),
       caption="Source: Novel Coronavirus (COVID-19) cases provided by JHU CSSE (https://systems.jhu.edu/research/public-health/ncov/). Graph by Andrew Leach")
ggsave("ab_covid.png",width = 16,height = 10)




#extract AB vs Texas








graph_data<-global_cases %>% filter(country_region=="US")%>%
  group_by(province_state) %>% mutate(last_cases=last(cases),test=(last(cases)/cases[n()-3])^(1/3),
                                      new_cases=cases-lag(cases),new_7d=roll_mean(new_cases,7))%>%
  mutate(test=last(new_7d/lag(new_7d,14))) %>%left_join(us_pop,by=c("province_state"="state"))%>%
  mutate(population=population/10^6) %>% bind_rows(cda_data %>% mutate(population=prov_pop/10^6) %>% select (-prov_pop))%>%
  filter(province_state %in% c("Alberta"))


ab_covid<-ggplot(filter(graph_data,date>ymd("2020-04-01"))) +
  #geom_ribbon(aes(x = hour_start, ymin = min_vol, ymax = max_vol,fill="A"),alpha=.25, linetype = 0)+
  geom_rect(aes(xmin = Sys.Date()-days(14), xmax =Sys.Date(),ymin = -Inf, ymax = Inf,
                fill = "Last 14 days"),alpha = .8)+
  
  geom_line(size=1.2,aes(date,new_7d/population,group=province_state,colour="7 day moving average of new cases"))+
  geom_point(size=1.5,aes(date,pmax(new_cases/population,0),group=province_state,shape="Daily new cases"),color=blakes_blue)+
  #facet_wrap(~province_state,scales="free_y",nrow = 2)+
  blake_theme()+ theme(panel.spacing = unit(.5,"lines"))+
  theme(legend.position = "bottom")+
  #annotate("text", x = as.Date(Sys.Date()-days(7)), y=0, label = "Last 14 days",size=1,hjust=0.5,vjust=0)+
  scale_x_date(date_breaks = "1 month",date_labels="%d\n%b")+
  expand_limits(y = 0)+ 
  scale_colour_manual("",values = c("grey30"))+
  scale_fill_manual("",values = c("grey70"))+
  scale_shape_manual("",values = c(20))+
  labs(y="New COVID-19 Cases (rate per million people)",x="",
       title="Alberta's COVID-19 Cases",
       #subtitle=paste("States shown have more than a ",cutoff,"x increase in the 7 day moving average of reported new cases from ",format(Sys.Date()-days(14),"%b %d")," to ",format(Sys.Date(),"%b %d"),sep=""),
       caption="Source: Novel Coronavirus (COVID-19) cases provided by JHU CSSE (https://systems.jhu.edu/research/public-health/ncov/). Graph by Andrew Leach")

ab_covid
ggsave("ab_covid.png",width = 16,height = 10)

library(cowplot)
logo_file <- system.file("extdata", "bateson_evidence.png", package = "cowplot")
my_plot_2 <- ggdraw() +
  draw_image(logo_file,  x = ymd("2020-10-31"), y =300, scale = .2) +
  draw_plot(ab_covid)




ggplot(filter(cda_data,date>ymd("2020-03-01"))%>%group_by(date)%>%summarize(new_cases=sum(new_cases,na.rm = T),province_state="Canada")%>%
         mutate(new_7d=roll_mean(new_cases,7))) +
  #geom_ribbon(aes(x = hour_start, ymin = min_vol, ymax = max_vol,fill="A"),alpha=.25, linetype = 0)+
  #geom_rect(aes(xmin = Sys.Date()-days(14), xmax =Sys.Date(),ymin = -Inf, ymax = Inf,
  #              fill = "Last 14 days"),alpha = .8)+
  #
  geom_line(size=1.2,aes(date,new_7d,group=province_state,colour="7 day moving average of new cases"))+
  #geom_point(size=1.5,aes(date,pmax(new_cases,0),group=province_state,shape="Daily new cases"),color=blakes_blue)+
  #facet_wrap(~province_state,scales="free_y",nrow = 2)+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 10, face = "italic",hjust=0),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", hjust=0.5,vjust=0.5),
    strip.text.x = element_text(size = 12, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  #annotate("text", x = as.Date(Sys.Date()-days(7)), y=0, label = "Last 14 days",size=1,hjust=0.5,vjust=0)+
  scale_x_date(date_breaks = "1 month",date_labels="%b\n%Y")+
  expand_limits(y = 0)+ 
  scale_colour_manual("",values = c("grey30"))+
  scale_fill_manual("",values = c("grey70"))+
  scale_shape_manual("",values = c(20))+
  labs(y="New Confirmed COVID-19 Cases (linear scale)",x="",
       #title="New Canadian COVID-19 Cases",
       #subtitle=paste("States shown have more than a ",cutoff,"x increase in the 7 day moving average of reported new cases from ",format(Sys.Date()-days(14),"%b %d")," to ",format(Sys.Date(),"%b %d"),sep=""),
       #caption="Source: Novel Coronavirus (COVID-19) cases provided by JHU CSSE (https://systems.jhu.edu/research/public-health/ncov/). Graph by Andrew Leach")
       NULL)
ggsave("can_natl_covid.png",width = 14,height = 7,dpi=300)




#US AIR TRAVEL


tsa_dat<-read_csv("tsa_csv.csv")%>% select(c(1,2,3,4)) %>% filter(!is.na(Date)) %>% pivot_longer(-Date,names_to = "Year",values_to = "travelers"
                                                                          )%>%
  mutate(Date=mdy(Date),
         Year=gsub(" Traveler Thoughput","",Year),
         travelers=as.numeric(travelers),
         date=(ymd(paste(2000,month(Date),day(Date),sep="-"))))%>% arrange(date)
  

ggplot(tsa_dat)+
  geom_line(aes(date,travelers/10^6,group=Year,colour=Year),size=1.)+
theme_minimal()+theme(
  legend.position = "bottom",
  legend.margin=margin(c(.05,0,.05,0),unit="cm"),
  legend.text = element_text(colour="black", size = 12),
  plot.caption = element_text(size = 10, face = "italic",hjust=0),
  plot.title = element_text(size=16,face = "bold"),
  plot.subtitle = element_text(size = 10),
  panel.grid.minor = element_blank(),
  text = element_text(size = 20,face = "bold"),
  axis.text.y = element_text(size = 12,face = "bold", colour="black"),
  #axis.text.x = element_blank(),
  axis.text.x = element_text(size = 10, colour = "black", hjust=0.5,vjust=0.5),
  strip.text.x = element_text(size = 12, colour = "black", angle = 0),
  axis.title.y = element_text(size = 14,face = "bold", colour="black"),
)+
  #annotate("text", x = as.Date(Sys.Date()-days(7)), y=0, label = "Last 14 days",size=1,hjust=0.5,vjust=0)+
  scale_x_date(date_breaks = "1 month",date_labels="%b",expand = c(0,0))+
  expand_limits(x=ymd("1999-12-20"))+
  
  expand_limits(y = 0)+ 
  scale_colour_manual("",values=colors_ua10())+
  labs(y="TSA checkpoint travel numbers (millions)",x="",
       title="TSA checkpoint travel numbers during COVID",
       #subtitle=paste("States shown have more than a ",cutoff,"x increase in the 7 day moving average of reported new cases from ",format(Sys.Date()-days(14),"%b %d")," to ",format(Sys.Date(),"%b %d"),sep=""),
       caption="Source: TSA COVID-19 Information (https://www.tsa.gov/coronavirus/passenger-throughput). Graph by Andrew Leach.",
       NULL)
ggsave("tsa_data.png",width=16,height=9, dpi=250)



#canadian data
#https://www.catsa-acsta.gc.ca/en/screened-passenger-data

catsa_data<-read_excel("catsa_data.xlsx",skip=3)
names(catsa_data)<-c("date","top8_2019","top8_2020","top8_2021","top15_2019","top15_2020","top15_2021")
catsa_data<-catsa_data %>% pivot_longer(-date)%>%separate(name,into=c("airports","year"),sep = "_")%>%
  mutate(date=as_date(date),airports=as_factor(airports),airports=fct_recode(airports,"8 Largest Canadian Airports"="top8","15 Largest Canadian Airports"="top15"),year=as_factor(year))

ggplot(catsa_data)+
  geom_line(aes(date,value/1000,group=year,colour=year),size=1.)+
  facet_wrap(~airports,nrow = 2)+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 10, face = "italic",hjust=0.5),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", hjust=0.5,vjust=0.5),
    strip.text.x = element_text(size = 12, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  #annotate("text", x = as.Date(Sys.Date()-days(7)), y=0, label = "Last 14 days",size=1,hjust=0.5,vjust=0)+
  scale_x_date(date_breaks = "1 month",date_labels="%b",expand = c(0,0))+
  #expand_limits(x=ymd("1999-12-20"))+
  
  expand_limits(y = 0)+ 
  scale_colour_manual("",values=colors_ua10())+
  labs(y="CATSA screened passengers numbers (thousands)",x="",
       title="CATSA passenger screening during COVID",
       #subtitle=paste("States shown have more than a ",cutoff,"x increase in the 7 day moving average of reported new cases from ",format(Sys.Date()-days(14),"%b %d")," to ",format(Sys.Date(),"%b %d"),sep=""),
       caption="Source: CATSA COVID-19 Information (https://www.catsa-acsta.gc.ca/en/screened-passenger-data). Graph by Andrew Leach. Thanks to Alicia Planincic for the data link.\n
       Eight largest airports include YYZ, YOW, YYC, YUL, YHZ, YWG, YEG, YVR. Fifteen largest adds YLW, YQB, YQR, YTZ, YXE, YYJ, and YYT.",
       NULL)
ggsave("catsa_data.png",width=16,height=9, dpi=300)


#google mobility reports


download.file("https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip",destfile = "google_regions.zip",mode="wb")
#canada file is 2020_CA_Region_Mobility_Report.csv
google_canada<-read_csv(unz("google_regions.zip", "2020_CA_Region_Mobility_Report.csv")) 


alberta_google<-google_canada %>% filter(sub_region_1=="Alberta") %>%
  mutate(sub_region_2=ifelse(is.na(sub_region_2),sub_region_1,sub_region_2))%>%
  select(-country_region_code,-country_region,-sub_region_1,-metro_area,-census_fips_code,-iso_3166_2_code,-place_id)%>%
  rename(region=sub_region_2)%>% pivot_longer(-c(date,region),names_to = "activity",values_to = "pct_from_base")%>%
  mutate(activity=gsub("_percent_change_from_baseline","",activity),
         activity=gsub("_"," ",activity),
         activity=str_to_title(activity))%>%
  separate(region,sep=" - ",into = c("district","region"))%>%
  mutate(region=ifelse(is.na(region),district,region))%>%
  select(-district)%>%
         mutate(region=as_factor(region),
         region=fct_other(region,keep = c("Alberta" ,"Edmonton","Calgary","Fort McMurray","Lethbridge") )
         )%>%
  group_by(date,region,activity) %>% summarize(pct_from_base=mean(pct_from_base,na.rm=T))


ggplot(alberta_google)+
  geom_line(aes(date,pct_from_base))+
  facet_grid(rows=vars(region),cols=vars(activity))





