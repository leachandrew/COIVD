library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)
library(scales)




get_data<-function(){
#load counter data
  download.file("https://data.edmonton.ca/api/views/tq23-qn4m/rows.csv?accessType=DOWNLOAD",mode = "wb",destfile = "bikes.csv")
  }

#

read_data<-function(){
  read_csv(file="https://data.edmonton.ca/api/views/tq23-qn4m/rows.csv?accessType=DOWNLOAD") %>% clean_names()
  #read_csv(file="Bike_Counts__Eco_Counter_.csv") %>% clean_names()
}

bike_read<-read_data()
loc_obs<-bike_read%>%
        mutate(time=mdy_hms(log_timstamp),
         hour=hour(time),
         month=factor(month.abb[month(time)],levels = month.abb),
         year=factor(year(time)))%>%
         group_by(counter_location_description,month,year) %>% 
  summarize(obs=n(),cyclists=mean(cyclist_count_ebd+cyclist_count_wbd+
                                              cyclist_count_nbd+cyclist_count_sbd,na.rm = T))

  
bike_data<-bike_read %>% filter(counter_location_description %in% c("High Level Bridge East","100 Avenue E of 107 Street"))
graph_data<-bike_data %>% 
              mutate(time=mdy_hms(log_timstamp),
                                hour=hour(time),
                                month=factor(month.abb[month(time)],levels = month.abb),
                                day=day(time),
                                year=factor(year(time)))%>%
  filter(month %in% month.abb[seq(1,6)])%>%
  group_by(counter_location_description,hour,year,month,day) %>% summarize(cyclists=sum(cyclist_count_ebd+cyclist_count_wbd+
                                                           cyclist_count_nbd+cyclist_count_sbd,na.rm = T))%>%
  group_by(counter_location_description,hour,month,year) %>% summarize(cyclists=mean(cyclists))
  


ggplot(filter(graph_data)) +
  #geom_ribbon(aes(x = hour_start, ymin = min_vol, ymax = max_vol,fill="A"),alpha=.25, linetype = 0)+
  geom_line(size=1,aes(hour,cyclists,color=year,group=year))+
  facet_grid(vars(counter_location_description),vars(month),scales = "free_y")+
  scale_x_continuous(breaks=seq(0,23,3))+
  scale_y_continuous(breaks=pretty_breaks())+
  scale_colour_manual("",values = c("grey30","dodgerblue"))+
  #theme_minimal()+
  theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 12,face = "bold"),
    axis.text = element_text(size = 12,face = "bold", colour="black")
  )+    labs(y="Average Hourly Cyclist Transits",x="Hour",
             titl22e="Bike Counter Data by Month",
             #subtitle=paste("Monthly mean and range of hourly generation from ",start_date," to ",end_date,sep=""),
             caption="Source: Counts via data.edmonton.ca\nGraph by Andrew Leach")
ggsave("covid_bikes.png",width = 16,height = 10)
