if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/")
print(getwd())
source("../andrew_base.R")

library(tidyverse)
library(lubridate)
library(readxl)
library(reshape2)
library(scales)
library(viridis)


set_png<-function(file_sent,width=1400,height=750,res=130){
  #MAC
  if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
    png(file=file_sent, width = width, height = height,res=res)
  #PC
  if(R.version$platform ==  "x86_64-w64-mingw32")
    png(file=file_sent, width = width, height = height,res=res,type='cairo')
}




open_table_data<-read_csv("YoY_Seated_Diner_Data.csv")%>%
  pivot_longer(-c(Type,Name),names_to="date",values_to="yoy_change")%>%
  mutate(date=ymd(paste("2020",date,sep="/"))) %>%clean_names()
  
can_table_data <- open_table_data %>% filter(name %in% c("Calgary","Edmonton"))
  




png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("seating.png",width=1600,height=900)
ggplot(can_table_data)+
  geom_line(aes(date,yoy_change,group=name,colour=name),size=1.25)+
  geom_point(aes(date,yoy_change*ifelse(wday(date)==1,1,NA),group=name,colour=name,shape=name),size=2.5)+
  scale_x_date(breaks = "1 months",date_labels = "%d\n%b")+
  scale_shape_manual("",values=c(15,16,17,18,0,1,2))+
  scale_size_manual("",values=c(0,rep(2.5,6)))+
  scale_y_continuous(breaks=pretty_breaks())+
  #scale_linetype_manual("",values=c(1,1))+
  scale_color_viridis("",discrete = T,option="B",direction = -1,end = .9)+
  scale_fill_viridis("",discrete = T,option="B",direction = -1,end=.9)+
  scale_linetype_manual("",values=c(1,2),labels=c("Historical Data","Forecast"))+
  #scale_fill_manual("",values=colors_tableau10()[2])+
  #ajl_line()+
  blake_theme()+
  guides(shape = guide_legend(keywidth = unit(1.6,"cm"),nrow = 1),
         linetype = guide_legend(keywidth = unit(1.6,"cm"),nrow = 1),
         colour = guide_legend(keywidth = unit(1.6,"cm"),nrow = 1),
         fill = guide_legend(keywidth = unit(1.6,"cm"),nrow = 1))+
  labs(y="Year-over-year change in seating (%)",x="",
       title=paste("Restaurant Seating in Calgary and Edmonton during COVID"),
       subtitle=paste("OpenTable year-over-year reported changes"),
       caption="Source: Data via Open Table, graph by Andrew Leach.")+
  annotate("text", x = as.Date("2020-03-27")+days(26), y =-15.5, label = str_wrap(width = 70,"Alberta restaurants and other non-essential businesses ordered closed March 27, dine-in service resumed at 50% capacity on May 14. Calgary restaurants were allowed to re-open May 25th, also at 50% capacity."),size=4.5,hjust=0.5)+
  annotate("text", x = as.Date("2020-03-27")+days(24), y =-85, label = str_wrap(width = 30,"Edmonton mandated closure"),size=4.5,hjust=0.5)+
  annotate("text", x = as.Date("2020-03-27")+days(30), y =-95, label = str_wrap(width = 30,"Calgary mandated closure"),size=4.5,hjust=0.5)+
  annotate("rect", fill = viridis_pal(1,option="B",direction = -1,end = .9)(2)[[2]], alpha = .5, 
           xmin = as.Date("2020-03-27"), xmax =as.Date("2020-05-13"),
           ymin = -90, ymax = -80)+
  annotate("rect", fill = viridis_pal(1,option="B",direction = -1,end = .9)(2)[[1]], alpha = .5, 
           xmin = as.Date("2020-03-27"), xmax =as.Date("2020-05-24"),
           ymin = -100, ymax = -90)
  
  
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()