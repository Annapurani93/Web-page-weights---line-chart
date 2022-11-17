library(tidytuesdayR)
library(tidyverse)
install.packages("lubridate")
library(lubridate)
image_alt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/image_alt.csv')
color_contrast <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/color_contrast.csv')
ally_scores <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/ally_scores.csv')
bytes_total <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/bytes_total.csv')
speed_index <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/speed_index.csv')

bytes_total %>% 
  mutate(Date = ymd(date))%>%
  data.frame()->bytes

glimpse(bytes)

bytes%>%
  select(-c(measure,date))%>%
  group_by(Date,client)%>%
  select(c(Date,client,p10,p25,p50,p75,p90))%>%
  rowwise()%>%
  mutate(Median=median(c(p10,p25,p50,p75,p90),na.rm = TRUE))->bytesf

bytesf$client[bytesf$client =="mobile"] <- "Mobile"
bytesf$client[bytesf$client =="desktop"] <- "Desktop"


bytesf%>%
  select(c(Date,client,Median))%>%
  ggplot(aes(Date,Median))+
  geom_line(aes(group=client,colour=client),size=1)+
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits=c(0,2600),breaks=c(0,200,400,600,800,1000,1200,1400,1600,1800,2000,2200,2400,2600))+
  scale_colour_manual(values=c("#87CEEB","#0047AB"))+
  scale_x_date(date_labels = "%Y", limit=c(as.Date("2010-11-15"),as.Date("2022-10-01")),date_breaks = "1 year")+
  labs(colour=" ")+
  theme(plot.margin=unit(c(0.5,1.5,0.5,1.5),"cm"),
        plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill="white"),
        axis.text = element_text(colour="gray30",size=10),
        axis.title =element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(color = "gray",
                                                size = 0.1,
                                                linetype = 1),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title=element_text(size=14, face="bold",colour="black",margin=margin(b=15)),
        plot.subtitle = element_text(size=12, colour="black",margin=margin(b=25)),
        plot.caption=element_text(size=10,colour="black",hjust=0,margin=margin(t=40)))+
  labs(title="WEB PAGES ARE GETTING LARGER",
       subtitle=str_wrap("The weight of web pages has been increasing steadily over the years, and this has led to data connections and devices being used more heavily now to surf the web than a decade ago. The below chart looks at the median page weight on mobiles and desktops over the years.",120),
       caption = str_wrap("Source: httparchive.org by way of Data is Plural for Tidy Tuesday. As seen in: “Why web pages can have a size problem” (Datawrapper)   Analysis and design: @annapurani93",70))+
  annotate(geom="text", x=as.Date("2014-01-01"), y=1300, 
           label="Desktop",color="#87CEEB",fontface="bold")+
  annotate(geom="text", x=as.Date("2016-03-15"), y=750, 
           label="Mobile",color="#0047AB",fontface="bold")->plot

ggsave("webpagesizes.png",plot,width=10,height=7.14)    
  

