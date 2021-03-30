
library(tidyverse)
library(readxl)
library(ggplot2)
library(lubridate)
library(zoo)
library(dplyr)
library(scales)

#set working directory
setwd("G:/YOUR DIRECTORY/CPI")


df<-read_excel("data.xlsx", sheet = "prices")
class(df)
tail(df)
str(df)

df<- df%>%mutate(date = mdy(date))
df2<-df%>%select(date,cpi_mfe,cpi)


df3<-df2%>%gather(measure, value, cpi_mfe,cpi,convert = FALSE)
df3<-df3 %>% rename(Prices=measure)
df4<-df3 %>% mutate(Prices = ifelse(Prices == "cpi", "CPI", "*CORE CPI"))


a<-ggplot(df4,aes(x=date, y=value, group=Prices))+
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-02-01"), ymin = -Inf, ymax = 4, fill = "gray", alpha = 0.2)+
  geom_line(aes(linetype=Prices, color=Prices))+
    geom_hline(yintercept=2.0, linetype="dashed", 
             color = "blue", size=.5)+
    scale_linetype_manual(values=c("solid", "twodash"))+
  scale_color_manual(values=c('red','black'))+theme_bw()+
  scale_x_date(date_breaks = "5 months",limits=c(as.Date("2019-01-01"),as.Date("2021-02-01")),labels = date_format("%b/%Y"),expand = c(0, 0))+
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 1),expand = c(0, 0),labels = function(value) paste0(value, "%"))+
  labs(title = "Consumer Price Index", subtitle="12-month Percent Change",
       caption = "*CORE Price Index excluding food and energy commodities\nSource: Bureau Of Labor Statistics")+xlab("")+ylab("")+
    theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(hjust = 0,size = 9),
        legend.text=element_text(size=8),
        axis.text.x = element_text(vjust = -2))+
   annotate("text", x = as.Date("2020-05-01"), y = 3,colour = "black", label = "COVID-19 Recession",fontface = 'italic',size = 3.1)
  


#using Price Index % of PCE

dfpce<-df%>%select(date,ppce_mfe,ppce)

dfpce2<-dfpce%>%gather(measure, value, ppce_mfe,ppce,convert = FALSE)
dfpce2<-dfpce2 %>% rename(Prices=measure)
dfpce3<-dfpce2 %>% mutate(Prices = ifelse(Prices == "ppce", "PCE Price", "*CORE PCE Price"))

b<-ggplot(dfpce3,aes(x=date, y=value, group=Prices))+
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-02-01"), ymin = -Inf, ymax = 4, fill = "gray", alpha = 0.2)+
  geom_line(aes(linetype=Prices, color=Prices))+
  geom_hline(yintercept=2.0, linetype="dashed", 
             color = "blue", size=.5)+
  annotate("text", x = as.Date("2020-09-01"), y = 2.6,colour = "blue", label = "Fed Inflation Target Rate",fontface = 'italic', size=2)+
  annotate("segment", x = as.Date("2020-09-01"), xend = as.Date("2020-09-01"), y = 2.05, yend = 2.4,colour = "blue")+
  scale_linetype_manual(values=c("solid", "twodash"))+
  scale_color_manual(values=c('red','black'))+theme_bw()+
  scale_x_date(date_breaks = "5 months",limits=c(as.Date("2019-01-01"),as.Date("2021-02-01")),labels = date_format("%b/%Y"),expand = c(0, 0))+
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 1),expand = c(0, 0),labels = function(value) paste0(value, "%"))+
  labs(title = "Personal Consumption Expenditures-Price Index", subtitle="12-month Percent Change",
       caption = "\nSource: Bureau Of Economic Analysis")+xlab("")+ylab("")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.subtitle = element_text(size = 7),
        plot.caption = element_text(hjust = 0,size = 9),
        legend.text=element_text(size=8),
        axis.text.x = element_text(vjust = -2))+
  annotate("text", x = as.Date("2020-06-01"), y = 3,colour = "black", label = "COVID-19 Recession",fontface = 'italic',size = 2.8)


p<-plot_grid(a, b)

title <- ggdraw() + draw_label("Inflationary Fears.. Are concerns justified?", fontface='bold')

plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
ggsave("AmazingPlots.png")

 
