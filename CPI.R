
library(tidyverse)
library(readxl)
library(ggplot2)
library(lubridate)
library(zoo)
library(dplyr)
library(scales)
setwd("G:/DRIVE/LOOKING WORK/LinkedIn Posts/CPI")


df<-read_excel("cpi.xlsx", sheet = "data")
class(df)
head(df)
str(df)
cpi <- ts(df$cpi, frequency = 12, start = c(2015, 01))
   
tidy_data <- data.frame(
  date = seq(as.Date("2015-01-01"), as.Date("2021-01-01"), by = "month"),
  cpi = cpi
)

tidy_data
tidy_data2<-tidy_data%>%mutate(date2=as.Date(date))

str(tidy_data)
head(tidy_data)

ggplot(tidy_data)+
  geom_line(mapping = aes_string(x = "date", y = "cpi")) +
  geom_rect(
    fill = "red", alpha = 0.5, 
    mapping = aes_string(x = "date", y = "cpi"), 
    xmin = decimal_date(as.Date(c("2020-03-01"))),
    xmax = decimal_date(as.Date(c("2021-01-01"))),
    ymin = 0,
    ymax = 2)


ggplot() +
  geom_rect(data = data.frame(xmin = as.Date(c("2020-03-01")),
                              xmax =as.Date(c("2021-01-01")),
                              ymin = -Inf,
                              ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.5) +
  geom_line(data = tidy_data, aes(date, cpi), color="blue",linetype="longdash") +
  scale_x_date(limits = as.Date(c("2015-01-01","2021-01-01")))+
  theme_bw()


df<-read_excel("data.xlsx", sheet = "prices")
class(df)
tail(df)
str(df)

df<- df%>%mutate(date = mdy(date))
df2<-df%>%select(date,cpi_mfe,cpi)


df3<-df2%>%gather(measure, value, cpi_mfe,cpi,convert = FALSE)
df3<-df3 %>% rename(Prices=measure)
df4<-df3 %>% mutate(Prices = ifelse(Prices == "cpi", "CPI", "*CORE CPI"))
#min <- as.Date("2017-1-1")
#max <- max(df$date)



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
ggsave("Twopricess.png")

#extra code below



geom_line(aes(y = mfp), color = "red", size=1)+

ggplot(df, aes(x=date))+
  geom_rect(xmin=	 as.Date(c("2020-03-01")), 
            xmax=as.Date(c("2021-01-01")), 
            ymin=0, ymax=2, fill="grey90", alpha=0.3, col="grey90") +
  geom_line(aes(y=cpi), color="blue",linetype="longdash")+
theme_bw()+theme(
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),plot.caption = element_text(hjust = 0,size = 9),
)

  geom_line(aes(y = mfp), color = "red", size=1) + 
  geom_line(aes(y = combine_input), color="darkblue", linetype="dashed")+
  scale_x_continuous(limits = c(1987,2019),breaks = seq(1987, 2019, by = 4),expand = c(0, .5))+
  scale_y_continuous(limits = c(40, 120), breaks = seq(40, 120, by = 10),expand = c(0, 0))+
  theme_bw()+theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),plot.caption = element_text(hjust = 0,size = 9),
  )+
  labs(title = "Private Business Sector", subtitle="Index (2012=100)",
       caption = "\nSource: Bureau Of Labor Statistics") +
  annotate("text", x = 1991, y = 90,colour = "red", label = "Multifactor Productivity",fontface = 'italic',size = 2.7)+
  annotate("text", x = 2002, y = 65,colour = "darkblue", label = "Real Value-Added Output",fontface = 'italic',size = 2.7)+
  annotate("text", x = 1990.5, y = 75,colour = "blue", label = "Combined Inputs\n(Capital and Labor)",fontface = 'italic',size = 2.5)+
  annotate("text", x = 2007.5, y = 85,colour = "black", label = "Great Recession",fontface = 'italic',size = 2.1)+
  xlab("")+ylab("")







install.packages("devtools")
install.packages("usethis")
install.packages("fs")
library(devtools)
devtools::install_github('bbc/bbplot')
library(bbplot)
#Starting with R 4.0.0 (released April 2020), R for Windows uses a brand new toolchain bundle called rtools40
install.packages("Rtools")
remove.packages("Rtools")

install.packages("extrafont")
library(extrafont)
font_import() 
y
#only do this one time - it takes a while
loadfonts(device = "win")

p <- ggplot(tidy_data, aes(x=date, y=cpi)) +
  geom_line(color="red", size=1.1) +
  geom_hline(yintercept = 230, size = 1, colour="#333333")+
  theme_minimal() +bbc_style()+
  labs(x = " ", y = "Index (1987=100)") +scale_y_continuous(breaks = seq(230, 350, by = 10))+
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
         axis.title.x=element_blank(),plot.caption = element_text(hjust = 0,size = 9),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 12))+
  labs(title = "US Consumer Price Index", subtitle="January 2015 Until January 2021",
       caption = "\nSource: Bureau Of Labor Statistics")


  ggtitle("US Consumer Price Index", subtitle = "From January 2015 Until January 2021")




p
 ggsave("CPI.png")
warnings()
 