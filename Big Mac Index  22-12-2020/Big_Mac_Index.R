#Libraries
library(tidyverse)
library(ggplot2)
library(tidytuesdayR)
library(funModeling)
library(lubridate)
library(extrafont)
library(Cairo)

#Import data
tuesdata <- tidytuesdayR::tt_load(2020, week = 52)
big_mac <- tuesdata$`big-mac`

#explore data
df_status(big_mac)
describe(big_mac)
freq(big_mac)

#Prepare data 
big_mac = big_mac %>% 
  rename(country = name) %>%
  add_count(country,name = "country_total") %>% 
  mutate(percent_usd_raw = round(usd_raw * 100,1)) %>%
  mutate(year = year(date)) %>%
  filter(country_total == max(country_total)) 

big_mac = big_mac %>% 
  mutate(Under_Over = if_else(percent_usd_raw >= 0,"Positive","Negative"))

big_mac %>% view()
attach(big_mac)

#Plotting 

sub_text = paste(
          " ",
          "The big mac index was invented by The Economist in 1986 as a lighthearted guide to whether currencies are at their “correct",
          "level. It is based on the theory of purchasing-power parity (PPP),the notion that in the long run exchange rates should move", 
          "towards the rate that would equalise the prices of an identical basket of goods and services (in this case, a burger) in",
          "any two countries.",
          "This graphic show local currency under(-)/over(+) valuation against the US dollar,%.",
           " ",
           sep = '\n'
           )

chart = big_mac %>% 
filter(country != 'United States') %>%
ggplot(aes(x= as_datetime(date) ,y= percent_usd_raw)) + 
geom_bar(data = filter(big_mac,percent_usd_raw > 0),fill='#3ebcd2',stat = "identity") +
geom_bar(data = filter(big_mac,percent_usd_raw < 0),fill='#e64e53', stat = "identity") + 
facet_wrap(~country,ncol = 5) + 
labs(x=" ", 
     y="Percent of value(%)",
     title= "Big Mac Index",
     subtitle = sub_text,
     caption = "Source: The Economist   |   Visualization: @joangeljimenez" ) +
theme_minimal() + 
theme(
  axis.title.y = element_text(family = "Verdana",size = 8),
  axis.text.x = element_text(angle = 90,size = 7,family = "Verdana"),
  axis.text.y = element_text(size = 7,family = "Verdana"),
  plot.margin = margin(3,2.5,0.5,2.5,unit = "lines"),
  plot.background = element_rect(fill = "#FAFAFA", color = NA),
  plot.title = element_text(family = "Verdana",size = 17,margin = margin(0,0,4,5),face = "bold"),
  plot.subtitle = element_text(family = "Verdana",size = 10,margin = margin(0,0,10,0)),
  plot.caption = element_text(family = "Verdana", size = 10, color = '#9b9b9b'),
  panel.spacing.y = unit(1, 'lines'),
  panel.spacing.x = unit(1,'lines'),
  strip.text = element_text(family = 'Verdana',size = 9,face = 'bold')
  
    ) + 
  ggsave('big_mac_index.png', width = 10, height = 14, dpi = 'retina')


