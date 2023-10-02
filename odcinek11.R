# materiały pomocnicze do Poradnika dla sponiewieranych Excelem
# sponiewierani.pl
#
# odcinek 11

library(tidyverse)
library(zoo)

setwd("c:/users/tomek/dane")
pogodawro <- read.csv("s_t_424_2022.csv", header = F)

pogodawro <- pogodawro %>% mutate ( 
  data = ymd_h( paste( V3, V4, V5, V6, sep="-") ),
  temp = V30,
  opad6h = V49
  ) %>%
  select(data, temp, opad6h)


pogodawro %>% 
  filter (data >= ymd("2022-04-01") & data <= ymd("2022-04-30") ) %>%
  ggplot(aes(x=data, y=temp))+
  geom_line()+
  theme_minimal()+
  theme(legend.position = "none")


pogodawro %>% 
  mutate(srednia24 = rollmean(temp, 24, fill=NA)) %>%
  filter (data >= ymd("2022-04-01") & data <= ymd("2022-04-30") ) %>%
  ggplot(aes(x=data))+
  geom_line(aes(y=temp), color="#868686")+
  geom_line(aes(y=srednia24), color="#D55E00", lwd=1)+
  theme_minimal()+
  theme(legend.position = "none")

ggsave("01.png", width = 800, height = 600, 
       units = "px", dpi=160, bg = "white")

pogodawro %>% 
  mutate(srednia12 = rollmean(temp, 12, fill=NA, align="center")) %>%
  mutate(srednia24 = rollmean(temp, 24, fill=NA, align="center")) %>%
  mutate(srednia36 = rollmean(temp, 36, fill=NA, align="center")) %>%
  filter (data >= ymd("2022-04-01") & data <= ymd("2022-04-05") ) %>%
  ggplot(aes(x=data))+
  geom_line(aes(y=temp), color="#868686")+
  geom_line(aes(y=srednia12), color="#D55E00", lwd=1)+
  geom_line(aes(y=srednia24), color="#009E73", lwd=1)+
  geom_line(aes(y=srednia36), color="#56B4E9", lwd=1)+
  theme_minimal()+
  theme(legend.position = "none")

ggsave("02.png", width = 800, height = 600, 
       units = "px", dpi=160, bg = "white")


pogodawro %>% 
  select(data, temp) %>%
  mutate(tempminus3h = lag(temp, n=3)) %>%
  mutate(tempplus7h = lead(temp, n=7)) %>%
  filter (data >= ymd("2022-04-01"))
  
sum(pogodawro$opad6h)

pogodawro %>%
  mutate(sumaopadow = cumsum(opad6h)) %>%
  ggplot(aes(x=data, y=sumaopadow))+
  geom_line()+
  theme_minimal()
                       
ggsave("04.png", width = 800, height = 600, 
       units = "px", dpi=160, bg = "white")


otomoto <- read.csv("otorandomized.csv")

otomoto$cwiartka_cen <- ntile(otomoto$price, n=4)

otomoto$procent_tanszych_aut <- 100*percent_rank(otomoto$price)

