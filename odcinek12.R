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
    temp = V30) %>%
  group_by( data = as.Date.POSIXct(data)) %>%
  summarize(temp = max(temp) ) %>%
  ungroup

pogodawro %>%
  ggplot(aes(x=data, y=temp)) +
  geom_line()


ruch <- read.csv("scpr2022.csv", sep = ";" ) %>%
  filter(STACJA == 2012) %>%
  mutate(data = dmy(DATA), auta = SDRD) %>%
  select(data, auta)


ruch %>%
  ggplot(aes(x=data, y=auta)) +
  geom_line()


bind_cols(pogodawro, ruch)

bind_cols(pogodawro, ruch %>% select(auta) )



pogodawro20 <- pogodawro %>% 
  head(n=30) %>% 
  sample_n(20) %>% 
  mutate(zawartosc="temperatura") %>%
  rename(pomiar = temp)

ruch20 <- ruch %>%
  head(n=30) %>% 
  sample_n(20) %>% 
  mutate(zawartosc="ruch_aut") %>%
  rename(pomiar = auta)

sklejka <- bind_rows(pogodawro20, ruch20)

pivot_wider(sklejka, 
            names_from = zawartosc, 
            values_from = pomiar) 

full_join(pogodawro20, ruch20, by="data")

zlaczenie <- full_join(pogodawro20, ruch20, by="data") %>% arrange(data)

