# materiały pomocnicze do Poradnika dla sponiewieranych Excelem
# sponiewierani.pl
#
# odcinek 13

library(tidyverse)
library(patchwork)

setwd("c:/users/tomek/dane")

ruch <- read.csv("scpr2022.csv", sep = ";" ) %>%
  mutate(data = dmy(DATA), auta = SDRD, stacja = STACJA) %>%
  select(stacja, data, auta)

nrstacji <- 2002

p1 <- ruch %>%
  filter(stacja == nrstacji) %>%
  ggplot(aes(x=data, y=auta)) +
  geom_line() +
  geom_smooth(method=lm) +
  xlab("Data") +
  ylab("Liczba samochodów") +
  theme_minimal() 

p2 <- ruch %>%
  filter(stacja == nrstacji) %>%
  group_by( dzien_tyg = wday(data, label=TRUE) ) %>%
  summarise( sredni_ruch = mean(auta) ) %>%
  ggplot(aes(x=dzien_tyg, y=sredni_ruch, fill=sredni_ruch)) +
  geom_col() +
  xlab("Dni tygodnia") +
  ylab("Liczba samochodów") +
  theme_minimal() +
  scale_fill_viridis_c()

p1 + p2
