# materiały pomocnicze do Poradnika dla sponiewieranych Excelem
# sponiewierani.pl
#
# odcinek 10

library(tidyverse)

setwd("c:/users/tomek/dane")
otomoto <- read.csv("otorandomized.csv")


mean(otomoto$price)
median(otomoto$price)
sum(otomoto$price)

# mediana globalna
otomoto %>%
  mutate(mediana = median(price), procent_mediany = 100*price/mediana) %>%
  select(mark, model, year, mileage, price, mediana, procent_mediany)

# mediana marki
otomoto %>% 
  group_by(mark) %>%
  mutate(mediana_marki = median(price), procent_mediany_marki = 100*price/mediana_marki ) %>%
  ungroup() %>%
  select(mark, model, year, mileage, price, mediana_marki, procent_mediany_marki)

# to, co powyżej, ale bez rozgrupowania
otomoto %>% 
  group_by(mark) %>%
  mutate(mediana_marki = median(price), procent_mediany_marki = 100*price/mediana_marki ) %>%
  select(mark, model, year, mileage, price, mediana_marki, procent_mediany_marki)

# grupowanie bez komendy group_by
otomoto %>% 
  mutate(procent_mediany_marki = 100*price/ median(price), .by = mark) 

# tworzenie podsumowań
otomoto %>%
  group_by(mark) %>%
  summarize (
    liczba_modeli = n_distinct(model),
    liczba_ofert = n(),
    srednia_cena = mean(price),
    odchylenie_standardowe_ceny = sd(price)
  ) %>% 
  ungroup 

# podsumowania wg wielu kryteriów
otomoto %>%
  group_by(mark, model, niski_przebieg = mileage < 100000) %>%
  summarize (
    liczba_ofert = n(),
    srednia_cena = mean(price),
    odchylenie_standardowe_ceny = sd(price)
  ) %>% 
  ungroup


