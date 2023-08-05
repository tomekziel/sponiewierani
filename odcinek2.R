# materiały pomocnicze do Poradnika dla sponiewieranych Excelem
# sponiewierani.pl
#
# odcinek 2 - ramka danych

library(tidyverse)

setwd("c:/users/tomek/dane")
otomoto <- read.csv("otorandomized.csv")


otomoto %>%
  mutate(oferty_marka = n(), .by=c(mark)) %>%
  summarise(oferty_markamodel=n(), .by = c(mark, model, oferty_marka) ) %>%
  arrange(desc(oferty_markamodel)) %>%
  slice_head(n=3, by=mark) %>%
  filter( oferty_marka>5000 &  grepl("d", mark) )

