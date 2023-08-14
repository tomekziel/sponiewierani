# materiały pomocnicze do Poradnika dla sponiewieranych Excelem
# sponiewierani.pl
#
# odcinek 4 - wczytywanie plików

library(tidyverse)

setwd("c:/users/tomek/dane")


otomoto <- read_csv("otorandomized.csv")

otomoto10k <- read_csv("otorandomized.csv", col_types = "fffnnnf?fn", n_max = 10000)


install.packages("readxl")
library(readxl)

otoexcel <- read_excel("otomoto.xlsx")

?read_csv

?read_excel

skomplikowany_wynik <-
  otoexcel %>%
  mutate(oferty_marka = n(), .by=c(mark)) %>%
  summarise(oferty_markamodel=n(), .by = c(mark, model, oferty_marka) ) %>%
  arrange(desc(oferty_markamodel)) %>%
  slice_head(n=3, by=mark) %>%
  filter( oferty_marka>5000 & grepl("d", mark) )

install.packages("writexl")
library(writexl)

write_xlsx( skomplikowany_wynik, "wynik.xlsx" )

