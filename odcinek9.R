# materiały pomocnicze do Poradnika dla sponiewieranych Excelem
# sponiewierani.pl
#
# odcinek 9

library(tidyverse)

setwd("c:/users/tomek/dane")
otomoto <- read.csv("otorandomized.csv")

# komenda z użyciem operatora %>%
otomoto %>%
  mutate(oferty_marka = n(), .by=c(mark)) %>%
  summarise(oferty_markamodel=n(), .by = c(mark, model, oferty_marka) ) %>%
  arrange(desc(oferty_markamodel)) %>%
  slice_head(n=3, by=mark) %>%
  filter( oferty_marka>5000 &  grepl("d", mark) )

# równoważny wariant bez tego operatora
filter(slice_head(arrange(summarise(mutate(otomoto, oferty_marka = n(), 
.by=c(mark)), oferty_markamodel=n(), .by = c(mark, model, oferty_marka) ), 
desc(oferty_markamodel)), n=3, by=mark), oferty_marka>5000 & grepl("d", mark) )


nrow(otomoto)
ncol(otomoto)

otomoto %>% nrow()
otomoto %>% nrow

head(otomoto, n=10)
tail(otomoto, n=10)

tail(otomoto, n=-15)

otomoto[1,]
otomoto[1000,]

otomoto[1:1000,]


otomoto$city
otomoto[,8]


otomoto[7,"mileage"]
otomoto[7,5]
otomoto[7,]$mileage


otomoto %>% select(city, price)

otomoto %>% select(-city, -province)

otomoto %>% select(contains("a"))

otomoto %>% select(starts_with("m"))

otomoto %>% select(year:fuel)

otomotocopy <- otomoto

otomotocopy$city <- NULL
ncol(otomoto)
ncol(otomotocopy)


otomoto %>% filter(fuel == "CNG")


otomoto %>% 
  filter(fuel == "CNG") %>%
  select(mark, model) %>%
  distinct()

otomoto %>% 
  filter(fuel == "CNG") %>%
  select(mark, model) %>%
  unique()


otomoto %>% mutate(zrodlo="otomoto", cena_w_milionach = price/1000000)

otomoto$cena_w_milionach <- otomoto$price/1000000

otomoto$stolica <- ifelse(otomoto$city=="Warszawa", TRUE, FALSE)

otomoto$czy_drogo <- 
  case_when(
    otomoto$price<10000 ~ "tanio", 
    otomoto$price<100000 ~ "tak sobie", 
    .default = "drogo" )


otomoto %>% relocate(czy_drogo, .after = cena_w_milionach)


otomoto %>%
  add_row(mark = "FSO", model = "Syrena", price = 10) %>%
  tail(n=3)


otomotopodwojone <- rbind(otomoto,otomoto)

otomoto %>% arrange (mark, desc(model))

