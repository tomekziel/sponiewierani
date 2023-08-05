# materiały pomocnicze do Poradnika dla sponiewieranych Excelem
# sponiewierani.pl
#
# odcinek 3 - wykresy


library(tidyverse)
library(ggthemes)

setwd("c:/users/tomek/dane")

otomoto <- 
  read.csv("otorandomized.csv") %>%
  filter(price<=400000 & year>=2016 & year<2022) %>%
  mutate(year_factor=as_factor(year))


# wykres 1
otomoto %>%
  ggplot(aes(x=price)) + 
  stat_bin(bins = 40)

# wykres 2
otomoto %>%
  ggplot(aes(x=price, fill=year_factor)) + 
  stat_bin(bins = 40) +
  scale_x_continuous(labels = scales::comma) 

# wykres 3
otomoto %>%
  ggplot(aes(x=price, fill=year_factor)) + 
  stat_bin(bins = 40, position = "fill") +
  scale_x_continuous(labels = scales::comma) 

# wykres 4
otomoto %>%
  ggplot(aes(x=price, fill=year_factor)) +
  stat_bin(bins = 40) +
  scale_x_continuous(labels = scales::comma) +
  theme_wsj() + scale_fill_wsj()

# wykres 5
otomoto %>%
  ggplot(aes(x=price, fill=year_factor)) + 
  stat_bin(bins = 40) +
  scale_x_continuous(labels = scales::comma) + 
  theme_economist() + scale_fill_economist()

# wykres 6
otomoto %>%
  ggplot(aes(x=price, fill=year_factor)) + 
  stat_bin(bins = 40)+
  scale_x_continuous(labels = scales::comma) +
  facet_wrap( ~year_factor )

# wykres 7
otomoto %>%
  ggplot(aes(x=price, fill=year_factor)) + 
  stat_bin(bins = 40) +
  scale_x_continuous(labels = scales::comma) +
  facet_grid( vars(year_factor), vars(fuel) )


# generowanie zestawu plików PNG z pojedynczymi wykresami

for(y in unique(otomoto$year_factor)){
  for (f in unique(otomoto$fuel)){

    otomoto %>%
      filter( year == y & fuel == f) %>%
      ggplot(aes(x=price)) + 
      stat_bin(bins = 40) +
      scale_x_continuous(labels = scales::comma, limits = c(0,400000)) 
    
    ggsave(paste0(y,"_",f,".png"), width = 800, height = 600, 
           units = "px", dpi=200)
    
  }
}
