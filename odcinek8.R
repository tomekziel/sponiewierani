#library(extrafont)
library(tidyverse)
library(readxl)

# odkomentuj, aby użyć własnych fontów z pomocą pakietu extrafont
# font_import()

gimby <- read_excel("roczniki.xlsx")
gimby <- gimby %>% filter (grepl("^[0-9]+$", Wiek)) %>% select(-Ogółem)
gimby <- gimby %>% pivot_longer(cols=c(Mężczyźni, Kobiety), names_to="Gender", values_to = "Number")
gimby$Wiek <- as.numeric(gimby$Wiek)

gimby[gimby$Gender=="Kobiety",]$Number <- gimby[gimby$Gender=="Kobiety",]$Number *-1
gimby %>% filter(Rok==2020) 
gimby <- gimby %>% mutate(RokUrodzenia = Rok-Wiek, Gimba = paste0(Gender, RokUrodzenia >=1986 & RokUrodzenia<=2003 ))

for(rok in seq(2023,2050)){
  
  gimby %>% filter(Rok==rok) %>%
    ggplot(aes(x = Wiek, y = Number, fill = Gimba, color=Gimba )) +
    geom_col(width = 1) +
    coord_flip()+
    scale_fill_manual(
      values = c("#fcdae0", "#fc617c","#c6effb", "#00effb"),
      aesthetics = c("fill","color"),
      breaks = waiver(),
      na.value = "grey50"
    )+
    
    scale_y_continuous(limits = c(-350000, 350000),
                       minor_breaks = seq(-350000,350000,50000),
                       breaks = seq(-300000,300000,100000),
                       labels = scales::comma(abs(seq(-300000,300000,100000)))
    )+
    scale_x_continuous(breaks = seq(0,99,2)) + 
    ylab("Liczebność grupy wiekowej")+
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position="none"
    )+
    
    # odkomentuj, aby użyć własnych fontów na obrazku
    # annotate("text", x=rok-1995, y=10, label= "GIMBY", size=15, family="Bryndan Write")+
    annotate("text", x=rok-1995, y=10, label= "GIMBY", size=15)+
    annotate("text", x=98, y=-250000, label= rok, size=10)+
    annotate("text", x=1, y=-300000, label= "kobiety", size=5)+
    annotate("text", x=1, y=280000, label= "mężczyźni", size=5)
  
  ggsave(paste0(rok,".png"), width = 1080, height = 1080, 
         units = "px", dpi=150, bg = "white")
  
}

