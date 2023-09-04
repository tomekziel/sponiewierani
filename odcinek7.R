library(tidyverse)

ramka <- data.frame( kategoria = c('auta','komputery'),
                     "2020" = c(123,100),
                     "2021" = c(323,200),
                     "2022" = c(231,300), 
                     check.names = FALSE)

ramka
t(ramka)
t(ramka %>% column_to_rownames(var = "kategoria"))


ramkaszeroka <- data.frame( kraj = c('A', 'B', 'C'),
                     "1999" = c(0.7, 37, 212),
                     "2000" = c(2, 80, 213), 
                     check.names = FALSE)
ramkaszeroka

ramkawaska <- pivot_longer(ramkaszeroka, cols = 2:3, names_to ="rok", values_to = "liczba")
ramkawaska

pivot_wider(ramkawaska, names_from = rok, values_from = liczba)

