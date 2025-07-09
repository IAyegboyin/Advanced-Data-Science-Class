
library(tidyverse)


sex_fly <- read_excel("C:\\Users\\DELL\\Documents\\Git in R\\Urban-Ecology\\Data\\Fly_community.xlsx",
                      sheet = "By_sex")
    # look for the data in "Urban ecology" repository on my Github

sum(is.na(sex_fly))
sex_fly <- replace(sex_fly, is.na(sex_fly), 0) %>% 
  rename(Method="Collection Method")


sex_fly %>% 
  view()

 
  
  sex_fly %>% 
    rename("P.sericata" = "Phaenicia sericata") %>% 
    select(Location, Longitude, Sex, P.sericata) %>% 
    mutate(P.sericata = ifelse(P.sericata>0, 1,0)) %>%
    group_by(Location, Sex, Longitude) %>% 
    summarise(P.sericata_prop = mean(P.sericata),
              Trials = length(P.sericata)) %>% 
    view()
  