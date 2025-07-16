
library(tidyverse)
library(readxl)
library(interactions)

sex_fly <- read_excel("C:\\Users\\DELL\\Documents\\Git in R\\Urban-Ecology\\Data\\Fly_community.xlsx",
                      sheet = "By_sex")
    # look for the data in "Urban ecology" repository on my Github

sum(is.na(sex_fly))
sex_fly <- replace(sex_fly, is.na(sex_fly), 0) %>% 
  rename(Method="Collection Method")


sex_fly <- sex_fly %>% 
  mutate(Longitude=  as.numeric(str_remove(Longitude, "°N")),
         Latitude = as.numeric(str_remove(Latitude, "°E")))
 
  
fly<-  sex_fly %>% 
    rename("P.sericata" = "Phaenicia sericata",
           "P.cuprina" = "Phaenicia cuprina") %>% 
    select(Location, Longitude, Latitude, P.sericata, P.cuprina) %>% 
    mutate(P.sericata = ifelse(P.sericata>0, 1,0),
           P.cuprina = ifelse(P.cuprina>0, 1,0)) %>%
    group_by(Location, Longitude, Latitude) %>% 
    summarise(P.sericata_prop = mean(P.sericata),
              P.cuprina_prop = mean(P.cuprina),
              Trials = length(P.sericata))  

  prop_P.sericata = glm(P.sericata_prop ~ Longitude+Latitude,
                              data = fly, weights = Trials,  family = "binomial")
  summary(prop_P.sericata) 
  
 
  prop_P.cuprina  = glm(P.cuprina_prop ~ Longitude*Latitude,
                        data = fly, weights = Trials,  family = "binomial")
  summary(prop_P.cuprina) 
  
  
  
  
  
interact_plot(prop_P.cuprina, pred = Longitude, modx = Latitude, 
              x.label = "Longitude",
              y.label = "Proportion of presence",
              vary.lty = FALSE)+
  scale_color_gradient(low = "yellow", high = "red") 
  
  
  