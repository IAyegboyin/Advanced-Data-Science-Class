library(lme4)
library(lmerTest)
library(tidyverse)
library(MASS)
library(ggfortify)
library(corrplot)
library(multcomp)
library(emmeans)
library(car)

omo_Data <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Mosquitoes-modelling-\\Data\\Updated_Omo_Phd.csv",
                     stringsAsFactors = TRUE)

####  Data cleaning
length(colnames(omo_Data))  

omo<- omo_Data[,1:24]    
# view(omo)
attach(omo)

library(dplyr)


omo <- omo %>%
  mutate(ecozone = if_else(Ecozones == "Lowland Rainforest", 
                           "Lowland Forest", 
                           Ecozones)) %>% 
dplyr:: select(-Ecozones) %>% 
  rename("Ecozones" = ecozone) %>% 
  dplyr::select(Ecozones, everything()) %>%
  as.data.frame()

Anopheles_pred1 <- glmer(Anopheles ~ scale(Turbidity)* scale(DO) +
                           scale(Aedes)+scale(Depth)+
                           scale(Magnesium) + # Fixed effects
                           
                           (1|Ecozones)+ (1|Habitat),  # Random Effects
                         data = omo,
                         family = poisson(link = "log"))
summary(Anopheles_pred1)### this is the best model, thus far!

