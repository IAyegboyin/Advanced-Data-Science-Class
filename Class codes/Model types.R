library(lme4)
library(lmerTest)
library(tidyverse)
library(MASS)
library(ggfortify)
library(corrplot)
library(multcomp)
library(emmeans)
library(car)

mosquito.data <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Advanced-Data-Science-Class\\Practice\\mosquito_clean.csv") %>% 
  dplyr::  select(-X)

qqnorm(mosquito.data$Aedes)
qqline(mosquito.data$Aedes)

qqnorm(mosquito.data$pH)
qqline(mosquito.data$pH)


summary(mosquito.data$Aedes)
summary(mosquito.data$pH)
hist(mosquito.data$Aedes, breaks = 10)




set.seed(99)

data4sim <- rnorm(150, mean = 8, sd = 2)
hist(data4sim)

qqnorm(data4sim)
qqline(data4sim)

discrete.data <- round(data4sim, 0)

qqnorm(discrete.data)
qqline(discrete.data)
hist(discrete.data, breaks = 12)

editedDD <- edit(discrete.data)
hist(editedDD)

editedDD <- edit(editedDD)
hist(editedDD)

qqnorm(editedDD)
qqline(editedDD)




view(melanomafor)


melanomafor <- melanomafor %>% 
  mutate(sex= factor(sex,    levels = c("0", "1"), 
                     labels =  c("Female", "Male")))

