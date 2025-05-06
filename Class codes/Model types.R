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

data4sim <- rnorm(150, mean = 20, sd = 6)
hist(data4sim)

qqnorm(data4sim)
qqline(data4sim)

discrete.data <- round(data4sim, 0)

qqnorm(discrete.data)
qqline(discrete.data)

editedDD <- edit(discrete.data)
hist(editedDD)

editedDD <- edit(editedDD)
hist(editedDD)

qqnorm(editedDD)
qqline(editedDD)
