library(tidyverse)
library(dunn.test)
library(DescTools)

mela <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\melanomafor_stat1.csv")

mela <- mela %>% 
  mutate(status = factor(status, levels = c("1", "2", "3"),
                         labels = c("Poor", "Middle-class", "Rich"))) %>% 
           as.data.frame()

attach(mela)
kruskal.test(thickness ~ status, data = mela) 

a___ <- aov(thickness ~ status, data = mela)
summary(a___)
TukeyHSD(a___)



dunn.test(mela$thickness, mela$status, method = "bonferroni")




mela %>% 
  select(sex, ulcer) %>% 
  pivot_wider(
    names_from = ulcer, 
    values_from = 
  )


mela_summary <- mela %>% 
  select(sex, ulcer) %>% 
  mutate(sex = factor(sex, labels = c("Female", "Male")),
         ulcer = factor(ulcer, labels = c("Observed", "Expected"))) %>% 
  group_by(sex, ulcer) %>% 
  summarise(count_ulcer = n()) %>% 
  pivot_wider(
    names_from = ulcer, 
    values_from = count_ulcer
  ) %>% 
  as.data.frame()

mela_summary <- edit(mela_summary)


view(mela_summary)

rownames(mela_summary) <- mela_summary[,c("sex")]
mela_summary <- mela_summary %>% 
  select(-sex)

chisq.test(mela_summary)
fisher.test(mela_summary)
GTest(mela_summary)
observed <- as.vector(c(79, 47))

chisq.test(observed)





