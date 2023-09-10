
library(rio)
library(dplyr)
library(ggfortify)
library(see)
library(patchwork)
library(performance)
library(nortest)
library(lmtest)
library(car)



library(rio)
data=import("state_democracy_final_23.xlsx")
names(data)
--
library(tidyverse)
data %>%
  count(Type,sort = T) 
--
data%>% 
  group_by(., Type) %>% 
  summarise(mean = mean(Democracy_Score,na.rm=TRUE), n = n()) %>%
  arrange(desc(mean))
---

summary(data$Inequality)

---

data%>% 
    filter(Inequality <= 5.24) %>% 
    count(.,)
---
MenMedia=data%>% 
         filter(Inequality <= 5.24)
---
MenMedia%>% 
    filter(Type=="Authoritarian regime") %>% 
    summarise(max(State_Legit),
              min(State_Legit),
              mean(State_Legit))
---
modelo1=data %>% 
lm(Democracy_Score ~ Security + State_Legit + Hum_Rights,data=.)
summary(modelo1)

---

#Exploración gráfica
plot(modelo1,1)
autoplot(modelo1,1)

autoplot(modelo1, 2) 

shapiro.test(modelo1$resid)

autoplot(modelo1, 3)
plot(modelo1, 3)

bptest(modelo1)

vif(modelo1)

library(car)
durbinWatsonTest(modelo1)

autoplot(modelo1,5)


