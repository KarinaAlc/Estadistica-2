
data <- rio::import("~/Downloads/DATA_AFE_2023.xlsx")

library(dplyr)
names(data)


#filter
data %>%
  filter(GastoBolsillo_porc > 50)

#arrange
data %>%
  filter(GastoBolsillo_porc > 50)%>%
  arrange(desc(GastoBolsillo_porc))

#select
data %>%
  filter(GastoBolsillo_porc > 50)%>%
  arrange(desc(GastoBolsillo_porc))%>%
  select(pais,GastoBolsillo_porc)

data %>%
  select(-GastoBolsillo_porc,-Generosidad)

#mutate
summary(data$GastoBolsillo_porc)

#En la data original, creemos una variable que indique si un país tiene un percepción alta de corrupción (1)- cuando supere a 0.1422- 
# o no (0) .Esta variable se llamará "AltaPerCorru". 

data <- data %>%
  mutate(AltaPerCorru = ifelse(Percep_corrup > 0.1422,1,0))


#count
data %>%
  count(AltaPerCorru)


#group_by  & summarise

data %>%
  group_by(AltaPerCorru)%>%
  summarise(Media = mean(ProcesoElectoral))


names(data)


##Modelo 1
modelo <-lm(data$Policulture ~ data$GastoBolsillo_porc)
summary(modelo)


##Modelo 2
modelo1 <- lm(data$Policulture~ data$GastoBolsillo_porc+
                data$ParDig+
                data$AltaPerCorru)

summary(modelo1)

modelo1$coefficients





