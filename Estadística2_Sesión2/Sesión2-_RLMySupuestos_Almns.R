
## ---- fig.show='hide',warning=FALSE,message=FALSE-----------------------------
library(rio)
library(dplyr)
library(ggfortify)
library(see)
library(patchwork)
library(performance)
library(nortest)
library(lmtest)
library(car)

## ---- fig.show='hide',warning=FALSE,message=FALSE-----------------------------
Egov<-import("EGov.xlsx")
names(Egov)

## ---- fig.show='hide',warning=FALSE,message=FALSE-----------------------------
modeloRL <- Egov %>% lm(accesoInformacion ~ participaciónDigital, data=.)
modeloRL

## ---- fig.show='hide',warning=FALSE,message=FALSE-----------------------------
summary(modeloRL)

## ----echo=FALSE, out.width="40%",fig.align="center",fig.cap="Tehc is a meme man character  which is similarly used for tasks for which one might feel disproportionately proud of doing, though Tehc is paired with technology"----
knitr::include_graphics("tehc.png") 

## ---- fig.show='hide',warning=FALSE,message=FALSE-----------------------------
modelo1<- Egov %>% lm(accesoInformacion ~ Capital_Humano + Policulture + Telecommunicacion_Infrastructura + ProcesoElectoral,data=.)
summary(modelo1)


## ---- fig.show='hide',warning=FALSE,message=FALSE-----------------------------
modelo1$coefficients

## ---- fig.show='hide',warning=FALSE,message=FALSE-----------------------------
#Exploración gráfica
autoplot(modelo1,1)

## ---- fig.show='hide',warning=FALSE,message=FALSE-----------------------------
autoplot(modelo1, 2) 

## ----  fig.show='hide',warning=FALSE,message=FALSE----------------------------
shapiro.test(modelo1$resid)

## ---- fig.show='hide',warning=FALSE,message=FALSE-----------------------------
autoplot(modelo1, 3)

## ----message=FALSE, warning=FALSE---------------------------------------------
bptest(modelo1)

## ---- fig.show='hide',warning=FALSE,message=FALSE-----------------------------
vif(modelo1)

## ----message=FALSE, warning=FALSE---------------------------------------------
autoplot(modelo1,5)

## ---- fig.show='hide',warning=FALSE,message=FALSE-----------------------------

modelo2 <- Egov %>% lm(accesoInformacion ~ ProcesoElectoral + Policulture + Capital_Humano + UsuariosInternet,data=.)
summary(modelo2)


