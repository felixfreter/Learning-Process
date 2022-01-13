#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
 #%%%%%%%%%%%////Hauptseminar\\\\\%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#setwd("C:/Users/felix/OneDrive/Felix Stuff/Uni/Master/Hauptseminar")
  
 
#-----Passagiere-----
passagiere <- read.csv("FaehrpassagiereKiel.csv")
head(passagiere)
str(passagiere)
summary(passagiere)
plot(passagiere)

cor(passagiere)

best_A <- bestglm(passagiere, family = poisson, IC="AIC")
best_B <- bestglm(passagiere, family = poisson, IC="BIC")

best_A
best_B

mod_alle <- glm(passagiere$Baltikum ~ . , family = poisson, data = passagiere)

summary(mod_alle)

#----Patente----
patente <- read.csv2("Patente.csv")
data <-patente

head(data)
#
str(data)
summary(data)
zitate <- data$azit
hist(zitate,35)
attach(data)

#-----
cor(data)
library(corrplot)
corrplot(cor(data))


plot(data$jahr,data$azit, xlab = "Jahr", ylab = "Anzahl Zitate")


library(bestglm)
#bestglm letzte spalte muss die abhängige Variable sein!
#Standard ist BIC
#bestglm gibt mir das beste model nach einem bestimmtheitsmaß an // zB AIC, BIC
bestglm(data, family=poisson, IC="BIC")

#AIC
bestglm(data,family=poisson, IC = 'AIC')

#Nun kann man die modelle bauen
mod_alle <- glm(data$azit ~ . , family=poisson, data = data)
mod_BIC <- glm(data$azit ~ einspruch + biopharm + uszw+patdsg + jahr + ansp + aland, family = poisson, data = data)
mod_AIC <- glm(data$azit ~ einspruch + biopharm + uszw + patus + patdsg + bpruef + bsuche + jahr + ansp + aland, family = poisson, data = data)

#Auswertung der Modelle
summary(mod_alle)
summary(mod_AIC)
summary(mod_BIC)
mod_BIC[["family"]][["link"]]
plot(mod_BIC)

Samples<-sample(seq(1,3),size=nrow(mtcars),replace=TRUE,prob=c(0.8,0.2,0.2))
Train<-data[Samples==1,]
Test<-data[Samples==2,]
Validate<-data[Samples==3,]
Train
Test
Train_BIC <-  glm(Train$azit ~ einspruch + biopharm + uszw+patdsg + jahr + ansp + aland, family = poisson, data = Train)
pred_BIC <- predict(Train_BIC,Test,type = "response")


pred_BIC2 <- predict(Train_BIC,Test[11,],type = "response")
#pred_BIC2<- as.factor(pred_BIC)



library(caret)

#pred_BIC <- factor(pred_BIC)
pred_BIC["names"] <-factor(pred_BIC["names"])

table(pred_BIC>=1,Test$azit)



#-----

library(pscl)

mod.zi <- zeroinfl(data$azit ~ einspruch + biopharm + uszw+patdsg + jahr + ansp + aland, dist = "poisson", data = data)
summary(mod.zi)
modb.zi <- zeroinfl(data$azit ~ einspruch + biopharm + uszw + patus + patdsg + bpruef + bsuche + jahr + ansp + aland, dist = "poisson", data = data)
summary(modb.zi)


modc.zi <- zeroinfl(data$azit ~ einspruch + biopharm  + patus + patdsg + bsuche + aland, dist = "poisson", data = data)
summary(modc.zi)
modd.zi <- zeroinfl(data$azit ~ einspruch + patus + patdsg  + aland, dist = "poisson", data = data)
summary(modd.zi)

mode.zi <- zeroinfl(data$azit ~ einspruch + patus  + aland, dist = "poisson", data = data)
summary(mode.zi)
plot(mode.zi)


Train_ZI <-  zeroinfl(Train$azit ~ einspruch + patus  + aland, dist = "poisson", data = Train)
pred_ZI <- predict(Train_ZI,Test,type = "response")
#pred_BIC <- as.factor(pred_BIC)

pred_BIC2 <- predict(Train_BIC,Test[11,],type = "response")
#pred_BIC2<- as.factor(pred_BIC)
table(pred_ZI>=1,Test$azit)

library(car)

Anova(mode.zi, type="II",test="Chisq")

library(rcompanion)
nagelkerke(mod.zi)

#wichtig
#AIC Modell == alle variablen // BIC gibt ein Modell heraus dass nur signifikante Variablen gibt


###

#Histogramm der prognostizierten Wahrscheinlichkeiten
hist(fitted.values(mod_BIC))

#---- Patente aufbereitet ----

aufb <- patente


#Daten zentrieren
aufb$jahr_c <- scale(aufb$jahr, center=FALSE)
aufb$aland_c <- scale(aufb$aland)
aufb$ansp_c <- scale(aufb$ansp)
hist(aufb$jahr_c)
#Polynome anlegen
aufb$jahr2 <- aufb$jahr^2
aufb$jahr3 <- aufb$jahr^3
aufb$aland2 <- aufb$aland^2
aufb$aland3 <- aufb$aland^3
aufb$ansp2 <- aufb$ansp^2
aufb$ansp3 <- aufb$ansp^3

aufb$jahr2_c <- scale(aufb$jahr2)
aufb$jahr3_c <- scale(aufb$jahr3)
aufb$aland2_c <- scale(aufb$aland2)
aufb$aland3_c <- scale(aufb$aland3)
aufb$ansp2_c <- scale(aufb$ansp2)
aufb$ansp3_c <- scale(aufb$ansp3)

head(aufb)
summary(aufb)
str(aufb)
plot(aufb)
#Model nach BIC Kriterium
bestglm(aufb, family = poisson, IC = "BIC")

#Model nach AIC Kriterium
bestglm(data2, family = poisson, IC = "AIC")


