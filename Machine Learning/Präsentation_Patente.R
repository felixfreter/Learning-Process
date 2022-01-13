#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%////Hauptseminar\\\\\%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#setwd("C:/Users/felix/OneDrive/Felix Stuff/Uni/Master/Hauptseminar")

patente <- read.csv2("Patente.csv")
data <-patente
data$azit <- data$azit[data$azit<5]
head(data)

str(data)

summary(data)

zitate <- data$azit


hist(zitate,35,xlab="Anzahl Zitate",ylab ="Häufigkeit", main=" ")

library(corrplot)
corrplot(cor(data),tl.col = NULL)

#Korrelation != Kausalerzusammenhang
library(bestglm)
#bestglm letzte spalte muss die abhängige Variable sein!
#Standard ist BIC
#bestglm gibt mir das beste model nach einem bestimmtheitsmaß an // zB AIC, BIC
bestglm(data, family=poisson, IC="BIC")

#AIC
bestglm(data,family=poisson, IC = 'AIC')

#Nun kann man die modelle bauen

mod_BIC <- glm(data$azit ~ einspruch + biopharm + uszw + patdsg + jahr + ansp + aland, family = poisson, data = data)
mod_AIC <- glm(data$azit ~ einspruch + biopharm + uszw + patus + patdsg + bpruef + bsuche + jahr + ansp + aland, family = poisson, data = data)

#Auswertung der Modelle

summary(mod_BIC)
summary(mod_AIC)

#Link Funktion
mod_BIC[["family"]][["link"]]


#Prediction

#TRAIN TEST SPLIT

Samples<-sample(seq(1,2),size=nrow(data),replace=TRUE,prob=c(0.8,0.2))
Train<-data[Samples==1,]
Test<-data[Samples==2,]

#Trainieren 
Train_BIC <-  glm(Train$azit ~ einspruch + biopharm+uszw +patdsg + jahr + ansp + aland, family = poisson, data = Train)

#Prediction
pred_BIC <- predict(Train_BIC,Test,type = "response")
#Confusion Matrix
hist(pred_BIC)
table(round(pred_BIC)-1,Test$azit)

negtest1 <- is.expression(pred_BIC <0) 
negtest2 <- is.expression(Test$azit <0) 
negtest1
negtest2

negtest3 <- is.expression(table(round(pred_BIC)-1,Test$azit))
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

Train_ZI <-  zeroinfl(Train$azit ~ einspruch + patus  + aland, dist = "poisson", data = Train)
pred_ZI <- predict(Train_ZI,Test,type = "response")

table(round(pred_BIC)-1,Test$azit)
table(round(pred_ZI)-1,Test$azit)
tab
