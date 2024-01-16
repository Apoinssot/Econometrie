library(ggplot2)
library(corrplot)
library(stringr)

#Chargement des données
df = read.csv("raw_data.csv", dec = ",", sep = ";")
df$Pop1 <- as.numeric(str_replace_all(df$Pop1, " ", ""))
df$Pop2 <- as.numeric(str_replace_all(df$Pop2, " ", ""))
df$PCelec = df$Pelec*100/df$IPC


#Représentation des données
ggplot(df, aes(Date, Celec))+
  geom_point(color = "blue")+
  geom_line()+
  theme_bw()

ggplot(df)+
  geom_line(aes(Date, Pelec), color="blue")+
  geom_line(aes(Date, PCelec), color="red")+
  theme_bw()

ggplot(df, aes(DJU, Celec))+
  geom_point(aes(color=Date), size=5)+
  theme_bw()

#Matrice de corrélation
M <- cor(df[,-1])
corrplot(M, method = "number")

#Essai de régression linéaire
df$logPIB = log(df$PIB)
df$logPCelec = log(df$PCelec)
df$logPop1 = log(df$Pop1)
df$logDJU = log(df$DJU)
df$logCelec = log(df$Celec)

lm = lm(logCelec ~ logPIB + logPCelec + logPop1 + logDJU, data = df)
#R²=0.97 mais Pop1 non significatif
summary(lm)

#Nouveau modèle sans la population

lm = lm(logCelec ~ logPIB + logPCelec + logDJU, data = df)
#R²=0.9696
summary(lm)

#Essai par tête
df$PIBpt = df$PIB/df$Pop2
df$logPIBpt = log(df$PIBpt)
df$Celecpt = df$Celec/df$Pop2
df$logCelecpt = log(df$Celecpt)

lm = lm(logCelecpt ~ logPIBpt + logPCelec + logDJU, data = df)
summary(lm)
#R²=0.94 -> moins intéressant que le modèle global

#Nouveau modèle sans log
lm = lm(Celec ~ PIB + PCelec + Pop1 + DJU, data=df)
summary(lm) #WOW R²=0.9765



