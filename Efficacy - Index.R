### AV Efficacy - Index
#Geändert test
# Annähernd normalverteilt?-jawoll
hist (allbus$pe01)
hist (allbus$pe05)

hist (allbus$pe02)
hist (allbus$pe04)
hist (allbus$pe06)

# Zu viele Missings?-nein
sum (is.na(allbus$pe01)) / length(allbus$pe01)
sum (is.na(allbus$pe05)) / length(allbus$pe05)

sum (is.na(allbus$pe02)) / length(allbus$pe02)
sum (is.na(allbus$pe04)) / length(allbus$pe04)
sum (is.na(allbus$pe06)) / length(allbus$pe06)


# pe05 und pe02 drehen
table(allbus$pe05)
allbus$pe05.gedreht <- (allbus$pe05)*(-1) + 5 
table(allbus$pe05.gedreht)
allbus$pe05.gedreht

table(allbus$pe02)
allbus$pe02.gedreht <- (allbus$pe02)*(-1) + 5 
table(allbus$pe02.gedreht)


# Subsetdatensatz erstellen und Variablen z-transformieren
vars <- c("pe01", "pe02.gedreht", "pe04", "pe05.gedreht", "pe06")
allbus.efficacy <- allbus [, vars]
allbus.efficacy.z <- scale (allbus.efficacy)

library(psych)
describe (allbus.efficacy.z)
## -> Variablen jetzt geeignet (normalverteilt etc)? - Joa



# Korrelationsmatrix
cormatrixefficacy <- cor(allbus.efficacy.z, use = "complete.obs")
det(cormatrixefficacy)
det(cormatrixefficacy) > 0.00001


# Korrelationsmatrix auf Signifikanz prüfen
corr.test (allbus.efficacy.z, use = "complete")

# Bartlett-Test
cortest.bartlett(allbus.efficacy.z, n = length(allbus.efficacy.z[,1]))
## Test ist glaub egal

# KMO und MSA
KMO(allbus.efficacy.z)


# Extraktion der Faktoren in R
modelefficacy <- principal(allbus.efficacy.z, rotate = "oblimin")
modelefficacy$values
sum(modelefficacy$values > 1)

modelefficacy <- principal (allbus.efficacy.z, nfactors = 2, rotate = "oblimin")
print.psych (modelefficacy)

# Reliabilitätstest
Gruppe1 <- as.data.frame(allbus.efficacy.z[,c("pe02.gedreht", "pe04", "pe06")])
Gruppe2 <- as.data.frame(allbus.efficacy.z[,c("pe01", "pe05.gedreht")])

psych::alpha(Gruppe1)
psych::alpha(Gruppe2)




### Jetzt noch die beiden Indizes bilden und mittelwertzentrieren für Vergleichbarkeit


# gucken ob Items numeric
effex_subset<-unlist(lapply(df, is.numeric)) 
effex_subset

effex_subset<-unlist(lapply(allbus.efficacy, is.numeric)) 
effex_subset

# Indizes bilden (rowmeans)
allbus$effex <- rowMeans(allbus.efficacy [,c("pe01", "pe05.gedreht")], na.rm = T)
sum(is.na(allbus$effex))
sum(is.na(allbus$effex))/length(allbus$effex)

allbus$effint<-rowMeans(allbus.efficacy[,c("pe02.gedreht", "pe04", "pe06")], na.rm = T)
sum(is.na(allbus$effint))
sum(is.na(allbus$effint))/length(allbus$effint)

table(allbus$effex)
table(allbus$effint)


# Indexgrafiken
library(ggplot2)
ggplot(allbus, aes(x = effex))+
  geom_histogram(stat = "count")+
  ggtitle("Häufigkeitsverteilung der External Efficacy (Index) in Deutschland")+
  labs(caption = "Datenquelle Allbus 2018, eigene Berechnungen, n=??, [Gewichtung]")+
  labs(x="External Efficacy", y="Anzahl der Befragten")

ggplot(allbus, aes(x = effint))+
  geom_histogram(stat = "count")+
  ggtitle("Häufigkeitsverteilung der Internal Efficacy (Index) in Deutschland")+
  labs(caption = "Datenquelle Allbus 2018, eigene Berechnungen, n=??, [Gewichtung]")+
  labs(x="Internal Efficacy", y="Anzahl der Befragten")

## -> rowmeans behält Ursprungsskala immer bei


# fehlende Werte händisch mit Mittelwert ersetzen
allbus [(is.na(allbus$pe01)), "pe01"] <- (mean(allbus$pe01, na.rm = T))
sum(is.na(allbus$pe01))

allbus [(is.na(allbus$pe05.gedreht)), "pe05.gedreht"] <- (mean(allbus$pe05.gedreht, na.rm = T))
sum(is.na(allbus$pe05.gedreht))


allbus [(is.na(allbus$pe02.gedreht)), "pe02.gedreht"] <- (mean(allbus$pe02.gedreht, na.rm = T))
sum(is.na(allbus$pe02.gedreht))

allbus [(is.na(allbus$pe04)), "pe04"] <- (mean(allbus$pe04, na.rm = T))
sum(is.na(allbus$pe04))

allbus [(is.na(allbus$pe06)), "pe06"] <- (mean(allbus$pe06, na.rm = T))
sum(is.na(allbus$pe06))


# Indizes bilden (additiv) + mittelwertzentrieren
allbus$effex02 <- allbus$pe01 + allbus$pe05.gedreht
sum(is.na(allbus$effex02))
sum(is.na(allbus$effex02))/length(allbus$effex02)
table(allbus$effex02, useNA = "always")

ggplot(allbus, aes(x = effex02))+
  geom_histogram(stat = "count")+
  ggtitle("Häufigkeitsverteilung der External Efficacy (Index) in Deutschland")+
  labs(caption = "Datenquelle Allbus 2018, eigene Berechnungen, n=??, [Gewichtung]")+
  labs(x="External Efficacy", y="Anzahl der Befragten")

allbus$effint02 <- allbus$pe02.gedreht + allbus$pe04 + allbus$pe06
sum(is.na(allbus$effint02))
sum(is.na(allbus$effint02))/length(allbus$effint02)
table(allbus$effint02, useNA = "always")

ggplot(allbus, aes(x = effint02))+
  geom_histogram(stat = "count")+
  ggtitle("Häufigkeitsverteilung der Internal Efficacy (Index) in Deutschland")+
  labs(caption = "Datenquelle Allbus 2018, eigene Berechnungen, n=??, [Gewichtung]")+
  labs(x="Internal Efficacy", y="Anzahl der Befragten")



#mwz; hat noch nicht funktioniert
allbus$effex02_mwz <- allbus$effex02 - mean(allbus$effex02)
table(allbus$effex02_mwz, useNA = "always")
ggplot(allbus, aes(x = effex02_mwz))+
  geom_histogram(stat = "count")+
  ggtitle("Häufigkeitsverteilung der External Efficacy (Index_mwz) in Deutschland")+
  labs(caption = "Datenquelle Allbus 2018, eigene Berechnungen, n=??, [Gewichtung]")+
  labs(x="External Efficacy", y="Anzahl der Befragten")





