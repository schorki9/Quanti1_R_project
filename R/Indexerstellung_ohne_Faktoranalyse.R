library(haven)
library(foreign)
library(car)
library(ggplot2)
library(dplyr)

getwd()
allbus <- read_sav("./Datensatz Allbus 2018/ZA5270_v2-0-0.sav")


# pe05 und pe02 drehen
table(allbus$pe05)
allbus$pe05.gedreht <- (allbus$pe05)*(-1) + 5 
table(allbus$pe05.gedreht)
allbus$pe05.gedreht

table(allbus$pe02)
allbus$pe02.gedreht <- (allbus$pe02)*(-1) + 5 
table(allbus$pe02.gedreht)

### Missings zu Mittelwert umkodieren
allbus [(is.na(allbus$pe01)), "pe01"] <- (mean(allbus$pe01, na.rm = T))
sum(is.na(allbus$pe01)) # Test, ob NAs weg sind 



allbus [(is.na(allbus$pe05.gedreht)), "pe05.gedreht"] <- (mean(allbus$pe05.gedreht, na.rm = T))
sum(is.na(allbus$pe05.gedreht))


allbus [(is.na(allbus$pe02.gedreht)), "pe02.gedreht"] <- (mean(allbus$pe02.gedreht, na.rm = T))
sum(is.na(allbus$pe02.gedreht))

allbus [(is.na(allbus$pe04)), "pe04"] <- (mean(allbus$pe04, na.rm = T))
sum(is.na(allbus$pe04))

allbus [(is.na(allbus$pe06)), "pe06"] <- (mean(allbus$pe06, na.rm = T))
sum(is.na(allbus$pe06))


#### 2 Optionen ######

######## Option 1: Alle einzelnen Variablen zentrieren ##########

# ### Mittelwertzentrierung OHNE z-Transformation (mit z-Transformation wäre scale = T)
# 
#allbus$pe01.mwz<- scale(allbus$pe01, center = T, scale = F) 
#allbus$pe01.mwz
# 
#allbus$pe05.gedreht.mwz<- scale(allbus$pe05.gedreht, center = T, scale = F) 
#summary(allbus$pe05.gedreht.mwz)
# 
#allbus$pe02.gedreht.mwz<- scale(allbus$pe02.gedreht, center = T, scale = F) 
#allbus$pe02.gedreht.mwz
# 
#allbus$pe04.mwz<- scale(allbus$pe04, center = T, scale = F) 
#allbus$pe04.mwz
# 
#allbus$pe06.mwz<- scale(allbus$pe06, center = T, scale = F)
#allbus$pe06.mwz

#Index bilden
#allbus$effex02.test <- allbus$pe01.mwz + allbus$pe05.gedreht.mwz

#summary(allbus$effex02.test) = #summary(allbus$effex02.mwz) #von Option 2




###### Option 2 (weniger Code): Indexs bilden und Index dann zentrieren  #######
#Nach meinem Test zeigt summary() für beide Methoden das gleiche an


# Indizes bilden (additiv)
allbus$effex02 <- allbus$pe01 + allbus$pe05.gedreht

allbus$effint02 <- allbus$pe02.gedreht + allbus$pe04 + allbus$pe06

#Index mittelwertzentrieren
allbus$effex02.mwz <- scale(allbus$effex02, center = T, scale = F)

summary(allbus$effex02.mwz)  #Veranschaulichung 


allbus$effint02.mwz <- scale(allbus$effint02, center = T, scale = F)

summary(allbus$effint02.mwz)


sum (!is.na(allbus$effint02.mwz))
sum (!is.na(allbus$effex02.mwz))

# Häufigkeitsverteilungen Indizes normal
exefficHaeuf<-ggplot(allbus, aes(x = effex02))+
  geom_histogram(stat = "count")+
  ggtitle("Häufigkeitsverteilung der External Efficacy (Index) in Deutschland")+
  labs(caption = "Datenquelle Allbus 2018, eigene Berechnungen, n=3477, [Gewichtung]")+
  labs(x="External Efficacy", y="Anzahl der Befragten")

exefficHaeuf


intefficHaeuf<-ggplot(allbus, aes(x = effint02))+
  geom_histogram(stat = "count")+
  ggtitle("Häufigkeitsverteilung der Internal Efficacy (Index) in Deutschland")+
  labs(caption = "Datenquelle Allbus 2018, eigene Berechnungen, n=3477, [Gewichtung]")+
  labs(x="Internal Efficacy", y="Anzahl der Befragten")

intefficHaeuf


# Häufigkeitsverteilungen Indizes mittelwertzentriert
external.mzw.plot<-ggplot(allbus, aes(x = effex02.mwz))+
  geom_histogram(stat = "count")+
  ggtitle("Häufigkeitsverteilung der External Efficacy (Index_mwz) in Deutschland")+
  labs(caption = "Datenquelle Allbus 2018, eigene Berechnungen, n=3477, [Gewichtung]")+
  labs(x="External Efficacy", y="Anzahl der Befragten")+
  coord_cartesian(xlim = c(-4,4), ylim=c(0,900))

external.mzw.plot


internal.mzw.plot<-ggplot(allbus, aes(x = effint02.mwz))+
  geom_histogram(stat = "count")+
  ggtitle("Häufigkeitsverteilung der Internal Efficacy (Index_mwz) in Deutschland")+
  labs(caption = "Datenquelle Allbus 2018, eigene Berechnungen, n=3477, [Gewichtung]")+
  labs(x="Internal Efficacy", y="Anzahl der Befragten")+
  coord_cartesian(xlim = c(-5,5), ylim = c(0,700))

internal.mzw.plot

### Grafiken speichern

ggsave("./plots/ExternalHaeufig.jpg",plot = exefficHaeuf, device = "jpg")

ggsave("./plots/InternalHaeufig.jpg",plot = intefficHaeuf, device = "jpg")

ggsave("./plots/Externalmwz.jpg",plot = external.mzw.plot, device = "jpg")

ggsave("./plots/Internalmwz.jpg",plot = internal.mzw.plot, device = "jpg")










## Mittelwertvergleich der mittelwertzentrierten Efficacy-Indizes
#Vorarbeiten
indexvariablen_mwz<-c("effex02.mwz", "effint02.mwz", "land")
df03<-allbus[indexvariablen_mwz]

df03$land<-as.character(df03$land) 
df03$land_fac <- car::recode(df03$land,
                           "10 = 'Schleswig-Holstein';
                      20 = 'Hamburg';
                      30 = 'Niedersachsen';
                      40 ='Bremen';
                      50 = 'Nordrhein-Westfalen';
                      60 = 'Hessen';
                      70 = 'Rheinland-Pfalz';
                      80 = 'Baden-Württemberg';
                      90 = 'Bayern';
                      100 = 'Saarland';
                      c(111,112) = 'Berlin';
                      120 = 'Brandenburg';
                      130 = 'Mecklenburg-Vorpommern';
                      140 = 'Sachsen';
                      150 = 'Sachsen-Anhalt';
                      160 = 'Thueringen'",
                           as.factor = TRUE)

df03$land<- as.integer(df03$land) 



by_land_03 <- df03 %>% group_by(land_fac) %>% summarise(effex02.mwz = mean(effex02.mwz, na.rm = "TRUE"),
                                                   effint02.mwz = mean(effint02.mwz, na.rm = "TRUE"))



graph_03 <- ggplot(by_land_03, aes(x = land_fac, y = effex02.mwz))+
  geom_bar(stat="identity")+
  coord_flip(ylim = c(-0.6,0.6))+
  ggtitle("Mittelwertvergleich von External Efficacy (effex02.mwz)")+
  labs(y="Mittelwerte", x="Bundesländer")+
  labs(caption = "Datenquelle Allbus 2018, eigene Berechnungen, n=3477, [Gewichtung]")

graph_03


graph_04 <- ggplot(by_land_03, aes(x = land_fac, y = effint02.mwz))+
  geom_bar(stat="identity")+
  coord_flip(ylim = c(-0.6,0.6))+
  ggtitle("Mittelwertvergleich von Internal Efficacy (effint02.mwz)")+
  labs(y="Mittelwerte", x="Bundesländer")+
  labs(caption = "Datenquelle Allbus 2018, eigene Berechnungen, n=3477, [Gewichtung]")

graph_04



ggsave("./plots/Mittelwertvergleich_Externalmwz.jpg",plot = graph_03, device = "jpg")

ggsave("./plots/Mittelwertvergleich_Internalmwz.jpg",plot = graph_04, device = "jpg")



