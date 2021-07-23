#erste Regression
#pakete öffnen
library(foreign)
library(psych)
library(ggplot2)
library(haven)
library(car)
library(dplyr)
library(multilevel)
library(lme4)
library(jtools)
#Daten prüfen
#einlesen
allbus <- read.spss("/Users/soere/Documents/DirekteDemokratie/ZA5270_v2-0-0.sav",
                    use.value.labels = FALSE,
                    to.data.frame = TRUE,
                    reencode = TRUE)

allbusclean <- na.omit(allbus)

makrodata <- read.csv("/Users/soere/Documents/DirekteDemokratie/BB_BE_Kommulativ_1956bis2015.csv", sep = ";" )

#Land Variable muss umcodiert werden zu char und Vektor 
allbus$land<- as.character(allbus$land)

allbus$land_fac <- car::recode(allbus$land,
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
                      160 = 'Thüringen'",
                               as.factor = TRUE)

# MERGING Datensätze zusammenführen anhand der Bundesländer

newdata <- merge(allbus, makrodata, by="land_fac", na.rm = "TRUE")


#Jede Person aus einem Bundesland hat jetzt die Werte des Bundeslandes auf der entsprechenden Makrovariable
newdata$Verfahren_gesamt

regData<-newdata[, c("effex02", "Verfahren_gesamt")]

#missings aussortieren
newdataclean <- newdata[!is.na(newdata$land),]
newdataclean <- newdata
newdataclean <- na.omit(newdata)
newdataclean <- newdata
sum(is.na(newdata))
#mittelwertzentrierung
#alter
newdataclean$age_mw <- newdataclean$age - mean(newdataclean$age)
#geschlecht
newdataclean$sex_mw <- newdataclean$sex - mean(newdataclean$sex)
#Arbeit - keine Arbeit
newdataclean$eseg_mw <- newdataclean$eseg - mean(newdataclean$eseg)
#allgemeiner schulabschluss
newdataclean$educ_mw <- newdataclean$educ - mean(newdataclean$educ)
#oder vlt einfacher: nach hochschulabschluss
newdataclean$de15_mw <- newdataclean$de15 - mean(newdataclean$de15)

#alter
newdata$age_mw <- newdata$age - mean(newdata$age)
#geschlecht
newdata$sex_mw <- newdata$sex - mean(newdata$sex)
#Arbeit - keine Arbeit
newdata$eseg_mw <- newdata$eseg - mean(newdata$eseg)
#allgemeiner schulabschluss
newdata$educ_mw <- newdata$educ - mean(newdata$educ)
#oder vlt einfacher: nach hochschulabschluss
newdata$de15_mw <- newdata$de15 - mean(newdata$de15)


#bereinigen falls nötig

#nullmodell
m0exeffv <- lme4::lmer(effex02 ~ 1 + (1|Verfahren_gesamt), data = regData)
#hier wird newdata genutzt da bei newdataclean der fehler "0 beobachtungen" kam
jtools::summ(m0exeffv)
#problem der ICC ist nur 0,02

regData<-newdata[, c("effex02", "Verfahren_gesamt",
                     "age_mw", "sex", "eseg", "de15", "land")]


#random intercept fixed slopes + level 1
rifs1dd <- lme4::lmer(effex02 ~ 1 + age_mw + eseg + sex
                      +de15 +(1|land), data = regData)
jtools::summ(rifs1dd)









