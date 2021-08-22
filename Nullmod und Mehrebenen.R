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
library(stargazer)
#einlesen
allbus <- read.spss("/Users/soere/Documents/DirekteDemokratie/ZA5270_v2-0-0.sav",
                    use.value.labels = FALSE,
                    to.data.frame = TRUE,
                    reencode = TRUE)

#Makrodaten aus einer exel tabelle
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

#indices erstellen
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



#####Indexs bilden und Index dann zentrieren  #######
#Nach meinem Test zeigt summary() für beide Methoden das gleiche an

# Indizes bilden (additiv)
allbus$effex02 <- allbus$pe01 + allbus$pe05.gedreht

allbus$effint02 <- allbus$pe02.gedreht + allbus$pe04 + allbus$pe06

#Index mittelwertzentrieren
allbus$effex02.mwz <- scale(allbus$effex02, center = T, scale = F)

summary(allbus$effex02.mwz)  #Veranschaulichung 


allbus$effint02.mwz <- scale(allbus$effint02, center = T, scale = F)

summary(allbus$effint02.mwz)



# MERGING Datensätze zusammenführen anhand der Bundesländer
newdata <- merge(allbus, makrodata, by="land_fac", na.rm = "TRUE")


#Jede Person aus einem Bundesland hat jetzt die Werte des Bundeslandes auf der entsprechenden Makrovariable
newdata$Verfahren_gesamt

#unemployed variable kodieren
newdata$arbeit <- recode(newdata$work, "1:3=1; 4=2")
#1: erwerbstätig 2: nicht erwerbstätig

#subset erstellen
regData <- subset(newdata, select = c(effex02, effint02, Verfahren_gesamt, age, sex, de15,land, eseg, pa01, ps03, rp01))
regData <- na.omit(regData)

#Mittelwertzentr der metr variablen
regData$effex02.mw <- regData$effex02 - mean(regData$effex02)
regData$effint02.mw <- regData$effint02 - mean(regData$effint02)
regData$Verfahren_gesamt.mw <- regData$Verfahren_gesamt - mean(regData$Verfahren_gesamt)
regData$age.mw <- regData$age - mean(regData$age)
regData$linksrechts.mw <- regData$pa01 -mean(regData$pa01)
regData$dzufr.mw <- regData$ps03 - mean(regData$ps03)
regData$kirchgang.mw <- regData$rp01 - mean(regData$rp01)

#dummyvariablen mit factor
class(newdata$eseg)
class(newdata$sex)
class(newdata$de15)
regData$unempl <- as.factor(regData$eseg)
regData$geschlecht <- as.factor(regData$sex)
regData$hochschulabschluss <- as.factor(regData$de15)

#nochmal missings entfernen sicherheitshalber
regData <- na.omit(regData)

#subset erstellen
regData2 <- subset(newdata, select = c(effex02.mw, effint02.mw, Verfahren_gesamt.mw, age.mw, geschlecht, hochschulabschluss,land, unempl, kirchgang.mw, linksrechts.mw, dzufr.mw))
regData2 <- na.omit(regData2)




#nullmodell
#f?r externe efficacy
m0exeffv <- lme4::lmer(effex02 ~ 1 + (1|land), data = regData)
#f?r interne efficacy
m0inteffv <- lme4::lmer(effint02 ~ 1 + (1|land), data = regData)
jtools::summ(m0exeffv)
#icc 0,03
jtools::summ(m0inteffv)
#icc 0,01



#mehrebenenanalyse
#external efficacy
#1 fixed slopes
#variablen auf mikroebene und gruppierungsvariable auf makroebene
#random intercept fixed slopes + level 1
rifs1dd2 <- lme4::lmer(effex02 ~ 1 + age.mw + unempl + geschlecht
                      +hochschulabschluss + linksrechts.mw + dzufr.mw + kirchgang.mw +(1|land), data = regData)
jtools::summ(rifs1dd2)



#2 fixed slopes mit makrovariablen
#Random Intercept und fixed slopes + Level 1 Pr?diktoren + Level 2 Pr?diktoren 
rifsl1l22 <- lme4::lmer(effex02 ~ 1 + age.mw + unempl + geschlecht
                       +hochschulabschluss + linksrechts.mw + dzufr.mw + kirchgang.mw +Verfahren_gesamt +(1|land), data = regData)
jtools::summ(rifsl1l22)



#3 random slopes
#random slopes alter
rirsl1l2age2 <- lme4::lmer(effex02 ~ 1 + age.mw + unempl + geschlecht
                           +hochschulabschluss + linksrechts.mw + dzufr.mw + kirchgang.mw +Verfahren_gesamt +(1 + age|land), data = regData)
jtools::summ(rirsl1l2age2)
#random slopes eseg
rirsl1l2eseg2 <- lme4::lmer(effex02 ~ 1 + age.mw + unempl + geschlecht
                            +hochschulabschluss + linksrechts.mw + dzufr.mw + kirchgang.mw +Verfahren_gesamt +(1 + eseg|land), data = regData)
jtools::summ(rirsl1l2eseg2)
#random slopes sex
rirsl1l2sex2 <- lme4::lmer(effex02 ~ 1 + age.mw + unempl + geschlecht
                           +hochschulabschluss + linksrechts.mw + dzufr.mw + kirchgang.mw +Verfahren_gesamt +(1 + sex|land), data = regData)
jtools::summ(rirsl1l2sex2)
#random slopes de15
rirsl1l2de152 <- lme4::lmer(effex02 ~ 1 + age.mw + unempl + geschlecht
                            +hochschulabschluss + linksrechts.mw + dzufr.mw + kirchgang.mw +Verfahren_gesamt +(1 + de15|land), data = regData)
jtools::summ(rirsl1l2de152)


#optional:
#4 random slopes und interaktionseffekte
#Random Intercept random slopes + Level 1 + Level 2 +interaktionseffekte
#random slopes de15 +interaktionseffekt
rirsl1l2de15V2 <- lme4::lmer(effex02 ~ 1 + age.mw + unempl + geschlecht
                             +hochschulabschluss + linksrechts.mw + dzufr.mw + kirchgang.mw +Verfahren_gesamt + Verfahren_gesamt*de15 +(1 + sex|land), data = regData)
jtools::summ(rirsl1l2de15V2)




#internal efficacy
#1 fixed slopes
#variablen auf mikroebene und gruppierungsvariable auf makroebene
#random intercept fixed slopes + level 1
eirifsmi2 <- lme4::lmer(effint02 ~ 1 + age.mw + unempl + geschlecht
                        +hochschulabschluss + linksrechts.mw + dzufr.mw + kirchgang.mw +(1|land), data = regData)
jtools::summ(eirifsmi2)


#2 fixed slopes mit makrovariablen
#Random Intercept und fixed slopes + Level 1 Pr?diktoren + Level 2 Pr?diktoren 
eirifsl1l22 <- lme4::lmer(effint02 ~ 1 + age.mw + unempl + geschlecht
                          +hochschulabschluss + linksrechts.mw + dzufr.mw + kirchgang.mw +Verfahren_gesamt +(1|land), data = regData)
jtools::summ(eirifsl1l22)


#3 random slopes
#random slopes alter
eirirsl1l2age2 <- lme4::lmer(effint02 ~ 1 + age.mw + unempl + geschlecht
                             +hochschulabschluss + linksrechts.mw + dzufr.mw + kirchgang.mw +Verfahren_gesamt +(1 + age|land), data = regData)
jtools::summ(eirirsl1l2age2)
#random slopes eseg
eirirsl1l2eseg2 <- lme4::lmer(effint02 ~ 1 + age.mw + unempl + geschlecht
                              +hochschulabschluss + linksrechts.mw + dzufr.mw + kirchgang.mw +Verfahren_gesamt +(1 + eseg|land), data = regData)
jtools::summ(eirirsl1l2eseg2)
#random slopes sex
eirirsl1l2sex2 <- lme4::lmer(effint02 ~ 1 + age.mw + unempl + geschlecht
                             +hochschulabschluss + linksrechts.mw + dzufr.mw + kirchgang.mw +Verfahren_gesamt +(1 + sex|land), data = regData)
jtools::summ(eirirsl1l2sex2)
#random slopes de15
eirirsl1l2de152 <- lme4::lmer(effint02 ~ 1 + age.mw + unempl + geschlecht
                              +hochschulabschluss + linksrechts.mw + dzufr.mw + kirchgang.mw +Verfahren_gesamt +(1 + de15|land), data = regData)
jtools::summ(eirirsl1l2de152)


#optional:
#4 random slopes und interaktionseffekte
#Random Intercept random slopes + Level 1 + Level 2 +interaktionseffekte
#random slopes de15 +interaktionseffekt
eirirsl1l2de15V2 <- lme4::lmer(effint02 ~ 1 + age.mw + unempl + geschlecht
                               +hochschulabschluss + linksrechts.mw + dzufr.mw + kirchgang.mw +Verfahren_gesamt + Verfahren_gesamt*de15 +(1 + sex|land), data = regData)
jtools::summ(eirirsl1l2de15V2)
