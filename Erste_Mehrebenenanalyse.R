#mehrebenennanalyse ab zeile 210
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
#Daten prüfen
#einlesen
allbus <- read.spss("/Users/soere/Documents/DirekteDemokratie/ZA5270_v2-0-0.sav",
                    use.value.labels = FALSE,
                    to.data.frame = TRUE,
                    reencode = TRUE)


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






# MERGING Datensätze zusammenführen anhand der Bundesländer

newdata <- merge(allbus, makrodata, by="land_fac", na.rm = "TRUE")


#Jede Person aus einem Bundesland hat jetzt die Werte des Bundeslandes auf der entsprechenden Makrovariable
newdata$Verfahren_gesamt

#subset erstellen
regData<-newdata%>%
  select(effex02, effint02, Verfahren_gesamt, age, sex, de15,land, eseg)
regData <- subset(newdata, select = c(effex02, effint02, Verfahren_gesamt, age, sex, de15,land, eseg))
regData <- na.omit(regData)



















#nullmodell
m0exeffv <- lme4::lmer(effex02 ~ 1 + (1|Verfahren_gesamt), data = regData)
#hier wird newdata genutzt da bei newdataclean der fehler "0 beobachtungen" kam
jtools::summ(m0exeffv)
#problem der ICC ist nur 0,02



#regression mit kontrollvariablen
freffc <- lm(effex02 ~ age + sex + eseg + de15, data = regData)

efhtr <- lm(effex02 ~ age + sex + eseg + de15, data = allbus)
summary(efhtr)

efitr <- lm(effint02 ~ age + sex + eseg + de15, data = allbus)
summary(efitr)

modeex2 <- lm(effex02 ~ pa01 + ps03 + rp01 + age + sex + eseg + de15, data = allbus)
summary(efhtr)

modeint2 <- lm(effint02 ~ pa01 + ps03 + rp01 + age + sex + eseg + de15, data = allbus)
summary(efitr)

#regressionstabelle
stargazer(efhtr, title = "Regressionsergebnisse", style = "default",
          decimal.mark = ",", out = "ersteregressionex.html")
stargazer(efitr, title = "Regressionsergebnisse", style = "default",
          decimal.mark = ",", out = "ersteregressionint.html")
stargazer(modeex2, title = "Regressionsergebnisse", style = "default",
          decimal.mark = ",", out = "ersteregression2ex.html")
stargazer(modeint2, title = "Regressionsergebnisse", style = "default",
          decimal.mark = ",", out = "ersteregression2int.html")












#mehrebenenanalyse
#external efficacy
#1 fixed slopes
#variablen auf mikroebene und gruppierungsvariable auf makroebene
#random intercept fixed slopes + level 1
m1 <- lme4::lmer(trust_dem ~ 1 + age_mw + soc_class_ged_mw + unempl + male +
                   edu_med + edu_high +(1|country), data = wvs)
jtools::summ(m1)

rifs1dd <- lme4::lmer(effex02 ~ 1 + age + eseg + sex
                      +de15 +(1|land), data = regData)
jtools::summ(rifs1dd)



#2 fixed slopes mit makrovariablen
#Random Intercept und fixed slopes + Level 1 Pr?diktoren + Level 2 Pr?diktoren 
m2 <- lme4::lmer(trust_dem ~ 1 + age_mw + soc_class_ged_mw + unempl +male +
                   edu_med + edu_high + dd_mw + (1|country), data = wvs)
jtools::summ(m2)

rifsl1l2 <- lme4::lmer(effex02 ~ 1 + age + eseg + sex
                       +de15 +Verfahren_gesamt +(1|land), data = regData)
jtools::summ(rifsl1l2)



#3 random slopes
#Random slopes soc-clas_ged_mw
m3_1 <- lme4::lmer(trust_dem ~ 1 + age_mw + soc_class_ged_mw + unempl + male+ 
                     edu_med + edu_high + dd_mw + (1 + soc_class_ged_mw|country), data = wvs)
#Random slopes male 
m3_2 <- lme4::lmer(trust_dem ~ 1 + age_mw + soc_class_ged_mw + unempl + male+
                     edu_med + edu_high + dd_mw + (1 + male|country), data = wvs)
summ(m3_1)
summ(m3_2)

#random slopes alter
rirsl1l2age <- lme4::lmer(effex02 ~ 1 + age + eseg + sex
                          +de15 +Verfahren_gesamt +(1 + age|land), data = regData)
jtools::summ(rirsl1l2age)
#random slopes eseg
rirsl1l2eseg <- lme4::lmer(effex02 ~ 1 + age + eseg + sex
                           +de15 +Verfahren_gesamt +(1 + eseg|land), data = regData)
jtools::summ(rirsl1l2eseg)
#random slopes sex
rirsl1l2sex <- lme4::lmer(effex02 ~ 1 + age + eseg + sex
                          +de15 +Verfahren_gesamt +(1 + sex|land), data = regData)
jtools::summ(rirsl1l2sex)
#random slopes de15
rirsl1l2de15 <- lme4::lmer(effex02 ~ 1 + age + eseg + sex
                           +de15 +Verfahren_gesamt +(1 + de15|land), data = regData)
jtools::summ(rirsl1l2de15)


#optional:
#4 random slopes und interaktionseffekte
#Random Intercept random slopes + Level 1 + Level 2 +interaktionseffekte
m4 <- lme4::lmer(trust_dem ~ 1 + age_mw + soc_class_ged_mw + unempl + male + 
                   edu_med + edu_high + dd_mw + dd_mw*edu_high+ (1 + soc_class_ged_mw|country), data = wvs)

jtools::summ(m4)
#random slopes de15 +interaktionseffekt
rirsl1l2de15V <- lme4::lmer(effex02 ~ 1 + age + eseg + sex
                            +de15 +Verfahren_gesamt + Verfahren_gesamt*de15 +(1 + sex|land), data = regData)
jtools::summ(rirsl1l2de15V)




#internal efficacy
#1 fixed slopes
#variablen auf mikroebene und gruppierungsvariable auf makroebene
#random intercept fixed slopes + level 1
eirifsmi <- lme4::lmer(effint02 ~ 1 + age + eseg + sex
                       +de15 +(1|land), data = regData)
jtools::summ(eirifsmi)


#2 fixed slopes mit makrovariablen
#Random Intercept und fixed slopes + Level 1 Pr?diktoren + Level 2 Pr?diktoren 
eirifsl1l2 <- lme4::lmer(effint02 ~ 1 + age + eseg + sex
                         +de15 +Verfahren_gesamt +(1|land), data = regData)
jtools::summ(eirifsl1l2)


#3 random slopes
#random slopes alter
eirirsl1l2age <- lme4::lmer(effint02 ~ 1 + age + eseg + sex
                            +de15 +Verfahren_gesamt +(1 + age|land), data = regData)
jtools::summ(eirirsl1l2age)
#random slopes eseg
eirirsl1l2eseg <- lme4::lmer(effint02 ~ 1 + age + eseg + sex
                             +de15 +Verfahren_gesamt +(1 + eseg|land), data = regData)
jtools::summ(eirirsl1l2eseg)
#random slopes sex
eirirsl1l2sex <- lme4::lmer(effint02 ~ 1 + age + eseg + sex
                            +de15 +Verfahren_gesamt +(1 + sex|land), data = regData)
jtools::summ(eirirsl1l2sex)
#random slopes de15
eirirsl1l2de15 <- lme4::lmer(effint02 ~ 1 + age + eseg + sex
                             +de15 +Verfahren_gesamt +(1 + de15|land), data = regData)
jtools::summ(eirirsl1l2de15)


#optional:
#4 random slopes und interaktionseffekte
#Random Intercept random slopes + Level 1 + Level 2 +interaktionseffekte
#random slopes de15 +interaktionseffekt
eirirsl1l2de15V <- lme4::lmer(effint02 ~ 1 + age + eseg + sex
                              +de15 +Verfahren_gesamt + Verfahren_gesamt*de15 +(1 + sex|land), data = regData)
jtools::summ(eirirsl1l2de15V)
