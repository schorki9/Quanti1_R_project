





#Überprüfung der Kontrollvariablen

#1. Wie viele Na prozentual? 
#2. Reihenfolge logisch : Viel Zustimmung -> hohe Werte? Ansonsten drehen
#3. Kategorische Variablen zu Faktoren machen und Levels prüfen
#4. Für Regression alle in Frage kommenden Variablen mittelwertzentrieren





# Alter
table(allbus$age, useNA = "always")
summary(allbus$age, useNA = "always")

describe(allbus$age)

#Links Rechts Skala = pa01
#1 = Links 10 = rechts
#schon gewichtet? Mit wghtpew laut Codebook S.140
table(allbus$pa01, useNA = "always")

sum(is.na(allbus$pa01))/length(allbus$pa01) #Na Anteil = 5%


