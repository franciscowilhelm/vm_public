# Bericht Nr. 3
berichte_cleaned_de[[3]][[59]] <- "Handlungsempfehlungen"

# Bericht Nr. 8
# Handlung* im falschen Feld; Titel wird hier angepasst
berichte_cleaned_de[[8]][[76]] <- "Handlungsmöglichkeiten, weitere Schritte"
x <- c(berichte_cleaned_de[[8]][1:75],
       "Beratungsergebnis",
       "",
       berichte_cleaned_de[[8]][76:85])
berichte_cleaned_de[[8]] <- x
rm(x)

# Bericht Nr. 13
# Ergebnis und Handlung zusammengefügt --> Umbenennen in Beratungsergebnis
# Text kann im Excel nachgeschaut werden --> Falls nötig, können auch beide
# Punkte verworfen werden.
ind_ber = 13
ind_zeile = 79
berichte_cleaned_de[[ind_ber]][[ind_zeile]] <- "Beratungsergebnis"
x <- c(berichte_cleaned_de[[ind_ber]][1:88],
       "Handlungsmöglichkeiten, weitere Schritte",
       "",
       berichte_cleaned_de[[ind_ber]][89:92])
berichte_cleaned_de[[ind_ber]] <- x
rm(x)

# Bericht Nr. 33
# Handlungsmöglichkeiten fehlen --> Titel eingefügt, aber leer gelassen
ind_ber = 33
x <- c(berichte_cleaned_de[[ind_ber]][1:47],
       "Handlungsmöglichkeiten, weitere Schritte",
       "",
       berichte_cleaned_de[[ind_ber]][48:51])
berichte_cleaned_de[[ind_ber]] <- x
rm(x)

# Bericht Nr. 37
# Ziele fehlen --> Titel eigefügt, leer gelassen
ind_ber = 37
x <- c(berichte_cleaned_de[[ind_ber]][1:35],
       "Ziele",
       "",
       berichte_cleaned_de[[ind_ber]][36:46])
berichte_cleaned_de[[ind_ber]] <- x
rm(x)

# Bericht Nr. 43
# Ergebnis und Handlung zusammengefügt
x <- c(berichte_cleaned_de[[43]][1:51], 
        "Handlungsmöglichkeiten, weitere Schritte", 
        berichte_cleaned_de[[43]][52:57])
berichte_cleaned_de[[43]] <- x
rm(x)
# Lösung: Einschieben von Handlungsmöglichkeiten. 
# Neu: Beratungsergebnis: 
#   - Die nächsten Laufbahnschritte sind klar
# Handlungs[...]: 
#   - Wechsel des Ressorts intern (November 21)
#   - Start Modul 1 der BSLB-Ausbildung im Herbst 21

# Bericht Nr. 48
# Handlung umbenannt
ind_ber = 48
ind_zeile = 75
berichte_cleaned_de[[ind_ber]][[ind_zeile]] <- "Handlungsempfehlungen"

# Bericht Nr. 55
# Variante 2, aber Ziele fehlen (siehe Bericht 37)
ind_ber = 55
x <- c(berichte_cleaned_de[[ind_ber]][1:49],
       "Ziele",
       "",
       berichte_cleaned_de[[ind_ber]][50:63])
berichte_cleaned_de[[ind_ber]] <- x
rm(x)

# Bericht Nr. 60
# Ergebnis und Handlung kombiniert; Ziele umbenannt
ind_ber <- 60
ind_zeile_1 <- 76
ind_zeile_2 <- 81
berichte_cleaned_de[[ind_ber]][[ind_zeile_1]] <- "Ziele"
berichte_cleaned_de[[ind_ber]][[ind_zeile_2]] <- "Beratungsergebnis"
x <- c(berichte_cleaned_de[[ind_ber]][1:93],
       "Handlungsmöglichkeiten, weitere Schritte",
       "",
       berichte_cleaned_de[[ind_ber]][94:99])
berichte_cleaned_de[[ind_ber]] <- x
rm(x)

# Bericht Nr. 62
# Ergebnis und Handlung kombiniert
ind_ber <- 62
ind_zeile <- 48
berichte_cleaned_de[[ind_ber]][[ind_zeile]] <- "Beratungsergebnis"
# Einfügen des Titels Handlungsmöglichkeiten[...], aber leerlassen
x <- c(berichte_cleaned_de[[ind_ber]][1:50],
       "Handlungsmöglichkeiten, weitere Schritte",
       "",
       berichte_cleaned_de[[ind_ber]][51:53])
berichte_cleaned_de[[ind_ber]] <- x
rm(x)

# Bericht Nr. 71
# Ergebnis und Handlung kombiniert
ind_ber <- 71
ind_zeile <- 81
berichte_cleaned_de[[ind_ber]][[ind_zeile]] <- "Beratungsergebnis"
# Umsortieren der Sätze, um Handlungsmögl. sauber einzufügen
berichte_cleaned_de[[ind_ber]][[89]] <- "zufrieden mit Ihrer aktuellen Stelle. Trotzdem wollen Sie sich herumschauen, was auf dem Arbeitsmarkt möglich ist."
berichte_cleaned_de[[ind_ber]][[90]] <- "Sie haben sich intern als- Junior Business Analyst beworben und"
x <- c(berichte_cleaned_de[[ind_ber]][1:89],
       "Handlungsmöglichkeiten, weitere Schritte",
        berichte_cleaned_de[[ind_ber]][90:100])
berichte_cleaned_de[[ind_ber]] <- x
rm(x)

# Bericht Nr. 113
# Ziele umbenannt
ind_ber <- 113
ind_zeile <- 81
berichte_cleaned_de[[ind_ber]][[ind_zeile]] <- "Ziele"

# Bericht Nr. 118
# Ziele umbenannt
ind_ber <- 118
ind_zeile <- 51
berichte_cleaned_de[[ind_ber]][[ind_zeile]] <- "Ziele"

# Bericht Nr. 120
# Ziele umbenannt
ind_ber <- 120
ind_zeile <- 58
berichte_cleaned_de[[ind_ber]][[ind_zeile]] <- "Ziele"

# Bericht Nr. 123
# Ziele umbenannt
ind_ber <- 123
ind_zeile <- 65
berichte_cleaned_de[[ind_ber]][[ind_zeile]] <- "Ziele"

# Bericht Nr. 129
# Ziele und Handlung umbenannt
ind_ber <- 129
ind_zeile_1 <- 53
ind_zeile_2 <- 67
berichte_cleaned_de[[ind_ber]][[ind_zeile_1]] <- "Ziele"
berichte_cleaned_de[[ind_ber]][[ind_zeile_2]] <- "Handlungsmöglichkeiten, weitere Schritte"

# Bericht Nr. 131
# Ziele und Handlung umbenannt
ind_ber <- 131
ind_zeile_1 <- 57
ind_zeile_2 <- 71
berichte_cleaned_de[[ind_ber]][[ind_zeile_1]] <- "Ziele"
berichte_cleaned_de[[ind_ber]][[ind_zeile_2]] <- "Handlungsmöglichkeiten, weitere Schritte"

# Bericht Nr. 132
# Ziele umbenannt
ind_ber <- 132
ind_zeile <- 63
berichte_cleaned_de[[ind_ber]][[ind_zeile]] <- "Ziele"

# Bericht Nr. 134
# Ziele und Handlung umbenannt
ind_ber <- 134
ind_zeile_1 <- 56
ind_zeile_2 <- 66
berichte_cleaned_de[[ind_ber]][[ind_zeile_1]] <- "Ziele"
berichte_cleaned_de[[ind_ber]][[ind_zeile_2]] <- "Handlungsmöglichkeiten, weitere Schritte"

# Bericht Nr. 137
# Ziele und Handlung umbenannt
ind_ber <- 137
ind_zeile_1 <- 48
ind_zeile_2 <- 58
berichte_cleaned_de[[ind_ber]][[ind_zeile_1]] <- "Ziele"
berichte_cleaned_de[[ind_ber]][[ind_zeile_2]] <- "Handlungsmöglichkeiten, weitere Schritte"

# Bericht Nr. 138
# Handlung umbenannt
ind_ber <- 138
ind_zeile <- 59
berichte_cleaned_de[[ind_ber]][[ind_zeile]] <- "Handlungsempfehlungen"

# Bericht Nr. 140
ind_ber <- 140
ind_zeile <- 84
berichte_cleaned_de[[ind_ber]][[ind_zeile]] <- "Ziele"

# Bericht Nr. 141
ind_ber <- 141
ind_zeile <- 52
berichte_cleaned_de[[ind_ber]][[ind_zeile]] <- "Ziele"

# Bericht Nr. 148
ind_ber <- 148
ind_zeile <- 62
berichte_cleaned_de[[ind_ber]][[ind_zeile]] <- "Ziele"

# Bericht Nr. 270
ind_ber <- 270
ind_zeile <- 22
berichte_cleaned_de[[ind_ber]][[ind_zeile]] <- "Ziele"
#entfernen einer überflüssigen Zeile, die zu Problemen führen könnte: the lazy way
berichte_cleaned_de[[270]][[21]] <- ""

