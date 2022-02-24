# Berichte "debuggen" / Berichte die nicht 100% dem vorgegeben Format entsprechen (manuell) einlesen. Supplement zu 03_tidy

# Berichte aufbereiten / tidy

library(corpus)
library(tidytext)
library(stopwords)
library(spacyr)
library(tidyverse)
load("data/berichte_cleaned.RData")
# remove all line breaks in Anliegen
#TODO: expand to other sections. Vielleicht direkt auf das ganze Dokument anwenden?
# anliegen_text_de <- map(anliegen_text_de, ~str_flatten(.x, collapse = " ")) %>% str_remove_all(., fixed("-"))

source("src/Skript_var2_NAs_Alan.R")


# determine section positions
determine_section <- function(txt, sections, fuzzy = FALSE) {
  if (fuzzy == FALSE) {
    return(map_dbl(sections, function(x) {
      out <- which(txt == x)
      ifelse(length(out) == 0, NA, out)
    }))
  }
  map_dbl(sections, function(x) {
    out <-  str_which(txt, x)
    ifelse(length(out) == 0, NA, out)
  })
}


sections_de <- c("Anliegen/Fragestellung de",
                 "Standortbestimmung Laufbahnressourcen",
                 "Wissen und Kompetenzen",
                 "Motivation und Persönlichkeit",
                 "Umfeld",
                 "Unternommene Aktivitäten zur Laufbahngestaltung",
                 "Arbeitsmarkttrends",
                 "Gesundheit",
                 "Einschätzung der Arbeitsmarktfähigkeit anhand der Laufbahnressourcen",
                 "Handlungsempfehlungen",
                 "Ziele",
                 "Beratungsergebnis",
                 "Handlungsmöglichkeiten, weitere Schritte",
                 "Fazit aus Sicht de")

# bestimmen um welchen Berichttyp (mit oder oh ne weitere Beratung ) es sich handelt
# BUG: berücksichtig nicht die unterschiedliche Struktur der varianten, insofern Fazit aus Sicht ... an unterschiedlichen Stellen ist.
# Da der teil erstmal egal ist aber zur zeit unerheblicher bug.
sections_de_loop <- data.frame(start = sections_de[1:13], end = sections_de[2:length(sections_de)])
df_sections <- map2_dfr(sections_de_loop$start, sections_de_loop$end, function(start, end) {
  doc_start <- map_dbl(berichte_cleaned_de, ~determine_section(.x, start, fuzzy = TRUE))
  doc_end <- map_dbl(berichte_cleaned_de, ~determine_section(.x, end, fuzzy = TRUE))
  out <- data.frame(section = start, start = doc_start, end = doc_end) %>% rowid_to_column()
  return(out)
})

df_sections_nonfuzzy <- map2_dfr(sections_de_loop$start, sections_de_loop$end, function(start, end) {
  doc_start <- map_dbl(berichte_cleaned_de, ~determine_section(.x, start, fuzzy = FALSE))
  doc_end <- map_dbl(berichte_cleaned_de, ~determine_section(.x, end, fuzzy = FALSE))
  out <- data.frame(section = start, start = doc_start, end = doc_end) %>% rowid_to_column()
  return(out)
})

#integrate non-fuzzy ziele to other fuzzies - Ziele as Start: straightforward.
# Ziele as End (End to: Einschätzung der Arbeitsmarktfähigkeit (only Variante 2). This is not important - we don't look at AMT.

# View(cbind(df_sections_nonfuzzy %>% filter(section == "Ziele"),
#            df_sections %>% filter(section == "Ziele")))

df_sections$start[df_sections$section == "Ziele"] <- df_sections_nonfuzzy$start[df_sections$section == "Ziele"]

# # Anliegen
# View(df_sections %>% filter(section == sections_de[1]))
# # Ziele
# View(df_sections %>% filter(section == sections_de[11]))

df_sections_wide <- df_sections %>% pivot_wider(id_cols = rowid, names_from = section, values_from = c(start, end))


berichte_anliegen_start <- map_dbl(berichte_cleaned_de, ~determine_section(.x, "Anliegen/Fragestellung",
                                                                           fuzzy = TRUE))
berichte_anliegen_end <- map_dbl(berichte_cleaned_de, ~determine_section(.x, c("Standortbestimmung Laufbahnressourcen"),
                                                                         fuzzy = TRUE))
# which(is.na(berichte_anliegen_end))
berichte_anliegen_end[33] <- 6


# indicates the persons which have the full version of report (variante 2). does not seemt to work to well as berater adapt it...
variant2 <- data.frame(is.na(df_sections_wide$start_Beratungsergebnis), is.na(df_sections_wide$`start_Handlungsmöglichkeiten, weitere Schritte`),
           is.na(df_sections_wide$start_Ziele))
names(variant2) <- c("na.Beratungsergebnis", "na.Handlung", "na.Ziele")
variant2 <- variant2 %>% mutate(SumNA = rowSums(variant2)) %>%  rowid_to_column() 
if(sum(variant2 %>% filter(SumNA == 1 | SumNA == 2)) > 0) { warning("unclear variant 2s! (German")}

variant2_labels <- sapply(variant2$SumNA, function(x) {
  if(x == 3) {
    return("short")
  }  else if(x == 0) return("long")

    })
variant2_labels <- variant2_labels %>% as.factor()

# already done by alan: write_excel_csv(variant2, "data_export/variant2_na_de.csv")
# manual debugging of variant 2. 


length_berichte_anliegen <- berichte_anliegen_end - berichte_anliegen_start

# extract the lines we want
anliegen_text_export <- pmap(list(start = berichte_anliegen_start, end = berichte_anliegen_end, text =berichte_cleaned_de),
                             function(start,end, text) {
                               return(text[start:end])
})
# put into one character with [1] ... markers for lines (identification)
anliegen_text_export <- map2_dfr(anliegen_text_export, 1:length(anliegen_text_export), function(txt, i) {
  return(data.frame(identifier = identifier[i],
                        start = berichte_anliegen_start[i],
                        end = berichte_anliegen_end[i],
                        totalLines = length_berichte_anliegen[i],
                        line_index = berichte_anliegen_start[i]:berichte_anliegen_end[i],
                        anliegen_text = txt))
})

write_excel_csv(anliegen_text_export, "data_export/anliegen_text_de.csv")



# check high values
hist(length_berichte_anliegen)
which(length_berichte_anliegen > 15)

berichte_cleaned_de[[17]] # mixes other stuff in
berichte_cleaned_de[[113]] #korrekt
berichte_cleaned_de[[140]]



berichte_ressourcen_start <- map_dbl(berichte_cleaned_de, ~determine_section(.x, "Standortbestimmung Laufbahnressourcen",
                                                                           fuzzy = TRUE))
berichte_ressourcen_end <- map_dbl(berichte_cleaned_de, ~determine_section(.x, "Wissen und Kompetenzen",
                                                                         fuzzy = TRUE))
berichte_ressourcen_end2 <- map_dbl(berichte_cleaned_de, ~determine_section(.x, "Wissen und Kompetenzen|Wissen und Können",
                                                                           fuzzy = TRUE)) # some guy uses koennen
length_berichte_ressourcen <- berichte_ressourcen_end2 - berichte_ressourcen_start
which(is.na(length_berichte_ressourcen)) 
berichte_cleaned_de[[33]] # starts directly with Wissen und Kompetenzen


berichte_amt_start <-  map_dbl(berichte_cleaned_de, ~determine_section(.x, "Arbeitsmarkttrends",
                                                                           fuzzy = TRUE))
which(is.na(berichte_amt_start))




berichte_anliegen_start <- map_dbl(berichte_cleaned_de, ~determine_section(.x, "Anliegen/Fragestellung",
                                                                           fuzzy = TRUE))
berichte_anliegen_end <- map_dbl(berichte_cleaned_de, ~determine_section(.x, c("Standortbestimmung Laufbahnressourcen"),
                                                                         fuzzy = TRUE))





# FRENCH VERSION --------------
sections_fr <- c("Demande/questions",
                 "Analyse de la situation en matière de ressources de carrière",
                 "Connaissances et compétences",
                 "Motivation et personnalité",
                 "Contexte",
                 "Activités de gestion de carrière entreprises",
                 "Tendances du marché du travail",
                 "Santé",
                 "Évaluation de l’employabilité",
                 "Actions recommandées",
                 "Objectifs",
                 "Résultats du conseil",
                 "Possibilités d’action",
                 "Conclusion du point de vue")

# bestimmen um welchen Berichttyp (mit oder oh ne weitere Beratung ) es sich handelt
sections_fr_loop <- data.frame(start = sections_fr[1:13], end = sections_fr[2:length(sections_fr)])
df_sections <- map2_dfr(sections_fr_loop$start, sections_fr_loop$end, function(start, end) {
  doc_start <- map_dbl(berichte_cleaned_fr, ~determine_section(.x, start, fuzzy = TRUE))
  doc_end <- map_dbl(berichte_cleaned_fr, ~determine_section(.x, end, fuzzy = TRUE))
  out <- data.frame(section = start, start = doc_start, end = doc_end) %>% rowid_to_column()
  return(out)
})

df_sections_nonfuzzy <- map2_dfr(sections_fr_loop$start, sections_fr_loop$end, function(start, end) {
  doc_start <- map_dbl(berichte_cleaned_fr, ~determine_section(.x, start, fuzzy = FALSE))
  doc_end <- map_dbl(berichte_cleaned_fr, ~determine_section(.x, end, fuzzy = FALSE))
  out <- data.frame(section = start, start = doc_start, end = doc_end) %>% rowid_to_column()
  return(out)
})


#integrate non-fuzzy ziele to other fuzzies - Ziele as Start: straightforward.
# Ziele as End (End to: Einschätzung der Arbeitsmarktfähigkeit (only Variante 2). This is not important - we don't look at AMT.

df_sections$start[df_sections$section == "Objectifs"] <- df_sections_nonfuzzy$start[df_sections$section == "Objectifs"]

df_sections_wide <- df_sections %>% pivot_wider(id_cols = rowid, names_from = section, values_from = c(start, end))

# indicates the persons which have the full version of report (variante 2). does not seemt to work to well as berater adapt it...
variant2_fr <- data.frame(is.na(df_sections_wide$`start_Résultats du conseil`), is.na(df_sections_wide$`start_Possibilités d’action`),
                       is.na(df_sections_wide$start_Objectifs))
names(variant2_fr) <- c("na.Beratungsergebnis", "na.Handlung", "na.Ziele")
variant2_fr <- variant2_fr %>% mutate(SumNA = rowSums(variant2_fr)) %>%  rowid_to_column() 
write_excel_csv(variant2_fr, "data_export/variant2_na_fr.csv")


berichte_demande_start <- map_dbl(berichte_cleaned_fr, ~determine_section(.x, sections_fr[1],
                                                                             fuzzy = TRUE))
berichte_demande_end <- map_dbl(berichte_cleaned_fr, ~determine_section(.x, sections_fr[2],
                                                                           fuzzy = TRUE))

berichte_demande_start[249] <- 4
berichte_demande_start[263] <- 3
berichte_demande_end[216] <- 14


which(is.na(berichte_demande_start))
which(is.na(berichte_demande_end))

# write_excel_csv(data.frame(berichte_cleaned_fr[[249]]), file = "data_export/fr_249.csv", col_names = FALSE)
# write_excel_csv(data.frame(berichte_cleaned_fr[[263]]), file = "data_export/fr_263.csv", col_names = FALSE)
# write_excel_csv(data.frame(berichte_cleaned_fr[[216]]), file = "data_export/fr_216.csv", col_names = FALSE)

length_berichte_demande <- berichte_demande_end - berichte_demande_start

# extract the lines we want
demande_text_export <- pmap(list(start = berichte_demande_start, end = berichte_demande_end, text =berichte_cleaned_fr),
                             function(start,end, text) {
                               return(text[start:end])
                             })
# put into one character with [1] ... markers for lines (identification)
demande_text_export <- map2_dfr(demande_text_export, 1:length(demande_text_export), function(txt, i) {
  return(data.frame(identifier = identifier[i],
                    start = berichte_demande_start[i],
                    end = berichte_demande_end[i],
                    totalLines = length_berichte_demande[i],
                    line_index = berichte_demande_start[i]:berichte_demande_end[i],
                    demande_text = txt))
})




# berichte_amtfr_start <- map_dbl(berichte_cleaned_fr, ~determine_section(.x, sections_fr[7],
#                                                                           fuzzy = TRUE))
# which(is.na(berichte_amtfr_start))
