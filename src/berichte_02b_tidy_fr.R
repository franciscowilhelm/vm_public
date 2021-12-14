# Berichte aufbereiten / tidy

library(corpus)
library(tidytext)
library(stopwords)
library(spacyr)
library(tidyverse)



# 1. PREREQUISITES ------

load("data/berichte_cleaned_integrated.RData")
# remove all line breaks in Anliegen
#TODO: expand to other sections. Vielleicht direkt auf das ganze Dokument anwenden?
# anliegen_text_fr <- map(anliegen_text_fr, ~str_flatten(.x, collapse = " ")) %>% str_remove_all(., fixed("-"))

berichte_cleaned_fr <- berichte_cleaned_fr_integrated
rm(berichte_cleaned_fr_integrated)

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

# tidy sections

tidy_txtsection <- function(berichte, section_1, section_2, fuzzy_1 = TRUE, fuzzy_2 = TRUE) {
  start_section <- map_dbl(berichte_cleaned_fr, ~determine_section(.x, section_1,
                                                  fuzzy_1))
  end_section <- map_dbl(berichte_cleaned_fr, ~determine_section(.x, section_2,
                                                                 fuzzy_2))
  df_startend <- data.frame(start = start_section, end = end_section) %>% 
    mutate(na_present = complete.cases(.))
  
  
  
  tidied_section <- pmap(list(berichte,
                              df_startend$start,
                              df_startend$end,
                              df_startend$na_present),
                           function(x, start, end, na_present) {
                             # some reports are shorter than expected, code them
                             short <- (start+1) == (end-1)
                             if(na_present == FALSE) {
                               return("")
                             } else if(short) {
                               x[start:end-1]
                             } else if(short == FALSE) {
                               x[(start+1):(end-1)]
                             }
                             
                           })
  # if ends on hyphen, remove hyphen and collapse. if not, collapse with space.
  source("lib/custom_flattener.R")
  tidied_section <- map(tidied_section, function(doc) {
    # does the line end with a hyphen?
    hyphen_end <- map_lgl(doc, function(line) {
      lastchar <- str_sub(line, start = -1, end = -1) # whats the last character of the line
      if(lastchar == "-") {
        return(TRUE)
      }
      else return(FALSE)
    })
    out <- if(length(doc) == 1) {
      return(doc)
    } else {
      return(custom_flattener(doc, hyphen_end))
    }
    return(out)
  })
  
  tidied_section <- str_remove_all(tidied_section, fixed("-"))
  return(tidied_section)
  
}


# FRZ VERSION --------
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


anliegen_text_fr <- tidy_txtsection(berichte_cleaned_fr, "Demande/questions",
                                    "Analyse de la situation en matière de ressources de carrière",
                                    TRUE, FALSE)
ziele_text_fr <- tidy_txtsection(berichte_cleaned_fr, "Objectifs", "Résultats du conseil", FALSE, FALSE)
moeglichkeiten_text_fr <- tidy_txtsection(berichte_cleaned_fr, "Possibilités d’action", "Conclusion du point de vue",
                                          TRUE, TRUE)
empfehlungen_text_fr <- tidy_txtsection(berichte_cleaned_fr, "Actions recommandées", "Conclusion du point de vue",
                                          FALSE, TRUE)


# Spacy Nouns

spacy_initialize(model = "fr_core_news_lg")
anliegen_spacyd <- spacy_parse(as.character(anliegen_text_fr))
anliegen_nounphrases <- spacy_extract_nounphrases(as.character(anliegen_text_fr))

ziele_spacyd <- spacy_parse(as.character(ziele_text_fr))
moeglichkeiten_spacyd <- spacy_parse(as.character(moeglichkeiten_text_fr))
empfehlungen_spacyd <- spacy_parse(as.character(empfehlungen_text_fr))

moeglichkeiten_nounphrases <- spacy_extract_nounphrases(as.character(moeglichkeiten_text_fr))
ziele_nounphrases <- spacy_extract_nounphrases(as.character(ziele_text_fr))
empfehlungen_nounphrases <- spacy_extract_nounphrases(as.character(empfehlungen_text_fr))

spacy_finalize()

# Cleaning -----

## lowercasing



anliegen_spacyd <- anliegen_spacyd %>% mutate(token = tolower(token), lemma = tolower(lemma)) %>% 
  anti_join(tibble(lemma = stopwords("fr", source = "stopwords-iso"))) %>%  #stopwords
  filter(pos == "NOUN" | pos == "PROPN")# only nouns

anliegen_nounphrases <- anliegen_nounphrases %>% mutate(text = tolower(text), root_text = tolower(root_text)) %>%
  anti_join(tibble(root_text = stopwords("fr", source = "stopwords-iso")))

# anliegen_nounphrases %>% count(root_text, sort = TRUE) %>% head(30)

stopwords_custom <- c("madame", "j’", "j'", "ans", "mme", "click")
anliegen_nounphrases <- anliegen_nounphrases %>% anti_join(tibble(root_text = stopwords_custom))


ziele_spacyd <- ziele_spacyd %>% mutate(token = tolower(token), lemma = tolower(lemma)) %>% 
  anti_join(tibble(lemma = stopwords("fr", source = "stopwords-iso"))) %>%  #stopwords
  anti_join(tibble(lemma = stopwords_custom)) %>% 
  filter(pos == "NOUN" | pos == "PROPN")# only nouns

ziele_nounphrases <- ziele_nounphrases %>% mutate(text = tolower(text), root_text = tolower(root_text)) %>%
  anti_join(tibble(root_text = stopwords("fr", source = "stopwords-iso"))) %>% anti_join(tibble(root_text = stopwords_custom))




ziele_nounphrases <-
  ziele_nounphrases %>% mutate(text = tolower(text), root_text = tolower(root_text)) %>%
  anti_join(tibble(root_text = stopwords("fr", source = "stopwords-iso"))) %>%
  anti_join(tibble(root_text = stopwords_custom))
moeglichkeiten_nounphrases <-
  moeglichkeiten_nounphrases %>% mutate(text = tolower(text), root_text = tolower(root_text)) %>%
  anti_join(tibble(root_text = stopwords("fr", source = "stopwords-iso"))) %>%
  anti_join(tibble(root_text = stopwords_custom))
empfehlungen_nounphrases <-
  empfehlungen_nounphrases %>% mutate(text = tolower(text), root_text = tolower(root_text)) %>%
  anti_join(tibble(root_text = stopwords("fr", source = "stopwords-iso"))) %>%
  anti_join(tibble(root_text = stopwords_custom))


moeglichkeiten_spacyd <- moeglichkeiten_spacyd %>% mutate(token = tolower(token), lemma = tolower(lemma)) %>% 
  anti_join(tibble(lemma = stopwords("fr", source = "stopwords-iso"))) %>%  #stopwords
  anti_join(tibble(lemma = stopwords_custom)) %>% 
  filter(pos == "NOUN" | pos == "PROPN")# only nouns

empfehlungen_spacyd <- empfehlungen_spacyd %>% mutate(token = tolower(token), lemma = tolower(lemma)) %>% 
  anti_join(tibble(lemma = stopwords("fr", source = "stopwords-iso"))) %>%  #stopwords
  anti_join(tibble(lemma = stopwords_custom)) %>% 
  filter(pos == "NOUN" | pos == "PROPN")# only nouns

# moeglichkeiten_spacyd %>% 
# count(lemma, sort = TRUE) %>% head(50)
# 
# empfehlungen_spacyd %>% 
#   count(lemma, sort = TRUE) %>% head(50)


# anliegen_nounphrase_count <-
#   anliegen_nounphrases %>% mutate(root_text = tolower(root_text)) %>%
#   count(root_text, sort = TRUE) %>% 
#   anti_join(tibble(root_text = stopwords("de", source = "stopwords-iso"))) %>%
#   anti_join(tibble(root_text = stopwords_custom))


# save
save(anliegen_spacyd, anliegen_nounphrases, empfehlungen_spacyd, moeglichkeiten_spacyd, ziele_spacyd, file = "cache/sektionen_spacyd_fr.RData")

