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
# anliegen_text_de <- map(anliegen_text_de, ~str_flatten(.x, collapse = " ")) %>% str_remove_all(., fixed("-"))

berichte_cleaned_de <- berichte_cleaned_de_integrated
rm(berichte_cleaned_de_integrated)

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
  start_section <- map_dbl(berichte, ~determine_section(.x, section_1,
                                                  fuzzy_1))
  end_section <- map_dbl(berichte, ~determine_section(.x, section_2,
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


# DEUTSCHE VERSION --------
anliegen_text_de <- tidy_txtsection(berichte_cleaned_de, "Anliegen/Fragestellung", "Standortbestimmung Laufbahnressourcen")
ziele_text_de <- tidy_txtsection(berichte_cleaned_de, "Ziele", "Beratungsergebnis", FALSE, FALSE)
moeglichkeiten_text_de <- tidy_txtsection(berichte_cleaned_de, "Handlungsmöglichkeiten, weitere Schritte", "Fazit aus Sicht",
                                          FALSE, TRUE)
empfehlungen_text_de <- tidy_txtsection(berichte_cleaned_de, "Handlungsempfehlungen", "Fazit aus Sicht",
                                          FALSE, TRUE)



# Spacy Nouns

spacy_initialize(model = "de_core_news_sm")
anliegen_spacyd <- spacy_parse(as.character(anliegen_text_de))
ziele_spacyd <- spacy_parse(as.character(ziele_text_de))
moeglichkeiten_spacyd <- spacy_parse(as.character(moeglichkeiten_text_de))
empfehlungen_spacyd <- spacy_parse(as.character(empfehlungen_text_de))

anliegen_nounphrases <- spacy_extract_nounphrases(as.character(anliegen_text_de))
moeglichkeiten_nounphrases <- spacy_extract_nounphrases(as.character(moeglichkeiten_text_de))
ziele_nounphrases <- spacy_extract_nounphrases(as.character(ziele_text_de))
empfehlungen_nounphrases <- spacy_extract_nounphrases(as.character(empfehlungen_text_de))


spacy_finalize()

# Cleaning -----

## lowercasing
stopwords_custom <- c("zug", "kanton", "%", "herr", "frau", "xy", "alt",
                      "kundin", "viamia", "gerne", "w?nschen", "frage", "situation", "anliegen", "arbeiten",
                      "fragestellung", "kunde", "ziel", "<U+F0B7>", "U+F0B7", "", "z.b.", "person", "xxx")


anliegen_spacyd <- anliegen_spacyd %>% mutate(token = tolower(token), lemma = tolower(lemma)) %>% 
  anti_join(tibble(lemma = stopwords("de", source = "stopwords-iso"))) %>%  #stopwords
  anti_join(tibble(lemma = stopwords_custom)) %>% 
  filter(pos == "NOUN" | pos == "PROPN")# only nouns

ziele_spacyd <- ziele_spacyd %>% mutate(token = tolower(token), lemma = tolower(lemma)) %>% 
  anti_join(tibble(lemma = stopwords("de", source = "stopwords-iso"))) %>%  #stopwords
  anti_join(tibble(lemma = stopwords_custom)) %>% 
  filter(pos == "NOUN" | pos == "PROPN")# only nouns

moeglichkeiten_spacyd <- moeglichkeiten_spacyd %>% mutate(token = tolower(token), lemma = tolower(lemma)) %>% 
  anti_join(tibble(lemma = stopwords("de", source = "stopwords-iso"))) %>%  #stopwords
  anti_join(tibble(lemma = stopwords_custom)) %>% 
  filter(pos == "NOUN" | pos == "PROPN")# only nouns

empfehlungen_spacyd <- empfehlungen_spacyd %>% mutate(token = tolower(token), lemma = tolower(lemma)) %>% 
  anti_join(tibble(lemma = stopwords("de", source = "stopwords-iso"))) %>%  #stopwords
  anti_join(tibble(lemma = stopwords_custom)) %>% 
  filter(pos == "NOUN" | pos == "PROPN")# only nouns


anliegen_nounphrases <-
  anliegen_nounphrases %>% mutate(text = tolower(text), root_text = tolower(root_text)) %>%
  anti_join(tibble(root_text = stopwords("de", source = "stopwords-iso"))) %>%
  anti_join(tibble(root_text = stopwords_custom))
ziele_nounphrases <-
  ziele_nounphrases %>% mutate(text = tolower(text), root_text = tolower(root_text)) %>%
  anti_join(tibble(root_text = stopwords("de", source = "stopwords-iso"))) %>%
  anti_join(tibble(root_text = stopwords_custom))
moeglichkeiten_nounphrases <-
  moeglichkeiten_nounphrases %>% mutate(text = tolower(text), root_text = tolower(root_text)) %>%
  anti_join(tibble(root_text = stopwords("de", source = "stopwords-iso"))) %>%
  anti_join(tibble(root_text = stopwords_custom))
empfehlungen_nounphrases <-
  empfehlungen_nounphrases %>% mutate(text = tolower(text), root_text = tolower(root_text)) %>%
  anti_join(tibble(root_text = stopwords("de", source = "stopwords-iso"))) %>%
  anti_join(tibble(root_text = stopwords_custom))

# save
save(anliegen_spacyd, empfehlungen_spacyd, moeglichkeiten_spacyd, ziele_spacyd,
     anliegen_nounphrases, ziele_nounphrases, moeglichkeiten_nounphrases, 
     empfehlungen_nounphrases,
     file = "cache/sektionen_spacyd.RData")
