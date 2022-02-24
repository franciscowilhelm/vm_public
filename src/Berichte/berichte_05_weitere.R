load("data/berichte_cleaned.RData")
library(corpus)
library(tidytext)
library(stopwords)
library(spacyr)

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
sections_de_loop <- data.frame(start = sections_de[1:14], end = sections_de[1:length(sections_de)])
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

df_sections_wide <- df_sections %>% pivot_wider(id_cols = rowid, names_from = section, values_from = c(start, end))

# ziele_idx <-  df_sections_wide %>% select(rowid, start_Ziele, start_Beratungsergebnis) %>% 
#   filter(!is.na(start_Ziele) & !is.na(start_Beratungsergebnis))
# 
# ziele_text_export <- pmap(list(start = ziele_idx$start_Ziele,
#                                end = ziele_idx$start_Beratungsergebnis,
#                                text = berichte_cleaned_de[ziele_idx$rowid]),
#                              function(start,end, text) {
#                                return(text[(start+1):(end-1)])
#                              })



handlungsempf_idx <-  df_sections_wide %>% select(rowid, `start_Handlungsmöglichkeiten, weitere Schritte`,
                                                  `start_Fazit aus Sicht de`) %>% 
  filter(!is.na(`start_Handlungsmöglichkeiten, weitere Schritte`) & !is.na(`start_Fazit aus Sicht de`))

handl_text_export <- pmap(list(start = handlungsempf_idx$`start_Handlungsmöglichkeiten, weitere Schritte`,
                               end = handlungsempf_idx$`start_Fazit aus Sicht de`,
                               text = berichte_cleaned_de[handlungsempf_idx$rowid]),
                          function(start,end, text) {
                            return(text[(start+1):(end-1)])
                          })


# if ends on hyphen, remove hyphen and collapse. if not, collapse with space.
source("lib/custom_flattener.R")
handl_text_flattened <- map(handl_text_export, function(doc) {
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

handl_text_flattened <- str_remove_all(handl_text_flattened, fixed("-"))

# 2. Parsing with spacy(R) ------

spacy_initialize(model = "de_core_news_sm")
parsed_handl <- spacy_parse(as.character(handl_text_flattened))

parsed_nounphrase <- spacy_extract_nounphrases(as.character(anliegen_text_de))
spacy_finalize()

# 3. Cleaning -----

## lowercasing
parsed_handl <- parsed_handl %>% mutate(token = tolower(token), lemma = tolower(lemma))

## stopwords
parsed_handl <- parsed_handl %>% anti_join(tibble(token = stopwords("de", source = "stopwords-iso")))
## remove punctuation and other / select only adjectivs, adverbs, nouns, proper nouns, verbs
parsed_handl <- parsed_handl %>% filter(pos == "ADJ" | pos == "ADV" | pos == "NOUN" | pos == "PROPN" | pos == "VERB")
# count
handl_de_counts <- parsed_handl %>%
  count(lemma, sort = TRUE) %>%
  mutate(n / 75)

#remove further words
#TODO: mehr Worte einfügen.
stopwords_custom <- c("zug", "kanton", "%", "herr", "frau", "xy", "alt", "kundin", "viamia", "gerne", "w?nschen", "frage", "situation", "anliegen", "arbeiten")
parsed_anliegen <- parsed_anliegen %>% anti_join(tibble(token = stopwords_custom))

# 4. Constructing document-feature matrix
library(quanteda)

dfm_anliegen <- as.tokens(parsed_anliegen, use_lemma = TRUE) %>% dfm() %>%
  dfm_trim(min_termfreq = 4, max_termfreq = 100)
#head(dfm_anliegen)
topfeatures(dfm_anliegen, 50)
library(quanteda.textplots)
textplot_wordcloud(dfm_anliegen, min_count = 20)


