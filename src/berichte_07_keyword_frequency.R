spacy_initialize(model = "de_core_news_sm")

# Motivation und Persönlichkeit
parsed_motper <- spacy_parse(as.character(motper_clean))

parsed_motper <- parsed_motper %>% mutate(token = tolower(token), lemma = tolower(lemma))

## stopwords
parsed_motper <- parsed_motper %>% anti_join(tibble(token = stopwords("de", source = "stopwords-iso")))
## remove punctuation and other / select only adjectivs, adverbs, nouns, proper nouns, verbs
parsed_motper <- parsed_motper %>% filter(pos == "ADJ" | pos == "ADV" | pos == "NOUN" | pos == "PROPN")
stopwords_custom <- c("zug", "kanton", "%", "herr", "frau", "xy", "alt",
                      "kundin", "viamia", "gerne", "frage", "situation", "anliegen", "arbeiten")
parsed_motper <- parsed_motper %>% anti_join(tibble(token = stopwords_custom))

# count
motper_de_counts <- parsed_motper %>%
  count(lemma, sort = TRUE)

# Wissen und K.

parsed_wissen <- spacy_parse(as.character(wissen_clean))

parsed_wissen <- parsed_wissen %>% mutate(token = tolower(token), lemma = tolower(lemma))

## stopwords
parsed_wissen <- parsed_wissen %>% anti_join(tibble(token = stopwords("de", source = "stopwords-iso")))
## remove punctuation and other / select only adjectivs, adverbs, nouns, proper nouns, verbs
parsed_wissen <- parsed_wissen %>% filter(pos == "ADJ" | pos == "ADV" | pos == "NOUN" | pos == "PROPN")
stopwords_custom <- c("zug", "kanton", "%", "herr", "frau", "xy", "alt",
                      "kundin", "viamia", "gerne", "frage", "situation", "anliegen", "arbeiten")
parsed_wissen <- parsed_wissen %>% anti_join(tibble(token = stopwords_custom))

# count
wissen_de_counts <- parsed_wissen %>%
  count(lemma, sort = TRUE)


# Umfeld
parsed_umfeld <- spacy_parse(as.character(umfeld_clean))

parsed_umfeld <- parsed_umfeld %>% mutate(token = tolower(token), lemma = tolower(lemma))

## stopwords
parsed_umfeld <- parsed_umfeld %>% anti_join(tibble(token = stopwords("de", source = "stopwords-iso")))
## remove punctuation and other / select only adjectivs, adverbs, nouns, proper nouns, verbs
parsed_umfeld <- parsed_umfeld %>% filter(pos == "ADJ" | pos == "ADV" | pos == "NOUN" | pos == "PROPN")
stopwords_custom <- c("zug", "kanton", "%", "herr", "frau", "xy", "alt",
                      "kundin", "viamia", "gerne", "frage", "situation", "anliegen", "arbeiten")
parsed_umfeld <- parsed_umfeld %>% anti_join(tibble(token = stopwords_custom))

# count
umfeld_de_counts <- parsed_umfeld %>%
  count(lemma, sort = TRUE)

# Aktivitäten
parsed_aktivitaet <- spacy_parse(as.character(aktivitaet_clean))

parsed_aktivitaet <- parsed_aktivitaet %>% mutate(token = tolower(token), lemma = tolower(lemma))

## stopwords
parsed_aktivitaet <- parsed_aktivitaet %>% anti_join(tibble(token = stopwords("de", source = "stopwords-iso")))
## remove punctuation and other / select only adjectivs, adverbs, nouns, proper nouns, verbs
parsed_aktivitaet <- parsed_aktivitaet %>% filter(pos == "ADJ" | pos == "ADV" | pos == "NOUN" | pos == "PROPN")
stopwords_custom <- c("zug", "kanton", "%", "herr", "frau", "xy", "alt",
                      "kundin", "viamia", "gerne", "frage", "situation", "anliegen", "arbeiten")
parsed_aktivitaet <- parsed_aktivitaet %>% anti_join(tibble(token = stopwords_custom))

# count
aktivitaet_de_counts <- parsed_aktivitaet %>%
  count(lemma, sort = TRUE)

# Trends
parsed_trends <- spacy_parse(as.character(trends_clean))

parsed_trends <- parsed_trends %>% mutate(token = tolower(token), lemma = tolower(lemma))

## stopwords
parsed_trends <- parsed_trends %>% anti_join(tibble(token = stopwords("de", source = "stopwords-iso")))
## remove punctuation and other / select only adjectivs, adverbs, nouns, proper nouns, verbs
parsed_trends <- parsed_trends %>% filter(pos == "ADJ" | pos == "ADV" | pos == "NOUN" | pos == "PROPN")
stopwords_custom <- c("zug", "kanton", "%", "herr", "frau", "xy", "alt",
                      "kundin", "viamia", "gerne", "frage", "situation", "anliegen", "arbeiten")
parsed_trends <- parsed_trends %>% anti_join(tibble(token = stopwords_custom))

# count
trends_de_counts <- parsed_trends %>%
  count(lemma, sort = TRUE)

# Gesundheit
parsed_gesundh <- spacy_parse(as.character(gesundh_clean))

parsed_gesundh <- parsed_gesundh %>% mutate(token = tolower(token), lemma = tolower(lemma))

## stopwords
parsed_gesundh <- parsed_gesundh %>% anti_join(tibble(token = stopwords("de", source = "stopwords-iso")))
## remove punctuation and other / select only adjectivs, adverbs, nouns, proper nouns, verbs
parsed_gesundh <- parsed_gesundh %>% filter(pos == "ADJ" | pos == "ADV" | pos == "NOUN" | pos == "PROPN")
stopwords_custom <- c("zug", "kanton", "%", "herr", "frau", "xy", "alt",
                      "kundin", "viamia", "gerne", "frage", "situation", "anliegen", "arbeiten")
parsed_gesundh <- parsed_gesundh %>% anti_join(tibble(token = stopwords_custom))

# count
gesundh_de_counts <- parsed_gesundh %>%
  count(lemma, sort = TRUE)


# count all tokens (not lemmas)
wissen_token_de_counts <- parsed_wissen %>%
  count(token, sort = TRUE)
aktivitaet_token_de_counts <- parsed_aktivitaet %>%
  count(token, sort = TRUE)
trends_token_de_counts <- parsed_trends %>%
  count(token, sort = TRUE)
motper_token_de_counts <- parsed_motper %>%
  count(token, sort = TRUE)
gesundh_token_de_counts <- parsed_gesundh %>%
  count(token, sort = TRUE)

# output
counts_export <- cbind(wissen_de_counts[1:50,], motper_de_counts[1:50,], aktivitaet_de_counts[1:50,],
                       trends_de_counts[1:50,], gesundh_de_counts[1:50,])

counts_export %>% write.table(., "clipboard", sep="\t", row.names=FALSE)

counts_token_export <- cbind(wissen_token_de_counts[1:50,], motper_token_de_counts[1:50,], aktivitaet_token_de_counts[1:50,],
                       trends_token_de_counts[1:50,], gesundh_token_de_counts[1:50,])

counts_token_export %>% write.table(., "clipboard", sep="\t", row.names=FALSE)
