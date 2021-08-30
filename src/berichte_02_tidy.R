# Berichte aufbereiten / tidy

library(corpus)
library(tidytext)
library(stopwords)
library(spacyr)
library(tidyverse)



# 1. PREREQUISITES ------

load("data/berichte_cleaned.RData")
# remove all line breaks in Anliegen
#TODO: expand to other sections. Vielleicht direkt auf das ganze Dokument anwenden?
# anliegen_text_de <- map(anliegen_text_de, ~str_flatten(.x, collapse = " ")) %>% str_remove_all(., fixed("-"))


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

# DEUTSCHE VERSION --------
berichte_anliegen_start <- map_dbl(berichte_cleaned_de, ~determine_section(.x, "Anliegen/Fragestellung",
                                                                           fuzzy = TRUE))
berichte_anliegen_end <- map_dbl(berichte_cleaned_de, ~determine_section(.x, c("Standortbestimmung Laufbahnressourcen"),
                                                                         fuzzy = TRUE))
berichte_anliegen_end[33] <- 6 #see debug_berichte

anliegen_text_de <- pmap(list(berichte_cleaned_de, berichte_anliegen_start, berichte_anliegen_end),  function(x, start, end) {
  x[(start+1):(end-1)]
})

# if ends on hyphen, remove hyphen and collapse. if not, collapse with space.
source("lib/custom_flattener.R")
anliegen_text_de <- map(anliegen_text_de, function(doc) {
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

anliegen_text_de <- str_remove_all(anliegen_text_de, fixed("-"))

# 2. Parsing with spacy(R) ------

spacy_initialize(model = "de_core_news_sm")
parsed_anliegen <- spacy_parse(as.character(anliegen_text_de))

# parsed_anliegen_alt <- spacy_parse(as.character(anliegen_text_de),
#                                    tag = TRUE,
#                                    nounphrase = TRUE,
#                                    dependency = TRUE)
# tmp <- map(unique(parsed_anliegen_alt$doc_id), function(doc) {
#   filter(parsed_anliegen_alt, doc_id == doc) %>% 
#     map(unique(.$sentence_id), function(sentence) {
#       filter()
#       return(data.frame(doc_id == doc, sentence_id = sentence, nvphrase = nvphrase))
#     }
# })

parsed_nounphrase <- spacy_extract_nounphrases(as.character(anliegen_text_de))
spacy_finalize()

# 3. Cleaning -----

## lowercasing
parsed_anliegen <- parsed_anliegen %>% mutate(token = tolower(token), lemma = tolower(lemma))

## stopwords
parsed_anliegen <- parsed_anliegen %>% anti_join(tibble(token = stopwords("de", source = "stopwords-iso")))
## remove punctuation and other / select only adjectivs, adverbs, nouns, proper nouns, verbs
parsed_anliegen <- parsed_anliegen %>% filter(pos == "ADJ" | pos == "ADV" | pos == "NOUN" | pos == "PROPN" | pos == "VERB")
# count
anliegen_de_counts <- parsed_anliegen %>%
  count(lemma, sort = TRUE)

#remove further words
#TODO: mehr Worte einf√ºgen.
stopwords_custom <- c("zug", "kanton", "%", "herr", "frau", "xy", "alt", "kundin", "viamia", "gerne", "w¸nschen", "frage", "situation", "anliegen", "arbeiten")
parsed_anliegen <- parsed_anliegen %>% anti_join(tibble(token = stopwords_custom))

# 4. Constructing document-feature matrix
library(quanteda)

dfm_anliegen <- as.tokens(parsed_anliegen, use_lemma = TRUE) %>% dfm() %>%
  dfm_trim(min_termfreq = 4, max_termfreq = 100)
#head(dfm_anliegen)
topfeatures(dfm_anliegen, 50)
library(quanteda.textplots)
textplot_wordcloud(dfm_anliegen, min_count = 20)



#head(dfm_anliegen)
topfeatures_df <- topfeatures(dfm_anliegen, 100) %>% as_tibble(rownames = "wort")
save(topfeatures_df, file = "topfeatures_df.RData")


# french version

# anliegen_text_fr_df <- map2_dfr(anliegen_text_fr, seq_along(anliegen_text_fr), function(x,y) {
#   tibble(id = y, text = x)
# })
# 
# anliegen_text_fr_df <- anliegen_text_fr_df %>%
#   unnest_tokens(word, text) %>% 
#   anti_join(tibble(word = stopwords("fr", source = "stopwords-iso")))
# 
# anliegen_fr_counts <- anliegen_text_fr_df %>%
#   count(word, sort = TRUE)
# 
# 
# 
# # tidytext v ersion
# anliegen_text_de <- pmap(list(berichte_cleaned_de, berichte_anliegen_start, berichte_anliegen_end),  function(x, start, end) {
#   x[[1]][(start+1):(end-1)]
# })
# 
# anliegen_text_de_df <- map2_dfr(anliegen_text_de, seq_along(anliegen_text_de), function(x,y) {
#   tibble(id = y, text = x)
# })
# 
# 
# 
# anliegen_text_de_df <- anliegen_text_de_df %>%
#   unnest_tokens(word, text) %>% 
#   anti_join(tibble(word = stopwords("de", source = "stopwords-iso")))
# 
# # stemming
# anliegen_text_de_df$word_stem <- stem_snowball(anliegen_text_de_df$word, algorithm = "de")
# 
# # filter weitere stopwords.
# anliegen_de_counts <- anliegen_text_de_df %>%
#   count(word_stem, sort = TRUE)


