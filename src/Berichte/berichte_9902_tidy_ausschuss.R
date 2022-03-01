

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





# Parsing with spacy(R) ------

spacy_initialize(model = "de_core_news_sm")
parsed_ziele <- spacy_parse(as.character(ziele_text_de))

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

parsed_nounphrase <- spacy_extract_nounphrases(as.character(ziele_text_de))
spacy_finalize()

# Cleaning -----

## lowercasing
parsed_ziele <- parsed_ziele %>% mutate(token = tolower(token), lemma = tolower(lemma))

## stopwords
parsed_ziele <- parsed_ziele %>% anti_join(tibble(token = stopwords("de", source = "stopwords-iso")))
## remove punctuation and other / select only adjectivs, adverbs, nouns, proper nouns, verbs
parsed_ziele <- parsed_ziele %>% filter(pos == "ADJ" | pos == "ADV" | pos == "NOUN" | pos == "PROPN" | pos == "VERB")
# count
ziele_de_counts <- parsed_ziele %>%
  count(lemma, sort = TRUE)

# nur substantive
ziele_de_noun_counts <- parsed_ziele %>%
  filter(pos == "NOUN") %>% count(lemma, sort = TRUE)
dfm_zielenouns <- as.tokens(parsed_ziele %>%
                              filter(pos == "NOUN"), use_lemma = TRUE) %>% dfm() %>%
  dfm_trim(min_termfreq = 4, max_termfreq = 200)
topfeatures(dfm_zielenouns, 50)
textplot_wordcloud(dfm_zielenouns, min_count = 20)


#remove further words
#TODO: mehr Worte einfügen.
stopwords_custom <- c("zug", "kanton", "%", "herr", "frau", "xy", "alt", "kundin", "viamia", "gerne", "w?nschen", "frage", "situation", "anliegen", "arbeiten")
parsed_ziele <- parsed_ziele %>% anti_join(tibble(token = stopwords_custom))

# 4. Constructing document-feature matrix
library(quanteda)

dfm_ziele <- as.tokens(parsed_ziele, use_lemma = TRUE) %>% dfm() %>%
  dfm_trim(min_termfreq = 4, max_termfreq = 200)
#head(dfm_anliegen)
topfeatures(dfm_ziele, 50)
library(quanteda.textplots)
# textplot_wordcloud(dfm_ziele, min_count = 50)



#head(dfm_anliegen)
topfeatures_df <- topfeatures(dfm_ziele, 100) %>% as_tibble(rownames = "wort")


# ziele nounphrases

ziele_nounphrase_count <-
  parsed_nounphrase %>% mutate(root_text = tolower(root_text)) %>%
  count(root_text, sort = TRUE) %>% 
  anti_join(tibble(root_text = stopwords("de", source = "stopwords-iso"))) %>%
  anti_join(tibble(root_text = stopwords_custom))