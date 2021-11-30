# anliegen
# requires berichte_02_tidy

# 4. Constructing document-feature matrix
library(quanteda)
library(quanteda.textplots)

# top features
dfm_obj <- as.tokens(anliegen_spacyd, use_lemma = TRUE) %>% dfm() %>%
  dfm_trim(min_termfreq = 10, max_termfreq = 999)
#head(dfm_anliegen)


topfeatures(dfm_obj, 100) %>% write.table(., "clipboard", sep="\t", row.names=TRUE)



wordcloud_generator <- function(text, nwords) {
  dfm_obj <- as.tokens(text, use_lemma = TRUE) %>% dfm() %>%
    dfm_trim(min_termfreq = 10, max_termfreq = 999)
  #head(dfm_anliegen)
  min_count <- topfeatures(dfm_obj, nwords)[nwords]
  textplot_wordcloud(dfm_obj, min_count = min_count)
}

# anliegen
wordcloud_generator(anliegen_spacyd, 30)
wordcloud_generator(ziele_spacyd, 30)
wordcloud_generator(empfehlungen_spacyd, 30)
wordcloud_generator(moeglichkeiten_spacyd, 30)


#head(dfm_anliegen)
topfeatures_df <- topfeatures(dfm_anliegen, 100) %>% as_tibble(rownames = "wort")
save(topfeatures_df, file = "topfeatures_df.RData")