# Berichte von den Rohdaten (PDF/Word) einlesen.

library(pdftools)
library(readtext)
library(stringr)
library(tidyverse)

berichte_path <- file.path("C:/Users/zoot6/OneDrive - Universitaet Bern/Other Projects/viamia_analysis/data/Berichte/0721/")

files <- list.files(path = berichte_path, pattern = "pdf$", recursive = TRUE)
kanton_pdf <- str_extract(files, "(\\w)+(?=\\/)")
files <- str_c(berichte_path, "/", files)
berichte_pdf <- lapply(files, pdf_text)
berichte_pdf_alt <- lapply(files, readtext)


files_word <- list.files(path = berichte_path, pattern = "docx$", recursive = TRUE)
kanton_word <- str_extract(files_word, "(\\w)+(?=\\/)")
files_word <- str_c(berichte_path, "/", files_word)

berichte_doc <- lapply(files_word, readtext)

# functions for cleaning text
clean_bericht_pdf <- function(txt) {
  out <- str_split(txt, fixed('\r')) %>% 
    str_split(., fixed('\n')) %>% 
    map(., ~str_remove(.x, fixed('\n')))
  out %>%
    map(., ~str_remove(.x, fixed('\n'))) %>% map(., ~str_trim(., side = "both"))
}

clean_bericht_doc <- function(txt) {
  out <- str_split(txt, fixed('\n')) %>%  map(., ~str_remove(.x, fixed('\n')))
  out %>% map(., ~str_remove(.x, fixed('\n'))) %>% map(., ~str_trim(., side = "both"))
}


berichte_pdf_munge <- map(berichte_pdf_alt, ~.x$text)
berichte_pdf_cleaned <- map(berichte_pdf_munge, clean_bericht_pdf)
# map_dbl(berichte_pdf_cleaned, ~length(.x[[1]])) #verify text are broken into reasonable length (failure would be length 1)

berichte_doc_munge <- map(berichte_doc, ~.x$text)
berichte_doc_cleaned <- map(berichte_doc_munge, clean_bericht_doc)
# map_dbl(berichte_doc_cleaned, ~length(.x[[1]])) #verify text are broken into reasonable length (failure would be length 1)

berichte_cleaned <- c(berichte_pdf_cleaned, berichte_doc_cleaned) #merge doc, pdf
# simplify lists (its unnecesssarily lested)
berichte_cleaned <- map(berichte_cleaned, function(txt) {
  txt[[1]]
})


identifier <- c(files, files_word)
kanton <- c(kanton_pdf, kanton_word)

berichte_cleaned_de <- berichte_cleaned[grepl("BL|BS|BernDE|Freiburg|ValaisDE|Zug|Zürich",kanton)]
# # french lang
berichte_cleaned_fr <- berichte_cleaned[grepl("BernFR|Fribourg|Geneve|ValaisFR|Vaud",kanton)] # Bern, Zug

berichte_cleaned_de_0721 <- berichte_cleaned_de
berichte_cleaned_fr_0721 <- berichte_cleaned_fr
kanton_0721 <- kanton
identifier_0721 <- identifier


save(berichte_cleaned_de_0721, berichte_cleaned_fr_0721, identifier_0721, kanton_0721, file = "data/berichte_cleaned_0721.RData")

load("data/berichte_cleaned.RData")

# berichte_cleaned_de <- c(berichte_cleaned_de, berichte_cleaned_de_0721)
# berichte_cleaned_fr <- c(berichte_cleaned_fr, berichte_cleaned_fr_0721)
# kanton <- c(kanton, kanton_0721)
# identifier <- c(identifier, identifier_0721)
# 
# save(berichte_cleaned_de, berichte_cleaned_fr, identifier, kanton, file = "data/berichte_cleaned_integration.RData")
