# Berichte von den Rohdaten (PDF/Word) einlesen.

library(pdftools)
library(readtext)
library(stringr)
library(tidyverse)

berichte_path <- file.path("C:/Users/zoot6/OneDrive - Universitaet Bern/Other Projects/viamia_analysis/data/Berichte/1121/")

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


identifier_1121 <- c(files, files_word)
kanton_1121 <- c(kanton_pdf, kanton_word)

lang_idx <- ifelse(grepl("BernFR|Fribourg|Geneve|Jura|ValaisFR|Vaud",kanton_1121), "fr", "de")

identifier_1121_de <- identifier_1121[lang_idx == "de"]
identifier_1121_fr <- identifier_1121[lang_idx == "fr"]

kanton_1121_de <- kanton_1121[lang_idx == "de"]
kanton_1121_fr <- kanton_1121[lang_idx == "fr"]

berichte_cleaned_de <- berichte_cleaned[lang_idx == "de"]
berichte_cleaned_fr <- berichte_cleaned[lang_idx == "fr"]


berichte_cleaned_de_1121 <- berichte_cleaned_de
berichte_cleaned_fr_1121 <- berichte_cleaned_fr



save(berichte_cleaned_de_1121, berichte_cleaned_fr_1121,
     identifier_1121_de, identifier_1121_fr,
     kanton_1121_de, kanton_1121_fr,
     file = "data/berichte_cleaned_1121.RData")
