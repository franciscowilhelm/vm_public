library(corpus)
library(tidytext)
library(stopwords)
library(spacyr)
library(tidyverse)

load("data/berichte_cleaned_integration.RData")

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
sektionen_standortbestimmung <- tibble(
  x1 = map_dbl(
    berichte_cleaned_de,
    ~ determine_section(.x, "Wissen und Kompetenzen",
                        fuzzy = FALSE)
  ),
  x2 = map_dbl(
    berichte_cleaned_de,
    ~ determine_section(.x, "Motivation und Persönlichkeit",
                        fuzzy = FALSE)
  ),
  x3 = map_dbl(
    berichte_cleaned_de,
    ~ determine_section(.x, "Umfeld",
                        fuzzy = FALSE)
  ),
  x4 = map_dbl(
    berichte_cleaned_de,
    ~ determine_section(.x, "Unternommene Aktivitäten zur Laufbahngestaltung",
                        fuzzy = FALSE)
  ),
  x5 = map_dbl(
    berichte_cleaned_de,
    ~ determine_section(.x, "Arbeitsmarkttrends",
                        fuzzy = FALSE)
  ),
  x6 = map_dbl(
    berichte_cleaned_de,
    ~ determine_section(.x, "Gesundheit",
                        fuzzy = FALSE)
  ),
  x7 = map_dbl(
    berichte_cleaned_de,
    ~ determine_section(.x, "Einschätzung der Arbeitsmarktfähigkeit",
                        fuzzy = TRUE)
  )
)

wissen <- pmap(list(berichte_cleaned_de, sektionen_standortbestimmung$x1, sektionen_standortbestimmung$x2),  function(x, start, end) {
  if(!is.na(start) & !is.na(end)) {
    x[(start+1):(end-1)]
  }
  else {
    NA
  }
})
# filter NA from list
wissen <- wissen %>%
  map(discard, is.na) %>%
  compact()

# if ends on hyphen, remove hyphen and collapse. if not, collapse with space.
source("lib/custom_flattener.R")
wissen_clean <- map(wissen, function(doc) {
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

wissen_clean <- str_remove_all(wissen_clean, fixed("-"))

sample(wissen_clean, 1)
