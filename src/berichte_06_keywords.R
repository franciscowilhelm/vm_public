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

#
##
### Wissen und Kompetenzen
##
#

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

#
##
### Motivation und Persönlichkeit
##
#

motper <- pmap(list(berichte_cleaned_de, sektionen_standortbestimmung$x2, sektionen_standortbestimmung$x3),  function(x, start, end) {
  if(!is.na(start) & !is.na(end)) {
    x[(start+1):(end-1)]
  }
  else {
    NA
  }
})

# filter NA from list
motper <- motper %>%
  map(discard, is.na) %>%
  compact()

# if ends on hyphen, remove hyphen and collapse. if not, collapse with space.
source("lib/custom_flattener.R")
motper_clean <- map(wissen, function(doc) {
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

motper_clean <- str_remove_all(motper_clean, fixed("-"))

#
##
### Umfeld
##
#

umfeld <- pmap(list(berichte_cleaned_de, sektionen_standortbestimmung$x3, sektionen_standortbestimmung$x4),  function(x, start, end) {
  if(!is.na(start) & !is.na(end)) {
    x[(start+1):(end-1)]
  }
  else {
    NA
  }
})

# filter NA from list
umfeld <- umfeld %>%
  map(discard, is.na) %>%
  compact()

# if ends on hyphen, remove hyphen and collapse. if not, collapse with space.
source("lib/custom_flattener.R")
umfeld_clean <- map(umfeld, function(doc) {
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

umfeld_clean <- str_remove_all(umfeld_clean, fixed("-"))

#
##
### Unternommene Aktivitäten zur Laufbahngestaltung
##
#

aktivitaet <- pmap(list(berichte_cleaned_de, sektionen_standortbestimmung$x4, sektionen_standortbestimmung$x5),  function(x, start, end) {
  if(!is.na(start) & !is.na(end)) {
    x[(start+1):(end-1)]
  }
  else {
    NA
  }
})

# filter NA from list
aktivitaet <- aktivitaet %>%
  map(discard, is.na) %>%
  compact()

# if ends on hyphen, remove hyphen and collapse. if not, collapse with space.
source("lib/custom_flattener.R")
aktivitaet_clean <- map(aktivitaet, function(doc) {
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

aktivitaet_clean <- str_remove_all(aktivitaet_clean, fixed("-"))

#
##
### Arbeitsmarkttrends
##
#

trends <- pmap(list(berichte_cleaned_de, sektionen_standortbestimmung$x5, sektionen_standortbestimmung$x6),  function(x, start, end) {
  if(!is.na(start) & !is.na(end)) {
    x[(start+1):(end-1)]
  }
  else {
    NA
  }
})

# filter NA from list
trends <- trends %>%
  map(discard, is.na) %>%
  compact()

# if ends on hyphen, remove hyphen and collapse. if not, collapse with space.
source("lib/custom_flattener.R")
trends_clean <- map(trends, function(doc) {
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

trends_clean <- str_remove_all(trends_clean, fixed("-"))

#
##
### Gesundheit
##
#

gesundh <- pmap(list(berichte_cleaned_de, sektionen_standortbestimmung$x6, sektionen_standortbestimmung$x7),  function(x, start, end) {
  if(!is.na(start) & !is.na(end)) {
    x[(start+1):(end-1)]
  }
  else {
    NA
  }
})

# filter NA from list
gesundh <- gesundh %>%
  map(discard, is.na) %>%
  compact()

# if ends on hyphen, remove hyphen and collapse. if not, collapse with space.
source("lib/custom_flattener.R")
gesundh_clean <- map(gesundh, function(doc) {
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

gesundh_clean <- str_remove_all(gesundh_clean, fixed("-"))


sample(wissen_clean, 1)

sample(motper_clean, 1)

sample(umfeld_clean, 1)

sample(aktivitaet_clean, 1)

sample(trends_clean, 1)

sample(gesundh_clean, 1)