library(haven)
library(tidyverse)
df_crqs <- read_sav("data/CRQ_Selbstaendige/CRQ Selbstständige_February 3, 2022_10.38.sav") %>%
  filter(StartDate >= lubridate::as_datetime("2022-02-02 17:20:00", tz = "Europe/Zurich")) %>% 
  filter(employ == 4 & whours >= 16)
# View(df_crqs)

names(df_crqs)

scalenames <- c("oexp", "jmk", "ssk", "inv", "con", "ccl", "jcha", "scs", "net", "cexpl", "lear")
crq_items <- df_crqs %>% select(starts_with(scalenames)) %>% select(-consent)
names(crq_items)


scalenames <- c("jsat", "csat", "weng")
cor_items <- df_crqs %>% select(starts_with(scalenames)) %>% select(-contains("timer"))
names(cor_items)

# careless / quality filter

# Time filter
# timer crq + timer weng + timer csat + timer jsat > anzahl items * 2s

# beide attention checks failed auch ausschluss

# wenn beide filter angewendet, dann sortieren nach DE / CH
# valide CH mind. 80


# Streuung Soziodemographisch
# Gender, Alter
sjlabelled::get_label(crq_items) %>% enframe() %>% View()
