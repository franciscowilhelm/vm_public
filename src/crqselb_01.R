library(haven)
library(tidyverse)
df_crqs <- read_sav("data/CRQ_Selbstaendige/CRQ Selbstständige_February 1, 2022_10.28.sav")
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
