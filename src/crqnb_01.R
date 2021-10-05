library(tidyverse)
source("https://raw.githubusercontent.com/franciscowilhelm/r-collection/master/scoreItemsMulti.R")
library(haven)
df <- read_sav("data/CRQ Nicht-Erwerbstaetige_October 1, 2021_10.49.sav")

scalenames <- c("oe", "jmk", "ssk", "inv", "con", "ccl", "scs", "net", "cexpl", "lear")

x <- map_dfc(scalenames, function(scl) df %>% select(starts_with(scl))) 

sjlabelled::get_label(x) %>% as.data.frame() %>% write.table(., "clipboard", sep="\t", row.names=FALSE)

# crq_scales <- map(scalenames, function(x) {
#   df <- crq %>% select(contains(x))
#   psych::scoreItems(keys =rep(1, ncol(df)),
#                     df, impute = "none")
# })

crq_scales <- scoreItemsMulti(scalenames, df, exclude = TRUE)
crq_scales$alpha
df_crq <- crq_scales$scores %>% as.data.frame()

# hierarchical scores
crq_scales_h <- psych::scoreItems(list(knsk = c("oe", "jmk", "ssk"),
                                       mot = c("inv", "con", "cl"),
                                       env = c("cop", "os", "jcha", "scs"),
                                       act = c("net", "cexpl", "lear")),
                                  df_crq)

df_crq <- bind_cols(df_crq,
                    crq_scales_h$scores %>% as.data.frame())
