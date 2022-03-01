library(haven)
library(tidyverse)

setwd("~/GitHub/vm_public")

datapath <- "data/CRQ_Selbststaendige/CRQ Selbstständige_February 14, 2022_10.00.sav"

df_crqs <- read_sav(datapath) %>%
  filter(StartDate >= lubridate::as_datetime("2022-02-02 17:20:00", tz = "Europe/Zurich"))

df_crqs_postfilter <- df_crqs

# View(df_crqs)

names(df_crqs)

scalenames <- c("oexp", "jmk", "ssk", "inv", "con", "ccl", "jcha", "scs", "net", "cexpl", "lear")
crq_items <- df_crqs %>% select(starts_with(scalenames)) %>% select(-consent)
names(crq_items)


scalenames <- c("jsat", "csat", "weng")
cor_items <- df_crqs %>% select(starts_with(scalenames)) %>% select(-contains("timer"))
names(cor_items)

### Screenouts

# selbststaendig
df_selb <- df_crqs %>% filter(employ == 4)

df_nicht_selbststaendig <- anti_join(df_crqs, df_selb)

tic_nicht_selbststaendig <- as.data.frame(df_nicht_selbststaendig$tic)


# pensum
df_valides_pensum <- df_selb %>% filter(whours >= 16)

df_unter_16h <- anti_join(df_selb, df_valides_pensum)

tic_unter_16h <- as.data.frame(df_unter_16h$tic)



### careless / quality filter

## Time filter
# timer crq + timer weng + timer csat + timer jsat > anzahl items * 2s
zeitkriterium <- df_valides_pensum %>% select(contains("_Page_Submit"))
names(zeitkriterium)
# Erstellen des Timer_totals
zeitkriterium <- zeitkriterium %>% mutate(timer_total = crq_timer_1_Page_Submit + crq_timer_2_Page_Submit + crq_timer_3_Page_Submit + crq_timer_4_Page_Submit + crq_timer_5_Page_Submit + crq_timer_6_Page_Submit + crq_timer_7_Page_Submit + crq_timer_8_Page_Submit + crq_timer_9_Page_Submit +jsat_timer_Page_Submit + weng_timer_Page_Submit + csat_timer_Page_Submit)
# 55 Items zwischen CRQ und Korrelaten --> 55 * 2 = 110 --> 2 Sekunden pro Item. Erstellen einer Vergleichsspalte
zeitkriterium <- zeitkriterium %>% mutate(timer_soll = as.numeric(110))
# Beobachtungen, die länger als 110 Sekunden hatten, werden mit TRUE markiert.
zeitkriterium <- zeitkriterium %>% mutate(timer_test_pass = timer_soll < timer_total)
# Integration ins main dataframe
df_valides_pensum <- df_valides_pensum %>% mutate(timer_test_pass = zeitkriterium$timer_test_pass)


## Attention Checks --> 0/2 = Ausschluss --> 1/2 bedeutet ein Pass
attchks <- df_valides_pensum %>% select(contains("attchk"))
attchks <- attchks %>% mutate(attchk_pass = attchk_1 == 2 | attchk_2 == 5)
df_valides_pensum <- df_valides_pensum %>% mutate(attchk_pass = attchks$attchk_pass)

df_crqs_final <- df_valides_pensum %>% filter(timer_test_pass == TRUE & attchk_pass == TRUE)


df_careless <- anti_join(df_valides_pensum, df_crqs_final)

tic_careless <- as.data.frame(df_careless$tic)

tic_valid <- as.data.frame(df_crqs_final$tic)


tic_valid %>% write_excel_csv2(file = "data/tic_valide.csv")

tic_nicht_selbststaendig %>% write_excel_csv2(file = "data/tic_nicht_selbststaendige.csv")

tic_unter_16h %>% write_excel_csv2(file = "data/tic_selbstst_aber_zu_geringes_pensum.csv")

tic_careless %>% write_excel_csv2(file = "data/tic_selbstst_und_genuegendes_pensum_aber_careless.csv")

# Streuung Soziodemographisch
# Gender, Alter
sjlabelled::get_label(crq_items) %>% enframe() %>% View()
