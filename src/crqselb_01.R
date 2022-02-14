library(haven)
library(tidyverse)

setwd("~/GitHub/vm_public")

datapath <- "data/CRQ_Selbststaendige/CRQ Selbstständige_February 14, 2022_10.00.sav"

df_crqs_prefilter <- read_sav(datapath) %>% 
  filter(StartDate >= lubridate::as_datetime("2022-02-02 17:20:00", tz = "Europe/Zurich"))

df_crqs <- read_sav(datapath) %>%
  filter(StartDate >= lubridate::as_datetime("2022-02-02 17:20:00", tz = "Europe/Zurich")) %>% 
  filter(employ == 4 & whours >= 16)

df_crqs_postfilter <- df_crqs

# View(df_crqs)

names(df_crqs)

scalenames <- c("oexp", "jmk", "ssk", "inv", "con", "ccl", "jcha", "scs", "net", "cexpl", "lear")
crq_items <- df_crqs %>% select(starts_with(scalenames)) %>% select(-consent)
names(crq_items)


scalenames <- c("jsat", "csat", "weng")
cor_items <- df_crqs %>% select(starts_with(scalenames)) %>% select(-contains("timer"))
names(cor_items)

### careless / quality filter

## Time filter
# timer crq + timer weng + timer csat + timer jsat > anzahl items * 2s
zeitkriterium <- df_crqs %>% select(contains("_Page_Submit"))
names(zeitkriterium)
# Erstellen des Timer_totals
zeitkriterium <- zeitkriterium %>% mutate(timer_total = crq_timer_1_Page_Submit + crq_timer_2_Page_Submit + crq_timer_3_Page_Submit + crq_timer_4_Page_Submit + crq_timer_5_Page_Submit + crq_timer_6_Page_Submit + crq_timer_7_Page_Submit + crq_timer_8_Page_Submit + crq_timer_9_Page_Submit +jsat_timer_Page_Submit + weng_timer_Page_Submit + csat_timer_Page_Submit)
# 55 Items zwischen CRQ und Korrelaten --> 55 * 2 = 110 --> 2 Sekunden pro Item. Erstellen einer Vergleichsspalte
zeitkriterium <- zeitkriterium %>% mutate(timer_soll = as.numeric(110))
# Beobachtungen, die länger als 110 Sekunden hatten, werden mit TRUE markiert.
zeitkriterium <- zeitkriterium %>% mutate(timer_test_pass = timer_soll < timer_total)
# Integration ins main dataframe
df_crqs <- df_crqs %>% mutate(timer_test_pass = zeitkriterium$timer_test_pass)


## Attention Checks --> 0/2 = Ausschluss --> 1/2 bedeutet ein Pass
attchks <- df_crqs %>% select(contains("attchk"))
attchks <- attchks %>% mutate(attchk_pass = attchk_1 == 2 | attchk_2 == 5)
df_crqs <- df_crqs %>% mutate(attchk_pass = attchks$attchk_pass)

df_crqs_final <- df_crqs %>% filter(timer_test_pass == TRUE & attchk_pass == TRUE)


# wenn beide filter angewendet, dann sortieren nach DE / CH
# valide CH mind. 80
df_crqs_final <- df_crqs_final[order(df_crqs_final$country_recode),]

anzahl_ch <- df_crqs_final %>% count(country_recode)
names(anzahl_ch) <- c("Kategorie","Anzahl")

# Total valider Fragebögen
total_valid <- data.frame("TOTAL_valid",nrow(df_crqs_final))
names(total_valid) <- c("Kategorie","Anzahl")

# Fragebögen, die aufgrund eines zu tiefen Pensums oder != Selbstständig ausgeschieden sind.
prefilter <- data.frame("<16h_or_not_se",nrow(df_crqs_prefilter)-nrow(df_crqs_postfilter))
names(prefilter) <- c("Kategorie","Anzahl")

# Disqualified due to carelessness
careless <- data.frame("meet_criteria_but_careless",nrow(df_crqs_postfilter)-nrow(df_crqs_final))
names(careless) <- c("Kategorie", "Anzahl")

# Total invalider Fragebögen
total_invalid <- data.frame("TOTAL_invalid",prefilter[1,2]+careless[1,2])
names(total_invalid) <- c("Kategorie","Anzahl")

# Total aller Fragebögen
total <- data.frame("TOTAL",total_valid[1,2]+total_invalid[1,2])
names(total) <- c("Kategorie","Anzahl")

anzahl_ch <- rbind(anzahl_ch, total_valid, prefilter, careless, total_invalid, total)

anzahl_ch

### Analyse Schweizer
land <- df_crqs_prefilter %>% count(country)
land_mit_employ <- df_crqs_prefilter %>% filter(employ == 4) %>% count(country)
land_mit_employ_und_whours <- df_crqs_prefilter %>% filter(employ == 4) %>% filter(whours > 16) %>% count(country)

land
land_mit_employ
land_mit_employ_und_whours

# Ausgeschiedene, weil nicht Selbstständig oder niedriges Pensum
sum(land[,2])-sum(land_mit_employ[,2])
sum(land_mit_employ_und_whours[,2])


tic_respondi <- as.data.frame(df_crqs_final$tic)

write_excel_csv2(tic_respondi, file = "data/tic_selbststaendige.csv")

# Streuung Soziodemographisch
# Gender, Alter
sjlabelled::get_label(crq_items) %>% enframe() %>% View()
