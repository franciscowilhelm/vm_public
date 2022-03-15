library(haven)
library(tidyverse)
source("https://raw.githubusercontent.com/franciscowilhelm/r-collection/master/scoreItemsMulti.R")


setwd("~/GitHub/vm_public")

datapath <- "data/CRQ_Selbststaendige/CRQ Selbststaendige_February 14, 2022_10.00.sav"

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


#### Rekodieren Field_other ----------------------------------------------------
df_recode <- df_crqs_final %>% filter(str_length(field_other) > 0)

new_fields <- c(24,24,18,24,11,18,6,24,18,11,19,4,14,23,6,18,18,18,23,21,11,18,18)

df_recode <- df_recode %>% mutate(field_new = new_fields)

df_recode <- df_recode %>% relocate(field_new, .after = field_other)

df_crqs_final[str_length(df_crqs_final$field_other) > 0, "field"] <- new_fields

# # wenn beide filter angewendet, dann sortieren nach DE / CH
# # valide CH mind. 80
# df_crqs_final <- df_crqs_final[order(df_crqs_final$country_recode),]
# 
# anzahl_ch <- df_crqs_final %>% count(country_recode)
# names(anzahl_ch) <- c("Kategorie","Anzahl")
# 
# # Total valider Fragebögen
# total_valid <- data.frame("TOTAL_valid",nrow(df_crqs_final))
# names(total_valid) <- c("Kategorie","Anzahl")
# 
# # Fragebögen, die aufgrund eines zu tiefen Pensums oder != Selbstständig ausgeschieden sind.
# prefilter <- data.frame("<16h_or_not_se",nrow(df_crqs_prefilter)-nrow(df_crqs_postfilter))
# names(prefilter) <- c("Kategorie","Anzahl")
# 
# # Disqualified due to carelessness
# careless <- data.frame("meet_criteria_but_careless",nrow(df_crqs_postfilter)-nrow(df_crqs_final))
# names(careless) <- c("Kategorie", "Anzahl")
# 
# # Total invalider Fragebögen
# total_invalid <- data.frame("TOTAL_invalid",prefilter[1,2]+careless[1,2])
# names(total_invalid) <- c("Kategorie","Anzahl")
# 
# # Total aller Fragebögen
# total <- data.frame("TOTAL",total_valid[1,2]+total_invalid[1,2])
# names(total) <- c("Kategorie","Anzahl")
# 
# anzahl_ch <- rbind(anzahl_ch, total_valid, prefilter, careless, total_invalid, total)
# 
# anzahl_ch
# 
# tic_respondi <- as.data.frame(df_crqs_final$tic)
# 
# write_excel_csv2(tic_respondi, file = "data/tic_selbststaendige.csv")
# 
# screenouts <- anti_join(df_crqs_prefilter,df_crqs_final)
# 
# tic_antijoin <- as.data.frame(screenouts$tic)
# 
# write_excel_csv2(tic_antijoin, file = "data/tic_selbststaendige_screenout.csv")

# Streuung Soziodemographisch
# Gender, Alter
# sjlabelled::get_label(crq_items) %>% enframe() %>% View()


#### Datenauswertung -----------------------------------------------------------

# Entfernen der überflüssigen Daten
rm(attchks,cor_items,crq_items,df_crqs,df_crqs_postfilter,df_crqs_prefilter,zeitkriterium)

# Bilden von einem dataframe pro Land (DE, CH)
df <- df_crqs_final %>% mutate(edu_de = as_factor(edu_de),
                               edu_ch = as_factor(edu_ch))

df_ch <- df %>% filter(country == 2)
df_de <- df %>% filter(country == 1)


# 2. generate scores and scale psychometrics -----------------------------------
scalenames <- c("oexp", "jmk", "ssk", "inv", "conf", "ccl", "jcha", "scs", "net", "cexpl", "lear")

x <- map_dfc(scalenames, function(scl) df %>% select(starts_with(scl)))

crq_scales <- scoreItemsMulti(scalenames, df, exclude = TRUE)
crq_scales$alpha
df_crq_scores <- crq_scales$scores %>% as.data.frame()

print(crq_scales, short = F)

# hierarchical scores for each person and factor
crq_scales_h <- psych::scoreItems(list(knsk = c("oexp", "jmk", "ssk"),
                                       mot = c("inv", "conf", "ccl"),
                                       env = c("jcha", "scs"),
                                       act = c("net", "cexpl", "lear")),
                                  df_crq_scores, impute = "none")

df_crq_scores <- bind_cols(df_crq_scores,
                           crq_scales_h$scores %>% as.data.frame())
df_crq_scores <- bind_cols(df_crq_scores, df %>% select(age, gender))

# 5. demographic description ---------------------------------------------------
hist(df$age)
sjmisc::frq(df$gender)
sjmisc::frq(df$edu_ch)
sjmisc::frq(df$edu_de)

df %>% filter(age > 29 & age < 41) %>% nrow()

df %>% filter(age > 49 & age < 61) %>% nrow()
df %>% filter(age > 49 & age < 61) %>% nrow()/nrow(df)

sjmisc::frq(df_ch$gender)
sjmisc::frq(df_ch$edu_ch)

sjmisc::frq(df$employees)
hist(df$whours)
sjmisc::frq(df$field)
sjmisc::frq(df$field_other) # einordnen wo möglich.


# Speichern des finalen Datensatzes --------------------------------------------
save(df_crqs_final, file = "data/df_crqs_final.Rdata")


# 4. Normwerte -----------------------------------------------------------------
linear_test_age_gender <- function(x) {
  x <- get(x,df_crq_scores)
  lm(x ~ age + gender, data = df_crq_scores) %>% summary()
}

linear_test_age_gender("knsk")
linear_test_age_gender("mot")
linear_test_age_gender("act")
linear_test_age_gender("env")
# effect of age on env.


# Correlation of CRQ w/ Correlates --------------------------------------------- 
# (Work engagement, job satisfaciton & Career Satisfaction)

# Creating total scores for each correlate
scalenames_corr <- c("jsat","weng","csat")

# dataframe with only the relevant items
y <- df %>% select(contains(scalenames_corr) & !contains("timer"))

corr_scales <- scoreItemsMulti(scalenames_corr, y, exclude = TRUE)
corr_scales$alpha
df_corr_scores <- corr_scales$scores %>% as.data.frame()
df_corr_scores

print(crq_scales, short = F)


# Korrelation zwischen Korrelaten und CRQ
correlates_x_crq <- cor(df_crq_scores, df_corr_scores)

cor_probs <- psych::corr.test(df_crq_scores, df_corr_scores)


correlates_x_crq

cor_probs$p

# Multiple regression ----------------------------------------------------------

# Creating DF containing all Scores (Correlates and CRQ Dimensions)
df_all_scores <- bind_cols(crq_scales$scores,df_corr_scores)

# Calculating linear models predicting each of the correlates based on the CRQ scores

# Relevant Function
linear_model_CRQ_corrs <- function(x){
  if (is.character(x) == FALSE){
    print("ERROR: please enter name as a character string")
  } else {
    x <- get(x,df_all_scores)
    lm(x ~ oexp + jmk + ssk + inv + conf + ccl + jcha + scs + net + cexpl + lear,
       data = df_all_scores) %>% summary()
  }
}

# Job Satisfaction
linear_model_CRQ_corrs("jsat")
# Work engagement
linear_model_CRQ_corrs("weng")
# Career Satisfaction
linear_model_CRQ_corrs("csat")

# Rest von Francisco -----------------------------------------------------------

library(ggplot2)
ggplot(df_crq_scores, aes(x = age, y = env)) +
  geom_jitter() +
  geom_smooth(method = "lm")


# mean / sd
crq_meansd <- df_crq_scores %>%
  summarize(across(oexp:act, list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)), .names = "{.col}.{.fn}"))


# Compute Stanines: 1) Where they begin in terms of CRQ raw scores, and transformed scores.

# we need to take mean = 5, sd/2 = 1, and compute values for each.
compute_stanine <- function(mean, sd) {
  x <- as.double(mean)
  y <- as.double(sd)
  # S1 begins at -2.25z, ends at -1.75z and so on (0.5z width) and so on.
  out <- c(x-2.25*y, x-1.75*y, x-1.25*y, x-0.75*y, x-0.25*y, x+0.25*y, x+0.75*y, x+1.25*y, x+1.75*y)
  return(out)
}

# set values out of bounds to upper and lower minimum (1, 5)
set_to_bounds <- function(x) {
  if(x < 1) x <- 1
  else if(x > 5) x <- 5
  x
}
# vectorize it
vset_to_bounds <- Vectorize(set_to_bounds)

# stanine rescaled values. Not really needed because in diagnostics stanines are always binned.
stanine_rescale <- function(x) {
  psych::rescale(x, mean = 5, sd = 2, df = TRUE)
}

names_for_stanines <- c(scalenames,"knsk","mot","env","act")

stanines_df <- map_dfc(names_for_stanines, function(scale){
    mean <- crq_meansd[1, str_c(scale, ".mean")]
    sd <- crq_meansd[1, str_c(scale, ".sd")]
    stanines <- compute_stanine(mean = mean,
                                sd = sd)
    out <- enframe(stanines) %>% select(value) 
    names(out) <- scale
    return(out)
  }) 

# Applying vset_to_bounds Function to each element of the dataframe, setting values
# below 1 to 1 and above 5 to 5
stanines_df <- modify(stanines_df, vset_to_bounds)

# Creating new Column, numbering the Stanine values
stanines_df <- stanines_df %>% mutate(Stanines = c(1:9), .before = "oexp")

stanines_df

# Cleaning up the Dataframe with the means and standard deviations of the CRQ scales
crq_mean <- crq_meansd %>% select(contains(".mean"))
crq_sd <- crq_meansd %>% select(contains(".sd"))

# Set Column names to be the same, for merging
names(crq_mean) <- names_for_stanines
names(crq_sd) <- names_for_stanines

# Merging the Dataframes
crq_meansd <- rbind(crq_mean,crq_sd) %>% mutate("measure" = c("mean","sd"), .before = "oexp")


writexl::write_xlsx(stanines_df, "outputs/normwerte.xlsx")
writexl::write_xlsx(crq_meansd, "outputs/means_sd.xlsx")
