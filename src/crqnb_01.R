# 1. init various packages and helpers, and data frame --------
library(tidyverse)
source("https://raw.githubusercontent.com/franciscowilhelm/r-collection/master/scoreItemsMulti.R")
library(haven)
df <- read_sav("data/CRQ Nicht-Erwerbstaetige_November 1, 2021_14.06.sav")
rawdf <- df
df <- df %>% filter(StartDate >= lubridate::as_datetime("2021-10-12 18:00:00", tz = "Europe/Zurich"))
df <- df[-c(1:4),] #date filter fails
df <- df %>% mutate(timerTotal = as.numeric(timerTotal))
# filter wrong responses
df <- df %>% filter(consent == 1 & age >= 30 & age <= 60 & employ == 7 & future_employ != 1 & timerTotal >= 66)


# correct factors
# tmp <- df$edu_de %>% as.factor() %>% 
#   recode(`12` = 1, `13` = 2, `14` = 3, `15` = 4, `16` = 5, `17` = 6, `18` = 7, `19` = 8)



df <- df %>% mutate(edu_de = as_factor(edu_de),
                    edu_ch = as_factor(edu_ch))

df_ch <- df %>% filter(country == 2)
df_de <- df %>% filter(country == 1)

# 2. generate scores and scale psychometrics ---------
scalenames <- c("oexp", "jmk", "ssk", "inv", "conf", "ccl", "scs", "net", "cexpl", "lear")

x <- map_dfc(scalenames, function(scl) df %>% select(starts_with(scl))) 

psych::alpha(x %>% select(starts_with("inv_")), keys = rep(1,5), impute = "none") # drop inv_5
psych::alpha(x %>% select(inv_1, inv_2, inv_3, inv_4), keys = rep(1,4), impute = "none")
# drop Inv_2 or inv_4 for maximum S/N/alpha. inv_2 is less redundant, pick it.
x <- x %>% select(-inv_4, -inv_5)

# psych::alpha(x %>% select(starts_with("lear_")), keys = rep(1,3), impute = "none")
# psych::alpha(x %>% select(starts_with("oexp_")), keys = rep(1,3), impute = "none")
# sjlabelled::get_label(x) %>% as.data.frame() %>% write.table(., "clipboard", sep="\t", row.names=FALSE)

# crq_scales <- map(scalenames, function(x) {
#   df <- crq %>% select(contains(x))
#   psych::scoreItems(keys =rep(1, ncol(df)),
#                     df, impute = "none")
# })

crq_scales <- scoreItemsMulti(scalenames, df, exclude = TRUE)
crq_scales$alpha
df_crq_scores <- crq_scales$scores %>% as.data.frame()

print(crq_scales, short = F)

# hierarchical scores
crq_scales_h <- psych::scoreItems(list(knsk = c("oexp", "jmk", "ssk"),
                                       mot = c("inv", "conf", "ccl"),
                                       env = c("scs"),
                                       act = c("net", "cexpl", "lear")),
                                  df_crq_scores, impute = "none")

df_crq_scores <- bind_cols(df_crq_scores,
                    crq_scales_h$scores %>% as.data.frame())
df_crq_scores <- bind_cols(df_crq_scores, df %>% select(age, gender))

# 3. 13 factor CFA (M1) ---------
library(tidySEM)
model <- tidy_sem(x)
model <- model %>% 
  measurement()
model

library(lavaan)
res <- lavaan(as_lavaan(model), data = x, estimator = "MLR" )
summary(res, estimates = FALSE, fit.measures = TRUE)
summary(res, estimates = TRUE, fit.measures = FALSE, standardized = TRUE)
# 3.2 hierarchical factor (M2) 

# tmp <- model$syntax %>% select(lhs, op, rhs)
# map_chr(seq_len(nrow(tmp)), ~str_c(x[.x,], collapse = " "))

model_2 <- "
  oexp =~ oexp_1 + oexp_2 + oexp_3
  jmk =~ jmk_1 + jmk_2 + jmk_3
  ssk =~ ssk_1 + ssk_2 + ssk_3
  inv =~ inv_1 + inv_2 + inv_3
  conf =~ conf_1 + conf_2 + conf_3 + conf_4
  ccl =~ ccl_1 + ccl_2 + ccl_3
  scs =~ scs_1 + scs_2 + scs_3
  net =~ net_1 + net_2 + net_3
  cexpl =~ cexpl_1 + cexpl_2 + cexpl_3
  lear =~ lear_1 + lear_2 + lear_3
  
  knsk =~ oexp + jmk + ssk
  mot =~ conf + inv + ccl
  act =~ net + cexpl + lear
  
  knsk ~~  mot + act + scs
  mot ~~ act + scs
  act ~~ scs
"

res_m2 <- cfa(model_2, data = x, estimator = "MLR" )
summary(res_m2, estimates = FALSE, fit.measures = TRUE)



# 3.3 4-factor CFA (only dimensions) (M3)-------
model_4factor <- '
  knsk =~ oexp_1 + oexp_2 + oexp_3 + jmk_1 + jmk_2 + jmk_3 + ssk_1 + ssk_2 + ssk_3
  mot =~ inv_1 + inv_2 + inv_3 + conf_1 + conf_2 + conf_3 + conf_4 + ccl_1 + ccl_2 + ccl_3
  env =~ scs_1 + scs_2 + scs_3
  act =~ net_1 + net_2 + net_3 +cexpl_1 + cexpl_2 + cexpl_3 + lear_1 + lear_2 + lear_3
  '
res_4factor <- cfa(model_4factor, x, std.lv = TRUE,
                   estimator = "MLR")

summary(res_4factor, estimates = FALSE, fit.measures = TRUE)

# model_4factor2 <- '
#   knsk =~ oexp + jmk + ssk
#   mot =~ inv + conf + 1*ccl
#   scs_ =~ scs_1 + scs_2 + scs_3
#   act =~ net + cexpl + lear
#   '
# 
# res_4factor2 <- cfa(model_4factor2,
#                     bind_cols(x %>% select(scs_1:scs_3),
#                               df_crq_scores %>% select(oexp:lear)),
#                     std.lv = TRUE,
#                     estimator = "MLR")
# 
# summary(res_4factor2, estimates = FALSE, fit.measures = TRUE)
# 3.4 one factor model (M4)

model_1factor <- '
  crq =~ oexp_1 + oexp_2 + oexp_3 + jmk_1 + jmk_2 + jmk_3 + ssk_1 + ssk_2 + ssk_3 +
  inv_1 + inv_2 + inv_3 + conf_1 + conf_2 + conf_3 + conf_4 + ccl_1 + ccl_2 + ccl_3 + 
  scs_1 + scs_2 + scs_3 + net_1 + net_2 + net_3 +cexpl_1 + cexpl_2 + cexpl_3 + lear_1 + lear_2 + lear_3
  '
res_1factor <- cfa(model_1factor, x, std.lv = TRUE,
                   estimator = "MLR")

summary(res_1factor, estimates = FALSE, fit.measures = TRUE)



# 5. Bestimmung der Normwerte
# Unterscheidung anhand von signifikanten Unterscheiden in soziodemographischen Aspekten: Altersgruppen, Geschlecht, ...
# Stanine scores, i.e. Mean = 5, SD = 2.

# psych::rescale(x, mean = 5, sd = 2)

# 5. demographic description
hist(df$age)
sjmisc::frq(df$gender)
sjmisc::frq(df$edu_ch)
sjmisc::frq(df$edu_de)

df %>% filter(age > 29 & age < 41) %>% nrow()

df %>% filter(age > 49 & age < 61) %>% nrow()
df %>% filter(age > 49 & age < 61) %>% nrow()/nrow(df)

sjmisc::frq(df_ch$gender)
sjmisc::frq(df_ch$edu_ch)
sjmisc::frq(df$future_employ)

# 24+12+6+2+5+16


# sum(is.na(df_de$inv_4))

# 4. Normwerte -----------
lm(knsk ~ age + gender, data = df_crq_scores) %>% summary()
lm(mot ~ age + gender, data = df_crq_scores) %>% summary()
lm(act ~ age + gender, data = df_crq_scores) %>% summary()
lm(env ~ age + gender, data = df_crq_scores) %>% summary()
# effect of age on env.

library(ggplot2)
ggplot(df_crq_scores, aes(x = age, y = env)) +
  geom_jitter() +
  geom_smooth(method = "lm")

agebin <- map_chr(seq_len(nrow(df_crq_scores)), function(i) {
  age <- df_crq_scores$age[i]
  if((age >= 30 & age < 40)) {
    x <- "30-39"
  } else if((age >= 40 & age < 50)) {
    x <- "40-49"
  } else if((age >= 50 & age < 61)) {
    x <- "50-60"
  }
})
df_crq_scores <-df_crq_scores %>%  mutate(agebin = as_factor(agebin) %>% fct_relevel(., "30-39", "40-49", "50-60"))

# mean / sd grouped by age bin
crq_age_meansd <- df_crq_scores %>% group_by(agebin) %>% 
  summarize(across(oexp:act, list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)), .names = "{.col}.{.fn}"))


# Compute Stanines: 1) Where they begin in terms of CRQ raw scores, and transformed scores.

# updated 2022/02/14: classify into ranges of 0.5 z width.



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


stanines_by_age <- map(levels(df_crq_scores$agebin), function(age) {
  x <- crq_age_meansd %>% filter(agebin == age)
  map_dfc(c(scalenames, "knsk", "mot", "act", "env"), function(scale) {
    mean <- x[1, str_c(scale, ".mean")]
    sd <- x[1, str_c(scale, ".sd")]
    stanines <- compute_stanine(mean = mean,
                           sd = sd)
  out <- enframe(stanines) %>% select(value)
  names(out) <- scale
  return(out)
  })
})

stanines_by_age <- map(stanines_by_age, ~mutate_each(.x, vset_to_bounds)) #tidy implementation
names(stanines_by_age) <- levels(df_crq_scores$agebin)
writexl::write_xlsx(stanines_by_age, "outputs/normwerte.xlsx")


# rescaled df with stanines (as continuous stanines)
# tmp <- map(levels(df_crq_scores$agebin),
#         ~stanine_rescale(df_crq_scores %>% filter(agebin == .x) %>% select(oexp:act)))


# correlation table

# apaTables useless
# # apaTables::apa.cor.table(df_crq_scores %>% select(-c(knsk:gender)),
#                          filename = "outputs/crq_nb_Kortabelle.doc",
#                          show.conf.interval = FALSE,
#                          show.sig.stars= FALSE)

cortable <- round(cor(df_crq_scores %>% select(-c(knsk:gender)), use = "pairwise"), 2)
diag(cortable) <- round(crq_scales$alpha, 2)
cortable[upper.tri(cortable)]<-""
cortable %>% write.table(., "clipboard", sep="\t", row.names=FALSE)

psych::corr.test(df_crq_scores %>% select(-c(knsk:gender)), adjust = "none")


df$tic %>% write.table(., "clipboard", sep="\t", row.names=FALSE)
