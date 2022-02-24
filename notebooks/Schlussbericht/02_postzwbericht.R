# Outcomes und Methoden ZUsammenhang

library(brms)


# korrelationen der Methoden
library(GGally)
source("lib/hdi_nonzero.R")

methoden_mutate <- function(x) {
  as.numeric(x) %>% recode(`1` = 0, `2` = 1)
}
methoden <- df_brm %>% select(starts_with("B2_A4")) %>% mutate(across(.fns = methoden_mutate))
# sjmisc::frq(methoden) # many NAs even when others were filled out. Patch this first.
tmp <- apply(methoden, 1, function(row) {
  if(sum(!is.na(row) != 0)) {
    row[is.na(row)] <- 0
    row
  }
  else {
    row
  }
})
methoden_rcd <- t(tmp) %>% as_tibble()
methoden_rcd <- bind_cols(df_brm %>% select(starts_with("A2_B3"), "B1_amf", Kanton, AnzahlSitzungen), methoden_rcd)

# a few methods are correlated with 0.3-.5, keep in mind.

ggcorr(methoden,
       method = c("pairwise", "spearman"), label = TRUE)


wissen_methoden <- brm(family = cumulative(probit),
                    formula = A2_B3SQ001 ~ B2_A4A1 + B2_A4A2 + B2_A4A3 + B2_A4A4 + B2_A4A5 + B2_A4A6 + B2_A4A7 + B2_A4A8 + B2_A4A9 + 
                      B2_A4A10 + B2_A4A11 + B2_A4A12 + B2_A4A13 + B2_A4A14 + B2_A4A15 + B2_A4A16 + B2_A4A17 + B2_A4A18 +
                      B2_A4A19 + B2_A4A20,
                    data = methoden_rcd,
                    warmup = 500,
                    iter = 1500,
                    chains = 4,
                    cores = 4,
                    file = "models/wissen_methoden")

ziele_methoden <- brm(family = cumulative(probit),
                       formula = A2_B3SQ002 ~ B2_A4A1 + B2_A4A2 + B2_A4A3 + B2_A4A4 + B2_A4A5 + B2_A4A6 + B2_A4A7 + B2_A4A8 + B2_A4A9 + 
                         B2_A4A10 + B2_A4A11 + B2_A4A12 + B2_A4A13 + B2_A4A14 + B2_A4A15 + B2_A4A16 + B2_A4A17 + B2_A4A18 +
                         B2_A4A19 + B2_A4A20,
                       data = methoden_rcd,
                       warmup = 500,
                       iter = 1500,
                       chains = 4,
                       cores = 4,
                       file = "models/ziele_methoden")

# has issues

zutrauen_methoden <- brm(family = cumulative(probit),
                      formula = A2_B3SQ003 ~ B2_A4A1 + B2_A4A2 + B2_A4A3 + B2_A4A4 + B2_A4A5 + B2_A4A6 + B2_A4A7 + B2_A4A8 + B2_A4A9 + 
                        B2_A4A10 + B2_A4A11 + B2_A4A12 + B2_A4A13 + B2_A4A14 + B2_A4A15 + B2_A4A16 + B2_A4A17 + B2_A4A18 +
                        B2_A4A19 + B2_A4A20,
                      data = methoden_rcd,
                      warmup = 500,
                      iter = 1500,
                      chains = 4,
                      cores = 4,
                      file = "models/zutrauen_methoden")

mot_methoden <- brm(family = cumulative(probit),
                    formula = A2_B3SQ004 ~ B2_A4A1 + B2_A4A2 + B2_A4A3 + B2_A4A4 + B2_A4A5 + B2_A4A6 + B2_A4A7 + B2_A4A8 + B2_A4A9 + 
                      B2_A4A10 + B2_A4A11 + B2_A4A12 + B2_A4A13 + B2_A4A14 + B2_A4A15 + B2_A4A16 + B2_A4A17 + B2_A4A18 +
                      B2_A4A19 + B2_A4A20,
                    data = methoden_rcd,
                    warmup = 500,
                    iter = 1500,
                    chains = 4,
                    cores = 4,
                    file = "models/mot_methoden")

# with multilevel

wissen_methoden_ml <- brm(family = cumulative(probit),
                       formula = A2_B3SQ001 ~ B2_A4A1 + B2_A4A2 + B2_A4A3 + B2_A4A4 + B2_A4A5 + B2_A4A6 + B2_A4A7 + B2_A4A8 + B2_A4A9 + 
                         B2_A4A10 + B2_A4A11 + B2_A4A12 + B2_A4A13 + B2_A4A14 + B2_A4A15 + B2_A4A16 + B2_A4A17 + B2_A4A18 +
                         B2_A4A19 + B2_A4A20 + (1 | Kanton),
                       data = methoden_rcd,
                       warmup = 500,
                       iter = 1500,
                       chains = 4,
                       cores = 4,
                       file = "models/wissen_methoden_ml")

# divergetn transitions, pairs takes ages to run --> model too complex?

ziele_methoden_ml <- brm(family = cumulative(probit),
                      formula = A2_B3SQ002 ~ B2_A4A1 + B2_A4A2 + B2_A4A3 + B2_A4A4 + B2_A4A5 + B2_A4A6 + B2_A4A7 + B2_A4A8 + B2_A4A9 + 
                        B2_A4A10 + B2_A4A11 + B2_A4A12 + B2_A4A13 + B2_A4A14 + B2_A4A15 + B2_A4A16 + B2_A4A17 + B2_A4A18 +
                        B2_A4A19 + B2_A4A20 + (1 | Kanton),
                      data = methoden_rcd,
                      warmup = 500,
                      iter = 1500,
                      chains = 4,
                      cores = 4,
                      file = "models/ziele_methoden_ml")


zutrauen_methoden_ml <- brm(family = cumulative(probit),
                         formula = A2_B3SQ003 ~ B2_A4A1 + B2_A4A2 + B2_A4A3 + B2_A4A4 + B2_A4A5 + B2_A4A6 + B2_A4A7 + B2_A4A8 + B2_A4A9 + 
                           B2_A4A10 + B2_A4A11 + B2_A4A12 + B2_A4A13 + B2_A4A14 + B2_A4A15 + B2_A4A16 + B2_A4A17 + B2_A4A18 +
                           B2_A4A19 + B2_A4A20 +  (1 | Kanton),
                         data = methoden_rcd,
                         warmup = 500,
                         iter = 2500,
                         chains = 4,
                         cores = 4,
                         file = "models/zutrauen_methoden_ml")

mot_methoden_ml <- brm(family = cumulative(probit),
                    formula = A2_B3SQ004 ~ B2_A4A1 + B2_A4A2 + B2_A4A3 + B2_A4A4 + B2_A4A5 + B2_A4A6 + B2_A4A7 + B2_A4A8 + B2_A4A9 + 
                      B2_A4A10 + B2_A4A11 + B2_A4A12 + B2_A4A13 + B2_A4A14 + B2_A4A15 + B2_A4A16 + B2_A4A17 + B2_A4A18 +
                      B2_A4A19 + B2_A4A20 + (1 | Kanton),
                    data = methoden_rcd,
                    warmup = 500,
                    iter = 1500,
                    chains = 4,
                    cores = 4,
                    file = "models/mot_methoden_ml")

# kontrolle AMF

ziele_methoden_ml_amf <- brm(family = cumulative(probit),
                         formula = A2_B3SQ002 ~ B2_A4A1 + B2_A4A2 + B2_A4A3 + B2_A4A4 + B2_A4A5 + B2_A4A6 + B2_A4A7 + B2_A4A8 + B2_A4A9 + 
                           B2_A4A10 + B2_A4A11 + B2_A4A12 + B2_A4A13 + B2_A4A14 + B2_A4A15 + B2_A4A16 + B2_A4A17 + B2_A4A18 +
                           B2_A4A19 + B2_A4A20 + B1_amf + (1 | Kanton),
                         data = methoden_rcd,
                         warmup = 500,
                         iter = 1500,
                         chains = 4,
                         cores = 4,
                         file = "models/ziele_methoden_ml_amf")


zutrauen_methoden_ml_amf <- brm(family = cumulative(probit),
                            formula = A2_B3SQ003 ~ B2_A4A1 + B2_A4A2 + B2_A4A3 + B2_A4A4 + B2_A4A5 + B2_A4A6 + B2_A4A7 + B2_A4A8 + B2_A4A9 + 
                              B2_A4A10 + B2_A4A11 + B2_A4A12 + B2_A4A13 + B2_A4A14 + B2_A4A15 + B2_A4A16 + B2_A4A17 + B2_A4A18 +
                              B2_A4A19 + B2_A4A20 + B1_amf + (1 | Kanton),
                            data = methoden_rcd,
                            warmup = 500,
                            iter = 2500,
                            chains = 4,
                            cores = 4,
                            file = "models/zutrauen_methoden_ml_amf")

mot_methoden_ml_amf <- brm(family = cumulative(probit),
                       formula = A2_B3SQ004 ~ B2_A4A1 + B2_A4A2 + B2_A4A3 + B2_A4A4 + B2_A4A5 + B2_A4A6 + B2_A4A7 + B2_A4A8 + B2_A4A9 + 
                         B2_A4A10 + B2_A4A11 + B2_A4A12 + B2_A4A13 + B2_A4A14 + B2_A4A15 + B2_A4A16 + B2_A4A17 + B2_A4A18 +
                         B2_A4A19 + B2_A4A20 + B1_amf + (1 | Kanton),
                       data = methoden_rcd,
                       warmup = 500,
                       iter = 1500,
                       chains = 4,
                       cores = 4,
                       file = "models/mot_methoden_ml_amf")

wissen_methoden_ml_amf <- brm(family = cumulative(probit),
                          formula = A2_B3SQ001 ~ B2_A4A1 + B2_A4A2 + B2_A4A3 + B2_A4A4 + B2_A4A5 + B2_A4A6 + B2_A4A7 + B2_A4A8 + B2_A4A9 + 
                            B2_A4A10 + B2_A4A11 + B2_A4A12 + B2_A4A13 + B2_A4A14 + B2_A4A15 + B2_A4A16 + B2_A4A17 + B2_A4A18 +
                            B2_A4A19 + B2_A4A20 + B1_amf + (1 | Kanton),
                          data = methoden_rcd,
                          warmup = 500,
                          iter = 1500,
                          chains = 4,
                          cores = 4,
                          file = "models/wissen_methoden_ml_amf")


library(broom.mixed)
summary(ziele_methoden_ml_amf)
summary(ziele_methoden_ml_amf)

methoden_outcomes_significant <- map_dfr(list(ziele_methoden_ml_amf, wissen_methoden_ml_amf,
                                              zutrauen_methoden_ml_amf, mot_methoden_ml_amf),
                                         function(fit) {
                                           tidy(fit) %>% hdi_nonzero() %>% mutate(outcome = fit[["formula"]][["resp"]])
                                           }) %>%
  .[!str_detect(.$term, "Intercept"),]

methoden_outcomes_significant %>% write.table(., "clipboard", sep="\t", row.names=FALSE)


## INteraktionen

brm(family = cumulative(probit),
    formula = A2_B3SQ003 ~ B2_A4A17*B1_amf + (1 | Kanton),
    data = methoden_rcd,
    warmup = 500,
    iter = 1500,
    chains = 4,
    cores = 4)

brm(family = cumulative(probit),
    formula = A2_B3SQ001 ~ B2_A4A6*B1_amf + (1 | Kanton),
    data = methoden_rcd,
    warmup = 500,
    iter = 1500,
    chains = 4,
    cores = 4)

## gesamtscore
df_resourcenfoerd <- df_brm %>% select(A2_B3SQ001:A2_B3SQ004)
psych::alpha(x = df_resourcenfoerd,
             keys = rep(1,4))
resourcenfoerd_score <- psych::scoreItems(rep(1,4), df_resourcenfoerd, impute = "none")

methoden_rcd <- methoden_rcd %>% mutate(A2_B3TOTAL = resourcenfoerd_score$scores[,1])

total_methoden_ml_amf <- brm(family = gaussian(identity),
                              formula = A2_B3TOTAL ~ B2_A4A1 + B2_A4A2 + B2_A4A3 + B2_A4A4 + B2_A4A5 + B2_A4A6 + B2_A4A7 + B2_A4A8 + B2_A4A9 + 
                                B2_A4A10 + B2_A4A11 + B2_A4A12 + B2_A4A13 + B2_A4A14 + B2_A4A15 + B2_A4A16 + B2_A4A17 + B2_A4A18 +
                                B2_A4A19 + B2_A4A20 + B1_amf + (1 | Kanton),
                              data = methoden_rcd,
                              warmup = 500,
                              iter = 1500,
                              chains = 4,
                              cores = 4,
                              file = "models/total_methoden_ml_amf")


## factors?
psych::vss(methoden_rcd %>% select(starts_with("B2_A4")), cor = "poly")
fa_out <- psych::fa(methoden_rcd %>% select(starts_with("B2_A4")), nfactors = 7, cor = "poly")
psych::fa.sort(fa_out)

# MR1 = Abklärung Persönlichkeit und Leistung
# MR3 = A19, A11, A10 = psychologische Unterstützung, kognitive Umstrukturierung, Mögliche Hindernisse identifiziert und Strategien zum Umgang besprochen
# MR2 = A12, A13, A15 Berufs/Weiterbildung, Gemeinsam mögliche Berufsoptionen bewertet 
# MR4 = A2, quantitative Testverfahren, A5 = Interessen.
# MR5 = Arbeitshefte, Psychoedukation, Hausaufgaben
# MR7 = A16, Bewerbungsunterlagen
# MR6 = Rollenspiele

# Methoden 19, 11, 10 sollten zusammengefasst werden, ebs. A12,A13,A15.  diea nderen Methoden laden  zu komplex.

methoden_new <- psych::scoreItems(list(psykog = c("B2_A4A10", "B2_A4A11", "B2_A4A19"),
                                       beruf = c("B2_A4A12", "B2_A4A13", "B2_A4A15")), methoden_rcd, impute = "none")

tmp <- ifelse(methoden_new$scores[,1] > 0, 1, 0)
methoden_rcd <- methoden_rcd %>% mutate(psykog = ifelse(methoden_new$scores[,1] > 0, 1, 0),
                                        beruf = ifelse(methoden_new$scores[,2] > 0, 1, 0))

wissen_methoden_ml_amf2 <- brm(family = cumulative(probit),
                              formula = A2_B3SQ001 ~ B2_A4A1 + B2_A4A2 + B2_A4A3 + B2_A4A4 + B2_A4A5 + B2_A4A6 + B2_A4A7 + B2_A4A8 + B2_A4A9 + 
                                B2_A4A14 + B2_A4A16 + B2_A4A17 + B2_A4A18 + B2_A4A20 + psykog + beruf + B1_amf + (1 | Kanton),
                              data = methoden_rcd,
                              warmup = 500,
                              iter = 1500,
                              chains = 4,
                              cores = 4,
                              file = "models/wissen_methoden_ml_amf2")

ziele_methoden_ml_amf2 <- brm(family = cumulative(probit),
                               formula = A2_B3SQ002 ~ B2_A4A1 + B2_A4A2 + B2_A4A3 + B2_A4A4 + B2_A4A5 + B2_A4A6 + B2_A4A7 + B2_A4A8 + B2_A4A9 + 
                                 B2_A4A14 + B2_A4A16 + B2_A4A17 + B2_A4A18 + B2_A4A20 + psykog + beruf + B1_amf + (1 | Kanton),
                               data = methoden_rcd,
                               warmup = 500,
                               iter = 1500,
                               chains = 4,
                               cores = 4,
                               file = "models/ziele_methoden_ml_amf2")

zutrauen_methoden_ml_amf2 <- brm(family = cumulative(probit),
                              formula = A2_B3SQ003 ~ B2_A4A1 + B2_A4A2 + B2_A4A3 + B2_A4A4 + B2_A4A5 + B2_A4A6 + B2_A4A7 + B2_A4A8 + B2_A4A9 + 
                                B2_A4A14 + B2_A4A16 + B2_A4A17 + B2_A4A18 + B2_A4A20 + psykog + beruf + B1_amf + (1 | Kanton),
                              data = methoden_rcd,
                              warmup = 500,
                              iter = 1500,
                              chains = 4,
                              cores = 4,
                              file = "models/zutrauen_methoden_ml_amf2")

mot_methoden_ml_amf2 <- brm(family = cumulative(probit),
                                 formula = A2_B3SQ004 ~ B2_A4A1 + B2_A4A2 + B2_A4A3 + B2_A4A4 + B2_A4A5 + B2_A4A6 + B2_A4A7 + B2_A4A8 + B2_A4A9 + 
                                   B2_A4A14 + B2_A4A16 + B2_A4A17 + B2_A4A18 + B2_A4A20 + psykog + beruf + B1_amf + (1 | Kanton),
                                 data = methoden_rcd,
                                 warmup = 500,
                                 iter = 1500,
                                 chains = 4,
                                 cores = 4,
                                 file = "models/mot_methoden_ml_amf2")

methoden_outcomes_significant <- map_dfr(list(ziele_methoden_ml_amf2, wissen_methoden_ml_amf2,
                                              zutrauen_methoden_ml_amf2, mot_methoden_ml_amf2),
                                         function(fit) {
                                           tidy(fit) %>% hdi_nonzero() %>% mutate(outcome = fit[["formula"]][["resp"]])
                                         }) %>%
  .[!str_detect(.$term, "Intercept"),]

methoden_outcomes_significant %>% write.table(., "clipboard", sep="\t", row.names=FALSE)
