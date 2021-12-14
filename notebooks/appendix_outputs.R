library(brms)
library(broom.mixed)
library(sjPlot)

# Tabelle mit den Korrelationen CRQ-Wert nach Dimension und AMF einfügen und im Text kurz darauf verweisen/erklären?



a1wbe2 %>% tidy() %>% write.table(., "clipboard", sep="\t", row.names=FALSE)
a1wbe2 %>% tab_model()
summary(a1wbe2)

df_brm %>% select(B1_amf, knsk, mot, act, env) %>% cor(., use = "pairwise", method = "spearman")
df_brm %>% select(B1_amf, knsk, mot, act, env) %>% cor(., use = "pairwise", method = "pearson") %>% write.table(., "clipboard", sep="\t", row.names=FALSE)

tmp <- df_brm %>% select(B1_amf, oe:lear) %>% cor(., use = "pairwise", method = "pearson")
t(sort(tmp[1,], decreasing = TRUE)) %>% write.table(., "clipboard", sep="\t", row.names=FALSE)
df_brm %>% select(B1_amf, jmk:lear) %>% get_label()

a1wbe3 %>% tidy() %>% write.table(., "clipboard", sep="\t", row.names=FALSE)
a1wbe4 %>% tidy() %>% write.table(., "clipboard", sep="\t", row.names=FALSE)

library(lme4)
fit_a1wbe4_glmer <- glmer(formula = A1_WBe ~ 1 + `A1_Gruende[SQ001]`  +
                            + `A1_Gruende[SQ002]`  + `A1_Gruende[SQ003]` + `A1_Gruende[SQ004]` +
                            `A1_Gruende[SQ005]`  + `A1_Gruende[SQ006]`  + `A1_Gruende[SQ007]` +
                            `A1_Gruende[SQ008]` + `A1_Gruende[SQ009]`  + `A1_Gruende[SQ010]`
                          + `A1_Gruende[SQ011]`  + (1 | Kanton),
                          family = binomial(logit),
                          data = df)
summary(fit_a1wbe4_glmer)
plot_model(fit_a1wbe4_glmer, order.terms = ) + labs(x = c("viamiaID", "Ausweg aus Arbeitslosigkeit",
                                                          "Beruflicher Wiedereinstieg (nach freiwilliger Erwerbspause)",
                                                          "Berufliche Weiterentwicklung",
                                                          "Berufliche Umorientierung",
                                                          "Erhöhung des Arbeitspensums",
                                                          "Unsicherheiten in Bezug auf künftige Arbeitssituation",
                                                          "Bessere gesellschaftliche Integration",
                                                          "Finanzielle Besserstellung",
                                                          "Grössere Zufriedenheit mit Arbeit",
                                                          "Generelle Wunsch nach Veränderung",
                                                          "Gesundheitliche Probleme"))

exp(fixef(fit_a1wbe4_glmer)) # odds ratios
x <- exp(confint(fit_a1wbe4_glmer,parm="beta_",method="Wald"))
cbind(exp(fixef(fit_a1wbe4_glmer)), x)  %>% write.table(., "clipboard", sep="\t", row.names=FALSE)

# nicht odds ratio:
fit_a1wbe4_glmer %>% tidy() %>% write.table(., "clipboard", sep="\t", row.names=FALSE)

c("Ausweg aus Arbeitslosigkeit",
  "Beruflicher Wiedereinstieg (nach freiwilliger Erwerbspause)",
  "Berufliche Weiterentwicklung",
  "Berufliche Umorientierung",
  "Erhöhung des Arbeitspensums",
  "Unsicherheiten in Bezug auf künftige Arbeitssituation",
  "Bessere gesellschaftliche Integration",
  "Finanzielle Besserstellung",
  "Grössere Zufriedenheit mit Arbeit",
  "Generelle Wunsch nach Veränderung",
  "Gesundheitliche Probleme") %>% write.table(., "clipboard", sep="\t", row.names=FALSE)

fit_a1wbe4_glmer %>% summary()


# nutzen der standortbesitmmung
kundnutz_table <- map_dfr(list(kundnutz_crq, kundnutz_crqb, kundnutz_ami, kundnutz_cv, kundnutz_insg), function(fit) {
  tidy(fit) %>% mutate(outcome = fit[["formula"]][["resp"]])
})
kundnutz_table %>% write.table(., "clipboard", sep="\t", row.names=FALSE)

summary(kundnutz_crq)


beratnutz_table <- map_dfr(list(beratnutz_crq, beratnutz_ami, beratnutz_cv, beratnutz_gespraech, beratnutz_insg), function(fit) {
  tidy(fit) %>% mutate(outcome = fit[["formula"]][["resp"]])
})
beratnutz_table %>% write.table(., "clipboard", sep="\t", row.names=FALSE)

# nach AMF  / CRQ
df_resourcenfoerd <- df_brm %>% select(A2_B3SQ001:A2_B3SQ004) %>% mutate(across(.fns = as.numeric))
psych::alpha(x = df_resourcenfoerd,
             keys = rep(1,4))
resourcenfoerd_score <- psych::scoreItems(rep(1,4), df_resourcenfoerd, impute = "none")
df_brm <- df_brm %>% mutate(A2_B3_total = resourcenfoerd_score$scores) %>% as_tibble()

fit_steigern1 <- lm(A2_B3_total ~ B1_amf + knsk + mot + act + env,
                    data = df_brm)
fit_steigern_wissen <- lm(A2_B3SQ001 ~ B1_amf + knsk + mot + act + env,
                    data = df_brm)
fit_steigern_ziele <- lm(A2_B3SQ002 ~ B1_amf + knsk + mot + act + env,
                          data = df_brm)
fit_steigern_zutrauen <- lm(A2_B3SQ003 ~ B1_amf + knsk + mot + act + env,
                         data = df_brm)
fit_steigern_act <- lm(A2_B3SQ004 ~ B1_amf + knsk + mot + act + env,
                            data = df_brm)


#  in brms
fit_steiger_crq_wissen <- brm(
  family = cumulative(probit),
  formula = `A2_B3SQ001` ~ 1 + mo(B1_amf) + knsk + mot + act + env + (1 | Kanton),
  data = df_brm,
  warmup = 500,
  iter = 1500,
  chains = 4,
  cores = 4,
  file = "models/steiger_crq_wissen")

fit_steiger_crq_ziele <- brm(
  family = cumulative(probit),
  formula = `A2_B3SQ002` ~ 1 + mo(B1_amf) + knsk + mot + act + env + (1 | Kanton),
  data = df_brm,
  warmup = 500,
  iter = 1500,
  chains = 4,
  cores = 4,
  file = "models/steiger_crq_ziele")

fit_steiger_crq_zutrau <- brm(
  family = cumulative(probit),
  formula = `A2_B3SQ003` ~ 1 + mo(B1_amf) + knsk + mot + act + env + (1 | Kanton),
  data = df_brm,
  warmup = 500,
  iter = 1500,
  chains = 4,
  cores = 4,
  file = "models/steiger_crq_zutrau")

fit_steiger_crq_act <- brm(
  family = cumulative(probit),
  formula = `A2_B3SQ004` ~ 1 + mo(B1_amf) + knsk + mot + act + env + (1 | Kanton),
  data = df_brm,
  warmup = 500,
  iter = 1500,
  chains = 4,
  cores = 4,
  file = "models/steiger_crq_act")

fit_steiger_crq_tot <- brm(
  family = gaussian(),
  formula = `A2_B3_total` ~ 1 + mo(B1_amf) + knsk + mot + act + env + (1 | Kanton),
  data = df_brm,
  warmup = 500,
  iter = 1500,
  chains = 4,
  cores = 4,
  file = "models/steiger_crq_tot")



tmp <- summary(fit_steiger_crq_ziele)


fixef_crq <- map_dfr(list(fit_steiger_crq_act, fit_steiger_crq_wissen, fit_steiger_crq_ziele, fit_steiger_crq_zutrau, fit_steiger_crq_tot),
    function(x) {
      fixef(x) %>% as.data.frame() %>% 
        rownames_to_column() %>% 
        mutate(outcome = x[["formula"]][["resp"]])})

fixef_crq %>% write.table(., "clipboard", sep="\t", row.names=FALSE)
# nach Beratungsinhalten

# steigern_mot2 %>% tidy() # does not work

fit_steigern_tot2 <- brm(
  family = gaussian(),
  formula = `A2_B3_total` ~ 1 + mo(B1_amf) + mo(B2_A1SQ001) + mo(B2_A1SQ002) +
    mo(B2_A1SQ003) + mo(B2_A1SQ004) + mo(B2_A1SQ005) + (1 | Kanton),
  data = df_brm,
  warmup = 500,
  iter = 1500,
  chains = 4,
  cores = 4,
  file = "models/steigern_tot2")

fixef_inhalte <- map_dfr(list(steigern_mot2, steigern_wissen2, steigern_ziele2, steigern_zutrauen2, fit_steigern_tot2),
                     function(x) {
                       fixef(x) %>% as.data.frame() %>% 
                         rownames_to_column() %>% 
                         mutate(outcome = x[["formula"]][["resp"]])})

fixef_inhalte %>% write.table(., "clipboard", sep="\t", row.names=FALSE)

# gruende
gruendenames <- df_brm %>% select(starts_with("A1_GruendeSQ")) %>% names()
outcomevarnames <- df_brm %>% select(starts_with("A2_B3SQ")) %>% names() %>% c(., "A2_B3_total")
outcome_gruende_pairs <- crossing(outcomevarnames, gruendenames)

glm_outc_gruende <- map2_dfr(outcome_gruende_pairs$outcomevarnames, outcome_gruende_pairs$gruendenames, function(y,x) {
  glm_formula <- str_c(y, " ~ ", x, "+ B1_amf") 
  glm(glm_formula, family = gaussian(link = "identity"), data = df_brm) %>% tidy() %>% mutate(outcome = y)
})

glm_outc_gruende %>% filter(term != "(Intercept)" & term != "B1_amf") %>%
  arrange(outcome) %>%
  write.table(., "clipboard", sep="\t", row.names=FALSE)

glm_outc_gruende %>% filter(term != "(Intercept)" & term != "B1_amf") %>%
  arrange(outcome) %>% filter(p.value < 0.05)
