library(tidyverse)
library(brms)
# einlesen aller BRMS Files aus Models
brm_fits <- map(list.files("models/"),
    ~readRDS(str_c("models/", .x)))
names(brm_fits) <- list.files("models")

summary(brm_fits$a1wbe2.rds)
exp(fixef(brm_fits$a1wbe2.rds))

summary(brm_fits$a1wbe3.rds)
exp(fixef(brm_fits$a1wbe3.rds))

summary(brm_fits$a1wbe4.rds)

summary(brm_fits$a1wbe4.rds)

summary(brm_fits$korrespondenz3.rds)
summary(brm_fits$korrespondenz3_knsk.rds)
summary(brm_fits$korrespondenz3_mot.rds)
summary(brm_fits$korrespondenz3_act.rds)
summary(brm_fits$korrespondenz3_env.rds)

cor(df_brm %>% select(B1_amf, knsk, act, mot, env), use = "pairwise")
lm(B1_amf ~ knsk + act + mot + env, data = df_brm) %>% summary()

cor(df_brm %>% select(B1_amf, oe:lear), use = "pairwise")


summary(brm_fits$korrespondenz3.rds)

summary(brm_fits$steiger_crq_tot.rds)
summary(brm_fits$steiger_crq_act.rds)
summary(brm_fits$steiger_crq_wissen.rds)
summary(brm_fits$steiger_crq_ziele.rds)
summary(brm_fits$steiger_crq_zutrau.rds)
tidy(brm_fits$steiger_crq_tot.rds) %>% write.table(., "clipboard", sep="\t", row.names=FALSE)

summary(brm_fits$steiger)
