# ICCs
library(lme4)
# glmer(B1_amf ~ 1 + (1 | Kanton), df, family = cumulative(probit))  #glmer cannot do ordinal responses

fit_amf1 <- brm(B1_amf ~ 1 + (1 | Kanton), df, family = cumulative(probit),
                chains = 2, cores = 2)

summary(fit_amf1)
