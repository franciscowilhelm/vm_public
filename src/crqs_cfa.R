# 3. 13 factor CFA (M1) ---------
library(lavaan)
library(tidyverse)


model11 <- "
 # knsk
  oexp =~ oexp_1 + oexp_2 + oexp_3
  jmk =~ jmk_1 + jmk_2 + jmk_3
  ssk =~ ssk_1 + ssk_2 + ssk_3
  # mot 
  inv =~ inv_1 + inv_2 + inv_3
  conf =~ conf_1 + conf_2 + conf_3 + conf_4
  ccl =~ ccl_1 + ccl_2 + ccl_3
  # env
  scs =~ scs_1 + scs_2 + scs_3
  jcha =~ jcha_1 + jcha_2 + jcha_3
  # act
  net =~ net_1 + net_2 + net_3
  cexpl =~ cexpl_1 + cexpl_2 + cexpl_3
  lear =~ lear_1 + lear_2 + lear_3
"

res11 <- cfa(model11, data = df_crqs_final, estimator = "MLR" )
# summary(res11, estimates = FALSE, fit.measures = TRUE)

model4h <- "
 # knsk
  oexp =~ oexp_1 + oexp_2 + oexp_3
  jmk =~ jmk_1 + jmk_2 + jmk_3
  ssk =~ ssk_1 + ssk_2 + ssk_3
  # mot 
  inv =~ inv_1 + inv_2 + inv_3
  conf =~ conf_1 + conf_2 + conf_3 + conf_4
  ccl =~ ccl_1 + ccl_2 + ccl_3
  # env
  scs =~ scs_1 + scs_2 + scs_3
  jcha =~ jcha_1 + jcha_2 + jcha_3
  # act
  net =~ net_1 + net_2 + net_3
  cexpl =~ cexpl_1 + cexpl_2 + cexpl_3
  lear =~ lear_1 + lear_2 + lear_3
  
  knsk =~ oexp + jmk + ssk
  mot =~ conf + inv + ccl
  act =~ net + cexpl + lear
  soc =~ a*scs + a*jcha
  knsk ~~  mot + act + soc
  mot ~~ act + soc
  act ~~ soc
"

res4h <- cfa(model4h, data = df_crqs_final, estimator = "MLR" )



# 3.3 4-factor CFA (only dimensions) (M3)-------
model_4factor <- '
  knsk =~ oexp_1 + oexp_2 + oexp_3 + jmk_1 + jmk_2 + jmk_3 + ssk_1 + ssk_2 + ssk_3
  mot =~ inv_1 + inv_2 + inv_3 + conf_1 + conf_2 + conf_3 + conf_4 + ccl_1 + ccl_2 + ccl_3
  env =~ scs_1 + scs_2 + scs_3 + jcha_1 + jcha_2 + jcha_3
  act =~ net_1 + net_2 + net_3 +cexpl_1 + cexpl_2 + cexpl_3 + lear_1 + lear_2 + lear_3
  '
res_4factor <- cfa(model_4factor, df_crqs_final, std.lv = TRUE,
                   estimator = "MLR")

# summary(res_4factor, estimates = FALSE, fit.measures = TRUE)
model_1factor <- '
  crq =~ oexp_1 + oexp_2 + oexp_3 + jmk_1 + jmk_2 + jmk_3 + ssk_1 + ssk_2 + ssk_3 +
  inv_1 + inv_2 + inv_3 + conf_1 + conf_2 + conf_3 + conf_4 + ccl_1 + ccl_2 + ccl_3 + 
  jcha_1 + jcha_2 + jcha_3 + scs_1 + scs_2 + scs_3 +
  net_1 + net_2 + net_3 +cexpl_1 + cexpl_2 + cexpl_3 + lear_1 + lear_2 + lear_3
  '
res_1factor <- cfa(model_1factor, df_crqs_final, std.lv = TRUE,
                   estimator = "MLR")

# summary(res_1factor, estimates = FALSE, fit.measures = TRUE)


# EFA
# scalenames <- c("oexp", "jmk", "ssk", "inv", "conf", "ccl", "jcha", "scs", "net", "cexpl", "lear")
# crqs_items <- map_dfc(scalenames, function(scl) df_crqs_final %>% select(starts_with(scl))) 
#psych::vss(crqs_items)
#psych::fa.parallel(crqs_items)
# psych::fa(crqs_items, 11) %>% psych::fa.sort()
