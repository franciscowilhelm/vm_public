
fits_ziele <- map(levels(df$A1_Zielezuord1), function(ziel) {
  data <- df_brm %>% filter(A1_Zielezuord1 == ziel)
  map(methodenvarnames, function(x) {
    data %>% select(.x)))
    }
    
  fit <- map_dfr(methodenvarnames, function(x) {
    formula <- str_c("A3_C2SQ001 ~ ", x, "+ B1_amf + (1 | Kanton)") 
    lmer(formula, data = data) %>% tidy() %>% filter(term != "(Intercept)" & term != "B1_amf") %>% 
      filter(p.value <= 0.05) %>% mutate(ziel)
  })
  return(fit)
})