df_brm %>% filter(`A1_ausgefüllt` == 1 & A1_WBe == "Ja") %>%
  mutate(`A2_ausgefüllt` = ifelse(is.na(`A2_ausgefüllt`), 0, 1)) %>% 
  glm(`A2_ausgefüllt` ~ B1_amf + A1_FB2SQ001 + A1_FB2SQ002 + A1_FB2SQ003 + A1_FB2SQ004 + A1_FB2SQ005,
      family = binomial(link = "logit"),
      data = , .) %>% tidy()


df_brm %>% filter(`A1_ausgefüllt` == 1 & A1_WBe == "Ja") %>%
  mutate(`A2_ausgefüllt` = ifelse(is.na(`A2_ausgefüllt`), 0, 1)) %>% 
  glm(`A2_ausgefüllt` ~ B1_amf + A1_FB2SQ001 + A1_FB2SQ002 + A1_FB2SQ003 + A1_FB2SQ004 + A1_FB2SQ005 + Geschlecht.x + Alter.x,
      family = binomial(link = "logit"),
      data = , .) %>% plot_model()




df_brm %>% filter(`A1_ausgefüllt` == 1 & A1_WBe == "Ja") %>%
  mutate(`A2_ausgefüllt` = ifelse(is.na(`A2_ausgefüllt`), 0, 1)) %>% 
  glm(`A2_ausgefüllt` ~ mot + knsk + env + act,
      family = binomial(link = "logit"),
      data = , .) %>% tidy()


df_brm %>% filter(`A1_ausgefüllt` == 1 & A1_WBe == "Ja") %>%
  mutate(`A3_ausgefüllt` = ifelse(is.na(`A3_ausgefüllt`), 0, 1)) %>% 
  glm(`A3_ausgefüllt` ~ B1_amf + A1_FB2SQ001 + A1_FB2SQ002 + A1_FB2SQ003 + A1_FB2SQ004 + A1_FB2SQ005 + Geschlecht.x + Alter.x,
      family = binomial(link = "logit"),
      data = , .) %>% tidy()

df_brm %>% filter(`A1_ausgefüllt` == 1 & A1_WBe == "Ja") %>%
  mutate(`A3_ausgefüllt` = ifelse(is.na(`A3_ausgefüllt`), 0, 1)) %>% 
  glm(`A3_ausgefüllt` ~ B1_amf + Geschlecht.x + Alter.x,
      family = binomial(link = "logit"),
      data = , .) %>% tidy()
