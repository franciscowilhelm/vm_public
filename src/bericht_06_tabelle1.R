# Tabelle 1

table(df$A1_ausgefüllt)
table(df$A2_ausgefüllt)
table(df$A3_ausgefüllt)
table(ecoplan_0721$A3)

table(df$B1_ausgefüllt)
table(df$B2_ausgefüllt)

# Ecopla nAusschluss von Personen ohne AnzahlSitzungen
tmp <- df %>% filter(AnzahlSitzungen > 0)
table(tmp$A1_ausgefüllt)
table(tmp$A2_ausgefüllt)
table(tmp$A3_ausgefüllt)
tmp %>% select(A3_B1) %>% filter(!is.na(A3_B1)) %>% nrow()
tmp %>% select(A3_C1) %>% filter(!is.na(A3_C1)) %>% nrow()
table(tmp$B1_ausgefüllt)
table(tmp$B2_ausgefüllt)
