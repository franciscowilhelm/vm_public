# load packages
library(readxl)
library(sjlabelled)
library(tidyverse)

source("lib/likert_labeller.R")
source("lib/numeric_conv.R")

# load excels. Excel has been modified: all count vars deleted.
ecoplan_1221 <- read_excel("data/ecoplan/Rohdaten_Befragungen und sozioekoDaten_Stand_Dez.xlsx", sheet = 3)
codebook_1221 <- read_excel("data/ecoplan/CodebookALL_20211206.xlsx")

# set label, make anonymous, mutate to factors
ecoplan_1221 <- ecoplan_1221 %>% select(-`...1`)


# setting labels
# x <-data.frame(Variable = c(codebook_1221$Code),
#                Frage = c(codebook_1221$Frage),
#                 Varname = c(names(ecoplan_1221))) %>% View()

var_idx <- map_dbl(names(ecoplan_1221), function(x) {
  out <- which(x == codebook_1221$Code)
  if(length(out) == 0) {
    out <- NA
  }
  return(out)
})

newlabels <- ifelse(var_idx, codebook_1221$Frage, "Label fehlt")

ecoplan_1221 <- set_label(ecoplan_1221, newlabels)


ecoplan_1221 <- ecoplan_1221 %>% select(-c(`A1_Kunde[SQ004]`, A2_email)) %>% 
  mutate(across(c(A1_Bisher:`A1_Gruende[SQ011]`,A1_WBe, A1_Kosten,A1_Zielezuord1, A1_Zielezuord2, A1_Zielezuord3,
                  Ausbildungsstand:Beschäftigungssituation,`A2_A1[SQ001]`:A2_D2, `A3_A1[SQ001]`:`A3_C2[SQ003]`,
                  B1_A1:B1_A3, B1_A5, `B2_A1[SQ001]`:`B2_A1[SQ005]`, `B2_A4[A1]`:`B2_A4[A20]`), as_factor))

# set labels


# get only those variables we need, and rename them properly.
ecoplan_1221 <- ecoplan_1221 %>%
  select(`viamiaID`, `PersonenID`, Geschlecht:Nationalität, Alter, #sociodemographisches
          Ausbildungsstand:Beschäftigungssituation,
         A1_WBe,
          A1_CV:`A1_Gruende[SQ011]`, A1_Zielezuord1, A1_Zielezuord2, A1_Zielezuord3, #A1 Inhalt
          `A2_A3[SQ001]`:A2_D3, #A2 Inhalt
          `A3_A1[SQ001]`:`A3_A1[SQ011]`, A3_B1:`A3_C2[SQ003]`,
          `B1_A1`:B1_A5, #B1
          `B2_A1[SQ001]`:`B2_A1[SQ005]`, #B2
          `B2_A4[A1]`:`B2_A4[A20]`,
         `A1_ausgefüllt`,`A2_ausgefüllt`,`A3_ausgefüllt`, `B1_ausgefüllt`, `B2_ausgefüllt`, AnzahlSitzungen)

# rename
# ecoplan_1221 <- ecoplan_1221 %>%
#   # rename(a1_amf = A1_FB1,
#   #        a1_f = `A1_FB2[SQ001]`,)

# reorder factors


# alphabetically
ecoplan_1221 <- ecoplan_1221 %>% mutate(across(where(is.factor), ~fct_relevel(.x, sort)))

# now the complex part, reorder the levels.
fct_levels_old <- map(ecoplan_1221, function(x) {
  if(is.factor(x)) { levels(x)}
})

fct_levels_index <- map_chr(ecoplan_1221, function(x) {
  if(is.factor(x)) {
    lvl <- levels(x)
    if(lvl[1] == "Eher ja") {
      return("ja4") }
    else if(lvl[2] == "Sehr hilfreich" & length(lvl) == 4) {
      return("hilfreich1") }
    else if(lvl[2] == "Hilfreich" & length(lvl) == 4) {
      return("hilfreich2") }
    else if(lvl[1] == "Eher Hilfreich" & length(lvl) == 3) {
      return("hilfreich3") }
    else if(lvl[1] == "Ja") {
      return("ja2")
    } else if (lvl[1] == "Kaum erreicht" & length(lvl) == 6) {
      return("erreicht")
    } else if(lvl[1] == "Eher vertieft behandelt") {
      return("behandelt")
    } else if(lvl[1] == "Eingesetzt") {
      return("eingesetzt")
    } else {
      return("other")
      }
  } else if(!is.factor(x)) {
    return("no factor")
  }
  })


fct_new <- map2(ecoplan_1221, fct_levels_index, function(x, i) {
  switch(i,
        ja4  = {return(fct_relevel(x, "Nein", "Eher nein", "Eher ja", "Ja"))},
        ja2 = {return(fct_relevel(x, "Nein", "Ja"))},
        hilfreich1 = {return(fct_relevel(x, "Überhaupt nicht hilfreich", "Wenig hilfreich", "Eher hilfreich", "Sehr hilfreich"))},
        hilfreich2 = {return(fct_relevel(x, "Überhaupt nicht hilfreich", "Wenig hilfreich", "Eher hilfreich", "Hilfreich"))},
        
        erreicht = {return(fct_relevel(x, "Nicht erreicht / keine Veränderung", "Kaum erreicht", "Teilweise erreicht",
                                       "Mehrheitlich erreicht", "Vollständig erreicht", "Ziel ist nicht mehr relevant"))},
        behandelt = {return(fct_relevel(x, "Nicht vertieft behandelt", "Wenig vertieft behandelt", "Eher vertieft behandelt",
                                        "Sehr vertieft behandelt"))},
        eingesetzt = {return(fct_relevel(x, "Nicht eingesetzt", "Eingesetzt"))}
        )
})
fct_new <- fct_new[!map_lgl(fct_new, is.null)] %>% as_tibble()

ecoplan_1221[,names(fct_new)] <- fct_new

# match with CRQ
# source("src/crq_01_read.R", encoding = 'UTF-8')
load("data/df_CRQ.RData")

df_crq <- df_crq %>% mutate(idmatch = str_c(toupper(kanton), id))

# unmatched df
unmatched <- anti_join(df_crq, ecoplan_1221, by = c("idmatch" = "viamiaID"))

# Correcting some observations identified in the unmatched data frame: 20.07.2021.
# Refer to "src/ecoplan_00_missmatches_alan.Rmd" for details
old_vd <- unmatched %>% as_tibble() %>% filter(kanton == "vd") %>% select("id") %>%
  deframe()
vd_id <- old_vd %>%  str_replace_all("021-", "o21-")

idx <- vd_id %>%  str_detect("[a-zA-Z]21\\d")

vd_id %>% .[idx] #trifft die richtigen
vd_id[idx]  <- vd_id[idx] %>% sub( '(?<=.{3})', '-', ., perl=TRUE) # the gods of stackoverflow are benign

idx_crqdf <- map_dbl(old_vd, ~str_which(df_crq$id, fixed(.x)))
df_crq$id[idx_crqdf] <- vd_id
df_crq <- df_crq %>% mutate(idmatch = str_c(toupper(kanton), id))
unmatched_corr <- anti_join(df_crq, ecoplan_1221, by = c("idmatch" = "viamiaID"))


df <- left_join(ecoplan_1221, df_crq %>% select(-Geschlecht, -Alter),
                by = c("viamiaID" = "idmatch"))

# filter AnzahlSitzungen NA or 0 (latter does not happen though)
#df <- df %>% filter(AnzahlSitzungen > 0 & !is.na(AnzahlSitzungen))
# new education variable



# mutate kanton variable
df <- df %>% rename(kanton_crq = kanton) %>% mutate(Kanton = as.factor(Kanton))


# manual fixes for specific factor variables
# Einschaetzung AMF, ziele recoded

zielerec <- map_dfc(levels(df$A1_Zielezuord1), function(ziel) {
  ifelse(df$A1_Zielezuord1 == ziel | df$A1_Zielezuord2 == ziel | df$A1_Zielezuord3 == ziel,
         1,
         0) %>%
    enframe(name = NULL, value = ziel)
})
names(zielerec) <- c("Allgemeine Fähigkeiten",
                     "Arbeitseinsatz",
                     "Arbeitsmarktwissen",
                     "Berufliche Expertise",
                     "Entwicklungsmöglichkeiten beim jetzigen Arbeitgeber",
                     "Klarheit",
                     "Netzwerken",
                     "Sonstiges",
                     "Unterstützung durch soziales Umfeld",
                     "Zutrauen")

idx <- apply(zielerec, 1, sum, na.rm = TRUE)
idx <- ifelse(idx > 0, TRUE, FALSE)
zielerec[idx,] <- apply(zielerec[idx,], 2, function(i) {
  # print(is.na(i))
  i[is.na(i)] <- 0 
  return(i)
  })

# einordnung in die vier kategorien
zielerec <- zielerec %>% mutate(z_knsk = ifelse(zielerec$`Arbeitsmarktwissen` == 1|
                                                  zielerec$`Berufliche Expertise` == 1| zielerec$`Allgemeine Fähigkeiten` == 1, 1, 0),
                                z_mot = ifelse(zielerec$`Klarheit` == 1|
                                                  zielerec$`Zutrauen` == 1| zielerec$`Arbeitseinsatz` == 1, 1, 0),
                                z_env = ifelse(zielerec$`Unterstützung durch soziales Umfeld` == 1|
                                                 zielerec$`Entwicklungsmöglichkeiten beim jetzigen Arbeitgeber` == 1, 1,0),
                                z_act = ifelse(zielerec$`Netzwerken` == 1, 1,0)
                                )

# as factors
zielerec <- zielerec %>% mutate(across(.fns = as.factor))

df <- df %>% mutate(B1_amf = as.ordered(fct_relevel(B1_A5, "Sehr schwach ausgeprägt",
                                         "Eher schwach ausgeprägt", "Mittelmässig ausgeprägt",
                                         "Eher stark ausgeprägt", "Sehr stark ausgeprägt"))) %>% 
  cbind(zielerec)
# remove keine Angabe for later analyses / make it NA
df$B1_amf[df$B1_amf == "Keine Angabe"] <- NA
df$B1_amf <- fct_drop(df$B1_amf)

# Keine Angabe into NA

# (ordered) factors to numeric (check that it did not break code)
df <- df %>% mutate(across(c(B1_amf, B1_A1, A1_FB1, `A2_B3[SQ001]`:`A2_B3[SQ004]`, `B2_A1[SQ001]`:`B2_A1[SQ005]`,
                             `A1_FB2[SQ001]`, `A1_FB2[SQ002]`, `A1_FB2[SQ003]`, `A1_FB2[SQ004]`, `A1_FB2[SQ005]`),
                           fact_to_num))
df <- df %>% mutate(AnzahlSitzungen = as.numeric(AnzahlSitzungen))

# score nutzen over all questions
df <- df %>% mutate(A1_NutzenInsg = psych::scoreItems(keys = rep(1,5),
                                                      items = df %>% select(`A1_FB2[SQ001]`:`A1_FB2[SQ005]`) %>% mutate(across(.fns = as.numeric))
) %>% .$scores)


# education recode
# tmp <- levels(df$Ausbildungsstand)
df <- df %>%
   mutate(Ausb_recode = fct_collapse(Ausbildungsstand,
                                     sek = c("Berufliche Grundbildung"),
                                     tert = c("Fachhochschule, Pädagogische Hochschule (inkl. NDS, CAS/DAS/MAS)",
                                              "Höhere Berufsbildung, Berufsbildung Höhere Fachprüfung",
                                              "Universität, ETH (inkl. NDS, CAS/DAS/MAS)"),
                                     other = c("Berufsmaturität (BM1, BM2)", "Fachmittelschule, andere Mittelschule",
                                               "Mittelschule mit gymnasialer Matura (Gymnasium)",
                                               "Schulisches Zwischenjahr oder berufliches Vorbereitungsjahr nach Sekundarstufe (Brückegebote)",
                                               "Volksschule, obligatorische Schulzeit, inkl. «ohne Ausbildung»"))) %>% 
  mutate(Ausb_recode = fct_relevel(Ausb_recode, "sek", "tert", "other"))

df <- df %>%
  mutate(Ausb_recode2 = fct_collapse(Ausbildungsstand,
                                    sek = c("Berufliche Grundbildung"),
                                    hb = c("Höhere Berufsbildung, Berufsbildung Höhere Fachprüfung"),
                                    uni_fh = c("Fachhochschule, Pädagogische Hochschule (inkl. NDS, CAS/DAS/MAS)",
                                             "Universität, ETH (inkl. NDS, CAS/DAS/MAS)"),
                                    other = c("Berufsmaturität (BM1, BM2)", "Fachmittelschule, andere Mittelschule",
                                              "Mittelschule mit gymnasialer Matura (Gymnasium)",
                                              "Schulisches Zwischenjahr oder berufliches Vorbereitungsjahr nach Sekundarstufe (Brückegebote)",
                                              "Volksschule, obligatorische Schulzeit, inkl. «ohne Ausbildung»"))) %>% 
  mutate(Ausb_recode2 = fct_relevel(Ausb_recode2, "sek", "hb", "uni_fh", "other"))





# brm is unable to deal with these varnames, so we have to rename
tmp <- map_chr(names(df), ~str_replace(.x, fixed("["),"")) %>% map_chr(~str_replace(.x, fixed("]"),""))
df_brm <- df %>% as_tibble()
names(df_brm) <- tmp

# set "ziel ist nicht mehr relevant" to NA
df_brm$A3_C2SQ001[df_brm$A3_C2SQ001 == "Ziel ist nicht mehr relevant"] <- NA
df_brm$A3_C2SQ002[df_brm$A3_C2SQ002 == "Ziel ist nicht mehr relevant"] <- NA
df_brm$A3_C2SQ003[df_brm$A3_C2SQ003 == "Ziel ist nicht mehr relevant"] <- NA

df_brm <- df_brm %>% mutate(across(starts_with('A3_C2SQ'), ~fct_drop(.x)))

# convert A3A1 : 11 ASpekte der Ressourcenförderung, set "nicht relevant" to NA
df_brm <- df_brm %>% mutate(across(starts_with("A3_A1SQ"), function(x) {
  x[x == "Nicht relevant"] <- NA
  x <- fct_drop(x)
  x
}))

df_brm <- df_brm %>% mutate(across(c(A3_A1SQ001:A3_A1SQ011)
                                   , .fns = as.numeric))

df_brm <- df_brm %>% mutate(A3_C2SQ001 = as_numeric(A3_C2SQ001))


save(ecoplan_1221, df,df_brm, file = "data/ecoplan_data.RData")
