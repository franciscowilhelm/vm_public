# CRQ einlesen, Skalen erstellen und mit Metadaten (Kantone etc.) anreichern.

# packages
library(stringr)
library(tidyverse)
library(sjlabelled)
source("https://raw.githubusercontent.com/franciscowilhelm/r-collection/master/scoreItemsMulti.R")

# read datakey
datakey_raw <- readLines("data/CRQ/sdbb-career-resources-datakey-14-Nov-2017 (2).txt", encoding = "UTF-8")


id <-  str_subset(datakey_raw, "ItemID:") %>% str_remove(fixed("ItemID: ")) %>% str_c("item", .)
name <- str_subset(datakey_raw, "Name:") %>% str_remove(fixed("Name: "))
label <- str_subset(datakey_raw, "Text:") %>% str_remove(fixed("Text: "))

# viamia excel(s)
crq_0321_raw <- readxl::read_excel('data/CRQ/ViaMia Export 1.1. - 31.3.2021.xlsx', skip = 2)
crq_0621_raw <- readxl::read_excel("data/CRQ/ViaMiaExport 01.04.-31.05.2021.xlsx", skip = 2)
crq_1121_raw <- readxl::read_excel("data/CRQ/ViaMiaExport 01.06.-31.10.2021.xlsx", skip = 2)

# compare variables

setdiff(names(crq_0321_raw), names(crq_0621_raw))
setdiff(names(crq_0621_raw), names(crq_0321_raw))
setdiff(names(crq_1121_raw), names(crq_0621_raw))

# View(crq_0321_raw[,"item2280"]) #2280 = empty anyway
same_viamia_id <- inner_join(crq_0321_raw, crq_0621_raw, by = "ViaMia-ID") # two persons have same ID. discard both.
crq_raw <- full_join(crq_0321_raw %>% filter(`ViaMia-ID` != "via-be-123080" | `ViaMia-ID` != "via-be-123500"),
                     crq_0621_raw %>% filter(`ViaMia-ID` != "via-be-123080" | `ViaMia-ID` != "via-be-123500"))

# new stuff november 
same_viamia_id <- inner_join(crq_raw, crq_1121_raw, by = "ViaMia-ID") # no persons have both
crq_raw <- full_join(crq_raw, crq_1121_raw)

# filter with out viamia ID as they do not belong to viamia (probably at least)
crq_raw <- crq_raw %>% filter(!is.na(`ViaMia-ID`))


# TRANSFORM DATA ---------

# 1. variable names, 2. labels, 3. extract kanton from ID, 4. build scales

# 1. variable names and subsetting

# TODO: could be enhanced such that not names are replaced, but string parts (str_replace).
# This would be helpful for renaming of timestamps and other vars
items_not_found <- !(id %in% names(crq_raw))
name[items_not_found] #unerheblich.
#extract only the first position, as the other ones are timestamps or open text fields.
id_pos <- map_dbl(id, ~str_which(names(crq_raw),
                   fixed(.x))[1]) 
# now with the positions, rename the items
crq <- crq_raw
names(crq)[id_pos] <- name
# reduce number of variables
crq <- crq %>% select(1:11, all_of(name))

# can stud be removed? yes:
# crq %>% select(contains("stud")) %>% map(., ~sum(is.na(.x)))
crq <- crq %>% select(-contains("stud")) %>% select(-contains("bachelor"))

# 2. set label
label2 <- c(names(crq)[1:11], label[1:44])
crq <- set_label(crq, label2)

# convert dates to dates data types and order data frame by date
crq$`Bearbeitung ab` <- parse_datetime(crq$`Bearbeitung ab`, "%d.%m.%Y %H:%M:%S")
crq$`Bis` <- parse_datetime(crq$`Bis`, "%d.%m.%Y %H:%M:%S")
crq <- arrange(crq, `Bearbeitung ab`)

# Alan's code to clean up IDs
# Copied first 3 rows of splitting step to help with canton identification
crq$`ViaMia-ID` <- tolower(crq$`ViaMia-ID`)
split_viamia_id <- str_split_fixed(crq$`ViaMia-ID`, "-", n = 3) %>% as.data.frame()
kantone <- split_viamia_id[,2]
# logical vector with invalid cantons
kant_false <- !(kantone %in% c("be","bl","bs","fr","ge","ju","ti","vd","vs","zg","zh"))
ind_false <- which(kant_false)
# display false ViaMia-IDs in base data (crq) and the relevant indexes

# df with all false IDs
id_false_df <-  crq[ind_false,3]

current_len_ind_false <- nrow(id_false_df)

if (current_len_ind_false != length(ind_false)) {
  print(crq[,3][ind_false,])
  cat("\nIndeces\n")
  print(ind_false)
  cat("\nNumber of false Indeces\n")
  print(length(ind_false))
  msgBox <- tcltk::tkmessageBox(title = "Manual correction required!", 
                         message = "1. compare 'Indeces' with the manual corrections from line 87 onwards
                         \n2. Correct any indeces that have not yet been corrected
                         \n3. change the value of 'current_len_ind_false' in line 74 to the new 'Number of false Indeces'
                         \n4. Click 'OK'",
                         icon = "warning",
                         type = "ok")
  
}

# Manual corrections of false IDs
crq[[ind_false[1],3]] <- "via-vs-88982"
crq[[ind_false[2],3]] <- "via-be-132019"
crq[[ind_false[3],3]] <- "via-vs-281428"
crq[[ind_false[4],3]] <- "via-be-123651"
crq[[ind_false[5],3]] <- "via-vs-151779"
crq[[ind_false[6],3]] <- "via-be-124344"
crq[[ind_false[7],3]] <- "via-be-130727"




# 3. extract kanton from ID
crq$`ViaMia-ID` <- tolower(crq$`ViaMia-ID`)
split_viamia_id <- str_split_fixed(crq$`ViaMia-ID`, "-", n = 3) %>% as.data.frame()
names(split_viamia_id) <- c("via", "kanton", "id")
# manual fixes needed where bindestrich was forgotten.
unique(split_viamia_id$kanton)

# 5. demographics ----
crq <- crq %>%  mutate(education_de = factor(education_de, labels = c("Obligatorische Schulpflicht",
                                                                                "Mittelschule (Gymnasium, Abitur, Handelsschule, Fachmittelschule)",
                                                                                "Berufslehre",
                                                                                "Höhere Fachschule",
                                                                                "Bachelor (Universität / Fachhochschule)",
                                                                                "Master (Universität / Fachhochschule)",
                                                                                "Doktorat")))

# 4. Scale scoring
scalenames <- c("oe", "jmk", "ssk", "inv", "con", "cl", "cop", "os", "jcha", "scs", "net", "cexpl", "lear")
crq <- crq %>% mutate(across(contains(scalenames), as.numeric)) # convert to numeric


# crq_scales <- map(scalenames, function(x) {
#   df <- crq %>% select(contains(x))
#   psych::scoreItems(keys =rep(1, ncol(df)),
#                     df, impute = "none")
# })

crq_scales <- scoreItemsMulti(scalenames, crq, exclude = TRUE)
crq_scales$alpha
df_crq <- crq_scales$scores %>% as.data.frame()

# hierarchical scores
crq_scales_h <- psych::scoreItems(list(knsk = c("oe", "jmk", "ssk"),
                       mot = c("inv", "con", "cl"),
                       env = c("cop", "os", "jcha", "scs"),
                       act = c("net", "cexpl", "lear")),
                  df_crq)

df_crq <- bind_cols(df_crq,
                    crq_scales_h$scores %>% as.data.frame())

df_descriptions <- crq %>% select(`Bearbeitung ab`:Testverfahren, education_de) %>%
  mutate(lang = recode(Testverfahren, "Karriere-Ressourcen Fragebogen, 1.4" = "de",
                       "Karriere-Ressourcen Fragebogen, 1.3"  = "de",
                       "Questionnaire des Ressources de Carrière, 1.1" = "fr",
                       
                       "Questionario sulle risorse di carriera personali, 1.1" = "it")) %>% 
  select(-Testverfahren, -`Bezeichnung der Befragung`) %>% 
  mutate(lang = as.factor(lang))
df_crq <- bind_cols(split_viamia_id %>% select(kanton, id),
                    df_descriptions,
                    df_crq,
                    crq %>% select(oe1_work:lear3_work))
save("df_crq", file = "data/df_crq.RData")

# gtsummary::tbl_summary(df_crq) # summarizing



