library (tidyverse)
library (stringr)
library (sjlabelled)
library (readxl)

#load viamia crq dataframe prepared in the file "crq_01_read.R"
load("data/df_crq.RData")

#load crq dataframe of the "Normstichprobe"
df_sdbb <- read_excel("data/CRQ/SDBB_cleaned_madeleine.xlsx")

# #load IT and FR dataframe
# df_frit <- read_excel("data/CRQ/ViaMiaExport 01.06.-31.10.2021_cleaned_FR_IT.xlsx")

#renaming viamia data frame for clarity
df_viamia <- df_crq
remove(df_crq)

#renaming cols in the sdbb data frame for more concistency between the two dfs
colnames(df_viamia)
colnames(df_sdbb)
# colnames(df_frit)

df_sdbb <- df_sdbb %>% 
  rename(
    "bearbeitung_ab" = "Bearbeitung ab",
    bis = Bis,
    oe = OE,
    jmk = JMK,
    ssk = SSK,
    inv = INV,
    con = CONF,
    cl = CCL,
    cop = COP,
    os = OCS,
    mot = MOT,
    jcha = JCHA,
    scs = SCS,
    env = ENV,
    net = NET,
    cexpl = CEXPL,
    lear = LEAR,
    act = ACT,
    knsk = KNSK,
    nutzer_id = "Nutzer-ID"
  )

# df_frit <- df_frit %>% 
#   rename(
#     "bearbeitung_ab" = "Bearbeitung ab",
#     bis = Bis,
#     oe = OE,
#     jmk = JMK,
#     ssk = SSK,
#     inv = INV,
#     con = CONF,
#     cl = CCL,
#     cop = COP,
#     os = OCS,
#     mot = MOT,
#     jcha = JCHA,
#     scs = SCS,
#     env = ENV,
#     net = NET,
#     cexpl = CEXPL,
#     lear = LEAR,
#     act = ACT,
#     knsk = KNSK,
#     nutzer_id = "Nutzer-ID"
#   )

# Since Madeleine said that some data points may be present in both dataframes, I checked the IDs and dates between the data frames, 
# but none of them really match. A few IDs are identical, but the remaining columns of the observation differ wildly; so I guess 
# it's just a consequence of using 5-6 digit ID codes in both data sets. I conclude that there is no observation that was used in both
# dataframes and thus I will move on to comparing them. Code used for comparison commented out but preserved for future reference.
# 
# # #Hit Ctrl + Shift + C to uncomment
# 
# #checking if any values in ID and id are identical. --> Vastly different values between identical numbers; --> different data
# #indices of duplicates in sdbb dataframe
# comp_sdbb <- df_sdbb$ID %in% df_viamia$id
# ind_sdbb <- which(comp_sdbb)
# 
# #indices of duplicates in viamia dataframe
# comp_viamia <- df_viamia$id %in% df_sdbb$id
# ind_viamia <- which(comp_viamia)
# 
# #show rows of non-unique ids to compare visually
# df_sdbb[ind_sdbb,]
# df_viamia[ind_viamia,]
# 
# #Same thing for the column nutzer_id in the sdbb dataframe. 1 identical ID, different data.
# comp_sdbb <- df_sdbb$nutzer_id %in% df_viamia$id
# ind_sdbb <- which(comp_sdbb)
# 
# comp_viamia <- df_viamia$id %in% df_sdbb$nutzer_id
# ind_viamia <- which(comp_viamia)
# 
# df_sdbb_crq[ind_sdbb,]
# df_viamia_crq[ind_viamia,]

# add source column
df_sdbb <- mutate(df_sdbb, src = "sdbb")
df_viamia <- mutate(df_viamia, src = "viamia")
# df_frit <- mutate(df_frit, src = "frit")

df_combined <- bind_rows(df_sdbb, df_viamia)

df_plot <- select(df_combined, knsk, mot, env, act, src)

# Knowledge SKill graph
ggplot() +
  aes(y = knsk, fill=src) +
  geom_boxplot(
    data = df_plot,
    outlier.size = 0,
  )

# Motivation graph
ggplot() +
  aes(y = mot, fill=src) +
  geom_boxplot(
    data = df_plot,
    outlier.size = 0,
  )

# Environment graph
ggplot() +
  aes(y = env, fill=src) +
  geom_boxplot(
    data = df_plot,
    outlier.size = 0,
  )

# Actions graph
ggplot() +
  aes(y = act, fill=src) +
  geom_boxplot(
    data = df_plot,
    outlier.size = 0,
  )
