library (tidyverse)
library (stringr)
library (sjlabelled)
library (readxl)
library (forcats)
library(rstatix)
library(ggthemes)
source("https://raw.githubusercontent.com/alanthompsonch/AOP_uni/main/crq_00_palette.R")

#load viamia crq dataframe prepared in the file "crq_01_read.R"
load("data/df_crq.RData")

#load crq dataframe of the "Normstichprobe"
df_sdbb <- read_excel("data/CRQ/SDBB_cleaned_madeleine.xlsx")

#renaming viamia data frame for clarity
df_viamia <- df_crq
remove(df_crq)

#renaming cols in the sdbb data frame for more concistency between the two dfs
colnames(df_viamia)
colnames(df_sdbb)

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

# # Example of code for comparing the data sets
# comp_sdbb <- df_sdbb$nutzer_id %in% df_viamia$id
# ind_sdbb <- which(comp_sdbb)
# 
# comp_viamia <- df_viamia$id %in% df_sdbb$nutzer_id
# ind_viamia <- which(comp_viamia)
# 
# df_sdbb[ind_sdbb,]
# df_viamia[ind_viamia,]

# add source column to each dataframe
df_sdbb <- mutate(df_sdbb, src = "sdbb")
df_viamia <- mutate(df_viamia, src = "viamia")

# Descriptive Statistics by group
# saved as "Table Descriptives Viamia SDBB.xlsx" in vm_public folder.
df_all <- bind_rows(df_sdbb, df_viamia)

group_by(df_all, src) %>% 
  summarise(across(c(knsk,mot,act,env), .fn = mean)) %>% 
  write.table(., "clipboard", sep="\t", row.names=FALSE)

means <- group_by(df_all, src) %>% 
  summarise(across(c(knsk,mot,act,env), .fn = mean))

# Unterschiede zwischen "frit" und "de" waren n.s., code nicht beibehalten, 
# weil too much shit und es nicht Inhalt dieser Auswertung ist.

# SDBB und Viamia unterscheiden sich signifikant --> können geplottet werden.
df_comp <- bind_rows(df_viamia, df_sdbb)
knsk <- aov(knsk ~ src, data = df_comp)
summary(knsk)
mot <- aov(mot ~ src, data = df_comp)
summary(mot)
env <- aov(env ~ src, data = df_comp)
summary(env)
act <- aov(act ~ src, data = df_comp)
summary(act)

# T-Tests zu Differenzen zwischen Datensätzen und deren Ausmass
# knsk
t.test(x = df_viamia$knsk,
       y = df_sdbb$knsk,
       alternative = "two.sided",
       conf.level = 0.95,
       paired = FALSE)

df_comp %>% cohens_d(knsk ~ src, var.equal = TRUE)

# mot
t.test(x = df_viamia$mot,
       y = df_sdbb$mot,
       alternative = "two.sided",
       conf.level = 0.95,
       paired = FALSE)

df_comp %>% cohens_d(mot ~ src, var.equal = TRUE)

# env
t.test(x = df_viamia$env,
       y = df_sdbb$env,
       alternative = "two.sided",
       conf.level = 0.95,
       paired = FALSE)

df_comp %>% cohens_d(env ~ src, var.equal = TRUE)

# act
t.test(x = df_viamia$act,
       y = df_sdbb$act,
       alternative = "two.sided",
       conf.level = 0.95,
       paired = FALSE)

df_comp %>% cohens_d(act ~ src, var.equal = TRUE)


# Table with results
fctrs <- c("knsk","mot","env","act")
means_viamia <- c(3.47,3.17,2.53,3.15)
means_sdbb <- c(3.57,3.37,2.88,3.21)
p_vals <- c(6.691e-06,1.594e-12,2.2e-16,0.01298)
eff_sizes <- c(0.177,0.311,0.520,0.0971)
mags <- c("vernachlässigbar","klein","moderat","vernachlässigbar")

T_test_cohens_results <- data.frame(fctrs,
                                    means_sdbb,
                                    means_viamia,
                                    p_vals,
                                    eff_sizes,
                                    mags)

T_test_cohens_results <- T_test_cohens_results %>% 
  rename(
    "CRQ-Dimension" = fctrs,
    "Mittelwert SDBB" = means_sdbb,
    "Mittelwert viamia" = means_viamia,
    "p-Wert" = p_vals,
    "Effektstärke" = eff_sizes,
    "Cohen's Einordnung" = mags
  )

T_test_cohens_results %>% write.table(., "clipboard", sep="\t", row.names=FALSE)

### PLOTTING ###
theme_set(theme_few(base_size = 11))

size_geom_text <- 3.5

df_plot <- select(df_all, knsk, mot, env, act, src)

# # Beispielplot Boxplot, veraltet
# # Knowledge SKill graph
# ggplot() +
#   aes(y = knsk, fill=src) +
#   geom_boxplot(
#     data = df_plot,
#     outlier.size = 0) +
#   scale_fill_manual(
#     values = c(colors_viamia[2],colors_viamia[4])) +
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.title.y=element_blank(),
#         legend.position = "bottom") +
#   labs(title = "Knowledge and Skills") +
#   guides(fill=guide_legend(title="")) +
#   ylim(c(1,5))



# # facet_wrap version. Dafür muss DF als long version gemacht werden (pivot longer)
# # ICH BEVORZUGE DEN UNTEREN PLOT
# df_plot %>% pivot_longer(c(knsk, mot, env, act)) %>% 
#   ggplot(aes(y = value, fill=src, x = name)) +
#   scale_y_continuous(limits=c(0,5)) +
#   geom_bar(position = "dodge2", stat="summary", fun = mean) +
#   scale_fill_manual(
#     values = c(colors_viamia[2],colors_viamia[4])) +
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.title.y=element_blank()) +
#   guides(fill=guide_legend(title="")) +
#     facet_wrap(. ~ name)

# FINAL PLOT
df_plot$src[df_plot$src == "sdbb"] <- "BSLB"
df_plot$src[df_plot$src =="viamia"] <- "Viamia"
df_plot %>% 
  rename("Wissen und Kompetenzen" = knsk, "Motivation" = mot, "Umfeld" = env, "Aktivitäten" = act) %>% 
  pivot_longer(c(`Wissen und Kompetenzen`, Motivation, Umfeld, Aktivitäten)) %>% 
  ggplot(aes(y = value, fill=src, x = name)) +
  geom_bar(position="dodge", stat="summary", fun = mean) +
  scale_fill_manual(values = c(colors_viamia[2],colors_viamia[4])) +
  aes(x = fct_inorder(name)) +
  xlab("CRQ-Dimension") +
  ylab("Durchschnittlicher Score") +
  guides(fill=guide_legend(title="Stichprobe")) +
  coord_cartesian(ylim = c(1,5))

ggsave("notebooks/plots/plot_vergleich_sdbb_viamia.png", width = 6.27, height = 5)


# post-hoc for subdimensions

map(c("os", "cop", "jcha", "scs"), function(scale) {
  t.test(x = df_viamia[,scale],
         y = df_sdbb[,scale],
         alternative = "two.sided",
         conf.level = 0.95,
         paired = FALSE)
  df_comp %>% cohens_d(formula(str_c(scale, " ~ src", collapse = "")), var.equal = TRUE)
})


map(c("os", "cop", "jcha", "scs"), function(scale) {
  t.test(x = df_viamia[,scale],
         y = df_sdbb[,scale],
         alternative = "two.sided",
         conf.level = 0.95,
         paired = FALSE)
  df_comp %>% cohens_d(formula(str_c(scale, " ~ src", collapse = "")), var.equal = TRUE)
})

sjmisc::frq(df_sdbb$age)
sjmisc::frq(df_viamia$Alter)
sjmisc::frq(df_sdbb$edu)
sjmisc::frq(df_sdbb$gender)
sjmisc::frq(df_viamia$Geschlecht)

# bei Ausschluss von U30
df_u40 <- df_sdbb %>% filter(age > 39)

df_comp2 <- bind_rows(df_viamia, df_u40)
knsk <- aov(knsk ~ src, data = df_comp)
summary(knsk)
mot <- aov(mot ~ src, data = df_comp)
summary(mot)
env <- aov(env ~ src, data = df_comp)
summary(env)
act <- aov(act ~ src, data = df_comp)
summary(act)

df_plot_u30 <- select(df_comp2, knsk, mot, env, act, src)
df_plot_u30$src[df_plot_u30$src == "sdbb"] <- "KBSB"
df_plot_u30$src[df_plot_u30$src =="viamia"] <- "Viamia"
df_plot_u30 %>% 
  rename("Wissen und Kompetenzen" = knsk, "Motivation" = mot, "Umfeld" = env, "Aktivitäten" = act) %>% 
  pivot_longer(c(`Wissen und Kompetenzen`, Motivation, Umfeld, Aktivitäten)) %>% 
  ggplot(aes(y = value, fill=src, x = name)) +
  geom_bar(position="dodge", stat="summary", fun = mean) +
  scale_fill_manual(values = c(colors_viamia[2],colors_viamia[4])) +
  aes(x = fct_inorder(name)) +
  xlab("CRQ-Dimension") +
  ylab("Durchschnittlicher Score") +
  guides(fill=guide_legend(title="Stichprobe")) +
  ylim(c(1,5))

t.test(x = df_viamia$knsk,
       y = df_sdbb$knsk,
       alternative = "two.sided",
       conf.level = 0.95,
       paired = FALSE)

df_comp2 %>% cohens_d(knsk ~ src, var.equal = TRUE)

# mot
t.test(x = df_viamia$mot,
       y = df_u40$mot,
       alternative = "two.sided",
       conf.level = 0.95,
       paired = FALSE)

df_comp2 %>% cohens_d(mot ~ src, var.equal = TRUE)

# env
t.test(x = df_viamia$env,
       y = df_sdbb$env,
       alternative = "two.sided",
       conf.level = 0.95,
       paired = FALSE)

df_comp2 %>% cohens_d(env ~ src, var.equal = TRUE)

# act
t.test(x = df_viamia$act,
       y = df_sdbb$act,
       alternative = "two.sided",
       conf.level = 0.95,
       paired = FALSE)

df_comp2 %>% cohens_d(act ~ src, var.equal = TRUE)
