# plots 
library(tidyverse)
library(ggplot2)
library(rockchalk)
library(ggpubr)
library(ggrastr)
source('src/crq_01_read.R', encoding = 'UTF-8')
source("https://raw.githubusercontent.com/alanthompsonch/AOP_uni/main/crq_00_palette.R")

# age etc
hist(df_crq$Alter)
sjmisc::frq(df_crq$Geschlecht)
sjmisc::frq(df_crq$education_de)

edlvls <- levels(df_crq$education_de)[-c(1,7)] #don't do it for obligatorisch and doktorat - not enough cases yet

alpha_by_edu <- map(edlvls, function(edlvl) {
  df_crq %>%
    select(education_de, oe1_work:lear3_work) %>% 
    filter(education_de == edlvl) %>% 
    scoreItemsMulti(scalenames, ., exclude = TRUE) %>% 
    .$alpha
}) 



alpha_by_edu %>% map(~min(.x)) # alpha mins increase by education, but its still adequate with lower level.
alpha_by_edu %>% map(~mean(.x)) 

# data frame modification Alan
crq_alan <- df_crq

# Renaming levels for easier coding (Checked the values, no levels were swapped)
levels(crq_alan$education_de) <- c("Obl", "MS", "BL", "HF", "BA", "Mas", "Dok")

# merging doctorate and Master factor levels
crq_alan$education_de <- combineLevels(crq_alan$education_de, 
                          levs = c("Mas", "Dok"),
                          newLabel = "MaD")
# Adding total crq score
crq_alan <- crq_alan %>% 
  mutate(total_score = (knsk + mot + env + act)/4)


aov(total_score ~ education_de, crq_alan) %>% summary()

### MEAN COMPARISONS ###

# Total
compare_means(total_score ~ education_de, data = crq_alan)

comp_tot <- list( c("Obl", "BL"),
                  c("BL", "HF"),
                  c("BL", "BA"),
                  c("BL", "MaD"))

# Knowledge Skills
compare_means(knsk ~ education_de, data = crq_alan)

comp_knsk <- list( c("BL", "MS"),
                   c("BL", "HF"),
                   c("BL", "BA"),
                   c("BL", "MaD"))

# Motivation
compare_means(mot ~ education_de, data = crq_alan)

comp_mot <- list( c("Obl", "BL"),
                  c("Obl", "HF"),
                  c("Obl", "BA"),
                  c("Obl", "MaD"),
                  c("Obl", "MS"))

# Environment
compare_means(env ~ education_de, data = crq_alan)

comp_env <- list( c("BL", "HF"),
                  c("HF", "BA"),
                  c("HF", "MaD"))

# Act
compare_means(act ~ education_de, data = crq_alan)

comp_act <- list( c("Obl", "BL"),
                  c("MS", "HF"),
                  c("MS", "MaD"),
                  c("BL", "HF"),
                  c("BL", "BA"),
                  c("BL", "MaD"))
### PLOTS ###

# Total Effect of education on total career resources questionnaire score
plot_tot <- ggplot(crq_alan) +
  aes(x = education_de, y = total_score, fill = education_de) +
  labs(title = "Gesamtscore", fill = "Bildungsstand") +
  xlab("Bildungsstand") +
  ylab("CRQ-Gesamtwert") +
  theme_minimal() +
  geom_boxplot_jitter(outlier.size = 1, outlier.jitter.width = 0.1, outlier.alpha = 0.75) + # jittering the outliers
  scale_y_continuous(labels = c(1,2,3,4,5,"")) +
  stat_compare_means(comparisons = comp_tot, 
                     symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), # Setting significance levels according to 'compare_means' above. Feel free to change levels, but the lists (comp_act etc.) must then be
                                        symbols = c("***", "**", "*", "ns")),   # changed to not include the comparisons with p == 0.01 < p < 0.05 if only the significant differences should be shown.
                     step.increase = 0.058) +         # distance btwn sig. bars
  stat_compare_means(label.y = 5.4, label.x = 0.8) +
  scale_x_discrete(labels = c("","","","","","")) +     # Deleting the x-axis labels
  fill_palette(palette = colors_viamia) # Palette pulled from GitHub (Line 8)

plot_tot

# Knowledge Skills
plot_knsk <- ggplot(crq_alan) +
  aes(x = education_de, y = knsk, fill = education_de) +
  geom_boxplot(shape = "circle", show.legend = FALSE, outlier.shape = NA) +
  labs(title = "Knowledge skills", fill = "Bildungsstand") +
  xlab("Bildungsstand") +
  ylab("Knowledge Skills") +
  theme_minimal() +
  geom_boxplot_jitter(outlier.size = 1, outlier.jitter.width = 0.1, outlier.alpha = 0.8) +
  scale_y_continuous(labels = c(1,2,3,4,5,"")) +
  stat_compare_means(comparisons = comp_knsk, 
                     symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                        symbols = c("***", "**", "*", "ns")),
                     step.increase = 0.058) +
  stat_compare_means(label.y = 5.88, label.x = 0.8) +
  scale_x_discrete(labels = c("","","","","","")) +
  fill_palette(palette = colors_viamia)

plot_knsk

# Motivation
plot_mot <- ggplot(crq_alan) +
  aes(x = education_de, y = mot, fill = education_de) +
  geom_boxplot(shape = "circle", show.legend = FALSE, outlier.shape = NA) +
  labs(title = "Motivation", fill = "Bildungsstand") +
  xlab("Bildungsstand") +
  ylab("Motivation") +
  theme_minimal() +
  geom_boxplot_jitter(outlier.size = 1, outlier.jitter.width = 0.1, outlier.alpha = 0.8) +
  scale_y_continuous(labels = c(1,2,3,4,5,"")) +
  stat_compare_means(comparisons = comp_mot, 
                     symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                        symbols = c("***", "**", "*", "ns")),
                     step.increase = 0.058) +
  stat_compare_means(label.y = 6.1, label.x = 5.5) +
  scale_x_discrete(labels = c("","","","","","")) +
  fill_palette(palette = colors_viamia)

plot_mot

# Environment
plot_env <- ggplot(crq_alan) +
  aes(x = education_de, y = env, fill = education_de) +
  geom_boxplot(shape = "circle", show.legend = FALSE, outlier.shape = NA) +
  labs(title = "Environment", fill = "Bildungsstand") +
  xlab("Bildungsstand") +
  ylab("Environment") +
  theme_minimal() +
  geom_boxplot_jitter(outlier.size = 1, outlier.jitter.width = 0.1, outlier.alpha = 0.8) +
  scale_y_continuous(labels = c(1,2,3,4,5,"")) +
  stat_compare_means(comparisons = comp_env, 
                     symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                        symbols = c("***", "**", "*", "ns")),
                     step.increase = 0.058) +
  stat_compare_means(label.y = 5.535, label.x = 0.8) +
  scale_x_discrete(labels = c("","","","","","")) +
  fill_palette(palette = colors_viamia)

plot_env

# Act
plot_act <- ggplot(crq_alan) +
  aes(x = education_de, y = act, fill = education_de) +
  geom_boxplot(shape = "circle", show.legend = FALSE, outlier.shape = NA) +
  labs(title = "Act", fill = "Bildungsstand") +
  xlab("Bildungsstand") +
  ylab("Act") +
  theme_minimal() +
  geom_boxplot_jitter(outlier.size = 1, outlier.jitter.width = 0.1, outlier.alpha = 0.8) +
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7), labels = c(0,1,2,3,4,5,"","")) +
  stat_compare_means(comparisons = comp_act, 
                     symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                        symbols = c("***", "**", "*", "ns")),
                     step.increase = 0.085) +
  stat_compare_means(label.y = 6.7, label.x = 0.8) +
  scale_x_discrete(labels = c("","","","","","")) +
  fill_palette(palette = colors_viamia)

plot_act

# ANOVA of total effect size of each dimension
aov(knsk ~ education_de, df_crq) %>% summary()
aov(mot ~ education_de, df_crq) %>% summary()
aov(env ~ education_de, df_crq) %>% summary()
aov(act ~ education_de, df_crq) %>% summary()
# differences are strongest in knsk.