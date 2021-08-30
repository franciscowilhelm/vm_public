library(sjPlot)
library(ggcharts)
library(ggthemes)
source("lib/likert_labeller.R")
source("https://raw.githubusercontent.com/alanthompsonch/AOP_uni/main/crq_00_palette.R")

# theme_set(theme_few())

fact_to_num <- function(var) {
  label <- levels(var)
  out <- as.numeric(var) %>% set_labels(labels = label)
}

df %>% select(A3_B1, A3_B2, A3_B3) %>%
  mutate(across(.fns = fact_to_num)) %>% 
  set_label(c("Hat der Beratungsbericht die Inhalte und Ergebnisse der Beratung gut dargestellt?",
              "Hat der Beratungsbericht mögliche weitere Schritte klar aufgeführt?",
              "Wie fanden Sie den Beratungsbericht insgesamt?")) %>%
  plot_stackfrq(show.total = FALSE, show.prc = FALSE, sort.frq = "last.desc") + 
  geom_text(
    aes(y = .data$ypos, label = likert_labeller(.data$prc)), size = size_geom_text) +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  scale_fill_manual(values = blau_abst, 
                    labels = c("Nein / Überhaupt nicht hilfreich",
                               "Eher nein / Wenig hilfreich",
                               "Eher ja / Eher hilfreich",
                               "Ja / Sehr Hilfreich"))

ggsave("notebooks/plots/beratungsbericht_nutzen.png", width = 6.27, height = 3)


