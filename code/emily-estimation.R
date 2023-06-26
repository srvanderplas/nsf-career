
library(tidyverse)
library(patchwork)
library(ggforce)
# library(ggpubr)
library(here)
library(readr)
library(knitr)
library(kableExtra)
# library(pander)

# library(ggwordcloud)
# library(wordcloud)
library(RColorBrewer)
# library(wordcloud2)
# library(tm)
# library(tidytext)
# library(corpus)
# library(reshape2)

# ragg_png = function(..., res = 192) {
#   ragg::agg_png(..., res = res, units = "in")
# }

# dev = "ragg_png", fig.ext = "png"
estimation_model_data <- read_csv("https://github.com/earobinson95/EmilyARobinson-UNL-dissertation/raw/main/data/03-estimation/estimation-model-data.csv")
q0_text_summary <- read_csv("https://github.com/earobinson95/EmilyARobinson-UNL-dissertation/raw/main/data/03-estimation/q0-text-summary.csv")
estimation_simulated_data <- read_csv("https://github.com/earobinson95/EmilyARobinson-UNL-dissertation/raw/main/data/03-estimation/estimation-simulated-data.csv") %>%
  mutate(x = x - 3000)
estimation_scenario_text <- read_csv("https://github.com/earobinson95/EmilyARobinson-UNL-dissertation/raw/main/data/03-estimation/estimation-scenario-text.csv")
estimation_parameters <- read_csv("https://github.com/earobinson95/EmilyARobinson-UNL-dissertation/raw/main/data/03-estimation/estimation-parameters.csv")
estimation_questions <- read_csv("https://github.com/earobinson95/EmilyARobinson-UNL-dissertation/raw/main/data/03-estimation/estimation-questions.csv")
population_estimates_data <- read_csv("https://github.com/earobinson95/EmilyARobinson-UNL-dissertation/raw/main/data/03-estimation/first-level-population-estimates.csv")

grid_lines_data <- tibble(scale = c(rep("linear", 12), rep("log2", 10)),
                          grid_lines = c(seq(0,55000, 5000),
                                         2^seq(7,16))
) %>%
  expand_grid(dataset = c("dataset1", "dataset2"))

densityPlot <- function(data, datasetID, estimate, xlabel = "Estimate", x_limits = c(0,70000), zoom = F, scalesx = T, zoom_limits = c(NA,NA), gridlines = T, rugjitter = 0.25){

  estPlot <- data %>%
    filter(dataset %in% datasetID) %>%
    ggplot(aes_string(x = estimate, fill = "scale", color = "scale")) +
    geom_density(alpha = 0.5, color = NA) +
    geom_rug(aes(y = -Inf), alpha = 0.6, show.legend = F, position = position_jitter(width = rugjitter, height = 0)) +
    geom_vline(aes(xintercept = true_value, linetype = "a")) +
    geom_vline(aes(xintercept = closest_pt_value, linetype = "b"))

  if(gridlines){
    estPlot <- estPlot +
      geom_vline(data = grid_lines_data %>% filter(dataset == "dataset1", grid_lines >= x_limits[1], grid_lines <= x_limits[2]),
                 aes(xintercept = grid_lines, color = scale, linetype = "c"))
  }

  estPlot <- estPlot +
    theme_bw() +
    theme(aspect.ratio = 0.5) +
    scale_color_manual("Scale", values = c("steelblue", "orange3")) +
    scale_fill_manual("Scale", values = c("steelblue", "orange3")) +
    scale_linetype_manual("", labels = c("True Value", "Closest Point Value", "Grid Line Breaks"), values = c("solid", "dashed", "dotted")) +
    scale_y_continuous("Density", labels = scales::comma)

  if(zoom){
    estPlot <- estPlot +
      facet_zoom(xlim = zoom_limits)
  }

  if(scalesx){
    estPlot <- estPlot +
      scale_x_continuous(xlabel, limits = x_limits)
  }

  return(estPlot)

}

qi1_data <- estimation_model_data %>%
  filter(q_id == "QI1") %>%
  mutate(response = as.numeric(response)) %>%
  filter(response < 500000)

# Intermediate Q1
# DATA SET 1
vline1 = c(14750)
vline2 = c(15000)

p1 <- qi1_data %>%
  filter(dataset %in% "dataset1") %>%
  ggplot(aes_string(x = "response", fill = "scale", color = "scale")) +
  geom_density(alpha = 0.5, color = NA) +
  geom_rug(aes(y = -Inf), alpha = 0.6, show.legend = F,
           position = position_jitter(width = 100, height = 0)) +
  geom_vline(aes(xintercept = vline1, linetype = "a")) +
  geom_vline(aes(xintercept = vline2, linetype = "b"))+
  theme_bw() +
  theme(aspect.ratio = 0.5) +
  scale_color_manual("Y Scale", values = c("steelblue", "orange3")) +
  scale_fill_manual("Y Scale", values = c("steelblue", "orange3")) +
  scale_linetype_manual("Estimation Strategy", labels = c("True Value", "Closest Point Value", "Grid Line Breaks"), values = c("solid", "dashed", "dotted")) +
  scale_y_continuous("Density", labels = scales::comma) +
  coord_cartesian(xlim = c(0, 20000)) +
  theme(legend.position = c(0.05, 0.95),
        legend.box = "horizontal",
        legend.direction = "vertical",
        legend.justification = c(0, 1),
        axis.title.x = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  ggtitle("Dataset 1 Participant Estimates") +
  annotate("label", x = 3000, y = 0.00015, label = "Participants estimating\nmultiplicative differences", hjust = 0.5) +
  annotate("label", x = 18000, y = 0.0003, label = "Log2 scale\nproduces more\naccurate estimates", hjust = 0.5)
# p1
