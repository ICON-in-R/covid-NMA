
# forest plots

library(ggplot2)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(Cairo)
library(reshape2)
library(dplyr)


outcome_names <-
  c("COVID_infection", "Symptomatic_infection", "Severe_Infection_All", "Hospitalizations", "Deaths")

for (i in outcome_names) {
  forest_plot(i)
  
  ggsave(
    filename = glue::glue("plots/forest_plot_{i}.jpg"),
    width = 8,
    height = 5,
    units = "in",
    dpi = 640)
  
  forest_plot(i, vs_placebo = FALSE)
  
  ggsave(
    filename = glue::glue("plots/forest_plot_vs_Moderna_{i}.jpg"),
    width = 8,
    height = 5,
    units = "in",
    dpi = 640)
  
  forest_plot(i, logOR = TRUE)
  
  ggsave(
    filename = glue::glue("plots/forest_plot_{i}_logOR.jpg"),
    width = 8,
    height = 5,
    units = "in",
    dpi = 640)
  
  forest_plot(i, vs_placebo = FALSE, logOR = TRUE)
  
  ggsave(
    filename = glue::glue("plots/forest_plot_vs_Moderna_{i}_logOR.jpg"),
    width = 8,
    height = 5,
    units = "in",
    dpi = 640)
  
  
  # forest_plot(i, vs_placebo = FALSE, vacc_effic = TRUE)
  # 
  # ggsave(
  #   filename = glue::glue("plots/forest_plot_vs_Moderna_{i}_vacc_effic.jpg"),
  #   width = 8,
  #   height = 5,
  #   units = "in",
  #   dpi = 640)
}

