library(tidyverse)
library(dplyr)
library(DBI)
library(RSQLite)
library(knitr)
library(animation)
library(gifski)

data <- read_rds("data.rds")
data <- data %>%
  mutate(trial = row_number()) %>%
  arrange(tension_strength, desc(elongation), desc(roughness)) %>%
  mutate(layer_height = layer_height*100) %>%
  mutate(wall_thickness = wall_thickness*10) %>%
  mutate(elongation = elongation*10)

V1 <- c("Layer\nHeight(pm)", "Wall\nThickness(µm)", "Infill\nDensity(%)", "Infill\nPattern",
        "Nozzle\nTemperature(°C)", "Bed\nTemperature(°C)", "Print\nSpeed(mm/s)", 
        "Material", "Fan\nSpeed(%)", "Roughness(mm)", "Tension\nStrength(MPa)", 
        "Elongation(p.p)", "Trial")
names(data) <- V1

data <- pivot_longer(data, names_to = "setting", values_to = "value", 
                     cols = c("Layer\nHeight(pm)", "Wall\nThickness(µm)", 
                              "Infill\nDensity(%)", "Nozzle\nTemperature(°C)", 
                              "Bed\nTemperature(°C)", "Print\nSpeed(mm/s)", 
                              "Fan\nSpeed(%)", "Roughness(mm)", 
                              "Tension\nStrength(MPa)", "Elongation(p.p)"))
data <- data %>%
  mutate(trial = factor(Trial)) %>%
  mutate(setting = factor(setting, levels = V1))

for(i in levels(data$trial)){
  data2 <- subset(data, trial == i)
  p <- ggplot(data2, aes(x = setting, y = value, fill = setting)) +
    facet_wrap(~ trial) +
    geom_bar(stat = "identity", show.legend = FALSE, position = "dodge") +
    geom_vline(data = data.frame(Vert = 7.5), aes(xintercept = Vert)) +
    xlab("Setting") +
    ylab("Value of Setting/Output") +
    labs(title = "Value of 3D Printer Settings and Their Effect on Print Strength",
         subtitle = sprintf("Trial %s had a %s infill pattern and was made out of %s.", 
                            i, data2$`Infill\nPattern`, data2$Material),
         caption: "Source: Selcuk University, February 26th 2019") +
    theme_classic()
  ggsave(paste0("images/gif/row_", i, ".png"), p, width = 11, height = 7)
}

png_files <- list.files("images/gif/", pattern = ".*png$", full.names = TRUE)
gifski(png_files, gif_file = "total.gif", delay = 1)
