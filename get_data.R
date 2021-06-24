library(tidyverse)
library(dplyr)
library(DBI)
library(RSQLite)
library(knitr)

con <- dbConnect(RSQLite::SQLite(), "print_data.db")
dbListTables(con)
data <- tbl(con, "Data")
# Because there aren't that many rows in the database, 
# I can easily collect the data without repercussions.
# I'm mainly using the database so that I can use the SQL predict function.
data <- collect(data)
data <- data %>%
  mutate(infill_pattern = factor(infill_pattern,
                                 levels = c("grid", "honeycomb"))) %>%
  mutate(material = factor(material, levels = c("pla", "abs"))) %>%
  mutate(layer_height = as.double(layer_height)) %>%
  mutate(wall_thickness = as.integer(wall_thickness)) %>%
  mutate(infill_density = as.integer(infill_density)) %>%
  mutate(nozzle_temperature = as.integer(nozzle_temperature)) %>%
  mutate(bed_temperature = as.integer(bed_temperature)) %>%
  mutate(print_speed = as.integer(print_speed)) %>%
  mutate(fan_speed = as.integer(fan_speed)) %>%
  mutate(roughness = as.integer(roughness)) %>%
  mutate(tension_strength = as.integer(tension_strength)) %>%
  mutate(elongation = as.double(elongation))
write_rds(data, "data.rds")
