#############
# Author: Bryanna Fayne
# Date: June 21st, 2021
# Combining csv files and analyzing behavior summary counts 

# Load packages 
library(tidyverse)
library(ggplot2)
library(dplyr)

# Read in csv files 
a <- read_csv("sow_behavior_sum_1_10.csv")
b <- read_csv("sow_behavior_sum_11_20.csv")

# Bind files and write to new file 
behavior_counts <- bind_rows(a, b)

write_csv(behavior_counts, "behavior_counts.csv")

# Find which counts are between/outside 112 and 450
good_data <- behavior_counts %>%
  filter(count >= 112 & count <= 450)
bad_data <- behavior_counts %>%
  filter(count < 112 & count > 450)
