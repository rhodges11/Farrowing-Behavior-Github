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
  filter(count < 112 | count > 450)

# Plot data to find outliers 

sow_sum <- read_xlsx("data/behavior_counts+SL.xlsx")

ggplot(data = sow_sum, aes(x = inc, y = lying)) + 
  geom_smooth(fill = NA) + 
  labs(title = "Lying vs. Inc")
ggplot(data = sow_sum, aes(x = inc, y = lying)) + 
  geom_point() 

ggplot(sow_sum, aes(inc, standing)) +
  geom_smooth(fill = NA) + 
  labs(title = "Standing vs. Inc")
ggplot(sow_sum, aes(inc, standing)) + 
  geom_point()

ggplot(sow_sum, aes(inc, sitting)) +
  geom_smooth(fill = NA) + 
  labs(title = "Sitting vs. Inc")
ggplot(sow_sum, aes(inc, sitting)) + 
  geom_point()

ggplot(sow_sum, aes(inc, kneeling)) +
  geom_smooth(fill = NA) + 
  labs(title = "Kneeling vs. Inc")
ggplot(sow_sum, aes(inc, kneeling)) + 
  geom_point()

ggplot(sow_sum, aes(inc, back_to_HL)) +
  geom_smooth(fill = NA) + 
  labs(title = "Back to HL vs. Inc")
ggplot(sow_sum, aes(inc, back_to_HL)) + 
  geom_point()

ggplot(sow_sum, aes(inc, udder_to_HL)) +
  geom_smooth(fill = NA) + 
  labs(title = "Udder to HL vs. Inc")
ggplot(sow_sum, aes(inc, udder_to_HL)) + 
  geom_point()

ggplot(sow_sum, aes(inc, drinking)) +
  geom_smooth(fill = NA) + 
  labs(title = "Drinking vs. Inc")
ggplot(sow_sum, aes(inc, drinking)) + 
  geom_point()

ggplot(sow_sum, aes(inc, kneeling_down)) +
  geom_smooth(fill = NA) + 
  labs(title = "Kneeling Down vs. Inc")
ggplot(sow_sum, aes(inc, kneeling_down)) + 
  geom_point()

ggplot(sow_sum, aes(inc, feeding)) +
  geom_smooth(fill = NA) + 
  labs(title = "Feeding vs. Inc")
ggplot(sow_sum, aes(inc, feeding)) + 
  geom_point()


# Removing bad DS2 and adding in good DS 2
old_data <- read_csv("behavior_counts.csv")
removed_data <- old_data %>% filter(DSnum != 2)

# Check to make sure there is no DS 2

removed_data %>% filter(DSnum == 2) # Nothing is there 

# Add new DS 2 to this file 

dstwo <- read_csv("sow_behavior_sum.csv")
behavior_counts <- bind_rows(removed_data, dstwo)

# Check to see if DS 2 is added
behavior_counts %>% filter(DSnum == 2)
dstwo

# Write to old file 
write_csv(behavior_counts, "behavior_counts.csv")

