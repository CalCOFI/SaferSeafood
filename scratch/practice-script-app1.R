# load packages ----
library(palmerpenguins)
library(tidyverse)
library(readr)

southern_fish <- read_csv("single-file-app/data/totalDDX_fish_southernCA.csv")
View(totalDDX_fish_southernCA)

# create scatterplot ----
ggplot(na.omit(southern_fish), 
       aes(x = TotalDDT, y = WeightAvg.g)) +
  geom_point(color = "red") +
  theme_minimal() +
  theme(legend.position = c(0.85, 0.2),
        legend.background = element_rect(color = "white"))

# filter southern_fish df for observations where DDT concentrations >= 1 & <= 150
DDT_df <- southern_fish %>%
  filter(TotalDDT %in% c(1:150))

# plot new, filtered data ----
ggplot(na.omit(DDT_df), 
       aes(x = TotalDDT, y = WeightAvg.g)) +
  geom_point(color = "red") +
  theme_minimal() +
  theme(legend.position = c(0.85, 0.2),
        legend.background = element_rect(color = "white"))