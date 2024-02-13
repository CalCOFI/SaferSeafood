#..........setup & data processing............

# load packages ----
library(tidyverse)

# read in raw data ----
fish_raw <- read_csv(here::here("raw_data", "fish_clean.csv"))

# calculate average DDT and group_by lat and long ----
avg_DDT <- fish_raw %>%
  select(CompositeStationArea, TotalDDT) %>%
  group_by(CompositeStationArea) %>%
  summarize(
    AvgDDT = round(mean(TotalDDT), 1)
  )

# join avg DDT original data (match rows based on site) ----
joined_dfs <- full_join(fish_raw, avg_DDT)

# get unique site observations (with corresponding lat, long, avgDDT) for mapping ----
unique_fish <- joined_dfs %>%
  select(CompositeStationArea, CompositeTargetLatitude, CompositeTargetLongitude, AvgDDT) %>%
  distinct() #keep only unique/distinct rows from a df

# save preprocessed data to app's data directory ----

write_csv(x = unique_fish, file = here::here("shinydashboard", "data", "fish_data_preprocessed.csv"))
                                             
                                             