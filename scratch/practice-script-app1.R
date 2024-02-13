# load packages ----
library(palmerpenguins)
library(tidyverse)
library(readr)
library(dplyr)

#data

southern_fish <- read_csv("single-file-app/data/totalDDX_fish_southernCA.csv")
View(totalDDX_fish_southernCA)


DDT_fish <- read_csv("single-file-app/data/totalDDX_fish_southernCA.csv")


sediment_summary = readRDS(here::here("single-file-app", "data","totalDDX_sediment_zone_summary.rds")) %>%
  dplyr::select(Name, Est.2003, Est.2008, Est.2013, Est.2018) %>% 
  gather(key="Year",value="TotalDDT",Est.2003:Est.2018) %>% 
  dplyr::mutate(Year = case_when(Year == "Est.2003" ~ "2003",
                                 Year == "Est.2008" ~ "2008",
                                 Year == "Est.2013" ~ "2013",
                                 Year == "Est.2018" ~ "2018")) %>% 
  dplyr::group_by(Name, Year) %>%
  dplyr::summarize(TotalDDT.sed = (mean((TotalDDT)))) %>%
  dplyr::ungroup() 


# Read in fish life history 
fish_lh = read.csv(here::here("data","fish_data","fish_life_history.csv")) %>% 
  dplyr::mutate(species = tolower(species))


# create scatterplot ----
ggplot(na.omit(southern_fish), 
       aes(x = TotalDDT, y = WeightAvg.g, color = region)) +
  geom_point() +
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


unique(southern_fish$CompositeCommonName)

#-------------------------------------------



# Read in sediment data 
sediment_summary = readRDS(here::here("single-file-app", "data","totalDDX_sediment_zone_summary.rds")) %>%
  dplyr::select(Name, Est.2003, Est.2008, Est.2013, Est.2018) %>% 
  gather(key="Year",value="TotalDDT",Est.2003:Est.2018) %>% 
  dplyr::mutate(Year = case_when(Year == "Est.2003" ~ "2003",
                                 Year == "Est.2008" ~ "2008",
                                 Year == "Est.2013" ~ "2013",
                                 Year == "Est.2018" ~ "2018")) %>% 
  dplyr::group_by(Name, Year) %>%
  dplyr::summarize(TotalDDT.sed = (mean((TotalDDT)))) %>%
  dplyr::ungroup() 

# Read in fish life history 
fish_lh = read.csv(here::here("single-file-app", "data", "fish_life_history.csv")) %>% 
  dplyr::mutate(species = tolower(species))

# select station area
fish_location = read.csv(here::here("single-file-app", "data", "totalDDX_fish_southernCA.csv")) %>% 
  dplyr::select(CompositeCompositeID, CompositeStationArea)

# Read in fish data, and join with sediments 
fish_reg = read.csv(here::here("single-file-app", "data","totalDDT_fish_southernCA.csv")) %>% # Read in fish DDT values 
  # We have sediment data blocked off by 2003, 2008, 2013, and 2018. Figure out what (continous) fish years go with which sediment years. 
  dplyr::mutate(NewYear = case_when(Year %in% c(1995:2005) ~ "2003", 
                                    Year %in% c(2006:2010) ~ "2008", 
                                    Year %in% c(2011:2015) ~ "2013", 
                                    Year %in% c(2016:2022) ~ "2018")) %>% 
  left_join(., fish_location) %>% 
  left_join(., sediment_summary, by=c("CompositeStationArea"="Name", 
                                      "NewYear"="Year")) %>% 
  dplyr::left_join(., fish_lh, by=c("CompositeCommonName"="species")) %>% 
  dplyr::mutate(feeding_position = case_when(feeding_position == "pelagic" ~ "Pelagic",
                                             feeding_position == "midwater" ~ "Midwater",
                                             feeding_position == "benthopelagic " ~ "Benthopelagic",
                                             feeding_position == "benthic" ~ "Benthic", 
                                             TRUE ~ feeding_position)) %>% 
  dplyr::mutate(feeding_position = factor(feeding_position, levels=c("Pelagic","Midwater","Benthopelagic","Benthic"))) %>% 
  dplyr::mutate(trophic_category = case_when(trophic_category == "herbivore" ~ "Herbivore",
                                             trophic_category == "primary_carnivore" ~ "Primary Carnivore",
                                             trophic_category == "secondary_carnivore" ~ "Secondary Carnivore",
                                             trophic_category == "tertiary_carnivore" ~ "Tertiary Carnivore")) 

# Add transformed columns (TotalDDT which is non-lipid normalized for fish)
fish_reg$TotalDDT.trans.non = log(fish_reg$TotalDDT + 1) # add + 1 to account for 0 values
fish_reg$TotalDDT.sed.trans = log(fish_reg$TotalDDT.sed + 1)

# Add censoring for values equal to zero (so value is constrained to fall between zero and MDL)
fish_reg = fish_reg %>% 
  dplyr::mutate(Censored = ifelse(TotalDDT.trans.non == 0, "interval","none"), 
                
                # ask about this limit since its divided by Lipid
                Detection.Limit = ifelse(is.na(MDL.min), 
                                         0.5, # if MDL.min is an NA value fill with this value
                                         log1p(MDL.min))) %>% 
  
  dplyr::mutate(Year = Year - 1998) # We want to use years since 1998 


##################################################################################################################
################### Plot non normalized fish against sediments   ###################
##################################################################################################################

fish_reg %>% 
  dplyr::mutate(feeding_position = factor(feeding_position, 
                                          levels=c("Benthic","Benthopelagic","Midwater","Pelagic"))) %>% 
  ggplot(mapping=aes(x=TotalDDT.sed.trans, # ddt concentration in sediment 
                     y=TotalDDT.trans.non, # ddt concentration in fish
                     fill= trophic_category )) +
  geom_jitter(size=1.5, pch=21) +
  ylab("[DDXfish] ng/g lipid") +
  xlab("[DDXsed] ng/g dw") +
  facet_wrap(~feeding_position, nrow=1)+
  scale_fill_manual(values = c("#ffffcc","#a1dab4","#41b6c4","#225ea8"), name="Diet")+
  theme_bw() +
  theme(legend.position = "none")