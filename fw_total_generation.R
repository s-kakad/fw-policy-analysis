#### SCRIPT 1/4: TOTAL FOOD WASTE GENERATION QUANTIFICATION ####
library(tidyverse)
library(dplyr)
library(readr)

### FOOD WASTE DATA EXTRACTION ###
fw_tot_df <- read.csv("ReFED_food_surplus_summary_2022.csv")

#select relevant columns
fw_tot_df <- fw_tot_df %>% filter(year == 2022) %>% select(state, sector, tons_waste)

#convert to appropriate class
fw_tot_df$state <- as.factor(fw_tot_df$state)
fw_tot_df$sector <- as.factor(fw_tot_df$sector)
fw_tot_df$tons_waste <- format(round(fw_tot_df$tons_waste, 3), nsmall = 3, scientific = FALSE)
fw_tot_df$tons_waste <- as.numeric(fw_tot_df$tons_waste)

#change state names to state abbreviation for better compatibility later on
#note this can be done because levels of state are in the same order as R vector state.abb
levels(fw_tot_df$state) <- state.abb

#quantify food waste for 2022 apart from farming and manufacturing (i.e. surplus)
fw_tot_df_processed <- fw_tot_df %>%
                            filter(sector != "Farm" & sector != "Manufacturing") %>%
                            group_by(state) %>%
                            summarise(total_fw_2022 = format(round(sum(tons_waste), 3), nsmall = 3))

write.csv(fw_tot_df_processed, "total_fw_2022.csv", row.names = FALSE)

#these numbers have been added to the fw_masterfile_2022.csv, alongside 2022 population data
#note data is in US short tons (1 short ton = 0.907185 metric tonne)
