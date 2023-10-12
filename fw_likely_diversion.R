#### SCRIPT 2/3:LIKELY FOOD WASTE DIVERSION QUANTIFICATION ###

library(tidyverse)
library(dplyr)
library(ggplot2)

### MERGING APPLICABLE FOOD WASTE WITH POLICY SCORING AND POPULATION DATA ###
#import applicable food waste file from script 1
applicable_fw_df <- read.csv("applicable_fw_2021.csv") ## note for further iterations the "2021" suffix will change ##
applicable_fw_df <- applicable_fw_df[, 2:ncol(applicable_fw_df)]
applicable_fw_df <- applicable_fw_df %>% rename(applicable_baseline = diversion_baseline,
                           applicable_alternative = diversion_alternative)

#import master file for likely food waste quantification
total_fw_df <- read.csv("fw_masterfile_2021.csv") ## suffix as per above ##

#rename column names to remove year stamp (data still in US short tons)
total_fw_df <- total_fw_df %>% rename(total_fw = total_fw_2021,
                                      population = population_2021,
                                      total_fw_tons_per_capita = total_fw_2021_tons_per_capita)

#prepare data frames for merging
total_fw_df$state <- as.factor(total_fw_df$state)
total_fw_df$state <- factor(total_fw_df$state, levels = state.abb)
total_fw_df$solution_level <- as.factor(total_fw_df$solution_level)
total_fw_df$solution_level <- factor(total_fw_df$solution_level, levels = solution_levels)
total_fw_df$total_fw <- as.numeric(total_fw_df$total_fw)
total_fw_df$population <- as.numeric(total_fw_df$population)
total_fw_df$factor_low <- as.numeric(total_fw_df$factor_low)
total_fw_df$factor_high <- as.numeric(total_fw_df$factor_high)
total_fw_df$total_fw_tons_per_capita <- as.numeric(total_fw_df$total_fw_tons_per_capita)

applicable_fw_df$state <- as.factor(applicable_fw_df$state)
applicable_fw_df$state <- factor(applicable_fw_df$state, levels = state.abb)
applicable_fw_df$solution_level <- as.factor(applicable_fw_df$solution_level)
applicable_fw_df$solution_level <- factor(applicable_fw_df$solution_level, levels = solution_levels)
applicable_fw_df$applicable_baseline <- as.numeric(applicable_fw_df$applicable_baseline)
applicable_fw_df$applicable_alternative <- as.numeric(applicable_fw_df$applicable_alternative)

#merge data frames
merged_fw_df <- merge(applicable_fw_df, total_fw_df)

### LIKELY FOOD WASTE DIVERSION QUANTIFICATION ###
names(merged_fw_df)

#reorder columns and save as new data frame for subsequent data transformation
likely_fw_df <- merged_fw_df %>% select(state,
                                        solution_level,
                                        total_fw,
                                        population,
                                        applicable_baseline,
                                        applicable_alternative,
                                        factor_low,
                                        factor_high,
                                        total_fw_tons_per_capita)

#calculate kg per capita for baseline and alternative scenario ranges
likely_fw_df <- likely_fw_df %>%
  mutate(total_fw_kg_per_capita = total_fw_tons_per_capita*0.907185*1000,
         likely_fw_baseline_low_kg_per_capita = applicable_baseline*0.907185*1000*factor_low/population,
         likely_fw_baseline_high_kg_per_capita = applicable_baseline*0.907185*1000*factor_high/population,
         likely_fw_alternative_low_kg_per_capita = applicable_alternative*0.907185*1000*factor_low/population,
         likely_fw_alternative_high_kg_per_capita = applicable_alternative*0.907185*1000*factor_high/population)

#keep only columns of interest for subsequent data visualisation
likely_fw_df <- likely_fw_df[, c(1:2, 10:14)]
likely_fw_df <- likely_fw_df %>%
  arrange(state, solution_level)

### DATA FRAME TRANSFORMATION ###
#run for loop to add a new row for solution_level "All" to find total per state diversion
for (s in state.abb) {
  baseline_low <- likely_fw_df %>%
    filter(state == s) %>%
    .$likely_fw_baseline_low_kg_per_capita %>%
    sum()
  baseline_high <- likely_fw_df %>%
    filter(state == s) %>%
    .$likely_fw_baseline_high_kg_per_capita %>%
    sum()
  alternative_low <- likely_fw_df %>%
    filter(state == s) %>%
    .$likely_fw_alternative_low_kg_per_capita %>%
    sum()
  alternative_high <- likely_fw_df %>%
    filter(state == s) %>%
    .$likely_fw_alternative_high_kg_per_capita %>%
    sum()
  lev <- solution_levels[5]
  tot <- likely_fw_df %>%
    filter(state == s) %>%
    .$total_fw_kg_per_capita %>%
    mean()
  #temp <- c(s, lev, tot, baseline_low, baseline_high, alternative_low, alternative_high)
  #print(temp)
  likely_fw_df[nrow(likely_fw_df) + 1, 1] <- s
  likely_fw_df[nrow(likely_fw_df), 2] <- lev
  likely_fw_df[nrow(likely_fw_df), 3] <- tot
  likely_fw_df[nrow(likely_fw_df), 4] <- baseline_low
  likely_fw_df[nrow(likely_fw_df), 5] <- baseline_high
  likely_fw_df[nrow(likely_fw_df), 6] <- alternative_low
  likely_fw_df[nrow(likely_fw_df), 7] <- alternative_high
}

likely_fw_df <- likely_fw_df %>%
  arrange(state, solution_level)

write.csv(likely_fw_df, "likely_fw.csv", row.names = TRUE)

### FURTHER TRANSFORMATION TO HAVE ALL DIVERSION DATA IN A SINGLE COLUMN ###
names(likely_fw_df)
likely_fw_df_alt <- data.frame(state = likely_fw_df$state,
                               solution_level = likely_fw_df$solution_level,
                               fw_kg_per_capita = likely_fw_df$total_fw_kg_per_capita,
                               scenario = c(rep("baseline", 500), rep("alternative", 500)),
                               conversion_factor = c(rep("low", 250), rep("high", 250), rep("low", 250), rep("high", 250)),
                               diversion_potential_kg_per_capita = c(likely_fw_df$likely_fw_baseline_low_kg_per_capita,
                                                                         likely_fw_df$likely_fw_baseline_high_kg_per_capita,
                                                                         likely_fw_df$likely_fw_alternative_low_kg_per_capita,
                                                                         likely_fw_df$likely_fw_alternative_high_kg_per_capita)
)

likely_fw_df_alt$scenario <- as.factor(likely_fw_df_alt$scenario)
likely_fw_df_alt$conversion_factor <- as.factor(likely_fw_df_alt$conversion_factor)
likely_fw_df_alt$solution_level <- factor(likely_fw_df_alt$solution_level, levels = solution_levels)

likely_fw_df_alt <- likely_fw_df_alt %>%
  arrange(state, solution_level, scenario, conversion_factor)

write.csv(likely_fw_df_alt, "likely_fw_alt.csv", row.names = TRUE)
