#### SCRIPT 1/3: SOLUTION EXTRACTION AND APPLICABLE FOOD WASTE QUANTIFICATION ####

library(tidyverse)
library(dplyr)
library(ggplot2)

### SOLUTION DATA EXTRACTION ###
solutions_all_df <- read.csv("fw_solutions_dummy.csv")

#select relevant columns
solutions_df <- solutions_all_df %>% select(state,
                                        solution_group, 
                                        solution_name,
                                        annual_tons_diversion_potential)

#rename column names for solution levels and diversion potential of food waste (data in US short tons)
solutions_df <- solutions_df %>% rename(solution_level = solution_group, diversion_detail = annual_tons_diversion_potential)

#convert to appropriate class
solutions_df$state <- as.factor(solutions_df$state)
solutions_df$solution_level <- as.factor(solutions_df$solution_level)
solutions_df$solution_name <- as.character(solutions_df$solution_name)
solutions_df$diversion_detail <- as.numeric(solutions_df$diversion_detail)

#change state names to state abbreviation for better compatibility later on
#note this can be done because levels of state are in the same order as R vector state.abb
levels(solutions_df$state)
levels(solutions_df$state) <- state.abb

# IMPORTANT #
#divide education campaigns by 2 since 50% assigned to recycling and 50% to prevention
#note this does not apply to donation education as 100% assigned to rescue level
#easier than changing later on
solutions_df$diversion_detail[solutions_df$solution_name %in%
      c("Consumer Education Campaigns", "K-12 Education Campaigns")] <- 
(solutions_df$diversion_detail[solutions_df$solution_name %in% 
      c("Consumer Education Campaigns", "K-12 Education Campaigns")])/2

#change solution level name assignment based on policy type for animal feed (Recycling -> Animal Feed)
solutions_df <- solutions_df %>%
  mutate(solution_level = ifelse(solution_name == "Livestock Feed", "Animal Feed", as.character(solution_level)))
solutions_df$solution_level <- as.factor(solutions_df$solution_level)

#change data frame order
solutions_df <- solutions_df %>%
  arrange(state, solution_level, solution_name)

#create vectors with relevant solutions
solution_levels <- c("Prevention", "Rescue", "Animal Feed", "Recycling", "All", "Real")

solution_names_baseline <- c("Standardized Date Labels",
                    "Donation Transportation",
                    "Donation Storage Handling & Capacity",
                    "Donation Coordination & Matching",
                    "Donation Value-Added Processing",
                    "Livestock Feed",
                    "Centralized Composting",
                    "Centralized Anaerobic Digestion",
                    "Co-Digestion At Wastewater Treatment Plants")

#vector for baseline + education and home/community composting (alternative scenario)
solution_names_alternative <- c("Standardized Date Labels",
                    "Consumer Education Campaigns",
                    "K-12 Education Campaigns",
                    "Donation Transportation",
                    "Donation Storage Handling & Capacity",
                    "Donation Coordination & Matching",
                    "Donation Value-Added Processing",
                    "Donation Education",
                    "Livestock Feed",
                    "Centralized Composting",
                    "Centralized Anaerobic Digestion",
                    "Co-Digestion At Wastewater Treatment Plants",
                    "Home Composting",
                    "Community Composting")

### APPLICABLE FOOD WASTE QUANTIFICATION ###
#create empty data frame with column names and nrow = 0
applicable_fw_df = data.frame(matrix(nrow = 0, ncol = 4))
colnames(applicable_fw_df) = c("state", "solution_level", "applicable_baseline", "applicable_alternative")

#test trial
solutions_df %>%
  filter(state == "AL" & solution_level == "Prevention" & solution_name %in% solution_names_baseline) %>%
  .$diversion_detail %>%
  sum()

#run for loop that calculates applicable diversion potential per level per state
#assign each loop run to data frame above, with both baseline and alternative scenarios
for (s in state.abb) {
  for (l in solution_levels[solution_levels != "All"]) {
    baseline <- solutions_df %>%
      filter(state == s & solution_level == l & solution_name %in% solution_names_baseline) %>%
      .$diversion_detail %>%
      sum()
    alternative <- solutions_df %>%
      filter(state == s & solution_level == l & solution_name %in% solution_names_alternative) %>%
      .$diversion_detail %>%
      sum()
    temp <- c(s, l, baseline, alternative)
    applicable_fw_df[nrow(applicable_fw_df) + 1, ] <- temp
  }
}

rm(s, l, baseline, alternative, temp)

write.csv(applicable_fw_df, "applicable_fw.csv", row.names = TRUE)