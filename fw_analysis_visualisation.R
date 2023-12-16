#### SCRIPT 4/4: DATA VISUALISATION ####
library(tidyverse)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(ggthemes)
library(ggh4x)
library(viridis)
library(ggrepel)

### PLOTTING TOTAL LIKELY DIVERSION POTENTIAL PER STATE ###
#likely_fw_df <- read.csv("likely_fw.csv") ## to avoid running code in R script 2
#likely_fw_df <- likely_fw_df[, 2:8] ## if importing from CSV file to remove column X

fw_target <- 74.389 ## US 2030 target for food waste in kg per capita (based on 2016 baseline)

myplot <- likely_fw_df %>%
  filter(solution_level == "All") %>%
  rowwise() %>% 
  mutate(mymean = mean(c(likely_fw_baseline_low_kg_per_capita,
                         likely_fw_baseline_high_kg_per_capita,
                         likely_fw_alternative_low_kg_per_capita,
                         likely_fw_alternative_high_kg_per_capita))) %>%
  arrange(mymean) %>%
  mutate(state = factor(state, levels = state[order(mymean)])) %>%
  ggplot() +
  geom_segment(aes(x = state,
                   xend = state, 
                   y = likely_fw_alternative_low_kg_per_capita, 
                   yend = likely_fw_alternative_high_kg_per_capita),
               colour = "black",
               alpha = 0.75,
               linewidth = 0.75) +
  geom_segment(aes(x = state,
                   xend = state, 
                   y = likely_fw_baseline_low_kg_per_capita, 
                   yend = likely_fw_baseline_high_kg_per_capita),
               colour = "blue",
               alpha = 0.75,
               linewidth = 1.5) +
  geom_point(aes(x = state, y = total_fw_kg_per_capita),
             color = rgb(0.7, 0.2, 0.1, 0.5),
             size = 2) +
  geom_hline(yintercept = fw_target,
             color = rgb(0.2, 0.7, 0.1, 0.5),
             linewidth = 1) +
  scale_y_continuous(trans = "log2",
                     limits = c(0.85, 600), #or remove for OG plot
                     breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500),
                     labels = c(1, 2, 5, 10, 20, 50, 100, 200, 500)) +
  coord_flip() +
  theme_economist() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Likely food waste diversion potential (kg per capita)")

pdf("likely_fw_diversion_total_2022.pdf")
print(myplot)   
dev.off()

### PLOTTING THE CONTRIBUTION OF TOTAL VS "REAL" (REVISED EPA DEFINITION) FOOD WASTE DIVERSION ###
#transform data to add a "real" level
### DATA FRAME TRANSFORMATION ###
#run for loop to add a new row for solution_level "All" to find total per state diversion
real_fw_df <- likely_fw_df

for (s in state.abb) {
  baseline_low <- real_fw_df %>%
    filter(solution_level %in% c("Prevention", "Rescue", "Animal Feed")) %>%
    filter(state == s) %>%
    .$likely_fw_baseline_low_kg_per_capita %>%
    sum()
  baseline_high <- real_fw_df %>%
    filter(solution_level %in% c("Prevention", "Rescue", "Animal Feed")) %>%
    filter(state == s) %>%
    .$likely_fw_baseline_high_kg_per_capita %>%
    sum()
  alternative_low <- real_fw_df %>%
    filter(solution_level %in% c("Prevention", "Rescue", "Animal Feed")) %>%
    filter(state == s) %>%
    .$likely_fw_alternative_low_kg_per_capita %>%
    sum()
  alternative_high <- real_fw_df %>%
    filter(solution_level %in% c("Prevention", "Rescue", "Animal Feed")) %>%
    filter(state == s) %>%
    .$likely_fw_alternative_high_kg_per_capita %>%
    sum()
  lev <- solution_levels[6]
  tot <- real_fw_df %>%
    filter(state == s) %>%
    .$total_fw_kg_per_capita %>%
    mean()
  real_fw_df[nrow(real_fw_df) + 1, 1] <- s
  real_fw_df[nrow(real_fw_df), 2] <- lev
  real_fw_df[nrow(real_fw_df), 3] <- tot
  real_fw_df[nrow(real_fw_df), 4] <- baseline_low
  real_fw_df[nrow(real_fw_df), 5] <- baseline_high
  real_fw_df[nrow(real_fw_df), 6] <- alternative_low
  real_fw_df[nrow(real_fw_df), 7] <- alternative_high
}

real_fw_df <- real_fw_df %>%
  arrange(state, solution_level)

write.csv(real_fw_df, "likely_fw_real.csv", row.names = FALSE)

#arrange by order of mean based on "All"
real_fw_df <- real_fw_df %>%
  rowwise() %>%
  mutate(mymean = mean(c(likely_fw_baseline_low_kg_per_capita[solution_level == "All"],
                         likely_fw_baseline_high_kg_per_capita[solution_level == "All"],
                         likely_fw_alternative_low_kg_per_capita[solution_level == "All"],
                         likely_fw_alternative_high_kg_per_capita[solution_level == "All"]))) %>%
  arrange(mymean) %>%
  mutate(state = factor(state, levels = state[order(mymean)]))

#make plot
myplot_real <- ggplot() +
  geom_segment(data = real_fw_df[real_fw_df$solution_level == "All", ],
               aes(x = state,
                   xend = state,
                   y = likely_fw_alternative_low_kg_per_capita, 
                   yend = likely_fw_alternative_high_kg_per_capita),
               colour = "black",
               alpha = 0.65,
               linewidth = 0.75) +
  geom_segment(data = real_fw_df[real_fw_df$solution_level == "All", ],
               aes(x = state,
                   xend = state, 
                   y = likely_fw_baseline_low_kg_per_capita, 
                   yend = likely_fw_baseline_high_kg_per_capita),
               colour = "blue",
               alpha = 0.75,
               linewidth = 1.5) +
  geom_segment(data = real_fw_df[real_fw_df$solution_level == "Real", ],
               aes(x = state,
                   xend = state,
                   y = likely_fw_alternative_low_kg_per_capita, 
                   yend = likely_fw_alternative_high_kg_per_capita),
               colour = "black",
               alpha = 0.65,
               linewidth = 0.75) +
  geom_segment(data = real_fw_df[real_fw_df$solution_level == "Real", ],
               aes(x = state,
                   xend = state, 
                   y = likely_fw_baseline_low_kg_per_capita, 
                   yend = likely_fw_baseline_high_kg_per_capita),
               colour = "purple",
               alpha = 0.85,
               linewidth = 1.5) +
  geom_point(data = real_fw_df,
             aes(x = state, y = total_fw_kg_per_capita),
             color = rgb(0.7, 0.2, 0.1, 0.5),
             size = 1.75,
             alpha = 0.1) +
  geom_hline(yintercept = fw_target,
             color = rgb(0.2, 0.7, 0.1, 0.5),
             linewidth = 0.85,
             alpha = 0.75) +
  scale_y_continuous(trans = "log2",
                     limits = c(0.85, 600),
                     breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500),
                     labels = c(1, 2, 5, 10, 20, 50, 100, 200, 500)) +
  coord_flip() +
  theme_economist() +
  xlab("") +
  ylab("Likely food waste diversion potential (kg per capita)")

pdf("likely_fw_diversion_real_2022.pdf")
print(myplot_real)   
dev.off()

min(real_fw_df$likely_fw_baseline_low_kg_per_capita)

### PLOTTING THE CONTRIBUTION OF INDIVIDUAL POLICY TYPES TOWARDS FOOD WASTE DIVERSION ###
#for this box plot makes most sense, otherwise too many variables are displayed on the lollipop graph
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.6 * IQR(x) | x > quantile(x, 0.75) + 1.6 * IQR(x))
}

myplot_bis <- likely_fw_df_alt %>%
  filter(solution_level != "All") %>%
  group_by(solution_level) %>%
  mutate(outlier = if_else(is_outlier(diversion_potential_kg_per_capita), state, NA_character_)) %>%
  ggplot(aes(x = solution_level,
             y = diversion_potential_kg_per_capita,
             fill = solution_level)) +
  geom_boxplot() +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_text_repel(aes(label = outlier), na.rm = TRUE, show.legend = F) +
  geom_hline(yintercept = fw_target,
             color = rgb(0.2, 0.7, 0.1, 0.5),
             linewidth = 1,
             alpha = 0.5) +
  scale_y_continuous(limits = c(0, 110),
                     breaks = c(0, 25, 50, 75, 100)) +
  theme_economist() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Likely food waste diversion potential (kg per capita)")

pdf("likely_fw_diversion_per_policy_boxplot_2022.pdf")
print(myplot_bis)   
dev.off()


### PAST SCRIPTS & PLOTS ###
#plot likely diversion potential per policy through lollipop graph
min(likely_fw_df$likely_fw_baseline_low_kg_per_capita)
max(likely_fw_df$total_fw_kg_per_capita)

myplot1 <- likely_fw_df %>%
  filter(solution_level == "Prevention") %>%
  rowwise() %>% 
  mutate(mymean = mean(c(likely_fw_baseline_low_kg_per_capita,
                         likely_fw_baseline_high_kg_per_capita,
                         likely_fw_alternative_low_kg_per_capita,
                         likely_fw_alternative_high_kg_per_capita))) %>%
  arrange(mymean) %>%
  mutate(state = factor(state, levels = state[order(mymean)])) %>%
  ggplot() +
  geom_segment(aes(x = state,
                   xend = state, 
                   y = likely_fw_alternative_low_kg_per_capita, 
                   yend = likely_fw_alternative_high_kg_per_capita),
               colour = "black",
               alpha = 0.75,
               linewidth = 1.5) +
  geom_segment(aes(x = state,
                   xend = state, 
                   y = likely_fw_baseline_low_kg_per_capita, 
                   yend = likely_fw_baseline_high_kg_per_capita),
               colour = "blue",
               alpha = 0.75,
               linewidth = 1.5) +
  geom_point(aes(x = state, y = total_fw_kg_per_capita),
             color = rgb(0.7, 0.2, 0.1, 0.5),
             size = 2) +
  geom_hline(yintercept = fw_target,
             color = rgb(0.2, 0.7, 0.1, 0.5),
             linewidth = 1) +
  scale_y_continuous(trans = "log2", limits = c(0.06, 750), breaks = c(0.1, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500),
                     labels = c(0.1, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500)) +
  coord_flip() +
  theme_economist() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Likely food waste diversion potential from prevention (kg per capita)")

myplot2 <- likely_fw_df %>%
  filter(solution_level == "Rescue") %>%
  rowwise() %>% 
  mutate(mymean = mean(c(likely_fw_baseline_low_kg_per_capita,
                         likely_fw_baseline_high_kg_per_capita,
                         likely_fw_alternative_low_kg_per_capita,
                         likely_fw_alternative_high_kg_per_capita))) %>%
  arrange(mymean) %>%
  mutate(state = factor(state, levels = state[order(mymean)])) %>%
  ggplot() +
  geom_segment(aes(x = state,
                   xend = state, 
                   y = likely_fw_alternative_low_kg_per_capita, 
                   yend = likely_fw_alternative_high_kg_per_capita),
               colour = "black",
               alpha = 0.75,
               linewidth = 1.5) +
  geom_segment(aes(x = state,
                   xend = state, 
                   y = likely_fw_baseline_low_kg_per_capita, 
                   yend = likely_fw_baseline_high_kg_per_capita),
               colour = "blue",
               alpha = 0.75,
               linewidth = 1.5) +
  geom_point(aes(x = state, y = total_fw_kg_per_capita),
             color = rgb(0.7, 0.2, 0.1, 0.5),
             size = 2) +
  geom_hline(yintercept = fw_target,
             color = rgb(0.2, 0.7, 0.1, 0.5),
             linewidth = 1) +
  scale_y_continuous(trans = "log2", limits = c(0.06, 750), breaks = c(0.1, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500),
                     labels = c(0.1, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500)) +
  coord_flip() +
  theme_economist() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Likely food waste diversion potential from rescue (kg per capita)")

myplot3 <- likely_fw_df %>%
  filter(solution_level == "Animal Feed") %>%
  rowwise() %>% 
  mutate(mymean = mean(c(likely_fw_baseline_low_kg_per_capita,
                         likely_fw_baseline_high_kg_per_capita,
                         likely_fw_alternative_low_kg_per_capita,
                         likely_fw_alternative_high_kg_per_capita))) %>%
  arrange(mymean) %>%
  mutate(state = factor(state, levels = state[order(mymean)])) %>%
  ggplot() +
  geom_segment(aes(x = state,
                   xend = state, 
                   y = likely_fw_alternative_low_kg_per_capita, 
                   yend = likely_fw_alternative_high_kg_per_capita),
               colour = "black",
               alpha = 0.75,
               linewidth = 1.5) +
  geom_segment(aes(x = state,
                   xend = state, 
                   y = likely_fw_baseline_low_kg_per_capita, 
                   yend = likely_fw_baseline_high_kg_per_capita),
               colour = "blue",
               alpha = 0.75,
               linewidth = 1.5) +
  geom_point(aes(x = state, y = total_fw_kg_per_capita),
             color = rgb(0.7, 0.2, 0.1, 0.5),
             size = 2) +
  geom_hline(yintercept = fw_target,
             color = rgb(0.2, 0.7, 0.1, 0.5),
             linewidth = 1) +
  scale_y_continuous(trans = "log2", limits = c(0.06, 750), breaks = c(0.1, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500),
                     labels = c(0.1, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500)) +
  coord_flip() +
  theme_economist() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Likely food waste diversion potential from redirecting to animal feed (kg per capita)")

myplot4 <- likely_fw_df %>%
  filter(solution_level == "Recycling") %>%
  rowwise() %>% 
  mutate(mymean = mean(c(likely_fw_baseline_low_kg_per_capita,
                         likely_fw_baseline_high_kg_per_capita,
                         likely_fw_alternative_low_kg_per_capita,
                         likely_fw_alternative_high_kg_per_capita))) %>%
  arrange(mymean) %>%
  mutate(state = factor(state, levels = state[order(mymean)])) %>%
  ggplot() +
  geom_segment(aes(x = state,
                   xend = state, 
                   y = likely_fw_alternative_low_kg_per_capita, 
                   yend = likely_fw_alternative_high_kg_per_capita),
               colour = "black",
               alpha = 0.75,
               linewidth = 1.5) +
  geom_segment(aes(x = state,
                   xend = state, 
                   y = likely_fw_baseline_low_kg_per_capita, 
                   yend = likely_fw_baseline_high_kg_per_capita),
               colour = "blue",
               alpha = 0.75,
               linewidth = 1.5) +
  geom_point(aes(x = state, y = total_fw_kg_per_capita),
             color = rgb(0.7, 0.2, 0.1, 0.5),
             size = 2) +
  geom_hline(yintercept = fw_target,
             color = rgb(0.2, 0.7, 0.1, 0.5),
             linewidth = 1) +
  scale_y_continuous(trans = "log2", limits = c(0.06, 750), breaks = c(0.1, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500),
                     labels = c(0.1, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500)) +
  coord_flip() +
  theme_economist() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Likely food waste diversion potential from landfill bans and recycling (kg per capita)")

pdf("likely_fw_per_policy_level_OG.pdf")
print(myplot1)     
print(myplot2)
print(myplot3)     
print(myplot4)
dev.off()

