# Join data to calculate BHI: Bridge Health Index

library(tidyverse)
library(readxl)
rm(list = ls())
setwd("C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Bridges/")
source("bhi_func.R")

folder <- "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Bridges/"
file1 <- "Bridges, Tunnels and Bridge Sized Culverts Condition Rating.xlsx"
testfile <- "test_data_simple.xlsx"

# Use as a switch to test different files below. 
# Also used to detect whether the test file is in use for testing purposes
file <- file1

#> Calculations ----
# Asset input, minimal conditioning
df <- read_xlsx(paste0(folder,file), sheet = "in") %>%
  select(-matches("^X")) %>% # Remove spurious columns without names
  rename_all(~str_replace_all(., "\\s", "_") %>% tolower(.)) %>%
  mutate_at(vars(contains("state")), ~replace(., is.na(.), 0)) %>%
  filter(proposed_zone == "River Zone")

# Element information table
element_table <- read_csv(paste0(folder, "tables.csv")) %>%
  rename_all(~str_replace(., "\r", "_") %>% tolower(.)) # Remove character returns in col header

# BHI lookup table returning BHI from state, importance and BHI group
lookup_table <- read_xlsx(paste0(folder, "tables.xlsx"), sheet = "table2") %>%
  rename_all(~str_replace_all(., "\\s", "_") %>% tolower(.)) %>%
  mutate(bhi = factor(bhi, levels = c("Poor", "Fair", "Good", "As-Built"),
                      ordered = T),
         importance_factor = factor(importance_factor,
                                    levels = c("Low", "Medium", "High"),
                                    ordered = T),
         bhi_group = factor(bhi_group, levels = LETTERS[1:4])) %>%
  rowid_to_column("logicgroup") %>%
  pivot_longer(contains("_cond"), names_to = "boundary", values_to = "limit_pct",
               values_drop_na = T) %>%
  separate(boundary, sep = "_cond", into = c("boundary", "boundary_state")) %>%
  filter(boundary_state <= 4) %>% # State 5 not mentioned in the data
  pivot_wider(names_from = boundary, values_from = limit_pct)

# Begin joining asset condition data with element tables
bridges <- df %>%
  pivot_longer(contains("state_"), names_to = "state", values_to = "elements") %>%
  mutate(state = str_extract(state, "[:digit:]")) %>%
  filter(state <= 4) %>%  # State 5 not mentioned in the data
  left_join(element_table, by = c("element_type" = "element_code")) %>%
  mutate(importance_factor = factor(importance_factor,
                                    levels = c("Low", "Medium", "High"),
                                    ordered = T),
         bhi_group = factor(bhi_group, levels = LETTERS[1:4])) %>%
  group_by(bno, element_type) %>%
  mutate(total_states = sum(elements)) %>%
  mutate(pct_elements = elements * 100 / total_states) %>%
  left_join(lookup_table, by = c("bhi_group", "importance_factor", 
                                 "state" = "boundary_state"))

bridges_load <- load_bridge(paste0(folder, file))
all.equal(bridges, bridges_load)


bridges_bhi <- bridges %>%
  mutate(condition_met = case_when(
    low <= pct_elements &  pct_elements  <= high ~ T,
    is.nan(pct_elements) & bhi == "As-Built" ~ T,
    TRUE ~ F
  )) %>%
  group_by(bno, element_type, bhi, logicgroup) %>%
  mutate(bhi_return = case_when(
    min(condition_met) == 1 ~ as.numeric(bhi), # min is the same as AND - all conditions have to be met
    TRUE ~ 0
  ),
  range = high - low,
  bhi_score = case_when(
    state <= 2 ~ 0,
    range == 0 ~ 0,
    pct_elements == 0 ~ 0,
    TRUE ~ round(condition_met *
                   # exp(((pct_elements - low)/range) * as.numeric(state) - 4),
                   pmin(exp(((pct_elements- low)/range) * as.numeric(state) - 
                              4 + as.numeric(importance_factor)), 
                        ifelse(state == 3, 0.75, 1)),
                 digits = 2)
    )) %>%
  group_by(bno, element_type) %>%
  mutate(bhi_score = pmin(sum(bhi_score, na.rm = T), 1)) %>%
  filter(bhi_return == max(bhi_return))

bridges_calc <- bridges_load %>% calc_bhi(original_layout = F)

all.equal(bridges_bhi, bridges_calc)

# Cross check against the master table
master <- read_xlsx(paste0(folder, "Bridge mastersheet.xlsx"))

masterbhi <- master %>%
  select(bno = BNO, bhi_code = "BHI CODE", project_name = `Project Name`,
         conops = ConOps_Score) %>%
  mutate(bno = as.numeric(bno),
         bhi = factor(bhi_code, levels = c(5,3,1,0),
                      labels = levels(bridges_calc$bhi),
                      ordered = T),
         project = ifelse(is.na(project_name), F, T))

bridges_out <- bridges_calc %>% 
  group_by(bno, element_type, bhi_score) %>%
  na.omit() %>%
  distinct(bhi) %>%
  select(bno, everything()) %>%
  left_join(masterbhi, by = "bno", suffix = c(".element", ".master"))
  
# BHI aggregated at the bridge level
bridge_agg <- bridges_out %>%
  group_by(bno) %>%
  summarise(bhi.bridge = min(bhi.element))

# Feed bridge level bhi back in and rename
bridges_out <- bridges_out %>%
  left_join(bridge_agg, by = "bno") %>%
  select(bno, bhi.bridge, element_type, bhi.element, bhi_score, bhi.master, 
         everything())

# Element level results
write_csv(bridges_out, paste0(folder,"modelled_element_BHI.csv"))
save(bridges_out, file = paste0(folder,"modelled_element_BHI.Rdata"))
# Bridge level results
write_csv(bridge_agg, paste0(folder,"modelled_bridge_BHI.csv"))
save(bridge_agg, file = paste0(folder,"modelled_bridge_BHI.Rdata"))

# Project inspection from master list
proj_inspection <- bridges_out %>%
  filter(project) %>%
  left_join(element_table, by = c("element_type" = "element_code"))
write_csv(proj_inspection, paste0(folder, "master_projects.csv"))
save(proj_inspection, file = paste0(folder, "master_projects.Rdata"))

#> Testing and evaluation ----
# Unit test
if(file == testfile){
  pass <- sum(bridges_bhi$bridge_description == bridges_bhi$bhi) == nrow(bridges_bhi)
  message(paste0("Test passed? ", pass))
}

# Plotting
ggplot(bridges_out,
       aes(x = bhi.element)) +
  geom_histogram(stat = "count")

ggplot(bridges_out,
       aes(x = reorder(bhi.element, desc(bhi.element)), y = bhi_score)) +
  geom_jitter() +
  labs(title = "BHI score against BHI category",
       x = "BHI category")

# This test is to check calculate BHIs with BHIs from the master table
test <- bridge_agg %>%
  inner_join(masterbhi, by = "bno") %>%
  rename(bhi.master = bhi, bhi.calc = bhi.bridge) %>%
  group_by(bhi.calc, bhi.master) %>%
  tally()

# Plot the results
ggplot(test, aes(x = bhi.master, y = bhi.calc, fill = n)) +
  geom_tile(show.legend = F) +
  geom_abline() +
  geom_text(aes(label = n))+
  scale_fill_gradient2(low = "grey", high = "red", mid = "orange",
                       midpoint = median(test$n, na.rm = T)/2,
                       trans = "log2") +
  labs(title = "Comparison between calculated bhi and bhi from master table",
       x = "Master BHI",
       y = "Calculated BHI")


# A subset of bridges have inconsistent modelled bhis compared with the master table
# Examine:

anomalous <- bridge_agg %>%
  inner_join(masterbhi, by = "bno") %>%
  rename(bhi.master = bhi) %>%
  filter(!is.na(bhi.master),
         bhi.master != bhi.bridge)

master %>%
  mutate(bno = as.numeric(BNO)) %>%
  right_join(anomalous, by = "bno") %>%
  select(-bhi.master, -bhi_code) %>%
  write_csv(paste0(folder, "anomalous_bhi.csv")) # anomaly table

# Examine the anomalous BHIs at the element level
anomalous %>%
  left_join(bridges_out, by = "bno") %>%
  View()
