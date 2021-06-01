library(dplyr)
library(magrittr)
library(DescTools)
library(readxl)
library(tidyr)

# PAVEMENTS ----------------------------------------------------------------------------------------

# how many are in bad condiiton using POF0

number = fmeca5 %>% group_by(ConOps_Rating) %>% 
  summarise(number = length(POF0))

minimum = fmeca5 %>% group_by(ConOps_Rating) %>% 
  filter(POF0 >= 1) %>% 
  tally(name = "Minimum") %>% 
  left_join(number, by = "ConOps_Rating") %>% 
  group_by(ConOps_Rating) %>% 
  summarise(Minimum = 100 * ((number - Minimum) / number))


minimum


# try using PHI old (i.e. provided by arcadis)- 5 as bad provided

number = projects1_pavement %>% group_by(ConOps_Rating) %>% 
  summarise(number = length(`PHI 2020`))

minimum = projects1_pavement %>% group_by(ConOps_Rating) %>% 
  filter(`PHI 2020` > 2.5) %>% 
  tally(name = "Minimum") %>% 
  left_join(number, by = "ConOps_Rating") %>% 
  group_by(ConOps_Rating) %>% 
  summarise(Minimum = 100 * ((number - Minimum) / number))


minimum


# try using PHI 2020 new- 5 as bad provided

number = projects1_pavement %>% group_by(ConOps_Rating) %>% 
  summarise(number = length(`PHI 2020`))

minimum = projects1_pavement %>% left_join(projects1_pavement4[,c("AssetID","PHI_2020_new")], by = "AssetID") %>% 
  group_by(ConOps_Rating) %>% 
  filter(`PHI_2020_new` > 3) %>% 
  tally(name = "Minimum") %>% 
  left_join(number, by = "ConOps_Rating") %>% 
  group_by(ConOps_Rating) %>% 
  summarise(Minimum = 100 * ((number - Minimum) / number))


minimum


# Testing Intervention strategies ----------------------------------------------------------------------------------------

## The difference between Intervention stragey and minimum level - those replaced using intervention vs year 1,2,3

test_p <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Pavements/pavement_assets ConOps Calcs.xlsx"),range = "A1:AF1596", sheet = "pavement_assets2")

number = test_p %>% group_by(ConOps_Rating) %>% 
  summarise(number = length(`PHI_2020_new`))

minimum = test_p %>% 
  group_by(ConOps_Rating) %>% 
  filter(`Intervention Y1` == 1) %>% 
  tally(name = "Minimum") %>% 
  left_join(number, by = "ConOps_Rating") %>% 
  group_by(ConOps_Rating) %>% 
  summarise(Minimum = 100 * ((number - Minimum) / number))


minimum



# used for the minimum level

test_p <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Pavements/pavement_assets ConOps Calcs.xlsx"),range = "A1:AX1596", sheet = "pavement_assets2")

number = test_p %>% group_by(ConOps_Rating) %>% 
  summarise(number = length(`PHI_2020_new`))

minimum = test_p %>% 
  group_by(ConOps_Rating) %>% 
  filter(`Minimum Level0` == 1) %>% 
  tally(name = "Minimum") %>% 
  left_join(number, by = "ConOps_Rating") %>% 
  group_by(ConOps_Rating) %>% 
  summarise(Minimum = 100 * ((number - Minimum) / number))


minimum


# functonise the above



Calculate_conops_minimum <- function(data = test_p,
                                     y = "Minimum_Level0"){
  
  # rename the columns
  colnames(data)[which(names(data) == y)] <- "y"
  
  # analysis
  
  number = data %>% group_by(ConOps_Rating) %>% 
    summarise(number = length(`PHI_2020_new`))
  
  minimum = data %>% 
    group_by(ConOps_Rating) %>% 
    filter(y == 1) %>% 
    tally(name = "Minimum") %>% 
    left_join(number, by = "ConOps_Rating") %>% 
    group_by(ConOps_Rating) %>% 
    summarise(Minimum = 100 * ((number - Minimum) / number))
  
  minimum
}

Calculate_conops_minimum(test_p, "Minimum_Level0")
Calculate_conops_minimum(test_p, "Minimum_Level1")
Calculate_conops_minimum(test_p, "Minimum_Level2")
Calculate_conops_minimum(test_p, "Minimum_Level3")

Calculate_conops_minimum(test_p, "Intervention_happened_in_Y1")
Calculate_conops_minimum(test_p, "Intervention_happened_in_Y2")
Calculate_conops_minimum(test_p, "Intervention_happened_in_Y3")

test_p <- read_excel(("C:/Users/TOsosanya/Desktop/NSW//Newdata 2705/Pavements/pavement_assets ConOps Calcs.xlsx"),range = "A1:AX1596", sheet = "pavement_assets2")


# CULVERTS --------------------------------------------------------------------------------------------------

test_cc <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Corridors/Corridor assets ConOps Calcs.xlsx"),range = "A1:AG4000", sheet = "Sheet 1")

number = test_cc %>% group_by(ConOps_Rating) %>% 
  filter(!is.na(ARL)) %>% # includes only assessed assets
  summarise(number = length(`ARL`))

minimum = test_cc %>% 
  group_by(ConOps_Rating) %>% 
  filter(`Intervention_happened_in_Y1` == 1) %>% 
  tally(name = "Minimum") %>% 
  left_join(number, by = "ConOps_Rating") %>% 
  group_by(ConOps_Rating) %>% 
  summarise(Minimum = 100 * ((Minimum) / number))


minimum

# functionise the above

Calculate_conops_minimum_2 <- function(data = test_cc,
                                     y = "Intervention_happened_in_Y1"){
  
  # rename the columns
  colnames(data)[which(names(data) == y)] <- "y"
  
  # analysis
  
  number = data %>% group_by(ConOps_Rating) %>% 
    filter(!is.na(ARL)) %>% # includes only assessed assets
    summarise(number = length(`ARL`))
  
  minimum = data %>% 
    group_by(ConOps_Rating) %>% 
    filter(y == 1) %>% 
    tally(name = "Minimum") %>% 
    left_join(number, by = "ConOps_Rating") %>% 
    group_by(ConOps_Rating) %>% 
    summarise(Minimum = 100 * ((Minimum) / number))
  
  minimum
}


# BRIDGES --------------------------------------------------------------------------------------------------

test_cc <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Corridors/Corridor assets ConOps Calcs.xlsx"),range = "A1:AG4000", sheet = "Sheet 1")

number = test_cc %>% group_by(ConOps_Rating) %>% 
  filter(!is.na(ARL)) %>% # includes only assessed assets
  summarise(number = length(`ARL`))

minimum = test_cc %>% 
  group_by(ConOps_Rating) %>% 
  filter(`Intervention_happened_in_Y1` == 1) %>% 
  tally(name = "Minimum") %>% 
  left_join(number, by = "ConOps_Rating") %>% 
  group_by(ConOps_Rating) %>% 
  summarise(Minimum = 100 * ((Minimum) / number))


minimum
