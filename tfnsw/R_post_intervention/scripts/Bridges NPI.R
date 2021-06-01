# Title     : Data Clean NSW - Bridges
# Objective : Clean NSW Datasets and process for modelling
# Created by: TOsosanya
# Created on: 02/06/2020

library(dplyr)
library(magrittr)
library(DescTools)
library(readxl)
library(tidyr)
library(data.table)

## bridges projects

# Functions ---------------------------------------------------------------------------------------

# Convert PHI to PCI
Convert_NewValue = function(OldValue, OldRangeMax, OldRangeMin, NewRangeMax, NewRangeMin){
  OldRange = (OldRangeMax - OldRangeMin)  
  NewRange = (NewRangeMax - NewRangeMin) 
  (((OldValue - OldRangeMax) * NewRange) / OldRange) + NewRangeMax
}

Convert_NewValue(3,0,5,100,0)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mode.infill <- function(x){
  x <- ifelse(is.na(x), Mode(x), x)
}

median.infill <- function(x){
  x <- ifelse(is.na(x) | x == 0, median(x,na.rm=TRUE), x)
  return(x)
}


### Bridges  -------------------------------------------------------------------------

path_b <- "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Bridges"
bridges_assets1 <- read_excel(paste0(path_b, "/Bridge mastersheet.xlsx"), range = "A1:AT450" ,sheet = "original mastersheet")
bridges_assets2 <- bridges_assets1 %>% dplyr::select('Asset Category',	Zone,	Latitude, Longitude, ObjectID, 'Asset_Type', 'Project Name', CURRENT_ZONE,
                                      'Delivery Year',	'ROAD_NUMBER',	'CONSTRUCTION_YEAR', LIFE_SPAN, CY_STATUS, BNO,
                                      'FMECA_Code',	'FMECA_Asset Level 1',	'FMECA_Asset Level 2',	'FMECA_Asset Level 3',	'FMECA_Asset Level 4',	'FMECA_Rating',	
                                      'ConOps_Walk',	'ConOps_Cycle',	'ConOps_Transit',	'ConOps_Freight',	'ConOps_GV',	'ConOps_Score',	'ConOps_Rating',rownumbering) %>% 
  mutate(CONSTRUCTION_YEAR = as.integer(CONSTRUCTION_YEAR)) %>%
  dplyr::filter(Asset_Type != "Bridge") 
#mutate(CONSTRUCTION_YEAR = median.infill(CONSTRUCTION_YEAR)) %>%
#mutate(PoF_bridges = (1- (((as.numeric(CONSTRUCTION_YEAR) + as.numeric(LIFE_SPAN)) - 2020)/ as.numeric(LIFE_SPAN))))


split_bridges <- split(bridges_assets2,bridges_assets2$Asset_Type)
split_bridges1 <- split_bridges[[2]]
no_rows = nrow(split_bridges1)
m = 1984
std = 5
impossible.year = as.numeric(1700)
CT = as.numeric(2020) # current year

unique(split_bridges1$CONSTRUCTION_YEAR)

# replace (groups with all) NA with the rnorm else median.infill
set.seed(1)
# split_bridges1$CONSTRUCTION_YEAR <- ifelse(min(split_bridges1$CONSTRUCTION_YEAR,na.rm = T) == max(split_bridges1$CONSTRUCTION_YEAR, na.rm = T),
#                                              round(rnorm(no_rows,mean = m, sd = std)),
#                                              #ifelse(is.na(split_bridges1$CONSTRUCTION_YEAR),
#                                                   #median.infill(split_bridges1$CONSTRUCTION_YEAR),
#                                                   as.numeric(split_bridges1$CONSTRUCTION_YEAR))

split_bridges1$CONSTRUCTION_YEAR <- median.infill(split_bridges1$CONSTRUCTION_YEAR)
unique(split_bridges1$CONSTRUCTION_YEAR)
split_bridges1 <- split_bridges1 %>% mutate(CONSTRUCTION_YEAR = ifelse(CONSTRUCTION_YEAR == m, round(rnorm(no_rows,mean = m, sd = std),0), as.numeric(CONSTRUCTION_YEAR)))
unique(split_bridges1$CONSTRUCTION_YEAR)                                          

# split_bridges1$CONSTRUCTION_YEAR <- ifelse(is.na(split_bridges1$CONSTRUCTION_YEAR),round(rnorm(no_rows,mean = m, sd = std)),
#                                    median.infill(split_bridges1$CONSTRUCTION_YEAR))
# 
# other_bridges2 <- other_bridges1 %>%
#   mutate(LIFE_SPAN_CY = as.numeric(LIFE_SPAN) + as.numeric(CONSTRUCTION_YEAR)) %>%
#   mutate(design_life_used = (CT - as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN)

unique(split_bridges1$CONSTRUCTION_YEAR)
# unique(other_bridges1$CONSTRUCTION_YEAR)

# does not use the above linear model as that part in commented. POF0 = 2020, POF1 = 2020
# other_bridges3 <- other_bridges2 %>%
#   mutate(POF0 = Convert_NewValue(design_life_used,0,1,0,1))


test.output <- NULL
output.final <- NULL
Calculate_POF_randomCY_fixedLS <- function(x, m, std, CT) { # uses design life used to calculate POF
  for (j in 1:length(x)) {
    CT = as.numeric(CT)
    impossible.year = as.numeric(1700)
    test <- data.table(x[[j]])
    no_rows = nrow(test)
    # calculation starts here
    set.seed(1)
    # replace (groups with all) NA with the rnorm else median.infill
    test$CONSTRUCTION_YEAR <- median.infill(test$CONSTRUCTION_YEAR)
    test <- test %>% mutate(CONSTRUCTION_YEAR = ifelse(CONSTRUCTION_YEAR == m,
                                                       round(rnorm(no_rows,mean = m, sd = std),0), as.numeric(CONSTRUCTION_YEAR)))
    # test$CONSTRUCTION_YEAR <- ifelse(is.na(test$CONSTRUCTION_YEAR), round(rnorm(no_rows,mean = m, sd = std)), 
    #                                              median.infill(test$CONSTRUCTION_YEAR)) 
    test.output <- test %>%
      mutate(LIFE_SPAN_CY = as.numeric(LIFE_SPAN) + as.numeric(CONSTRUCTION_YEAR)) %>% 
      mutate(design_life_used = (CT - as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN) %>% 
      mutate(POF0 = (CT - as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN,
             POF1 = ((CT + 1) - as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN,
             POF2 = ((CT + 2)- as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN,
             POF3 = ((CT + 3) - as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN,
             POF4 = ((CT + 4) - as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN,
             POF5 = ((CT + 5)- as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN,
             POF6 = ((CT + 6) - as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN,
             POF7 = ((CT + 7) - as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN,
             POF8 = ((CT + 8)- as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN,
             POF9 = ((CT + 9) - as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN,
             POF10 = ((CT + 10) - as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN)
    if (j == 1) {
      output.final <- test.output
    } else{
      output.final <- rbind(output.final, test.output)
    }
  }
  return(as.data.table(output.final))
}

above_result_bridges <- Calculate_POF_randomCY_fixedLS(split_bridges,1984,5,2020) %>% as.data.frame()


# subset for bridges - bring in bridges and bring in bridges calculation -------------------------------------------------

bridges_bridges <- bridges_assets1 %>% dplyr::select('Asset Category',	Zone,	Latitude, Longitude, ObjectID, 'Asset_Type', 'Project Name', CURRENT_ZONE,
                                              'Delivery Year',	'ROAD_NUMBER',	'CONSTRUCTION_YEAR', LIFE_SPAN, CY_STATUS, BNO,
                                              'FMECA_Code',	'FMECA_Asset Level 1',	'FMECA_Asset Level 2',	'FMECA_Asset Level 3',	'FMECA_Asset Level 4',	'FMECA_Rating',	
                                              'ConOps_Walk',	'ConOps_Cycle',	'ConOps_Transit',	'ConOps_Freight',	'ConOps_GV',	'ConOps_Score',	'ConOps_Rating',rownumbering) %>% 
  mutate(CONSTRUCTION_YEAR = as.numeric(CONSTRUCTION_YEAR)) %>%
  dplyr::filter(Asset_Type == "Bridge")

# bring in bridges POFs
source("C:/Users/TOsosanya/Desktop/NSW/tfnsw/state condition with bhi_func.r")

above_result_bridges_bridges <- bridges_bridges %>% left_join(full_bhi2, by = c("BNO"="bno"))

# dplyr::select necessary columns
columns <- c("Asset Category","ObjectID","Zone","ROAD_NUMBER", "Latitude","Longitude","Delivery Year",
             "Asset_Type","Project Name", "CONSTRUCTION_YEAR", "CY_STATUS", "BNO", "LIFE_SPAN", "ConOps_Score", 
             "POF0","POF1","POF2","POF3","POF4","POF5","POF6","POF7","POF8","POF9","POF10","rownumbering")

above_result_bridges2 <- rbind(above_result_bridges[,columns], above_result_bridges_bridges[,columns])

# FMECA new--------------------------------------------------------------------------------------------------------

fmeca1_bridges1 <- read_excel(paste0(path_b, "/Bridge mastersheet.xlsx"), range = "AD14:AN25" ,sheet = "fmeca")
names(fmeca1_bridges1)
fmeca1_bridges2 <- fmeca1_bridges1 %>% rename('FMECA_Asset Level 3' = 'Row Labels')
fmeca1_bridges3 <- above_result_bridges2 %>% left_join(fmeca1_bridges2, by = c("Asset_Type"="FMECA_Asset Level 3")) 

# cols.num <- c(24,37:48) # mutate_at('ConOps_Score','Sum of R (Reliability)':'Sum of Total Consequence')
# fmeca3[cols.num] <- sapply(fmeca3[cols.num],as.numeric)
# sapply(fmeca3, class)

fmeca_npi <- fmeca1_bridges3 %>% 
  # fmeca x conops
  mutate(FMECA_CONOPS = as.numeric(Sum_of_Total_Consequence) * as.integer(ConOps_Score)) %>% 
  mutate(FMECA_Reliability_CONOPS = Sum_of_R_Reliability * ConOps_Score) %>%
  mutate(FMECA_Availability_CONOPS = Sum_of_A_Availability * ConOps_Score) %>%
  mutate(FMECA_Maintainability_CONOPS = Sum_of_M_Maintainability * ConOps_Score) %>%
  mutate(FMECA_Safety_CONOPS = Sum_of_S_Safety * ConOps_Score) %>%
  mutate(FMECA_Security_CONOPS = Sum_of_Se_Security * ConOps_Score) %>%
  mutate(FMECA_Health_CONOPS = Sum_of_He_Health * ConOps_Score) %>%
  mutate(FMECA_Economics_CONOPS = Sum_of_Ec_Economics * ConOps_Score) %>%
  mutate(FMECA_Environment_CONOPS = Sum_of_En_Environment * ConOps_Score) %>%
  mutate(FMECA_Political_CONOPS = Sum_of_P_Political * ConOps_Score)



