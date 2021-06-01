# Title     : Data Clean NSW - corridors
# Objective : Clean NSW Datasets and process for modelling
# Created by: TOsosanya
# Created on: 02/06/2020

library(dplyr)
library(magrittr)
library(DescTools)
library(readxl)
library(tidyr)
library(data.table)


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


## corridors projects

### culverts -------------------------------------------------------------------------

path_c <- "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Corridors"
corridor_culvert1 <- read_excel(paste0(path_c, "/Corridor mastersheet.xlsx"), range = "A1:BK9138" ,sheet = "original mastersheet")
corridor_culvert2 <- corridor_culvert1 %>% dplyr::select('Asset Category',	ObjectID, Zone,	Latitude, Longitude, 'Asset_Type', 'Project Name', QuantityAsset, CURRENT_ZONE,
                                                  'Asset Number',	'ROAD_NUMBER',	'CONSTRUCTION_YEAR', ARL, LIFE_SPAN, CY_STATUS, 'No. Pipes/Cells', 'Headwall / Wingwall Repairs', 
                                                  'Clearing of Barrell', 'Clearing Inlet / Outlet', 'Pipe/Box Repairs', 'Joint Repairs', Last_Inspection_Date,
                                                  'FMECA_Code',	'FMECA_Asset Level 1',	'FMECA_Asset Level 2',	'FMECA_Asset Level 3',	'FMECA_Asset Level 4',	'FMECA_Rating',	
                                                  'ConOps_Walk',	'ConOps_Cycle',	'ConOps_Transit',	'ConOps_Freight',	'ConOps_GV',	'ConOps_Score',	'ConOps_Rating') %>% 
  dplyr::filter(Asset_Type == "Culvert") %>% 
  mutate(CONSTRUCTION_YEAR = substring(CONSTRUCTION_YEAR,1,4)) %>% 
  mutate(Last_Inspection_Date = substring(Last_Inspection_Date,1,4))
  
  

# replace na with 0
corridor_culvert2[, 17:21][is.na(corridor_culvert2[, 17:21])] <- 0 

corridor_culvert3 <- corridor_culvert2 %>% mutate(Repair.Total = dplyr::select(., 'Headwall / Wingwall Repairs':'Joint Repairs') %>% rowSums(na.rm = TRUE)) %>% 
  mutate(ARL = ifelse(is.na(ARL), 1, as.numeric(ARL))) %>% # infill ARL
  mutate(PoF_LID = (as.numeric(Repair.Total) * 0.05) +  ((as.numeric(ARL)-1)*0.25)) # dans formula

  
write.csv(corridor_culvert3,"c-culvert with PoF.csv", row.names = F)

# linear model - get the gradient (m)

for_linear2 <- corridor_culvert3 %>% 
  filter(Last_Inspection_Date != 0) %>% mutate(Date_diff = as.numeric(Last_Inspection_Date) - as.numeric(CONSTRUCTION_YEAR)) %>%
  filter(Date_diff != 0) %>% 
  filter(PoF_LID != 0)
plot(for_linear2$Date_diff, for_linear2$PoF_LID)
summary(for_linear2)
linearMod2 <- lm(PoF_LID ~ Date_diff, data = for_linear2)
summary(linearMod2)
m2 = summary(linearMod2)$coefficients[2,1]
c2 = summary(linearMod2)$coefficients[1,1]


# bring the data back

corridor_culvert3$Last_Inspection_Date <- median.infill(corridor_culvert3$Last_Inspection_Date) # infill last inspection date
corridor_culvert4 <- corridor_culvert3 %>% 
  mutate(Date_diff_2020 = 2020 - as.numeric(as.character(Last_Inspection_Date))) %>% # Date diff from 2020 till last intervention date
  mutate(POF0 = ifelse(Date_diff_2020 > 0, 
                           PoF_LID - c2 + ((m2*Date_diff_2020) + c2) , PoF_LID)) %>%  # apply the flat deterioration
  mutate(POF1 = POF0 - c2 + (m2*1 + c2)) %>% 
  mutate(POF2 = POF1 - c2 + (m2*1 + c2)) %>% 
  mutate(POF3 = POF2 - c2 + (m2*1 + c2)) %>% 
  mutate(POF4 = POF3 - c2 + (m2*1 + c2)) %>% 
  mutate(POF5 = POF4 - c2 + (m2*1 + c2)) %>% 
  mutate(POF6 = POF5 - c2 + (m2*1 + c2)) %>% 
  mutate(POF7 = POF6 - c2 + (m2*1 + c2)) %>% 
  mutate(POF8 = POF7 - c2 + (m2*1 + c2)) %>% 
  mutate(POF9 = POF8 - c2 + (m2*1 + c2)) %>% 
  mutate(POF10 = POF9 - c2 + (m2*1 + c2))
  


### slope -------------------------------------------------------------------------

path_c <- "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Corridors"
corridor_slope1 <- read_excel(paste0(path_c, "/Corridor mastersheet.xlsx"), range = "A1:BK9138" ,sheet = "original mastersheet")
corridor_slope2 <- corridor_slope1 %>% dplyr::select('Asset Category',	ObjectID, Zone,	Latitude, Longitude, 'Asset_Type', 'Project Name', QuantityAsset, CURRENT_ZONE,
                                                  'Asset Number',	'ROAD_NUMBER',	'CONSTRUCTION_YEAR', ARL, LIFE_SPAN, CY_STATUS, 'No. Pipes/Cells', 'Headwall / Wingwall Repairs', 
                                                  'Clearing of Barrell', 'Clearing Inlet / Outlet', 'Pipe/Box Repairs', 'Joint Repairs', Last_Inspection_Date,
                                                  'FMECA_Code',	'FMECA_Asset Level 1',	'FMECA_Asset Level 2',	'FMECA_Asset Level 3',	'FMECA_Asset Level 4',	'FMECA_Rating',	
                                                  'ConOps_Walk',	'ConOps_Cycle',	'ConOps_Transit',	'ConOps_Freight',	'ConOps_GV',	'ConOps_Score',	'ConOps_Rating') %>% 
  dplyr::filter(Asset_Type == "Slope Site")

#mutate(no_rows = nrow(.))
no_rows = nrow(corridor_slope2)
m = 1970
std = 5
CT = as.numeric(2020) # current year

set.seed(1)
corridor_slope3 <- corridor_slope2 %>%
  mutate(CONSTRUCTION_YEAR = round(rnorm(no_rows,mean = m, sd = std)),0) %>% # random generated construction_year
  mutate(LIFE_SPAN_CY = as.numeric(LIFE_SPAN) + as.numeric(CONSTRUCTION_YEAR)) %>% 
  mutate(design_life_used = (CT - as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN,
         design_life_used1 = ((CT + 1) - as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN,
         design_life_used2 = ((CT + 2)- as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN,
         design_life_used3 = ((CT + 3) - as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN,
         design_life_used4 = ((CT + 4) - as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN,
         design_life_used5 = ((CT + 5)- as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN,
         design_life_used6 = ((CT + 6) - as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN,
         design_life_used7 = ((CT + 7) - as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN,
         design_life_used8 = ((CT + 8)- as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN,
         design_life_used9 = ((CT + 9) - as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN,
         design_life_used10 = ((CT + 10) - as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN) %>% 
  mutate(ARL = ifelse(is.na(ARL), 1, as.numeric(ARL)))
  
# linear model - get the gradient (m)
for_linear2 <- corridor_slope3 %>% 
  filter(design_life_used != 0) %>%
  filter(ARL != 0) 
plot(for_linear2$design_life_used, for_linear2$ARL)
summary(for_linear2)
linearMod2 <- lm(ARL ~ design_life_used, data = for_linear2)
summary(linearMod2)
m2 = summary(linearMod2)$coefficients[2,1]
c2 = summary(linearMod2)$coefficients[1,1]

# does not use the above linear model as that part in commented
corridor_slope4 <- corridor_slope3 %>% 
  # mutate(Date_diff_2020 = CT - as.numeric(as.character(LIFE_SPAN))) %>% # Date diff from 2020 till last intervention date
  # mutate(PoF_2020 = ifelse(Date_diff_2020 > 0, 
  #                          PoF_LID + ((m2*Date_diff_2020) + c2) , PoF_LID)) # apply the flat deterioration
  mutate(POF0 = design_life_used,
         POF1 = design_life_used1,
         POF2 = design_life_used2,
         POF3 = design_life_used3,
         POF4 = design_life_used4,
         POF5 = design_life_used5,
         POF6 = design_life_used6,
         POF7 = design_life_used7,
         POF8 = design_life_used8,
         POF9 = design_life_used9,
         POF10 = design_life_used10)


# no inspection date -------------------------------------------------------------------------------

# use AssetNumber / ObjectID
# if quantity = 0, 1, quantity
# median infill CONSTRUCTION_YEAR and LIFE_SPAN
# Take Asset Category, Asset_Type, Asset Number, Quantity, Zone, CONSTRUCTION_YEAR,	LIFE_SPAN
# 

# no inspection date

# function writing ------------------------------------------------------------------------------------------

corridor <- read_excel(paste0(path_c, "/Corridor mastersheet.xlsx"), range = "A1:BK9138" ,sheet = "original mastersheet", guess_max = 5000)
other_corridors1 <- corridor %>% dplyr::select('Asset Category',	ObjectID, Zone,	Latitude, Longitude, 'Asset_Type', 'Project Name', QuantityAsset, CURRENT_ZONE,
                                              'Asset Number',	'ROAD_NUMBER',	'CONSTRUCTION_YEAR', ARL, LIFE_SPAN, CY_STATUS, 'No. Pipes/Cells', 'Headwall / Wingwall Repairs',
                                              'Clearing of Barrell', 'Clearing Inlet / Outlet', 'Pipe/Box Repairs', 'Joint Repairs', Last_Inspection_Date,
                                              'FMECA_Code',	'FMECA_Asset Level 1',	'FMECA_Asset Level 2',	'FMECA_Asset Level 3',	'FMECA_Asset Level 4',	'FMECA_Rating',
                                              'ConOps_Walk',	'ConOps_Cycle',	'ConOps_Transit',	'ConOps_Freight',	'ConOps_GV',	'ConOps_Score',	'ConOps_Rating') %>%
  dplyr::filter(Asset_Type != "Slope Site", Asset_Type != "Culvert") %>%
  mutate(CONSTRUCTION_YEAR = as.numeric(CONSTRUCTION_YEAR))
unique(other_corridors1$CONSTRUCTION_YEAR)

split <- split(other_corridors1,other_corridors1$Asset_Type)
split1 <- split[[1]]
no_rows = nrow(split1)
m = 1970
std = 5
impossible.year = as.numeric(1700)
CT = as.numeric(2020) # current year

unique(split1$CONSTRUCTION_YEAR)

# replace (groups with all) NA with the rnorm else median.infill
set.seed(1)
# split1$CONSTRUCTION_YEAR <- ifelse(min(split1$CONSTRUCTION_YEAR,na.rm = T) == max(split1$CONSTRUCTION_YEAR, na.rm = T),
#                                              round(rnorm(no_rows,mean = m, sd = std)),
#                                              #ifelse(is.na(split1$CONSTRUCTION_YEAR),
#                                                   #median.infill(split1$CONSTRUCTION_YEAR),
#                                                   as.numeric(split1$CONSTRUCTION_YEAR))

split1$CONSTRUCTION_YEAR <- median.infill(split1$CONSTRUCTION_YEAR)
unique(split1$CONSTRUCTION_YEAR)
split1 <- split1 %>% mutate(CONSTRUCTION_YEAR = ifelse(CONSTRUCTION_YEAR == m, round(rnorm(no_rows,mean = m, sd = std),0), as.numeric(CONSTRUCTION_YEAR)))
unique(split1$CONSTRUCTION_YEAR)                                          

# split1$CONSTRUCTION_YEAR <- ifelse(is.na(split1$CONSTRUCTION_YEAR),round(rnorm(no_rows,mean = m, sd = std)),
#                                    median.infill(split1$CONSTRUCTION_YEAR))
# 
# other_corridors2 <- other_corridors1 %>%
#   mutate(LIFE_SPAN_CY = as.numeric(LIFE_SPAN) + as.numeric(CONSTRUCTION_YEAR)) %>%
#   mutate(design_life_used = (CT - as.numeric(CONSTRUCTION_YEAR))/LIFE_SPAN)

unique(split1$CONSTRUCTION_YEAR)
# unique(other_corridors1$CONSTRUCTION_YEAR)

# does not use the above linear model as that part in commented. POF0 = 2020, POF1 = 2020
# other_corridors3 <- other_corridors2 %>%
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

above_result <- Calculate_POF_randomCY_fixedLS(split,1970,5,2020) %>% as.data.frame()

# merge with culvert and slope

columns <- c("Asset Category", "Asset Number","ObjectID","Zone","Latitude","Longitude", "QuantityAsset",
             "Asset_Type","Project Name", "CONSTRUCTION_YEAR", "CY_STATUS", "ARL", "LIFE_SPAN", "ConOps_Score",
             "POF0","POF1","POF2","POF3","POF4","POF5","POF6","POF7","POF8","POF9","POF10")
above_result2 <- plyr::rbind.fill(corridor_culvert4[,columns], corridor_slope4[,columns], above_result[,columns])


# FMECA new--------------------------------------------------------------------------------------------------------

#fmeca1 <- read_excel(paste0(path_p, "/Pavement mastersheet.xlsx"), range = "A4:BP83" ,sheet = "fmeca")
fmeca1 <- read_excel(paste0(path_c, "/Corridor mastersheet.xlsx"), range = "AD14:AN25" ,sheet = "fmeca")
names(fmeca1)
fmeca2 <- fmeca1 %>% rename('FMECA_Asset Level 3' = 'Row Labels')
fmeca3 <- above_result2 %>% left_join(fmeca2, by = c("Asset_Type"="FMECA_Asset Level 3"))

# cols.num <- c(24,37:48) # mutate_at('ConOps_Score','Sum of R (Reliability)':'Sum of Total Consequence')
# fmeca3[cols.num] <- sapply(fmeca3[cols.num],as.numeric)
# sapply(fmeca3, class)

fmeca4 <- fmeca3 %>% 
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

# write results

openxlsx::write.xlsx(fmeca4, file = paste0(path_c, "/Corridor results.xlsx"))






# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ####################
# 
# ### culverts -------------------------------------------------------------------------
# 
# path_c <- "Newdata 2705/Corridors"
# corridor_culvert1 <- read_excel(paste0(path_c, "/Corridor mastersheet.xlsx"), range = "A1:BK9138" ,sheet = "original mastersheet")
# corridor_culvert2 <- corridor_culvert1 %>% dplyr::select('Asset Category',	Zone,	Latitude, Longitude, 'Asset_Type', 'Project Name', QuantityAsset, CURRENT_ZONE,
#                                                       'Asset Number',	'ROAD_NUMBER',	'CONSTRUCTION_YEAR', ARL, 'No. Pipes/Cells', 'Headwall / Wingwall Repairs', 
#                                                       'Clearing of Barrell', 'Clearing Inlet / Outlet', 'Pipe/Box Repairs', 'Joint Repairs',
#                                                       'FMECA_Code',	'FMECA_Asset Level 1',	'FMECA_Asset Level 2',	'FMECA_Asset Level 3',	'FMECA_Asset Level 4',	'FMECA_Rating',	
#                                                       'ConOps_Walk',	'ConOps_Cycle',	'ConOps_Transit',	'ConOps_Freight',	'ConOps_GV',	'ConOps_Score',	'ConOps_Rating') %>% 
#   dplyr::filter(Asset_Type == "Culvert") %>% 
#   mutate(CONSTRUCTION_YEAR = substring(CONSTRUCTION_YEAR,1,4))
# 
# # replace na with 0
# corridor_culvert2[, 14:18][is.na(corridor_culvert2[, 14:18])] <- 0 
# 
# corridor_culvert3 <- corridor_culvert2 %>% mutate(Repair.Total = dplyr::select(., 'Headwall / Wingwall Repairs':'Joint Repairs') %>% rowSums(na.rm = TRUE)) %>% 
#   mutate(Repair.Total = Repair.Total + 1) %>% 
#   mutate(Repair.Total = ifelse(Repair.Total > 5, 5, as.numeric(Repair.Total))) %>% # rating for repair full
#   mutate(CHI_reverse_LID = ifelse(is.na(ARL), 1, as.numeric(ARL)) * Repair.Total * 4)
# 
# 
# # Convert CHI to PoF
# Convert_NewValue = function(OldValue, OldRangeMax, OldRangeMin, NewRangeMax, NewRangeMin){
#   OldRange = (OldRangeMax - OldRangeMin)  
#   NewRange = (NewRangeMax - NewRangeMin) 
#   (((OldValue - OldRangeMax) * NewRange) / OldRange) + NewRangeMax
# }
# 
# Convert_NewValue(3,0,100,100,0)
# 
# # bring the data back
# corridor_culvert4 <- corridor_culvert3 %>% 
#   mutate(CHI_LID = Convert_NewValue(CHI_reverse_LID,0,100,100,0)) %>% # convert to PCI
#   mutate(PoF_CHI_LID = Convert_NewValue(CHI_LID,100,0,0,1))
# 
# write.csv(corridor_culvert4,"c-culvert with PoF.csv", row.names = F)
# 
# 
# ### slope --------------------------------------------------------------------------
# 
# path_c <- "Newdata 2705/Corridors"
# corridor_slope1 <- read_excel(paste0(path_c, "/Corridor mastersheet.xlsx"), range = "A1:BK9138" ,sheet = "original mastersheet")
# corridor_slope2 <- corridor_slope1 %>% dplyr::select('Asset Category',	Zone,	Latitude, Longitude, 'Asset_Type', 'Project Name', QuantityAsset, CURRENT_ZONE,
#                                                         'Asset Number',	'ROAD_NUMBER',	'CONSTRUCTION_YEAR', ARL, 
#                                                         MAX_HAZARD	MAX_MAGNITUDE	SLOPE_SCORE,
#                                                         'FMECA_Code',	'FMECA_Asset Level 1',	'FMECA_Asset Level 2',	'FMECA_Asset Level 3',	'FMECA_Asset Level 4',	'FMECA_Rating',	
#                                                         'ConOps_Walk',	'ConOps_Cycle',	'ConOps_Transit',	'ConOps_Freight',	'ConOps_GV',	'ConOps_Score',	'ConOps_Rating') %>% 
#   dplyr::filter(Asset_Type == "Slope") %>% 
#   mutate(CONSTRUCTION_YEAR = substring(CONSTRUCTION_YEAR,1,4))
# 
# # replace na with 0
# corridor_slope2[, 13:18][is.na(corridor_slope2[, 13:18])] <- 0 
# 
# corridor_slope3 <- corridor_slope2 %>% mutate(Repair.Total = dplyr::select(., 'No. Pipes/Cells':'Joint Repairs') %>% rowSums(na.rm = TRUE)) %>% 
#   mutate(Repair.Total = Repair.Total + 1) %>% 
#   mutate(Repair.Total = ifelse(Repair.Total > 5, 5, as.numeric(Repair.Total))) %>% # rating for repair full
#   mutate(CHI_reverse = ifelse(is.na(ARL), 1, as.numeric(ARL)) * Repair.Total * 4)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ####################
# 
# 
# # add inspection date and states 1:4
# 
# culvert_rev2 <- read_excel(paste0(path_c, "/Corridor mastersheet.xlsx"), range = "A1:H4227" ,sheet = "culvert inspections")
# culvert_rev2 <- culvert_rev2 %>% 
#   group_by(`Culvert Number`) %>% 
#   pivot_wider(names_from = `Inspection Number`,values_from = c(`Inspection Date`, `Risk Assessment Required`)) %>% 
#   dplyr::select(-c(`Inspection Date_NA`,`Risk Assessment Required_NA`)) %>% 
#   #mutate(ARL = ifelse(is.na(ARL), median.infill(ARL),ARL) %>% 
#   mutate(`Inspection Date_1` = substring(`Inspection Date_1`,1,4)) %>% # take the year alone
#   mutate(`Inspection Date_2` = substring(`Inspection Date_2`,1,4)) %>% 
#   mutate(`Inspection Date_3` = substring(`Inspection Date_3`,1,4)) %>% 
#   mutate(`Inspection Date_4` = substring(`Inspection Date_4`,1,4))
# 
# 
# culvert_inspection <- projects1_corridors %>% left_join(culvert_rev2, by = c("Asset Number"="Culvert Number"))
# 
# culvert_inspection %>% filter(ARL == 1) %>% mutate(Date1 = CONSTRUCTION_YEAR - 'Inspection Date_1')
#   
# 
# # merge with corridors condition data on BNO - Asset Number
# path_bc <- "Newdata 2705/Corridors/"
# assets1_corridors1 <- read.csv(paste0(path_bc, "Brdiges, Tunnels and corridor Sized Culverts Condition Rating.csv"))
# assets1_corridors1 <- assets1_corridors1 %>% dplyr::select(BNO, Proposed.Zone, Insp.Date, Element.Type,	Env,	Qty.Total,
#                                                     State.1,	State.2,	State.3,	State.4,	State.5) %>% 
#   filter(Proposed.Zone == "River Zone") %>% 
#   rename(ENV = Env)
# assets1_corridors2 <- read.csv(paste0(path_bc, "corridors with Outstanding Underwater Inspections.csv"))
# assets1_corridors2 <- assets1_corridors2 %>% dplyr::select(BNO, 'Proposed.Zone', 'Insp.Date', 'Element.Type',	'ENV',	'Qty.Total',
#                                               'State.1',	'State.2',	'State.3',	'State.4',	'State.5') %>% 
#   filter(Proposed.Zone == "River Zone")
# 
# assets1_corridors3 <- rbind(assets1_corridors1, assets1_corridors2) %>% 
#   mutate(BNO = as.character(BNO))
# 
# projects1_assets_corridors <- projects1_corridors %>% left_join(assets1_corridors3, by = c("Asset Number" = "BNO")) %>% 
#   filter(!is.na('Project Name'))
# 
# # add inspection year
# 
# projects1_assets_corridors$Insp.Year <- substring(projects1_assets_corridors$Insp.Date,7,10)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ##### -------------------------------------------------------------------------------------------
# 
# # corridor - MX
# 
# path <- "Asset port"
# srapc_corridor <- read_excel(paste0(path, "/SRAPC - Asset Number Overview (Version 2).xlsx"), range = "A2:AN3382" ,sheet = "corridors")
# names(srapc_corridor)
# srapc_corridor <- srapc_corridor %>% dplyr::select('Asset Class',	'Asset Category...2',	'Asset Type',	'Road Number',	'Unique ID',
#                                         Zone,	Latitude,	Longitude,	'Length (m)',	'Quantity...10', 'Diameter (m)',	'Height (m)',
#                                         'Condition - Meaning',	'Condition - Rating...15',	 'Construction Date', 	Age,
#                                         Asset_Walk,	Asset_Cycle, Asset_Transit, Asset_Freight,	Asset_GV,	Conops_Score,	Conops_Rank,
#                                         'Unique ID Description', 'Asset Type Description', 'FMECA Code', 'Component Code', 'Component', 'Sub-Component',
#                                         'Asset Type', C1,C2,C3,C4,C5)
# 
# 
# 
# 
# 
# srapc_corridor <- read_excel(paste0(path, "/SRAPC - Asset Number Overview (Version 2).xlsx"), range = "A2:AM3382" ,sheet = "corridors")
# srapc_corridor <- srapc_corridor %>% select('Asset Class',	'Asset Category...2',	'Asset Type...3',	'Road Number',	'Unique ID...5',
#                                         Zone,	Latitude,	Longitude,	'Length (m)',	'Quantity...10', 'Diameter (m)',	'Height (m)',
#                                         'Condition - Meaning',	'Condition - Rating...15',	 'Construction Date', 	Age,
#                                         Asset_Walk,	Asset_Cycle, Asset_Transit, Asset_Freight,	Asset_GV,	Conops_Score,	Conops_Rank,
#                                         C1,C2,C3,C4,C5)
# 
# 
# 
# corridor_rev2 <- read.csv(paste0(path, "/B_corridor_Rev_2.csv"), encoding = "UTF-8")
# corridor_rev2 <- corridor_rev2 %>% rename(OBJECTID = X.U.FEFF.OBJECTID) %>% 
#   dplyr::select(OBJECTID, LABEL, ASSET_TYPE, CONSTRUCTION_DATE, PROPOSED_ZONE,	
#                 BRDG_TYPE_DESC, OVERALL_LENGTH,	OVERALL_MIN_DECK_WIDTH, BHI_CODE,
#                 BHI_DESCR, LATITUDE, LONGITUDE, LIFE_SPAN, Asset_Walk,	Asset_Cycle,
#                 Asset_Transit, Asset_Freight,	Asset_GV,	Conops_Score,	Conops_Rank
#   ) %>% 
#   filter(PROPOSED_ZONE == "River Zone")
# 
# 
# ## corridor - culvert
# 
# culvert_rev2 <- read.csv(paste0(path, "/B_corridor_SIZE_CULVERT_Rev_2.csv"), encoding = "UTF-8")
# culvert_rev2 <- culvert_rev2 %>% rename(OBJECTID = X.U.FEFF.OBJECTID) %>% 
#   dplyr::select(OBJECTID, LABEL, ASSET_TYPE, CONSTRUCTION_DATE, PROPOSED_ZONE,	
#                 BRDG_TYPE_DESC, OVERALL_LENGTH,	OVERALL_MIN_DECK_WIDTH, BHI_CODE,
#                 BHI_DESCR, LATITUDE, LONGITUDE, LIFE_SPAN, Asset_Walk,	Asset_Cycle,
#                 Asset_Transit, Asset_Freight,	Asset_GV,	Conops_Score,	Conops_Rank
#   ) %>% 
#   filter(PROPOSED_ZONE == "River Zone")
# 
# 
# ## corridor - culvert
# 
# elec_mech_rev2 <- read.csv(paste0(path, "/B_ELECTRICAL_MECHANICAL_ASSETS_Rev_2.csv"), encoding = "UTF-8")
# elec_mech_rev2 <- elec_mech_rev2 %>% rename(OBJECTID = X.U.FEFF.OBJECTID) %>% 
#   dplyr::select(OBJECTID, LABEL, ASSET_TYPE, CONSTRUCTION_DATE, PROPOSED_ZONE,	
#                 BRDG_TYPE_DESC, OVERALL_LENGTH,	OVERALL_MIN_DECK_WIDTH, BHI_CODE,
#                 BHI_DESCR, LATITUDE, LONGITUDE, LIFE_SPAN, Asset_Walk,	Asset_Cycle,
#                 Asset_Transit, Asset_Freight,	Asset_GV,	Conops_Score,	Conops_Rank
#   ) %>% 
#   filter(PROPOSED_ZONE == "River Zone")
# 
# 
# 
# # merge all the above
# 
# corridor_assets <- rbind(corridor_rev2,culvert_rev2)
# 
# 
# 
# # Conditions ----------------------------------------------------------------------------------
# 
# ## corridor - Conditions 
# 
# corridor_conditions <- read.csv(paste0(path, "/Brdiges, Tunnels and corridor Sized Culverts Condition Rating.csv"))
# corridor_conditions <- corridor_conditions %>% 
#   rename(PROPOSED_ZONE = Proposed.Zone) %>% 
#   dplyr::select(BNO, Insp.Date, PROPOSED_ZONE, Element.Type,
#                 Env, Qty.Total, State.1, State.2, State.3, State.4
#   ) %>% 
#   filter(PROPOSED_ZONE == "River Zone")
# 
# ## corridor - Conditions
# 
# corridor_conditions2 <- read.csv(paste0(path, "/corridors with Outstanding Underwater Inspections.csv"))
# corridor_conditions2 <- corridor_conditions2 %>% 
#   rename(PROPOSED_ZONE = Proposed.Zone) %>%
#   rename(Env = ENV) %>%
#   dplyr::select(BNO, Insp.Date, PROPOSED_ZONE, Element.Type,
#                 Env, Qty.Total, State.1, State.2, State.3, State.4
#   ) %>% 
#   filter(PROPOSED_ZONE == "River Zone")
# 
# ## merge 
# 
# corridor_conditions_full <- rbind(corridor_conditions, corridor_conditions2)
# corridor_conditions_full$Insp.Year <- substring(corridor_conditions_full$Insp.Date,7,10)
# 
# 
# # join the above conditions with the actual asset base
# 
# corridor_conditions_assetbase <- corridor_conditions_full %>% left_join(corridor_assets, by = c("BNO"="LABEL"))
# 
# # Pavement
# 
# 
# 
# 
