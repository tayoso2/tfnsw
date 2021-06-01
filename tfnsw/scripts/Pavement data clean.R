# Title     : Data Clean NSW - Pavement
# Objective : Clean NSW Datasets and process for modelling
# Created by: TOsosanya
# Created on: 02/06/2020

library(dplyr)
library(magrittr)
library(DescTools)
library(readxl)
library(tidyr)

# All projects/assets - at least some. consists of unique id --------------------------------------------------

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("C:/Users/TOsosanya/Desktop/NSW/")
path <- "Asset port"

projects1 <- read_excel(paste0(path, "/10.04.05 Tactical Works - RFT Addendum 3 (NF Aligned).xlsx"), range = "B20:AQ2471" ,sheet = "CALCS_Financial")
names(projects1)
projects1 <- projects1 %>% select(Business_Unit,	Zone,	Latitude,	Longitude,	Asset_Level_1,	Asset_Level_2, Asset_Level_3,	
                              #Initiative,	Bucket,	
                              'Asset Number',	'Project Name',	Condition,	Project,	Phase,	'Task Description / Scope of work',
                              'FMECA\r\nCode',	'FMECA\r\nAsset Level 1',	'FMECA\r\nAsset Level 2',	'FMECA\r\nAsset Level 3',	'FMECA\r\nAsset Level 4',	'FMECA\r\nRating',	
                              'ConOps\r\nWalk',	'ConOps\r\nCycle',	'ConOps\r\nTransit',	'ConOps\r\nFreight',	'ConOps\r\nGV',	'ConOps\r\nScore',	'ConOps\r\nRating'
                              )

## pavement projects

path_p <- "Newdata 2705/Pavements"
projects1_pavement <- read_excel(paste0(path_p, "/Pavement mastersheet.xlsx"), range = "A1:AM2440" ,sheet = "original mastersheet")
projects1_pavement <- projects1_pavement %>% 
  rename('Asset Number' = ObjectID) %>% 
  select('Asset Category',	Zone,	WKT, 'Asset_Type', 'Project Name', QuantityAsset, CURRENT_ZONE,
         'Asset Number',	'ROAD_NUMBER',	'CONSTRUCTION_YEAR',	'RESURFACING_YEAR',	'PAVEMENT_TYPE_DESCRIPTION',
         SURFACE_TYPE,	AADT, SHAPE_Length,	WIDTH, 'PHI 2020',	'PHI Description', REHABILITATION_YEAR,
         'FMECA_Code',	'FMECA_Asset Level 1',	'FMECA_Asset Level 2',	'FMECA_Asset Level 3',	'FMECA_Asset Level 4',	'FMECA_Rating',	
         'ConOps_Walk',	'ConOps_Cycle',	'ConOps_Transit',	'ConOps_Freight',	'ConOps_GV',	'ConOps_Score',	'ConOps_Rating') %>% 
  dplyr::filter(Asset_Type == "Maintenance Segments")



# FMECA --------------------------------------------------------------------------------------------------------

# path <- "Asset port"
# names(fmeca1)
# fmeca1 <- read_excel(paste0(path, "/10.04.05 Tactical Works - RFT Addendum 3 (NF Aligned).xlsx"), range = "A4:AJ227" ,sheet = "FMECA")
# fmeca1 <- fmeca1 %>% select('Code#',	'Asset Group\r\n(level 1)',	'Asset Class\r\n(level 2)',	'Asset Type\r\n (level 3)',	'Asset Component\r\n (level 4)',
#                             Likelihood, 	'Consequence...26', 	'R (Reliability)',	'A (Availability)',	'M (Maintainability)',	'S (Safety)',	'Se ( Security)',	'He (Health)',
#                             'En (Environment)',	'Ec (Economics)',	'Risk Score\r\nValue',	'Risk Score'
# )

# # Assets -------------------------------------------------------------------------------------------------------
# 
# path <- "Asset port"
# names(assets1)
# fmeca1 <- read_excel(paste0(path, "/10.04.05 Tactical Works - RFT Addendum 3 (NF Aligned).xlsx"), range = "A4:AJ227" ,sheet = "FMECA")
# fmeca1 <- fmeca1 %>% select('Code#',	'Asset Group\r\n(level 1)',	'Asset Class\r\n(level 2)',	'Asset Type\r\n (level 3)',	'Asset Component\r\n (level 4)',
#                             Likelihood, 	'Consequence...26', 	'R (Reliability)',	'A (Availability)',	'M (Maintainability)',	'S (Safety)',	'Se ( Security)',	'He (Health)',
#                             'En (Environment)',	'Ec (Economics)',	'Risk Score\r\nValue',	'Risk Score'
# )



# Functions

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

median.infill(projects1_pavement$'ConOps_Rating')

# infill <- function(x){
#   x <- ifelse(is.na(x) & (class(x) == "character" | class(x) == "logical"), mean(x, na.rm = TRUE), mode(x))
#   x <- as.integer(x)
# }

# pavements 
## get pavement asset base
# linear model - get the slope (m) # RUN WITHOUT MEDIAN INFILL OF PHI 2020
for_linear <- projects1_pavement %>%
  mutate(Repair = ifelse(REHABILITATION_YEAR < RESURFACING_YEAR, RESURFACING_YEAR, REHABILITATION_YEAR)) %>% 
  rename(InstallationDate = CONSTRUCTION_YEAR) %>%
  filter(Repair != 0) %>% mutate(Date_diff = Repair - InstallationDate) %>%
  filter(Date_diff != 0) %>%
  rename(PHI_2020 = 'PHI 2020') %>%
  filter(PHI_2020 != 0)
plot(for_linear$Date_diff, for_linear$PHI_2020)
summary(for_linear)
linearMod <- lm(PHI_2020 ~ Date_diff, data = for_linear)
summary(linearMod)
m = summary(linearMod)$coefficients[2,1]
c = summary(linearMod)$coefficients[1,1]

for_linear <- as.data.frame(for_linear)

ggplot(for_linear, aes(Date_diff, PHI_2020)) + 
  geom_jitter(aes(), col = "blue") +
  geom_smooth(aes(),col = "red", method = "lm") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "bottom")

# prepare asset base
projects1_pavement <- projects1_pavement %>% filter(Asset_Type == "Maintenance Segments") #%>% filter(!is.na(`ConOps_Score`))
projects1_pavement$Area1 <- "TfNSW"
projects1_pavement$AssetID <- projects1_pavement$'Asset Number'; unique(projects1_pavement$'Asset Number')
projects1_pavement$Type <-  'Pavement' # projects1_pavement$'FMECA_Asset Level 2'
projects1_pavement$Highway <- "All"
projects1_pavement$Section <- projects1_pavement$'CURRENT_ZONE'; projects1_pavement$Section <- mode.infill(projects1_pavement$Section); unique(projects1_pavement$Section)
projects1_pavement$Direction <-  "All"
projects1_pavement$AvailabilitySegment <- "All"
projects1_pavement$PavementClass <- "Flexible"
projects1_pavement$ELANES <-  6
projects1_pavement$LaneNo <- 3
projects1_pavement$Width <- projects1_pavement$'WIDTH'; projects1_pavement$Width <- median.infill(projects1_pavement$Width); unique(projects1_pavement$Width)
projects1_pavement$InstallationDate <- projects1_pavement$'CONSTRUCTION_YEAR'; median.infill(projects1_pavement$InstallationDate); summary(projects1_pavement$InstallationDate)
projects1_pavement$DesignMaxLife <- 30
projects1_pavement$Cost <- 1
projects1_pavement$Length <- projects1_pavement$'QuantityAsset'; projects1_pavement$Length <- median.infill(projects1_pavement$Length); unique(projects1_pavement$Length)
projects1_pavement$AADT <- projects1_pavement$'AADT'; projects1_pavement$AADT <- median.infill(projects1_pavement$AADT); summary(projects1_pavement$AADT)
projects1_pavement$'PHI 2020' <- median.infill(projects1_pavement$'PHI 2020')
# for age  ...
Age1_median <- projects1_pavement %>% filter(RESURFACING_YEAR != 0) %>% summarise(summarise = median(RESURFACING_YEAR,na.rm =T))
Age3_median <- projects1_pavement %>% filter(REHABILITATION_YEAR != 0) %>% summarise(summarise = median(REHABILITATION_YEAR,na.rm =T))
projects1_pavement$Age1 <- 2019 - (ifelse(projects1_pavement$RESURFACING_YEAR == 0 | is.na(projects1_pavement$RESURFACING_YEAR), Age1_median[[1]], projects1_pavement$RESURFACING_YEAR))
projects1_pavement$Age3 <- 2019 - (ifelse(projects1_pavement$REHABILITATION_YEAR == 0 | is.na(projects1_pavement$REHABILITATION_YEAR), Age3_median[[1]], projects1_pavement$REHABILITATION_YEAR)) #; 2019 - projects1_pavement$REHABILITATION_YEAR;
projects1_pavement$Age2 <- projects1_pavement$Age3
projects1_pavement$Age4 <- 2019 - projects1_pavement$'CONSTRUCTION_YEAR'

# take the last repair date
projects1_pavement3 <- projects1_pavement %>% select(Area1, AssetID, Type, Highway, Section, Direction, AvailabilitySegment,
                                                     PavementClass, ELANES, LaneNo, Width, InstallationDate,DesignMaxLife, Cost,
                                                     Length, AADT, 'PHI 2020', REHABILITATION_YEAR, RESURFACING_YEAR) %>% 
  mutate(Repair = ifelse(REHABILITATION_YEAR < RESURFACING_YEAR | is.na(REHABILITATION_YEAR), RESURFACING_YEAR, REHABILITATION_YEAR)) 




# bring the data back
projects1_pavement4 <- projects1_pavement3 %>% 
  mutate(Date_diff_2019 = 2019 - Repair) %>% # Date diff from 2019 till last intervention date
  rename(PHI_2020 = 'PHI 2020') %>% 
  mutate(PHI_2020_new = ifelse(Date_diff_2019 > 0, PHI_2020 + ((m*Date_diff_2019) + c) , PHI_2020)) %>% # apply the flat deterioration
  mutate(PHI_2020_new = ifelse(PHI_2020_new >= 5, 5, as.numeric(PHI_2020_new))) %>% # cant be greater than 5
  mutate(PCI_2020 = Convert_NewValue(PHI_2020_new,0,5,100,0)) %>% # convert to PCI
  mutate(PCI_2020 = ifelse(is.na(PHI_2020_new), Convert_NewValue(PHI_2020,0,5,100,0),PCI_2020)) %>%
  mutate(PoF_2020 = Convert_NewValue(PCI_2020,100,50,0,1))

# output assetbase for EDA
projects1_pavement2 <- projects1_pavement %>% select(Area1, AssetID, Type, Highway, Section, Direction, AvailabilitySegment,
                                                     PavementClass, ELANES, LaneNo, Width, InstallationDate,DesignMaxLife, Cost, Length, AADT, Age1, Age2, Age3, Age4)
projects1_pavement2 <- cbind(projects1_pavement2, PCI_2020 = projects1_pavement4$PCI_2020)
write.csv(projects1_pavement2,"pavement_assets.csv", row.names = F)



## Operation Get AADT 

# aadt1 <- read_excel(paste0(path, "/10.04.05 Tactical Works - RFT Addendum 3 (NF Aligned).xlsx"),sheet = "lookups") %>% 
#   filter(PROPOSED_Z == "River Zone")
# aadt2 <- read_excel(paste0(path, "/10.04.05 Tactical Works - RFT Addendum 3 (NF Aligned).xlsx"),sheet = "lookups2") %>% 
#   filter(PROPOSED_ZONE == "River Zone")
# aadt3 <- read_excel(paste0(path, "/10.04.05 Tactical Works - RFT Addendum 3 (NF Aligned).xlsx"),sheet = "lookups3") 
# aadt <- aadt3 %>% left_join(aadt2, by = c("Culvert Number"= "LABEL")) %>% 
#   left_join(aadt1, by = c("ROAD_NUMBER"="ROAD_NUMBE"))
# 
# write.csv(aadt, "aadt and uid.csv")




# read results from EDA ----------------------------------------------------------------------------------

column_list <- c("Asset Number","Project Name","FMECA_Asset Level 1", "FMECA_Asset Level 2", "FMECA_Asset Level 3", "FMECA_Asset Level 4", 
                 "FMECA_Rating", "ConOps_Walk", "ConOps_Cycle", "ConOps_Transit", "ConOps_Freight", "ConOps_GV", "ConOps_Score", "ConOps_Rating")
npi_assets <- read.csv("NPI Assets.csv")
npi_assets2 <- npi_assets %>% select(AssetID,PCI0:PCI10) %>% 
  left_join(projects1_pavement[,column_list], by = c("AssetID" = 'Asset Number'))

# create new columns for POF over an 11 year period. Note: POF1 is for 2020
npi_assets3 <- cbind(npi_assets2,POF0=0,POF1=0,POF2=0,POF3=0,POF4=0,POF5=0,POF6=0,POF7=0,POF8=0,POF9=0,POF10=0)
npi_assets3[,26:36] <- Convert_NewValue(npi_assets2[,2:12],100,50,0,1)



# insert fmeca,conops --------------------------------------------------------------------------------------

# FMECA new--------------------------------------------------------------------------------------------------------

#fmeca1 <- read_excel(paste0(path_p, "/Pavement mastersheet.xlsx"), range = "A4:BP83" ,sheet = "fmeca")
fmeca1 <- read_excel(paste0(path_p, "/Pavement mastersheet.xlsx"), range = "AD14:AN22" ,sheet = "objectid is asset number")
names(fmeca1)
fmeca2 <- fmeca1 %>% rename('FMECA_Asset Level 3' = 'Row Labels')
fmeca3 <- npi_assets3 %>% left_join(fmeca2, by = "FMECA_Asset Level 3")

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

# Add the rutting RDM, ravelling ARV, roughness IRI, skid resistance SFC. Cracking ACA, could not be added.. its flat

pavement_det_cols <- c("AssetID", "RDM0","RDM1","RDM2","RDM3","RDM4","RDM5","RDM6","RDM7","RDM8","RDM9","RDM10",
                       "ARV0","ARV1","ARV2","ARV3","ARV4","ARV5","ARV6","ARV7","ARV8","ARV9","ARV10",
                       "IRI0","IRI1","IRI2","IRI3","IRI4","IRI5","IRI6","IRI7","IRI8","IRI9","IRI10",
                       "SFC0","SFC1","SFC2","SFC3","SFC4","SFC5","SFC6","SFC7","SFC8","SFC9","SFC10")
fmeca5 <- fmeca4 %>% left_join(npi_assets[,pavement_det_cols], by = "AssetID")
  
# write results

openxlsx::write.xlsx(fmeca5, file = paste0(path_p, "/Pavement results.xlsx"))

################




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









# 
# 
# 
# 
# 
# 
# 
# 
# #########################
# 
# 
# 
# # bridge - MX
# 
# path <- "Asset port"
# srapc_bridge <- read_excel(paste0(path, "/SRAPC - Asset Number Overview (Version 2).xlsx"), range = "A2:AN3382" ,sheet = "Bridges")
# names(srapc_bridge)
# srapc_bridge <- srapc_bridge %>% select('Asset Class',	'Asset Category...2',	'Asset Type',	'Road Number',	'Unique ID',
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
# srapc_bridge <- read_excel(paste0(path, "/SRAPC - Asset Number Overview (Version 2).xlsx"), range = "A2:AM3382" ,sheet = "Bridges")
# srapc_bridge <- srapc_bridge %>% select('Asset Class',	'Asset Category...2',	'Asset Type...3',	'Road Number',	'Unique ID...5',
#                                         Zone,	Latitude,	Longitude,	'Length (m)',	'Quantity...10', 'Diameter (m)',	'Height (m)',
#                                         'Condition - Meaning',	'Condition - Rating...15',	 'Construction Date', 	Age,
#                                         Asset_Walk,	Asset_Cycle, Asset_Transit, Asset_Freight,	Asset_GV,	Conops_Score,	Conops_Rank,
#                                         C1,C2,C3,C4,C5)
# 
# 
# 
# bridge_rev2 <- read.csv(paste0(path, "/B_BRIDGE_Rev_2.csv"), encoding = "UTF-8")
# bridge_rev2 <- bridge_rev2 %>% rename(OBJECTID = X.U.FEFF.OBJECTID) %>% 
#   dplyr::select(OBJECTID, LABEL, ASSET_TYPE, CONSTRUCTION_DATE, PROPOSED_ZONE,	
#                 BRDG_TYPE_DESC, OVERALL_LENGTH,	OVERALL_MIN_DECK_WIDTH, BHI_CODE,
#                 BHI_DESCR, LATITUDE, LONGITUDE, LIFE_SPAN, Asset_Walk,	Asset_Cycle,
#                 Asset_Transit, Asset_Freight,	Asset_GV,	Conops_Score,	Conops_Rank
#   ) %>% 
#   filter(PROPOSED_ZONE == "River Zone")
# 
# 
# ## Bridge - culvert
# 
# culvert_rev2 <- read.csv(paste0(path, "/B_BRIDGE_SIZE_CULVERT_Rev_2.csv"), encoding = "UTF-8")
# culvert_rev2 <- culvert_rev2 %>% rename(OBJECTID = X.U.FEFF.OBJECTID) %>% 
#   dplyr::select(OBJECTID, LABEL, ASSET_TYPE, CONSTRUCTION_DATE, PROPOSED_ZONE,	
#                 BRDG_TYPE_DESC, OVERALL_LENGTH,	OVERALL_MIN_DECK_WIDTH, BHI_CODE,
#                 BHI_DESCR, LATITUDE, LONGITUDE, LIFE_SPAN, Asset_Walk,	Asset_Cycle,
#                 Asset_Transit, Asset_Freight,	Asset_GV,	Conops_Score,	Conops_Rank
#   ) %>% 
#   filter(PROPOSED_ZONE == "River Zone")
# 
# 
# ## Bridge - culvert
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
# bridge_assets <- rbind(bridge_rev2,culvert_rev2)
# 
# 
# 
# # Conditions ----------------------------------------------------------------------------------
# 
# ## Bridge - Conditions 
# 
# bridge_conditions <- read.csv(paste0(path, "/Brdiges, Tunnels and Bridge Sized Culverts Condition Rating.csv"))
# bridge_conditions <- bridge_conditions %>% 
#   rename(PROPOSED_ZONE = Proposed.Zone) %>% 
#   dplyr::select(BNO, Insp.Date, PROPOSED_ZONE, Element.Type,
#                 Env, Qty.Total, State.1, State.2, State.3, State.4
#   ) %>% 
#   filter(PROPOSED_ZONE == "River Zone")
# 
# ## Bridge - Conditions
# 
# bridge_conditions2 <- read.csv(paste0(path, "/Bridges with Outstanding Underwater Inspections.csv"))
# bridge_conditions2 <- bridge_conditions2 %>% 
#   rename(PROPOSED_ZONE = Proposed.Zone) %>%
#   rename(Env = ENV) %>%
#   dplyr::select(BNO, Insp.Date, PROPOSED_ZONE, Element.Type,
#                 Env, Qty.Total, State.1, State.2, State.3, State.4
#   ) %>% 
#   filter(PROPOSED_ZONE == "River Zone")
# 
# ## merge 
# 
# bridge_conditions_full <- rbind(bridge_conditions, bridge_conditions2)
# bridge_conditions_full$Insp.Year <- substring(bridge_conditions_full$Insp.Date,7,10)
# 
# 
# # join the above conditions with the actual asset base
# 
# bridge_conditions_assetbase <- bridge_conditions_full %>% left_join(bridge_assets, by = c("BNO"="LABEL"))
# 
# # Pavement
# 
# # add inspection date and states 1:4
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
# ###############################################
# 
# 
# 
# 
# 
# # read data
# 
# ## Bridge - bridge
# 
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# path <- "Newdata 2705/Bridges"
# bridge_rev2 <- read.csv(paste0(path, "/B_BRIDGE_Rev_2.csv"), encoding = "UTF-8")
# bridge_rev2 <- bridge_rev2 %>% rename(OBJECTID = X.U.FEFF.OBJECTID) %>% 
#   dplyr::select(OBJECTID, LABEL, ASSET_TYPE, CONSTRUCTION_DATE, PROPOSED_ZONE,	
#                                       BRDG_TYPE_DESC, OVERALL_LENGTH,	OVERALL_MIN_DECK_WIDTH, BHI_CODE,
#                                       BHI_DESCR, LATITUDE, LONGITUDE, LIFE_SPAN, Asset_Walk,	Asset_Cycle,
#                                       Asset_Transit, Asset_Freight,	Asset_GV,	Conops_Score,	Conops_Rank
#                                       ) %>% 
#   filter(PROPOSED_ZONE == "River Zone")
# 
# 
# ## Bridge - culvert
# 
# culvert_rev2 <- read.csv(paste0(path, "/B_BRIDGE_SIZE_CULVERT_Rev_2.csv"), encoding = "UTF-8")
# culvert_rev2 <- culvert_rev2 %>% rename(OBJECTID = X.U.FEFF.OBJECTID) %>% 
#   dplyr::select(OBJECTID, LABEL, ASSET_TYPE, CONSTRUCTION_DATE, PROPOSED_ZONE,	
#                 BRDG_TYPE_DESC, OVERALL_LENGTH,	OVERALL_MIN_DECK_WIDTH, BHI_CODE,
#                 BHI_DESCR, LATITUDE, LONGITUDE, LIFE_SPAN, Asset_Walk,	Asset_Cycle,
#                 Asset_Transit, Asset_Freight,	Asset_GV,	Conops_Score,	Conops_Rank
#   ) %>% 
#   filter(PROPOSED_ZONE == "River Zone")
# 
# 
# ## Bridge - culvert
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
# bridge_assets <- rbind(bridge_rev2,culvert_rev2)
# 
# 
# 
# # Conditions ----------------------------------------------------------------------------------
# 
# ## Bridge - Conditions 
# 
# bridge_conditions <- read.csv(paste0(path, "/Brdiges, Tunnels and Bridge Sized Culverts Condition Rating.csv"))
# bridge_conditions <- bridge_conditions %>% 
#   rename(PROPOSED_ZONE = Proposed.Zone) %>% 
#   dplyr::select(BNO, Insp.Date, PROPOSED_ZONE, Element.Type,
#                 Env, Qty.Total, State.1, State.2, State.3, State.4
#   ) %>% 
#   filter(PROPOSED_ZONE == "River Zone")
# 
# ## Bridge - Conditions
# 
# bridge_conditions2 <- read.csv(paste0(path, "/Bridges with Outstanding Underwater Inspections.csv"))
# bridge_conditions2 <- bridge_conditions2 %>% 
#   rename(PROPOSED_ZONE = Proposed.Zone) %>%
#   rename(Env = ENV) %>%
#   dplyr::select(BNO, Insp.Date, PROPOSED_ZONE, Element.Type,
#                 Env, Qty.Total, State.1, State.2, State.3, State.4
#   ) %>% 
#   filter(PROPOSED_ZONE == "River Zone")
# 
# ## merge 
# 
# bridge_conditions_full <- rbind(bridge_conditions, bridge_conditions2)
# bridge_conditions_full$Insp.Year <- substring(bridge_conditions_full$Insp.Date,7,10)
# 
# 
# # join the above conditions with the actual asset base
# 
# bridge_conditions_assetbase <- bridge_conditions_full %>% left_join(bridge_assets, by = c("BNO"="LABEL"))
# 
# # Pavement
# 
# # add inspection date and states 1:4
# 
