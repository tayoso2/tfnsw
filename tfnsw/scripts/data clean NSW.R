# Title     : Data Clean NSW
# Objective : Clean NSW Datasets and process for modelling
# Created by: TOsosanya
# Created on: 02/06/2020

library(dplyr)
library(magrittr)
library(DescTools)
library(readxl)
library(xlsx)

## All assets - at least some

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path <- "Asset port"

assets1 <- read_excel(paste0(path, "/10.04.05 Tactical Works - RFT Addendum 3 (NF Aligned).xlsx"), range = "B20:AQ2471" ,sheet = "CALCS_Financial")
assets1 <- assets1 %>% select(Business_Unit,	Zone,	Latitude,	Longitude,	Asset_Level_1,	Asset_Level_2, Asset_Level_3,	
                              #Initiative,	Bucket,	
                              'Asset Number',	'Project Name',	Condition,	Project,	Phase,	'Task Description / Scope of work',
                              'FMECA Code',	'FMECA Asset Level 1',	'FMECA Asset Level 2',	'FMECA Asset Level 3',	'FMECA Asset Level 4',	'FMECA Rating',	
                              'ConOps Walk',	'ConOps Cycle',	'ConOps Transit',	'ConOps Freight',	'ConOps GV',	'ConOps Score',	'ConOps Rating'
                              )



# FMECA

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path <- "Asset port"

fmeca1 <- read_excel(paste0(path, "/10.04.05 Tactical Works - RFT Addendum 3 (NF Aligned).xlsx"), range = "A4:AJ227" ,sheet = "FMECA")
fmeca1 <- fmeca1 %>% select('Code#',	'Asset Group\r\n(level 1)',	'Asset Class\r\n(level 2)',	'Asset Type\r\n (level 3)',	'Asset Component\r\n (level 4)',
                            Likelihood, 	'Consequence...26', 	'R (Reliability)',	'A (Availability)',	'M (Maintainability)',	'S (Safety)',	'Se ( Security)',	'He (Health)',
                            'En (Environment)',	'Ec (Economics)',	'Risk Score\r\nValue',	'Risk Score'
                            )




# bridge



srapc_bridge <- read_excel(paste0(path, "/SRAPC - Asset Number Overview (Version 2).xlsx"), range = "A2:AM3382" ,sheet = "Bridges")
srapc_bridge <- srapc_bridge %>% select('Asset Class',	'Asset Category...2',	'Asset Type...3',	'Road Number',	'Unique ID...5',
                                        Zone,	Latitude,	Longitude,	'Length (m)',	'Quantity...10', 'Diameter (m)',	'Height (m)',
                                        'Condition - Meaning',	'Condition - Rating...15',	 'Construction Date', 	Age,
                                        Asset_Walk,	Asset_Cycle, Asset_Transit, Asset_Freight,	Asset_GV,	Conops_Score,	Conops_Rank,
                                        C1,C2,C3,C4,C5)



bridge_rev2 <- read.csv(paste0(path, "/B_BRIDGE_Rev_2.csv"), encoding = "UTF-8")
bridge_rev2 <- bridge_rev2 %>% rename(OBJECTID = X.U.FEFF.OBJECTID) %>% 
  dplyr::select(OBJECTID, LABEL, ASSET_TYPE, CONSTRUCTION_DATE, PROPOSED_ZONE,	
                BRDG_TYPE_DESC, OVERALL_LENGTH,	OVERALL_MIN_DECK_WIDTH, BHI_CODE,
                BHI_DESCR, LATITUDE, LONGITUDE, LIFE_SPAN, Asset_Walk,	Asset_Cycle,
                Asset_Transit, Asset_Freight,	Asset_GV,	Conops_Score,	Conops_Rank
  ) %>% 
  filter(PROPOSED_ZONE == "River Zone")


## Bridge - culvert

culvert_rev2 <- read.csv(paste0(path, "/B_BRIDGE_SIZE_CULVERT_Rev_2.csv"), encoding = "UTF-8")
culvert_rev2 <- culvert_rev2 %>% rename(OBJECTID = X.U.FEFF.OBJECTID) %>% 
  dplyr::select(OBJECTID, LABEL, ASSET_TYPE, CONSTRUCTION_DATE, PROPOSED_ZONE,	
                BRDG_TYPE_DESC, OVERALL_LENGTH,	OVERALL_MIN_DECK_WIDTH, BHI_CODE,
                BHI_DESCR, LATITUDE, LONGITUDE, LIFE_SPAN, Asset_Walk,	Asset_Cycle,
                Asset_Transit, Asset_Freight,	Asset_GV,	Conops_Score,	Conops_Rank
  ) %>% 
  filter(PROPOSED_ZONE == "River Zone")


## Bridge - culvert

elec_mech_rev2 <- read.csv(paste0(path, "/B_ELECTRICAL_MECHANICAL_ASSETS_Rev_2.csv"), encoding = "UTF-8")
elec_mech_rev2 <- elec_mech_rev2 %>% rename(OBJECTID = X.U.FEFF.OBJECTID) %>% 
  dplyr::select(OBJECTID, LABEL, ASSET_TYPE, CONSTRUCTION_DATE, PROPOSED_ZONE,	
                BRDG_TYPE_DESC, OVERALL_LENGTH,	OVERALL_MIN_DECK_WIDTH, BHI_CODE,
                BHI_DESCR, LATITUDE, LONGITUDE, LIFE_SPAN, Asset_Walk,	Asset_Cycle,
                Asset_Transit, Asset_Freight,	Asset_GV,	Conops_Score,	Conops_Rank
  ) %>% 
  filter(PROPOSED_ZONE == "River Zone")



# merge all the above

bridge_assets <- rbind(bridge_rev2,culvert_rev2)



# Conditions ----------------------------------------------------------------------------------

## Bridge - Conditions 

bridge_conditions <- read.csv(paste0(path, "/Brdiges, Tunnels and Bridge Sized Culverts Condition Rating.csv"))
bridge_conditions <- bridge_conditions %>% 
  rename(PROPOSED_ZONE = Proposed.Zone) %>% 
  dplyr::select(BNO, Insp.Date, PROPOSED_ZONE, Element.Type,
                Env, Qty.Total, State.1, State.2, State.3, State.4
  ) %>% 
  filter(PROPOSED_ZONE == "River Zone")

## Bridge - Conditions

bridge_conditions2 <- read.csv(paste0(path, "/Bridges with Outstanding Underwater Inspections.csv"))
bridge_conditions2 <- bridge_conditions2 %>% 
  rename(PROPOSED_ZONE = Proposed.Zone) %>%
  rename(Env = ENV) %>%
  dplyr::select(BNO, Insp.Date, PROPOSED_ZONE, Element.Type,
                Env, Qty.Total, State.1, State.2, State.3, State.4
  ) %>% 
  filter(PROPOSED_ZONE == "River Zone")

## merge 

bridge_conditions_full <- rbind(bridge_conditions, bridge_conditions2)
bridge_conditions_full$Insp.Year <- substring(bridge_conditions_full$Insp.Date,7,10)


# join the above conditions with the actual asset base

bridge_conditions_assetbase <- bridge_conditions_full %>% left_join(bridge_assets, by = c("BNO"="LABEL"))

# Pavement

# add inspection date and states 1:4

































###############################################





# read data

## Bridge - bridge

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path <- "Newdata 2705/Bridges"
bridge_rev2 <- read.csv(paste0(path, "/B_BRIDGE_Rev_2.csv"), encoding = "UTF-8")
bridge_rev2 <- bridge_rev2 %>% rename(OBJECTID = X.U.FEFF.OBJECTID) %>% 
  dplyr::select(OBJECTID, LABEL, ASSET_TYPE, CONSTRUCTION_DATE, PROPOSED_ZONE,	
                                      BRDG_TYPE_DESC, OVERALL_LENGTH,	OVERALL_MIN_DECK_WIDTH, BHI_CODE,
                                      BHI_DESCR, LATITUDE, LONGITUDE, LIFE_SPAN, Asset_Walk,	Asset_Cycle,
                                      Asset_Transit, Asset_Freight,	Asset_GV,	Conops_Score,	Conops_Rank
                                      ) %>% 
  filter(PROPOSED_ZONE == "River Zone")


## Bridge - culvert

culvert_rev2 <- read.csv(paste0(path, "/B_BRIDGE_SIZE_CULVERT_Rev_2.csv"), encoding = "UTF-8")
culvert_rev2 <- culvert_rev2 %>% rename(OBJECTID = X.U.FEFF.OBJECTID) %>% 
  dplyr::select(OBJECTID, LABEL, ASSET_TYPE, CONSTRUCTION_DATE, PROPOSED_ZONE,	
                BRDG_TYPE_DESC, OVERALL_LENGTH,	OVERALL_MIN_DECK_WIDTH, BHI_CODE,
                BHI_DESCR, LATITUDE, LONGITUDE, LIFE_SPAN, Asset_Walk,	Asset_Cycle,
                Asset_Transit, Asset_Freight,	Asset_GV,	Conops_Score,	Conops_Rank
  ) %>% 
  filter(PROPOSED_ZONE == "River Zone")


## Bridge - culvert

elec_mech_rev2 <- read.csv(paste0(path, "/B_ELECTRICAL_MECHANICAL_ASSETS_Rev_2.csv"), encoding = "UTF-8")
elec_mech_rev2 <- elec_mech_rev2 %>% rename(OBJECTID = X.U.FEFF.OBJECTID) %>% 
  dplyr::select(OBJECTID, LABEL, ASSET_TYPE, CONSTRUCTION_DATE, PROPOSED_ZONE,	
                BRDG_TYPE_DESC, OVERALL_LENGTH,	OVERALL_MIN_DECK_WIDTH, BHI_CODE,
                BHI_DESCR, LATITUDE, LONGITUDE, LIFE_SPAN, Asset_Walk,	Asset_Cycle,
                Asset_Transit, Asset_Freight,	Asset_GV,	Conops_Score,	Conops_Rank
  ) %>% 
  filter(PROPOSED_ZONE == "River Zone")



# merge all the above

bridge_assets <- rbind(bridge_rev2,culvert_rev2)



# Conditions ----------------------------------------------------------------------------------

## Bridge - Conditions 

bridge_conditions <- read.csv(paste0(path, "/Brdiges, Tunnels and Bridge Sized Culverts Condition Rating.csv"))
bridge_conditions <- bridge_conditions %>% 
  rename(PROPOSED_ZONE = Proposed.Zone) %>% 
  dplyr::select(BNO, Insp.Date, PROPOSED_ZONE, Element.Type,
                Env, Qty.Total, State.1, State.2, State.3, State.4
  ) %>% 
  filter(PROPOSED_ZONE == "River Zone")

## Bridge - Conditions

bridge_conditions2 <- read.csv(paste0(path, "/Bridges with Outstanding Underwater Inspections.csv"))
bridge_conditions2 <- bridge_conditions2 %>% 
  rename(PROPOSED_ZONE = Proposed.Zone) %>%
  rename(Env = ENV) %>%
  dplyr::select(BNO, Insp.Date, PROPOSED_ZONE, Element.Type,
                Env, Qty.Total, State.1, State.2, State.3, State.4
  ) %>% 
  filter(PROPOSED_ZONE == "River Zone")

## merge 

bridge_conditions_full <- rbind(bridge_conditions, bridge_conditions2)
bridge_conditions_full$Insp.Year <- substring(bridge_conditions_full$Insp.Date,7,10)


# join the above conditions with the actual asset base

bridge_conditions_assetbase <- bridge_conditions_full %>% left_join(bridge_assets, by = c("BNO"="LABEL"))

# Pavement

# add inspection date and states 1:4

