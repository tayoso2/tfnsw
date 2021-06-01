# Title     : Data Clean NSW - Pavement
# Objective : Clean NSW Datasets and process for modelling
# Created by: TOsosanya
# Created on: 02/06/2020

library(dplyr)
library(magrittr)
library(DescTools)
library(readxl)
library(tidyr)
library(ggplot2)
library(data.table)

rm(list=ls())

# All projects/assets - at least some. consists of unique id --------------------------------------------------

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("C:/Users/TOsosanya/Desktop/NSW/")
path <- "Asset port"

projects1 <- read_excel(paste0(path, "/10.04.05 Tactical Works - RFT Addendum 3 (NF Aligned).xlsx"), range = "B20:AQ2471" ,sheet = "CALCS_Financial")
names(projects1)
projects1 <- projects1 %>% dplyr::select(Business_Unit,	Zone,	Latitude,	Longitude,	Asset_Level_1,	Asset_Level_2, Asset_Level_3,	
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
  dplyr::select('Asset Category',	Zone,	WKT, 'Asset_Type', 'Project Name', QuantityAsset, CURRENT_ZONE,
         'Asset Number',	'ROAD_NUMBER',	'CONSTRUCTION_YEAR',	'RESURFACING_YEAR',	'PAVEMENT_TYPE_DESCRIPTION',
         SURFACE_TYPE,	AADT, SHAPE_Length,	WIDTH, 'PHI 2020',	'PHI Description', REHABILITATION_YEAR,
         'FMECA_Code',	'FMECA_Asset Level 1',	'FMECA_Asset Level 2',	'FMECA_Asset Level 3',	'FMECA_Asset Level 4',	'FMECA_Rating',	
         'ConOps_Walk',	'ConOps_Cycle',	'ConOps_Transit',	'ConOps_Freight',	'ConOps_GV',	'ConOps_Score',	'ConOps_Rating') %>% 
  dplyr::filter(Asset_Type == "Maintenance Segments")



# FMECA --------------------------------------------------------------------------------------------------------

# path <- "Asset port"
# names(fmeca1)
# fmeca1 <- read_excel(paste0(path, "/10.04.05 Tactical Works - RFT Addendum 3 (NF Aligned).xlsx"), range = "A4:AJ227" ,sheet = "FMECA")
# fmeca1 <- fmeca1 %>% dplyr::select('Code#',	'Asset Group\r\n(level 1)',	'Asset Class\r\n(level 2)',	'Asset Type\r\n (level 3)',	'Asset Component\r\n (level 4)',
#                             Likelihood, 	'Consequence...26', 	'R (Reliability)',	'A (Availability)',	'M (Maintainability)',	'S (Safety)',	'Se ( Security)',	'He (Health)',
#                             'En (Environment)',	'Ec (Economics)',	'Risk Score\r\nValue',	'Risk Score'
# )

# # Assets -------------------------------------------------------------------------------------------------------
# 
# path <- "Asset port"
# names(assets1)
# fmeca1 <- read_excel(paste0(path, "/10.04.05 Tactical Works - RFT Addendum 3 (NF Aligned).xlsx"), range = "A4:AJ227" ,sheet = "FMECA")
# fmeca1 <- fmeca1 %>% dplyr::select('Code#',	'Asset Group\r\n(level 1)',	'Asset Class\r\n(level 2)',	'Asset Type\r\n (level 3)',	'Asset Component\r\n (level 4)',
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

Convert_NewValue(70,100,50,0,1)

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
projects1_pavement$Age1 <- 2020 - (ifelse(projects1_pavement$RESURFACING_YEAR == 0 | is.na(projects1_pavement$RESURFACING_YEAR), Age1_median[[1]], projects1_pavement$RESURFACING_YEAR))
projects1_pavement$Age3 <- 2020 - (ifelse(projects1_pavement$REHABILITATION_YEAR == 0 | is.na(projects1_pavement$REHABILITATION_YEAR), Age3_median[[1]], projects1_pavement$REHABILITATION_YEAR)) #; 2020 - projects1_pavement$REHABILITATION_YEAR;
projects1_pavement$Age2 <- projects1_pavement$Age3
projects1_pavement$Age4 <- 2020 - projects1_pavement$'CONSTRUCTION_YEAR'

# take the last repair date
projects1_pavement3 <- projects1_pavement %>% dplyr::select(Area1, AssetID, Type, Highway, Section, Direction, AvailabilitySegment,
                                                     PavementClass, ELANES, LaneNo, Width, InstallationDate,DesignMaxLife, Cost,
                                                     Length, AADT, 'PHI 2020', REHABILITATION_YEAR, RESURFACING_YEAR) %>% 
  mutate(Repair = ifelse(REHABILITATION_YEAR < RESURFACING_YEAR | is.na(REHABILITATION_YEAR), RESURFACING_YEAR, REHABILITATION_YEAR)) 




# bring the data back
projects1_pavement4 <- projects1_pavement3 %>% 
  mutate(Date_diff_2020 = 2020 - Repair) %>% # Date diff from 2020 till last intervention date
  rename(PHI_2020 = 'PHI 2020') %>% 
  mutate(PHI_2020_new = ifelse(Date_diff_2020 > 0, PHI_2020 + ((m*Date_diff_2020) + c) , PHI_2020)) %>% # apply the flat deterioration
  mutate(PHI_2020_new = ifelse(PHI_2020_new >= 5, 5, as.numeric(PHI_2020_new))) %>% # cant be greater than 5
  mutate(PCI_2020 = Convert_NewValue(PHI_2020_new,0,5,100,0)) %>% # convert to PCI
  mutate(PCI_2020 = ifelse(is.na(PHI_2020_new), Convert_NewValue(PHI_2020,0,5,100,0),PCI_2020)) %>%
  mutate(PoF_2020 = Convert_NewValue(PCI_2020,100,50,0,1))

# output assetbase for EDA
projects1_pavement2 <- projects1_pavement %>% dplyr::select(Area1, AssetID, Type, Highway, Section, Direction, AvailabilitySegment,
                                                     PavementClass, ELANES, LaneNo, Width, InstallationDate,DesignMaxLife, Cost, Length, AADT, Age1, Age2, Age3, Age4)
projects1_pavement2 <- cbind(projects1_pavement2, PCI_2020 = 100) # >> changed
#write.csv(projects1_pavement2,"pavement_assets_Sc1.csv", row.names = F)

# output assetbase for EDA
# projects1_pavement2 <- projects1_pavement %>% dplyr::select(Area1, AssetID, Type, Highway, Section, Direction, AvailabilitySegment, PavementClass, ELANES, 
#                                                      LaneNo, Width, InstallationDate,DesignMaxLife, Cost, Length, AADT, Age1, Age2, Age3, Age4, ConOps_Rating)
# projects1_pavement2 <- cbind(projects1_pavement2, PCI_2020 = projects1_pavement4$PCI_2020, PoF_2020 = projects1_pavement4$PoF_2020,
#                              PHI_2020_new = projects1_pavement4$PHI_2020_new)
# write.csv(projects1_pavement2,"pavement_assets2_Sc1.csv", row.names = F)



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
npi_assets <- read.csv("NPI 0PCIstart Assets.csv")
npi_assets2 <- npi_assets %>% dplyr::select(AssetID,PCI0:PCI10) %>% 
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
p_fmeca_post_intervention <- fmeca4 %>% left_join(npi_assets[,pavement_det_cols], by = "AssetID")

# write results

# openxlsx::write.xlsx(fmeca5, file = paste0(path_p, "/Pavement results.xlsx"))

################ PLOT THE MAP

# library(sp)
# library(sf)
# library(raster)
# library(maptools)
# library(leaflet)
# library(tmap)
# library(rgdal)
# setwd("C:/Users/TOsosanya/Desktop/NSW/")
# path_p <- "Newdata 2705/Pavements"
# pavement_map <-  st_read(paste0(path_p, "/pavement map.csv"))
# pavement_map <- pavement_map[!st_is_empty(pavement_map),,drop=FALSE]
# mastersheet <- st_sf(pavement_map, crs = 4326)
# tmap_mode("view")
# qtm(na.omit(mastersheet), basemap = "Esri.WorldGrayCanvas") + tm_style("beaver")


# >> changed -----------------------------------------------------------------------



source("C:/Users/TOsosanya/Desktop/NSW/tfnsw/R_post_intervention/Pavements NPI.R")


p_fmeca_npi
p_fmeca_post_intervention

# import the project intervention spreadsheet and change project names 

scen_path <- "C:/Users/TOsosanya/Desktop/NSW/UVO/SRAPC V2 Model Files"
pi <- read_excel(paste0(scen_path, "/Sc 1 Loaded Plan.xlsx"), range = "A1:O676" ,sheet = "Sc 1 Loaded Plan (1)")


p_fmeca_post_intervention_pofrenamed <- p_fmeca_post_intervention %>% 
  rename(Sc_One_POF0 = POF0) %>% 
  rename(Sc_One_POF1 = POF1) %>% 
  rename(Sc_One_POF2 = POF2) %>% 
  rename(Sc_One_POF3 = POF3) %>% 
  rename(Sc_One_POF4 = POF4) %>% 
  rename(Sc_One_POF5 = POF5) %>% 
  rename(Sc_One_POF6 = POF6) %>% 
  rename(Sc_One_POF7 = POF7) %>% 
  rename(Sc_One_POF8 = POF8) %>% 
  rename(Sc_One_POF9 = POF9) %>% 
  rename(Sc_One_POF10 = POF10) 

columns_AN_POF <- c("POF0","POF1","POF2","POF3","POF4","POF5","POF6","POF7","POF8","POF9","POF10")

fmeca_pi_pofmerged <- cbind(p_fmeca_post_intervention_pofrenamed,p_fmeca_npi[,columns_AN_POF])

p_fmeca_post_intervention_edited <- fmeca_pi_pofmerged %>% 
  inner_join(pi[,c("Name","selection.date","selection.date.year")], by = c("Project Name"="Name")) 

# -----------------------------------------------------
test.output <-  NULL
p_fmeca_post_intervention_edited_split <- split(p_fmeca_post_intervention_edited,p_fmeca_post_intervention_edited$selection.date.year)
p_fmeca_post_intervention_edited_split1 <- p_fmeca_post_intervention_edited_split[[7]]

# create function
calc_det_after_intervention <- function(p_fmeca_post_intervention_edited_split){
  for(projects in 1:length(p_fmeca_post_intervention_edited_split)){
    test.output <- data.table(p_fmeca_post_intervention_edited_split[[projects]])
    m = unique(as.integer(test.output$selection.date.year))
    y = 10 - m;x = 10 - y
    b = x + 1;c = x + 2;d = x + 3;e = x + 4;f = x + 5
    g = x + 6;h = x + 7;i = x + 8;j = x + 9;k = x + 10
    test.output[,paste0("pof_new",m)] = test.output[,"Sc_One_POF0"]
    test.output[,paste0("pof_new",b)] = test.output[,"Sc_One_POF1"]
    test.output[,paste0("pof_new",c)] = test.output[,"Sc_One_POF2"]
    test.output[,paste0("pof_new",d)] = test.output[,"Sc_One_POF3"]
    test.output[,paste0("pof_new",e)] = test.output[,"Sc_One_POF4"]
    test.output[,paste0("pof_new",f)] = test.output[,"Sc_One_POF5"]
    test.output[,paste0("pof_new",g)] = test.output[,"Sc_One_POF6"]
    test.output[,paste0("pof_new",h)] = test.output[,"Sc_One_POF7"]
    test.output[,paste0("pof_new",i)] = test.output[,"Sc_One_POF8"]
    test.output[,paste0("pof_new",j)] = test.output[,"Sc_One_POF9"]
    test.output[,paste0("pof_new",k)] = test.output[,"Sc_One_POF10"]
    if (projects == 1) {
      output.final <- test.output
    } else{
      output.final <- plyr::rbind.fill(output.final, test.output)
    }
    
  }
  output.final2 <- output.final %>% 
    mutate(Sc_One_POF1 = ifelse(is.na(pof_new1),POF1,pof_new1),
           Sc_One_POF2 = ifelse(is.na(pof_new2),POF2,pof_new2),
           Sc_One_POF3 = ifelse(is.na(pof_new3),POF3,pof_new3),
           Sc_One_POF4 = ifelse(is.na(pof_new4),POF4,pof_new4),
           Sc_One_POF5 = ifelse(is.na(pof_new5),POF5,pof_new5),
           Sc_One_POF6 = ifelse(is.na(pof_new6),POF6,pof_new6),
           Sc_One_POF7 = ifelse(is.na(pof_new7),POF7,pof_new7),
           Sc_One_POF8 = ifelse(is.na(pof_new8),POF8,pof_new8),
           Sc_One_POF9 = ifelse(is.na(pof_new9),POF9,pof_new9),
           Sc_One_POF10 = ifelse(is.na(pof_new10),POF10,pof_new10)) %>% 
    dplyr::select(`AssetID`:SFC10) %>% as.data.table()
  return(output.final2)
}



pavement_post_int <- calc_det_after_intervention(p_fmeca_post_intervention_edited_split)


# testing

p_fmeca_post_intervention_edited %>% filter(`Project Name` == "M4 WESTERN MWY, GREYSTANES - R6004115002.000BU_17.557")
pavement_post_int %>% filter(`Project Name` == "M4 WESTERN MWY, GREYSTANES - R6004115002.000BU_17.557")

p_fmeca_post_intervention_edited %>% filter(selection.date.year == 3)
pavement_post_int %>% filter(`Project Name` == "WARREN RD, SMITHFIELD - R0013111500.161BU_9.400")

# write results

openxlsx::write.xlsx(pavement_post_int, file = paste0(path_p, "/Pavement results post_intervention.xlsx"))





