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


# import the project intervention spreadsheet and change project names 

health_path <- "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Pavements"
pavement_health_ni<- read_excel(paste0(health_path, "/Pavement Health Sheet.xlsx"), range = "A1:BQ1596" ,sheet = "No Investment")
pavement_health_sc_one<- read_excel(paste0(health_path, "/Pavement Health Sheet.xlsx"), range = "A1:AT1596" ,sheet = "Scenario 1")

p_post_intervention_pcirenamed <- pavement_health_sc_one %>% 
  rename(Sc_One_PCI0 = PCI0) %>% 
  rename(Sc_One_PCI1 = PCI1) %>% 
  rename(Sc_One_PCI2 = PCI2) %>% 
  rename(Sc_One_PCI3 = PCI3) %>% 
  rename(Sc_One_PCI4 = PCI4) %>% 
  rename(Sc_One_PCI5 = PCI5) %>% 
  rename(Sc_One_PCI6 = PCI6) %>% 
  rename(Sc_One_PCI7 = PCI7) %>% 
  rename(Sc_One_PCI8 = PCI8) %>% 
  rename(Sc_One_PCI9 = PCI9) %>% 
  rename(Sc_One_PCI10 = PCI10) %>%
  rename(Sc_One_RDM0 = RDM0) %>% 
  rename(Sc_One_RDM1 = RDM1) %>% 
  rename(Sc_One_RDM2 = RDM2) %>% 
  rename(Sc_One_RDM3 = RDM3) %>% 
  rename(Sc_One_RDM4 = RDM4) %>% 
  rename(Sc_One_RDM5 = RDM5) %>% 
  rename(Sc_One_RDM6 = RDM6) %>% 
  rename(Sc_One_RDM7 = RDM7) %>% 
  rename(Sc_One_RDM8 = RDM8) %>% 
  rename(Sc_One_RDM9 = RDM9) %>% 
  rename(Sc_One_RDM10 = RDM10) %>%
  rename(Sc_One_ARV0 = ARV0) %>% 
  rename(Sc_One_ARV1 = ARV1) %>% 
  rename(Sc_One_ARV2 = ARV2) %>% 
  rename(Sc_One_ARV3 = ARV3) %>% 
  rename(Sc_One_ARV4 = ARV4) %>% 
  rename(Sc_One_ARV5 = ARV5) %>% 
  rename(Sc_One_ARV6 = ARV6) %>% 
  rename(Sc_One_ARV7 = ARV7) %>% 
  rename(Sc_One_ARV8 = ARV8) %>% 
  rename(Sc_One_ARV9 = ARV9) %>% 
  rename(Sc_One_ARV10 = ARV10) %>%
  rename(Sc_One_IRI0 = IRI0) %>% 
  rename(Sc_One_IRI1 = IRI1) %>% 
  rename(Sc_One_IRI2 = IRI2) %>% 
  rename(Sc_One_IRI3 = IRI3) %>% 
  rename(Sc_One_IRI4 = IRI4) %>% 
  rename(Sc_One_IRI5 = IRI5) %>% 
  rename(Sc_One_IRI6 = IRI6) %>% 
  rename(Sc_One_IRI7 = IRI7) %>% 
  rename(Sc_One_IRI8 = IRI8) %>% 
  rename(Sc_One_IRI9 = IRI9) %>% 
  rename(Sc_One_IRI10 = IRI10)

scen_path <- "C:/Users/TOsosanya/Desktop/NSW/UVO/SRAPC V2 Model Files"
pi <- read_excel(paste0(scen_path, "/Sc 1 Loaded Plan.xlsx"), range = "A1:O676" ,sheet = "Sc 1 Loaded Plan (1)")


columns_AN_pci <- c("PCI0","PCI1","PCI2","PCI3","PCI4","PCI5","PCI6","PCI7","PCI8","PCI9","PCI10",
                    "RDM0","RDM1","RDM2","RDM3","RDM4","RDM5","RDM6","RDM7","RDM8","RDM9","RDM10",
                    "ARV0","ARV1","ARV2","ARV3","ARV4","ARV5","ARV6","ARV7","ARV8","ARV9","ARV10",
                    "IRI0","IRI1","IRI2","IRI3","IRI4","IRI5","IRI6","IRI7","IRI8","IRI9","IRI10")

pi_pcimerged <- cbind(p_post_intervention_pcirenamed,pavement_health_ni[,columns_AN_pci])

p_post_intervention_edited <- pi_pcimerged %>% 
  inner_join(pi[,c("Name","selection.date","selection.date.year")], by = c("Project Name"="Name")) 

# -----------------------------------------------------
test.output <-  NULL
p_post_intervention_edited_split <- split(p_post_intervention_edited,p_post_intervention_edited$selection.date.year)
p_post_intervention_edited_split1 <- p_post_intervention_edited_split[[7]]

# create function
calc_pav_det_after_intervention <- function(p_fmeca_post_intervention_edited_split){
  for(projects in 1:length(p_fmeca_post_intervention_edited_split)){
    test.output <- data.table(p_fmeca_post_intervention_edited_split[[projects]])
    m = unique(as.integer(test.output$selection.date.year))
    y = 10 - m;x = 10 - y
    b = x + 1;c = x + 2;d = x + 3;e = x + 4;f = x + 5
    g = x + 6;h = x + 7;i = x + 8;j = x + 9;k = x + 10
    test.output[,paste0("pci_new",m)] = test.output[,"Sc_One_PCI0"]
    test.output[,paste0("pci_new",b)] = test.output[,"Sc_One_PCI1"]
    test.output[,paste0("pci_new",c)] = test.output[,"Sc_One_PCI2"]
    test.output[,paste0("pci_new",d)] = test.output[,"Sc_One_PCI3"]
    test.output[,paste0("pci_new",e)] = test.output[,"Sc_One_PCI4"]
    test.output[,paste0("pci_new",f)] = test.output[,"Sc_One_PCI5"]
    test.output[,paste0("pci_new",g)] = test.output[,"Sc_One_PCI6"]
    test.output[,paste0("pci_new",h)] = test.output[,"Sc_One_PCI7"]
    test.output[,paste0("pci_new",i)] = test.output[,"Sc_One_PCI8"]
    test.output[,paste0("pci_new",j)] = test.output[,"Sc_One_PCI9"]
    test.output[,paste0("pci_new",k)] = test.output[,"Sc_One_PCI10"]
    
    test.output[,paste0("ARV_new",m)] = test.output[,"Sc_One_ARV0"]
    test.output[,paste0("ARV_new",b)] = test.output[,"Sc_One_ARV1"]
    test.output[,paste0("ARV_new",c)] = test.output[,"Sc_One_ARV2"]
    test.output[,paste0("ARV_new",d)] = test.output[,"Sc_One_ARV3"]
    test.output[,paste0("ARV_new",e)] = test.output[,"Sc_One_ARV4"]
    test.output[,paste0("ARV_new",f)] = test.output[,"Sc_One_ARV5"]
    test.output[,paste0("ARV_new",g)] = test.output[,"Sc_One_ARV6"]
    test.output[,paste0("ARV_new",h)] = test.output[,"Sc_One_ARV7"]
    test.output[,paste0("ARV_new",i)] = test.output[,"Sc_One_ARV8"]
    test.output[,paste0("ARV_new",j)] = test.output[,"Sc_One_ARV9"]
    test.output[,paste0("ARV_new",k)] = test.output[,"Sc_One_ARV10"]
    
    test.output[,paste0("RDM_new",m)] = test.output[,"Sc_One_RDM0"]
    test.output[,paste0("RDM_new",b)] = test.output[,"Sc_One_RDM1"]
    test.output[,paste0("RDM_new",c)] = test.output[,"Sc_One_RDM2"]
    test.output[,paste0("RDM_new",d)] = test.output[,"Sc_One_RDM3"]
    test.output[,paste0("RDM_new",e)] = test.output[,"Sc_One_RDM4"]
    test.output[,paste0("RDM_new",f)] = test.output[,"Sc_One_RDM5"]
    test.output[,paste0("RDM_new",g)] = test.output[,"Sc_One_RDM6"]
    test.output[,paste0("RDM_new",h)] = test.output[,"Sc_One_RDM7"]
    test.output[,paste0("RDM_new",i)] = test.output[,"Sc_One_RDM8"]
    test.output[,paste0("RDM_new",j)] = test.output[,"Sc_One_RDM9"]
    test.output[,paste0("RDM_new",k)] = test.output[,"Sc_One_RDM10"]
    
    test.output[,paste0("IRI_new",m)] = test.output[,"Sc_One_IRI0"]
    test.output[,paste0("IRI_new",b)] = test.output[,"Sc_One_IRI1"]
    test.output[,paste0("IRI_new",c)] = test.output[,"Sc_One_IRI2"]
    test.output[,paste0("IRI_new",d)] = test.output[,"Sc_One_IRI3"]
    test.output[,paste0("IRI_new",e)] = test.output[,"Sc_One_IRI4"]
    test.output[,paste0("IRI_new",f)] = test.output[,"Sc_One_IRI5"]
    test.output[,paste0("IRI_new",g)] = test.output[,"Sc_One_IRI6"]
    test.output[,paste0("IRI_new",h)] = test.output[,"Sc_One_IRI7"]
    test.output[,paste0("IRI_new",i)] = test.output[,"Sc_One_IRI8"]
    test.output[,paste0("IRI_new",j)] = test.output[,"Sc_One_IRI9"]
    test.output[,paste0("IRI_new",k)] = test.output[,"Sc_One_IRI10"]
    if (projects == 1) {
      output.final <- test.output
    } else{
      output.final <- plyr::rbind.fill(output.final, test.output)
    }
    
  }
  output.final2 <- output.final %>% 
    mutate(PCI1 = ifelse(is.na(pci_new1),PCI1,pci_new1),
           PCI2 = ifelse(is.na(pci_new2),PCI2,pci_new2),
           PCI3 = ifelse(is.na(pci_new3),PCI3,pci_new3),
           PCI4 = ifelse(is.na(pci_new4),PCI4,pci_new4),
           PCI5 = ifelse(is.na(pci_new5),PCI5,pci_new5),
           PCI6 = ifelse(is.na(pci_new6),PCI6,pci_new6),
           PCI7 = ifelse(is.na(pci_new7),PCI7,pci_new7),
           PCI8 = ifelse(is.na(pci_new8),PCI8,pci_new8),
           PCI9 = ifelse(is.na(pci_new9),PCI9,pci_new9),
           PCI10 = ifelse(is.na(pci_new10),PCI10,pci_new10),
           
           ARV1 = ifelse(is.na(ARV_new1),ARV1,ARV_new1),
           ARV2 = ifelse(is.na(ARV_new2),ARV2,ARV_new2),
           ARV3 = ifelse(is.na(ARV_new3),ARV3,ARV_new3),
           ARV4 = ifelse(is.na(ARV_new4),ARV4,ARV_new4),
           ARV5 = ifelse(is.na(ARV_new5),ARV5,ARV_new5),
           ARV6 = ifelse(is.na(ARV_new6),ARV6,ARV_new6),
           ARV7 = ifelse(is.na(ARV_new7),ARV7,ARV_new7),
           ARV8 = ifelse(is.na(ARV_new8),ARV8,ARV_new8),
           ARV9 = ifelse(is.na(ARV_new9),ARV9,ARV_new9),
           ARV10 = ifelse(is.na(ARV_new10),ARV10,ARV_new10),
           
           RDM1 = ifelse(is.na(RDM_new1),RDM1,RDM_new1),
           RDM2 = ifelse(is.na(RDM_new2),RDM2,RDM_new2),
           RDM3 = ifelse(is.na(RDM_new3),RDM3,RDM_new3),
           RDM4 = ifelse(is.na(RDM_new4),RDM4,RDM_new4),
           RDM5 = ifelse(is.na(RDM_new5),RDM5,RDM_new5),
           RDM6 = ifelse(is.na(RDM_new6),RDM6,RDM_new6),
           RDM7 = ifelse(is.na(RDM_new7),RDM7,RDM_new7),
           RDM8 = ifelse(is.na(RDM_new8),RDM8,RDM_new8),
           RDM9 = ifelse(is.na(RDM_new9),RDM9,RDM_new9),
           RDM10 = ifelse(is.na(RDM_new10),RDM10,RDM_new10),
           
           IRI1 = ifelse(is.na(IRI_new1),IRI1,IRI_new1),
           IRI2 = ifelse(is.na(IRI_new2),IRI2,IRI_new2),
           IRI3 = ifelse(is.na(IRI_new3),IRI3,IRI_new3),
           IRI4 = ifelse(is.na(IRI_new4),IRI4,IRI_new4),
           IRI5 = ifelse(is.na(IRI_new5),IRI5,IRI_new5),
           IRI6 = ifelse(is.na(IRI_new6),IRI6,IRI_new6),
           IRI7 = ifelse(is.na(IRI_new7),IRI7,IRI_new7),
           IRI8 = ifelse(is.na(IRI_new8),IRI8,IRI_new8),
           IRI9 = ifelse(is.na(IRI_new9),IRI9,IRI_new9),
           IRI10 = ifelse(is.na(IRI_new10),IRI10,IRI_new10)) %>% 
    dplyr::select(`AssetID`,`Project Name`,PCI0:selection.date.year) %>% as.data.table()
  return(output.final2)
}



pavement_post_int <- calc_pav_det_after_intervention(p_post_intervention_edited_split) %>% as_tibble()
pavement_post_int$Scenario <- "Scenario 1"

# testing

p_post_intervention_edited %>% filter(`Project Name` == "M4 WESTERN MWY, GREYSTANES - R6004115002.000BU_17.557")
pavement_post_int %>% filter(`Project Name` == "M4 WESTERN MWY, GREYSTANES - R6004115002.000BU_17.557")

p_post_intervention_edited %>% filter(selection.date.year == 3)
pavement_post_int %>% filter(`Project Name` == "WARREN RD, SMITHFIELD - R0013111500.161BU_9.400")

pavement_no_intervention <- pavement_health_ni %>% filter(!is.na(`Project Name`)) %>% mutate(Scenario = "No Investment")

columns_AN_all <- c("AssetID","Project Name","PCI0","PCI1","PCI2","PCI3","PCI4","PCI5","PCI6","PCI7","PCI8","PCI9","PCI10",
                    "RDM0","RDM1","RDM2","RDM3","RDM4","RDM5","RDM6","RDM7","RDM8","RDM9","RDM10",
                    "ARV0","ARV1","ARV2","ARV3","ARV4","ARV5","ARV6","ARV7","ARV8","ARV9","ARV10",
                    "IRI0","IRI1","IRI2","IRI3","IRI4","IRI5","IRI6","IRI7","IRI8","IRI9","IRI10","Scenario")

pavement_both <- plyr::rbind.fill(pavement_post_int[,columns_AN_all],pavement_no_intervention[,columns_AN_all])

# write results
setwd("C:/Users/TOsosanya/Desktop/NSW/")
path_p <- "Newdata 2705/Pavements"
openxlsx::write.xlsx(pavement_both, file = paste0(path_p, "/Pavement health post_intervention.xlsx"))





