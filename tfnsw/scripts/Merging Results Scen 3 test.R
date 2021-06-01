library(dplyr)
library(magrittr)
library(DescTools)
library(readxl)
library(tidyr)
library(data.table)
library(janitor)
library(stringr)

rm(list=ls())
memory.limit(size = 1600000)

## The difference between Intervention stragey and minimum level - those replaced using intervention vs year 1,2,3

#test_p <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Pavements/pavement_assets ConOps Calcs.xlsx"),range = "A1:AF1596", sheet = "pavement_assets2")
result_p <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Pavements/Pavement results.xlsx"),range = "A1:HC1596", sheet = "Sheet 1")
path_pp <- "C:/Users/TOsosanya/Desktop/NSW/Asset port"
assgnd_p <- read_excel(paste0(path_pp, "/10.04.05 Tactical Works - RFT Addendum 3 (NF Aligned).xlsx"), range = "A1:F400" ,sheet = "DY 21-23")
unassgnd_p <- read_excel(paste0(path_pp, "/10.04.05 Tactical Works - RFT Addendum 3 (NF Aligned).xlsx"), range = "A1:F400" ,sheet = "DY 24-30")
all_p <- plyr::rbind.fill(assgnd_p,unassgnd_p)

result_c <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Corridors/Corridor results.xlsx"),range = "A1:EY4704", sheet = "Sheet 1")
result_b <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Bridges/Bridges results.xlsx"),range = "A1:EY450", sheet = "Sheet 1")
result_i <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/ITS/ITS results.xlsx"),range = "A1:EY3118", sheet = "Sheet 1", guess_max = 5000)

# for all assets
result_p <- result_p %>% select(AssetID,`Project Name`,POF1:POF10,POF1_FMECA_CONOPS:POF10_FMECA_Political_CONOPS) %>% 
  mutate(`Asset Category` = "Pavement")
result_c <- result_c %>% 
  filter(`Asset_Type`=="Culvert"|`Asset_Type`=="Slope Site") %>% 
  select(`Asset Number`,`Asset Category`,`Project Name`,POF1:POF10,POF1_FMECA_CONOPS:POF10_FMECA_Political_CONOPS) %>% 
  rename(AssetID = `Asset Number`)
result_b <- result_b %>% select(BNO,`Asset Category`,`Project Name`,POF1:POF10,POF1_FMECA_CONOPS:POF10_FMECA_Political_CONOPS)%>% 
  arrange(`Project Name`) %>% 
  rename(AssetID = `BNO`)
result_i <- result_i %>%
  select(LABEL,`Project Name`,POF1:POF10,POF1_FMECA_CONOPS:POF10_FMECA_Political_CONOPS)%>% 
  mutate(`Asset Category` = "ITS")

# result_b <- result_b[c(1:29,31,37:39,44,50),] # select unique bridge projects

result_all <- plyr::rbind.fill(result_p,result_c,result_b,result_i)
# remove dup project names and select bridges with projects
result_all_nop <- result_all %>% 
  filter(is.na(`Project Name`))
result_all <- result_all %>% 
  group_by(`Project Name`) %>% 
  arrange(desc(POF1_FMECA_CONOPS)) %>% 
  mutate(rown = row_number(`Project Name`)) %>% 
  filter(rown == 1)
###
##
#



#fwrite(result_all, "test scen 1.csv")

# select the projects with project name only AND remove 5% of projects using simplified sampling rather than random sampling. 
## pavement_UP_notselected wil be used later

pavement_UP <- all_p %>% 
  #na.omit() %>% 
  inner_join(result_all, by = "Project Name") %>% 
  #filter(must == "FALSE")
  mutate(selector = ifelse(must == "FALSE",rep(1:20),0)) %>% 
  filter(selector != "1") %>% 
  select(-selector)

pavement_UP_notselected <- all_p %>% 
  #na.omit() %>% 
  inner_join(result_all, by = "Project Name") %>% 
  #filter(must == "FALSE")
  mutate(selector = ifelse(must == "FALSE",rep(1:20),0)) %>% 
  filter(selector == "1") %>% 
  select(-selector)
result_all_nop %>% ungroup() %>% group_by(`Asset Category`) %>% summarise(cv=sum(POF1_FMECA_CONOPS))
result_all %>% ungroup() %>% group_by(`Asset Category`) %>% summarise(cv=sum(POF1_FMECA_CONOPS))
result_all %>% ungroup() %>% filter(`Asset Category`=="Pavement") %>% summarise(cv=sum(POF1_FMECA_CONOPS))
result_all %>% ungroup() %>% filter(`Asset Category`=="Pavement",!is.na(`Project Name`)) %>% group_by(`Asset Category`=="Pavement") %>% summarise(cv=sum(POF1_FMECA_CONOPS))

# pavement_UP <- result_all %>% 
#   left_join(all_p, by = "Project Name") %>% 
#   filter(is.na(`Project Name`)) 

# colSums(pavement_UP[,c(13:14)],na.rm = T)

pavement_UP %>% filter(`Asset Category`=="Pavement",!is.na(`Project Name`)) %>% distinct() %>% group_by(`Asset Category`=="Pavement") %>% summarise(cv=sum(POF1_FMECA_CONOPS))
pavement_UP$AssetID %>% unique()
pavement_UP$`Project Name` %>% unique()


# remove duplicate projects retaining the max total impact score ------------------------------------------------------------
# THIS IS THE FUNCTION INPUT DATA

pavement_UP2 <- pavement_UP %>% 
  group_by(`Project Name`) %>% 
  arrange(desc(POF1_FMECA_CONOPS)) %>% 
  mutate(rown = row_number(`Project Name`)) %>% 
  filter(rown == 1)

pavement_UP2 %>% filter(`Asset Category`=="Pavement") %>% group_by(`Asset Category`=="Pavement") %>% summarise(cv=sum(POF1_FMECA_CONOPS))
pavement_UP2 %>% filter(`Project Name` == "VICTORIA RD, ERMINGTON - R0165047000.000BU_20.002") %>% as.data.frame()
pavement_UP2 %>% filter(`Asset Category`=="Pavement",!is.na(`Project Name`)) %>% distinct() %>% group_by(`Asset Category`=="Pavement") %>% summarise(cv=sum(POF1_FMECA_CONOPS))


# order column names
function1 <- function(pavement_UP2){
  ordered_columns <- c("Asset Category","Asset Group","AssetID","Delivery Year","must","Project Name","rown","selection.timing","Total_Cost",
                       #"Total_Cost1","Total_Cost2","Total_Cost3","Total_Cost4","Total_Cost5","Total_Cost6","Total_Cost7","Total_Cost8","Total_Cost9","Total_Cost10",
                       "POF1","POF2","POF3","POF4","POF5","POF6","POF7","POF8","POF9","POF10",
                       "POF1_FMECA_CONOPS","POF2_FMECA_CONOPS","POF3_FMECA_CONOPS","POF4_FMECA_CONOPS","POF5_FMECA_CONOPS","POF6_FMECA_CONOPS","POF7_FMECA_CONOPS","POF8_FMECA_CONOPS","POF9_FMECA_CONOPS","POF10_FMECA_CONOPS",
                       "POF1_FMECA_Reliability_CONOPS","POF2_FMECA_Reliability_CONOPS","POF3_FMECA_Reliability_CONOPS","POF4_FMECA_Reliability_CONOPS","POF5_FMECA_Reliability_CONOPS","POF6_FMECA_Reliability_CONOPS","POF7_FMECA_Reliability_CONOPS","POF8_FMECA_Reliability_CONOPS","POF9_FMECA_Reliability_CONOPS","POF10_FMECA_Reliability_CONOPS",
                       "POF1_FMECA_Availability_CONOPS","POF2_FMECA_Availability_CONOPS","POF3_FMECA_Availability_CONOPS","POF4_FMECA_Availability_CONOPS","POF5_FMECA_Availability_CONOPS","POF6_FMECA_Availability_CONOPS","POF7_FMECA_Availability_CONOPS","POF8_FMECA_Availability_CONOPS","POF9_FMECA_Availability_CONOPS","POF10_FMECA_Availability_CONOPS",
                       "POF1_FMECA_Maintainability_CONOPS","POF2_FMECA_Maintainability_CONOPS","POF3_FMECA_Maintainability_CONOPS","POF4_FMECA_Maintainability_CONOPS","POF5_FMECA_Maintainability_CONOPS","POF6_FMECA_Maintainability_CONOPS","POF7_FMECA_Maintainability_CONOPS","POF8_FMECA_Maintainability_CONOPS","POF9_FMECA_Maintainability_CONOPS","POF10_FMECA_Maintainability_CONOPS",
                       "POF1_FMECA_Safety_CONOPS","POF2_FMECA_Safety_CONOPS","POF3_FMECA_Safety_CONOPS","POF4_FMECA_Safety_CONOPS","POF5_FMECA_Safety_CONOPS","POF6_FMECA_Safety_CONOPS","POF7_FMECA_Safety_CONOPS","POF8_FMECA_Safety_CONOPS","POF9_FMECA_Safety_CONOPS","POF10_FMECA_Safety_CONOPS",
                       "POF1_FMECA_Security_CONOPS","POF2_FMECA_Security_CONOPS","POF3_FMECA_Security_CONOPS","POF4_FMECA_Security_CONOPS","POF5_FMECA_Security_CONOPS","POF6_FMECA_Security_CONOPS","POF7_FMECA_Security_CONOPS","POF8_FMECA_Security_CONOPS","POF9_FMECA_Security_CONOPS","POF10_FMECA_Security_CONOPS",
                       "POF1_FMECA_Health_CONOPS","POF2_FMECA_Health_CONOPS","POF3_FMECA_Health_CONOPS","POF4_FMECA_Health_CONOPS","POF5_FMECA_Health_CONOPS","POF6_FMECA_Health_CONOPS","POF7_FMECA_Health_CONOPS","POF8_FMECA_Health_CONOPS","POF9_FMECA_Health_CONOPS","POF10_FMECA_Health_CONOPS",
                       "POF1_FMECA_Economics_CONOPS","POF2_FMECA_Economics_CONOPS","POF3_FMECA_Economics_CONOPS","POF4_FMECA_Economics_CONOPS","POF5_FMECA_Economics_CONOPS","POF6_FMECA_Economics_CONOPS","POF7_FMECA_Economics_CONOPS","POF8_FMECA_Economics_CONOPS","POF9_FMECA_Economics_CONOPS","POF10_FMECA_Economics_CONOPS",
                       "POF1_FMECA_Environment_CONOPS","POF2_FMECA_Environment_CONOPS","POF3_FMECA_Environment_CONOPS","POF4_FMECA_Environment_CONOPS","POF5_FMECA_Environment_CONOPS","POF6_FMECA_Environment_CONOPS","POF7_FMECA_Environment_CONOPS","POF8_FMECA_Environment_CONOPS","POF9_FMECA_Environment_CONOPS","POF10_FMECA_Environment_CONOPS",
                       "POF1_FMECA_Political_CONOPS","POF2_FMECA_Political_CONOPS","POF3_FMECA_Political_CONOPS","POF4_FMECA_Political_CONOPS","POF5_FMECA_Political_CONOPS","POF6_FMECA_Political_CONOPS","POF7_FMECA_Political_CONOPS","POF8_FMECA_Political_CONOPS","POF9_FMECA_Political_CONOPS","POF10_FMECA_Political_CONOPS"
  )
  
  pavement_UP2x <- pavement_UP2[,ordered_columns] %>% 
    mutate_at(c("POF1","POF2","POF3","POF4","POF5","POF6","POF7","POF8","POF9","POF10"),as.numeric) %>% 
    pivot_longer(cols = POF1:POF10_FMECA_Political_CONOPS,
                 names_to = c("measure")
                 #names_pattern = "(.)(.)"
    ) %>% 
    group_by(AssetID) %>% 
    mutate(timestep = rep(1:10,11),
           value = round(value,1)) 
  pavement_UP2x$measure = gsub('[[:digit:]]+', '', pavement_UP2x$measure)
  pavement_UP2x <- pavement_UP2x %>% 
    as.data.table()
  # add selected which serves as UID
  
  pavement_UP3 <- pavement_UP2x %>% ungroup() %>% 
    group_by(`Project Name`, timestep) %>% 
    mutate(measure = "Selected",
           value = 0) %>% 
    distinct() %>% 
    ungroup() %>% 
    as.data.table()
  
  pavement_UP4 <- rbind(pavement_UP2x,pavement_UP3) %>% 
    mutate(solution = 0)
  
  pavement_UP4 %>% as.data.table()
  
  # add solution = 1. i.e. if an action is taken
  
  pavement_UP5 <- pavement_UP4 %>%
    mutate(solution = 1,
           value = ifelse(measure == "Selected",1,0)) 
  
  pavement_all <- rbind(pavement_UP4,pavement_UP5) %>% 
    distinct() %>% 
    #filter(`Asset Group` != "ITS asset management") %>% 
    as.data.table()
  return(pavement_all)
}

pavement_all <- function1(pavement_UP2)

# openxlsx::write.xlsx(pavement_all, file = "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Pavements/Pavement UVO 3.xlsx")



# END



pnames_id <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/UVO/Final-NSW-Scen1.xlsx"),range = "A1:C512", sheet = "id") %>%
  filter(`Asset Group` != "ITS asset management") %>%
  select(1:2)

pnames_id %>% filter(`name` == "BN 257 Service Br ov Haslams Ck Rehab")
pavement_all %>% filter(grepl('BN 257 Serv',`Project Name`))

pnames_id <- pnames_id %>% left_join(pavement_all, by = c("name"="Project Name")) %>%
  rename(project_name = name) %>%
  select(project,project_name,measure,timestep,value, solution,selection.timing, must,Total_Cost)
# 




######
# Add total cost

# add the total cst bridge etc

pnames_id_2 <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/UVO/Final-NSW-Scen1.xlsx"),range = "A1:C512", sheet = "id") %>% 
  filter(`Asset Group` != "ITS asset management") %>% 
  filter(!grepl('Remaining ',`name`)) %>%
  select(1:2)

function2 <- function(pnames_id_2){
  pnames_id_2 <- pnames_id_2 %>% left_join(pavement_all, by = c("name"="Project Name")) %>% 
    rename(project_name = name) %>% 
    select(project,project_name,`Asset Group`,measure,timestep,value, solution,selection.timing, must,Total_Cost)
  
  pnames_id_3 <- pnames_id_2 %>% left_join(pavement_UP2[,1:2], by = "Asset Group") %>% 
    distinct() %>% 
    mutate(Total_Cost_Bridges = ifelse(`Asset Group` == "Bridge asset management",Total_Cost, 0)) %>%
    mutate(Total_Cost_Pavement = ifelse(`Asset Group` == "Pavement asset management", Total_Cost, 0)) %>%
    mutate(Total_Cost_Corridor = ifelse(`Asset Group` == "Corridor asset management", Total_Cost, 0))  %>%
    mutate(Total_Cost_ITS = ifelse(`Asset Group` == "ITS asset management", Total_Cost, 0))
  
  pnames_id_4 <- pnames_id_3 %>% 
    na.omit() %>% 
    select(project,project_name,Total_Cost, Total_Cost_Bridges:Total_Cost_ITS) %>% 
    group_by(project,project_name) %>% 
    rename(Total_Cost1 = Total_Cost) %>%
    rename(Total_Cost_Bridges1 = Total_Cost_Bridges) %>% 
    rename(Total_Cost_Pavement1 = Total_Cost_Pavement) %>% 
    rename(Total_Cost_Corridor1 = Total_Cost_Corridor) %>% 
    rename(Total_Cost_ITS1 = Total_Cost_ITS) %>% 
    distinct() 
  pnames_id_4 <- pnames_id_4 %>% 
    mutate(Total_Cost2=0,
           Total_Cost3=0,
           Total_Cost4=0,
           Total_Cost5=0,
           Total_Cost6=0,
           Total_Cost7=0,
           Total_Cost8=0,
           Total_Cost9=0,
           Total_Cost10=0,
           
           Total_Cost_Bridges2=0,
           Total_Cost_Bridges3=0,
           Total_Cost_Bridges4=0,
           Total_Cost_Bridges5=0,
           Total_Cost_Bridges6=0,
           Total_Cost_Bridges7=0,
           Total_Cost_Bridges8=0,
           Total_Cost_Bridges9=0,
           Total_Cost_Bridges10=0,
           
           Total_Cost_Pavement2=0,
           Total_Cost_Pavement3=0,
           Total_Cost_Pavement4=0,
           Total_Cost_Pavement5=0,
           Total_Cost_Pavement6=0,
           Total_Cost_Pavement7=0,
           Total_Cost_Pavement8=0,
           Total_Cost_Pavement9=0,
           Total_Cost_Pavement10=0,
           
           Total_Cost_Corridor2=0,
           Total_Cost_Corridor3=0,
           Total_Cost_Corridor4=0,
           Total_Cost_Corridor5=0,
           Total_Cost_Corridor6=0,
           Total_Cost_Corridor7=0,
           Total_Cost_Corridor8=0,
           Total_Cost_Corridor9=0,
           Total_Cost_Corridor10=0,
           
           Total_Cost_ITS2=0,
           Total_Cost_ITS3=0,
           Total_Cost_ITS4=0,
           Total_Cost_ITS5=0,
           Total_Cost_ITS6=0,
           Total_Cost_ITS7=0,
           Total_Cost_ITS8=0,
           Total_Cost_ITS9=0,
           Total_Cost_ITS10=0) %>% 
    select(Total_Cost1,Total_Cost2:Total_Cost10,Total_Cost_Bridges1,Total_Cost_Bridges2:Total_Cost_Bridges10,
           Total_Cost_Pavement1,Total_Cost_Pavement2:Total_Cost_Pavement10,Total_Cost_Corridor1,Total_Cost_Corridor2:Total_Cost_Corridor10,
           Total_Cost_ITS1,Total_Cost_ITS2:Total_Cost_ITS10,everything()) %>% 
    pivot_longer(cols = Total_Cost1:Total_Cost_ITS10,
                 names_to = c("measure")
                 #names_pattern = "(.)(.)"
    ) %>% 
    as.data.table()
  
  # add solution = 1 for the costing
  pnames_id_4_3 <- pnames_id_4 %>%
    group_by(project) %>% 
    mutate(timestep = rep(1:10,5),
           value = round(value,1)) %>% 
    mutate(solution = 1) %>% 
    as.data.frame()
  
  pnames_id_4_3$measure = gsub('[[:digit:]]+', '', pnames_id_4_3$measure)
  
  # add solution = 0
  
  pnames_id_4_4 <- pnames_id_4_3 %>% ungroup() %>% 
    group_by(project, timestep) %>% 
    mutate(solution = 0,
           value = 0) %>% 
    distinct() %>% 
    ungroup() %>% 
    as.data.frame()
  
  pnames_id_4_5 <- rbind(pnames_id_4_3,pnames_id_4_4) %>% 
    inner_join(pnames_id[,c("project","selection.timing", "must")], by = "project") %>% 
    distinct()
  
  all_xx3 <- rbind(pnames_id[,1:8],pnames_id_4_5) %>% 
    filter(!is.na(measure))
  return(all_xx3)
}

all_xx3 <- function2(pnames_id_2)

####

source("C:/Users/TOsosanya/Desktop/NSW/tfnsw/Merging Results Scen 3 non projects test.R")

# bind both non projects and projects

all_complete <- rbind(all_xx3,up_all_xx3)

test1 <- all_complete %>% left_join(pavement_all[,c("Asset Category","Project Name")], by = c("project_name"="Project Name")) %>% 
  distinct() %>% filter(`Asset Category` == "Pavement",!is.na(project_name), measure == "POF_FMECA_CONOPS", timestep == 1) %>% 
  summarise(cv=sum(value))
test2 <- all_complete %>% filter(`project_name` == "Remaining Pavements", measure == "POF_FMECA_CONOPS", timestep == 1, solution == 1) %>% 
  summarise(cv=sum(value))
test1+test2


# scenario 3 -------------------------------------------------------------------------

all_complete <- rbind(all_xx3,up_all_xx3)
all_complete_Scen3 <- all_complete %>% 
  mutate(selection.timing = ifelse(selection.timing == "ynnnnnnnnn","yyynnnnnnn",
                                   ifelse(selection.timing == "nynnnnnnnn","nyyynnnnnn",
                                          ifelse(selection.timing == "nnynnnnnnn","nnyyynnnnn",selection.timing))))

all_complete %>% filter(measure == "Total_Cost")
all_complete_Scen3 %>% filter(measure == "Total_Cost")

## added this to get bucket and asset group --------------------------------
projects1 <- read_excel(paste0(path_pp, "/10.04.05 Tactical Works - RFT Addendum 3 (NF Aligned).xlsx"), range = "A1:G512" ,sheet = "delivery year")
all_complete_Scen3 <- all_complete_Scen3 %>% 
  left_join(projects1[,c(1,2,7)], by = c("project_name" = "Project Name")) %>%
  rename(Asset_Group = `Asset Group`) %>% 
  mutate(Asset_Group = ifelse(is.na(Asset_Group),paste0(word(project_name,-1)," asset management"),Asset_Group)) %>% 
  mutate(Asset_Group = ifelse(Asset_Group == "Bridges asset management", "Bridges with no project", 
                              ifelse(Asset_Group == "Corridors asset management", "Corridors with no project",
                                     ifelse(Asset_Group == "Pavements asset management", "Pavements with no project",Asset_Group))),
         Asset_Group = ifelse(project_name == "Remaining ITS","ITS with no project",Asset_Group)) %>% 
  mutate(Bucket = ifelse(is.na(Bucket),paste0("Other ",word(project_name,-1)," works"),Bucket)) %>% 
  mutate(value = ifelse(is.na(value),0,value))

all_complete_Scen3_project <- all_complete_Scen3 %>% select(project,project_name,must,Asset_Group, Bucket) %>%
  rename(name = project_name) %>%
  rename(Zone = Asset_Group) %>%
  rename(Region = Bucket) %>% select(project,name, must,Region, Zone) %>% 
  distinct()
all_complete_Scen3_solution <- all_complete_Scen3 %>% select(project,solution,selection.timing) %>% 
  filter(solution == 1) %>% 
  mutate(time.to.benefits = 1) %>% distinct()
all_complete_Scen3_measures <- all_complete_Scen3 %>% select(project:solution)

write.csv(all_complete_Scen3_project,"C:/Users/TOsosanya/Desktop/NSW/UVO/Scen 3-Project.csv",row.names = F)
write.csv(all_complete_Scen3_solution,"C:/Users/TOsosanya/Desktop/NSW/UVO/Scen 3-Solution.csv",row.names = F)
write.csv(all_complete_Scen3_measures,"C:/Users/TOsosanya/Desktop/NSW/UVO/Scen 3-Measures.csv",row.names = F)

## ends here

openxlsx::write.xlsx(all_complete_Scen3, file = "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Pavements/Pavement UVO 3 ids Scen3.xlsx")

