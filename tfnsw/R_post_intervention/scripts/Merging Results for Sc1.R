library(dplyr)
library(magrittr)
library(DescTools)
library(readxl)
library(tidyr)
library(data.table)
library(janitor)
library(stringr)

rm(list=ls())

## The difference between Intervention stragey and minimum level - those replaced using intervention vs year 1,2,3

#test_p <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Pavements/pavement_assets ConOps Calcs.xlsx"),range = "A1:AF1596", sheet = "pavement_assets2")
path_pp <- "C:/Users/TOsosanya/Desktop/NSW/UVO/SRAPC V2 Model Files/"
result_p <- read_excel(paste0(path_pp, "/Sc 1 Loaded Plan.xlsx"), range = "A1:HC1596" ,sheet = "Pavement")
result_c <- read_excel(paste0(path_pp, "/Sc 1 Loaded Plan.xlsx"), range = "A1:EY4703" ,sheet = "Corridor")
result_b <- read_excel(paste0(path_pp, "/Sc 1 Loaded Plan.xlsx"), range = "A1:EY450" ,sheet = "Bridges")
result_i <- read_excel(paste0(path_pp, "/Sc 1 Loaded Plan.xlsx"), range = "A1:EY3118" ,sheet = "ITS")

# for all assets
result_p <- result_p %>% dplyr::select(AssetID,`Project Name`,Sc_One_POF1:Sc_One_POF10,Sc_One_POF1_FMECA_CONOPS:Sc_One_POF10_FMECA_Political_CONOPS) %>% 
  mutate(`Asset Category` = "Pavement")
result_c <- result_c %>% 
  filter(`Asset_Type`=="Culvert"|`Asset_Type`=="Slope Site") %>% 
  dplyr::select(`Asset Number`,`Asset Category`,`Project Name`,Sc_One_POF1:Sc_One_POF10,Sc_One_POF1_FMECA_CONOPS:Sc_One_POF10_FMECA_Political_CONOPS) %>% 
  rename(AssetID = `Asset Number`)
result_b <- result_b %>% dplyr::select(BNO,`Asset Category`,`Project Name`,Sc_One_POF1:Sc_One_POF10,Sc_One_POF1_FMECA_CONOPS:Sc_One_POF10_FMECA_Political_CONOPS)%>% 
  arrange(`Project Name`) %>% 
  rename(AssetID = `BNO`)
result_i <- result_i %>%
  dplyr::select(LABEL,`Project Name`,Sc_One_POF1:Sc_One_POF10,Sc_One_POF1_FMECA_CONOPS:Sc_One_POF10_FMECA_Political_CONOPS)%>% 
  mutate(`Asset Category` = "ITS")

# remove dup project names and dplyr::select bridges with projects

# result_b <- result_b[c(1:29,31,37:39,44,50),] # dplyr::select unique bridge projects

result_all <- plyr::rbind.fill(result_p,result_c,result_b,result_i) %>% 
  filter(!is.na(`Project Name`)) %>% 
  group_by(`Project Name`) %>% 
  arrange(desc(Sc_One_POF1_FMECA_CONOPS)) %>% 
  mutate(rown = row_number(`Project Name`)) %>% 
  filter(rown == 1)

# concat with total cost

plan <- read_excel(paste0(path_pp, "/Sc 1 Loaded Plan.xlsx"), range = "A1:Z672" ,sheet = "Sc 1 Loaded Plan (1)")
total_cost_columns <- c("Name","Total_Cost1","Total_Cost2","Total_Cost3","Total_Cost4","Total_Cost5","Total_Cost6","Total_Cost7","Total_Cost8","Total_Cost9","Total_Cost10")
result_all <- result_all %>% 
  left_join(plan[,total_cost_columns], by = c("Project Name"="Name"))

# remove Sc_one from column names

Sc_One_columns <- c("Sc_One_POF1","Sc_One_POF2","Sc_One_POF3","Sc_One_POF4","Sc_One_POF5","Sc_One_POF6","Sc_One_POF7","Sc_One_POF8","Sc_One_POF9","Sc_One_POF10",
                     "Sc_One_POF1_FMECA_CONOPS","Sc_One_POF2_FMECA_CONOPS","Sc_One_POF3_FMECA_CONOPS","Sc_One_POF4_FMECA_CONOPS","Sc_One_POF5_FMECA_CONOPS","Sc_One_POF6_FMECA_CONOPS","Sc_One_POF7_FMECA_CONOPS","Sc_One_POF8_FMECA_CONOPS","Sc_One_POF9_FMECA_CONOPS","Sc_One_POF10_FMECA_CONOPS",
                     "Sc_One_POF1_FMECA_Reliability_CONOPS","Sc_One_POF2_FMECA_Reliability_CONOPS","Sc_One_POF3_FMECA_Reliability_CONOPS","Sc_One_POF4_FMECA_Reliability_CONOPS","Sc_One_POF5_FMECA_Reliability_CONOPS","Sc_One_POF6_FMECA_Reliability_CONOPS","Sc_One_POF7_FMECA_Reliability_CONOPS","Sc_One_POF8_FMECA_Reliability_CONOPS","Sc_One_POF9_FMECA_Reliability_CONOPS","Sc_One_POF10_FMECA_Reliability_CONOPS",
                     "Sc_One_POF1_FMECA_Availability_CONOPS","Sc_One_POF2_FMECA_Availability_CONOPS","Sc_One_POF3_FMECA_Availability_CONOPS","Sc_One_POF4_FMECA_Availability_CONOPS","Sc_One_POF5_FMECA_Availability_CONOPS","Sc_One_POF6_FMECA_Availability_CONOPS","Sc_One_POF7_FMECA_Availability_CONOPS","Sc_One_POF8_FMECA_Availability_CONOPS","Sc_One_POF9_FMECA_Availability_CONOPS","Sc_One_POF10_FMECA_Availability_CONOPS",
                     "Sc_One_POF1_FMECA_Maintainability_CONOPS","Sc_One_POF2_FMECA_Maintainability_CONOPS","Sc_One_POF3_FMECA_Maintainability_CONOPS","Sc_One_POF4_FMECA_Maintainability_CONOPS","Sc_One_POF5_FMECA_Maintainability_CONOPS","Sc_One_POF6_FMECA_Maintainability_CONOPS","Sc_One_POF7_FMECA_Maintainability_CONOPS","Sc_One_POF8_FMECA_Maintainability_CONOPS","Sc_One_POF9_FMECA_Maintainability_CONOPS","Sc_One_POF10_FMECA_Maintainability_CONOPS",
                     "Sc_One_POF1_FMECA_Safety_CONOPS","Sc_One_POF2_FMECA_Safety_CONOPS","Sc_One_POF3_FMECA_Safety_CONOPS","Sc_One_POF4_FMECA_Safety_CONOPS","Sc_One_POF5_FMECA_Safety_CONOPS","Sc_One_POF6_FMECA_Safety_CONOPS","Sc_One_POF7_FMECA_Safety_CONOPS","Sc_One_POF8_FMECA_Safety_CONOPS","Sc_One_POF9_FMECA_Safety_CONOPS","Sc_One_POF10_FMECA_Safety_CONOPS",
                     "Sc_One_POF1_FMECA_Security_CONOPS","Sc_One_POF2_FMECA_Security_CONOPS","Sc_One_POF3_FMECA_Security_CONOPS","Sc_One_POF4_FMECA_Security_CONOPS","Sc_One_POF5_FMECA_Security_CONOPS","Sc_One_POF6_FMECA_Security_CONOPS","Sc_One_POF7_FMECA_Security_CONOPS","Sc_One_POF8_FMECA_Security_CONOPS","Sc_One_POF9_FMECA_Security_CONOPS","Sc_One_POF10_FMECA_Security_CONOPS",
                     "Sc_One_POF1_FMECA_Health_CONOPS","Sc_One_POF2_FMECA_Health_CONOPS","Sc_One_POF3_FMECA_Health_CONOPS","Sc_One_POF4_FMECA_Health_CONOPS","Sc_One_POF5_FMECA_Health_CONOPS","Sc_One_POF6_FMECA_Health_CONOPS","Sc_One_POF7_FMECA_Health_CONOPS","Sc_One_POF8_FMECA_Health_CONOPS","Sc_One_POF9_FMECA_Health_CONOPS","Sc_One_POF10_FMECA_Health_CONOPS",
                     "Sc_One_POF1_FMECA_Economics_CONOPS","Sc_One_POF2_FMECA_Economics_CONOPS","Sc_One_POF3_FMECA_Economics_CONOPS","Sc_One_POF4_FMECA_Economics_CONOPS","Sc_One_POF5_FMECA_Economics_CONOPS","Sc_One_POF6_FMECA_Economics_CONOPS","Sc_One_POF7_FMECA_Economics_CONOPS","Sc_One_POF8_FMECA_Economics_CONOPS","Sc_One_POF9_FMECA_Economics_CONOPS","Sc_One_POF10_FMECA_Economics_CONOPS",
                     "Sc_One_POF1_FMECA_Environment_CONOPS","Sc_One_POF2_FMECA_Environment_CONOPS","Sc_One_POF3_FMECA_Environment_CONOPS","Sc_One_POF4_FMECA_Environment_CONOPS","Sc_One_POF5_FMECA_Environment_CONOPS","Sc_One_POF6_FMECA_Environment_CONOPS","Sc_One_POF7_FMECA_Environment_CONOPS","Sc_One_POF8_FMECA_Environment_CONOPS","Sc_One_POF9_FMECA_Environment_CONOPS","Sc_One_POF10_FMECA_Environment_CONOPS",
                     "Sc_One_POF1_FMECA_Political_CONOPS","Sc_One_POF2_FMECA_Political_CONOPS","Sc_One_POF3_FMECA_Political_CONOPS","Sc_One_POF4_FMECA_Political_CONOPS","Sc_One_POF5_FMECA_Political_CONOPS","Sc_One_POF6_FMECA_Political_CONOPS","Sc_One_POF7_FMECA_Political_CONOPS","Sc_One_POF8_FMECA_Political_CONOPS","Sc_One_POF9_FMECA_Political_CONOPS","Sc_One_POF10_FMECA_Political_CONOPS"
)

pof_columns <- c("POF1","POF2","POF3","POF4","POF5","POF6","POF7","POF8","POF9","POF10",
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

result_all[,pof_columns] <- result_all[,Sc_One_columns] 

# order column names
function1 <- function(pavement_UP2){
  ordered_columns <- c("Asset Category","LABEL","AssetID","Project Name",
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
                       "POF1_FMECA_Political_CONOPS","POF2_FMECA_Political_CONOPS","POF3_FMECA_Political_CONOPS","POF4_FMECA_Political_CONOPS","POF5_FMECA_Political_CONOPS","POF6_FMECA_Political_CONOPS","POF7_FMECA_Political_CONOPS","POF8_FMECA_Political_CONOPS","POF9_FMECA_Political_CONOPS","POF10_FMECA_Political_CONOPS",
                       "Total_Cost1","Total_Cost2","Total_Cost3","Total_Cost4","Total_Cost5","Total_Cost6","Total_Cost7","Total_Cost8","Total_Cost9","Total_Cost10"
  )
  
  pavement_UP2x <- pavement_UP2[,ordered_columns] %>% 
    mutate_at(c("POF1","POF2","POF3","POF4","POF5","POF6","POF7","POF8","POF9","POF10"),as.numeric) %>% 
    pivot_longer(cols = POF1:Total_Cost10,
                 names_to = c("measure")
                 #names_pattern = "(.)(.)"
    ) %>% 
    group_by(`Project Name`) %>% 
    mutate(timestep = rep(1:10,12),
           value = round(value,2),
           value = ifelse(is.na(value),0,value)) 
  pavement_UP2x$measure = gsub('[[:digit:]]+', '', pavement_UP2x$measure)
  pavement_UP2x <- pavement_UP2x %>% 
    as.data.table()
  # add selected which serves as UID
  
  return(pavement_UP2x)
}
assets_with_projects <- function1(result_all)



# add the intervention year, number of items in backlog - 
#and the total_cost


plan <- read_excel(paste0(path_pp, "/Sc 1 Loaded Plan.xlsx"), range = "A1:Z672" ,sheet = "Sc 1 Loaded Plan (1)")
projects_v2 <- read_csv(paste0(path_pp, "/Projects V2.csv"))
assets_with_projects2 <- assets_with_projects %>% left_join(plan[,c("Name","selection.date","selection.date.year")], by = c("Project Name"= "Name")) %>% 
  mutate(Scenario = "Scenario 1") %>% 
  rename(project_name = `Project Name`) %>% 
  left_join(projects_v2[,c("project","name")], by = c("project_name"="name")) 

### added the total costs for the different asset categories >>>

assets_with_projects2_bridges <- assets_with_projects2 %>% filter(measure == "Total_Cost")
assets_with_projects2_bridges <- assets_with_projects2_bridges %>% mutate(value = ifelse(`Asset Category` == "Bridges",value, 0))
assets_with_projects2_bridges$measure <- gsub('Total_Cost', 'Total_Cost_Bridges', assets_with_projects2_bridges$measure)

assets_with_projects2_pavement <- assets_with_projects2 %>% filter(measure == "Total_Cost")
assets_with_projects2_pavement <- assets_with_projects2_pavement %>% mutate(value = ifelse(`Asset Category` == "Pavement",value, 0))
assets_with_projects2_pavement$measure <- gsub('Total_Cost', 'Total_Cost_Pavement', assets_with_projects2_pavement$measure)

assets_with_projects2_corridor <- assets_with_projects2 %>% filter(measure == "Total_Cost")
assets_with_projects2_corridor <- assets_with_projects2_corridor %>% mutate(value = ifelse(`Asset Category` == "Corridor",value, 0))
assets_with_projects2_corridor$measure <- gsub('Total_Cost', 'Total_Cost_Corridor', assets_with_projects2_corridor$measure)

assets_with_projects2_its <- assets_with_projects2 %>% filter(measure == "Total_Cost")
assets_with_projects2_its <- assets_with_projects2_its %>% mutate(value = ifelse(`Asset Category` == "ITS",value, 0))
assets_with_projects2_its$measure <- gsub('Total_Cost', 'Total_Cost_ITS', assets_with_projects2_its$measure)

assets_with_projects2_sol_one <- rbind(assets_with_projects2_bridges,assets_with_projects2_corridor,
                                   assets_with_projects2_pavement,assets_with_projects2_its,assets_with_projects2) %>% 
  mutate(solution = case_when(substring(measure,1,3) == "POF" ~ 0, 
                              substring(measure,1,5) == "Total" ~ 1))

assets_with_projects2_sol_two <- rbind(assets_with_projects2_bridges,assets_with_projects2_corridor,
                                       assets_with_projects2_pavement,assets_with_projects2_its,assets_with_projects2) %>% 
  mutate(solution = case_when(substring(measure,1,3) == "POF" ~ 1, 
                              substring(measure,1,5) == "Total" ~ 0),
         value = case_when(substring(measure,1,3) == "POF" ~ 0, 
                              substring(measure,1,5) == "Total" ~ 0))

assets_with_projects2_all <- rbind(assets_with_projects2_sol_one,assets_with_projects2_sol_two)
         
# write the above


openxlsx::write.xlsx(assets_with_projects2_all, "C:/Users/TOsosanya/Desktop/NSW/UVO/SRAPC V2 Model Files/Backlog Sc1 measure.xlsx")  





# # AVAILABILITY SUMMARY # ---------------------------------------------------------------------------
# # add in Sc_One_Int Y1 to Y10...
# ## for the above and those without project names...
# 
# # get backlog reference Sc1_y1 ----------------------------------------------------------------------
# 
# path_pp <- "C:/Users/TOsosanya/Desktop/NSW/Asset port"
# assgnd_p_p_bl <- read_excel(paste0(path_pp, "/Backlog Reference.xlsx"), range = "A1:FN1596" ,sheet = "Pavement")
# assgnd_p_c_bl <- read_excel(paste0(path_pp, "/Backlog Reference.xlsx"), range = "A1:FN4703" ,sheet = "Corridor")
# assgnd_p_b_bl <- read_excel(paste0(path_pp, "/Backlog Reference.xlsx"), range = "A1:FN450" ,sheet = "Bridges")
# assgnd_p_i_bl <- read_excel(paste0(path_pp, "/Backlog Reference.xlsx"), range = "A1:FN3118" ,sheet = "ITS")
# 
# path_b <- "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Bridges"
# path_p <- "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Pavements"
# path_c <- "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Corridors"
# path_i <- "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/ITS"
# 
# result_p_sc1 <- read_excel(paste0(path_p, "/Pavement results post_intervention.xlsx"))
# result_b_sc1 <- read_excel(paste0(path_b, "/Bridge results post_intervention.xlsx"))
# result_c_sc1 <- read_excel(paste0(path_c, "/Corridor results post_intervention.xlsx"))
# result_i_sc1 <- read_excel(paste0(path_i, "/ITS results post_intervention.xlsx"))
# 
# result_all_sc1 <- plyr::rbind.fill(result_p_sc1,result_b_sc1,result_c_sc1,result_i_sc1)
# 
# # pavements >>>
# 
# p_bl_cols <- c("AssetID","Sc_One_IntY1","Sc_One_IntY2","Sc_One_IntY3","Sc_One_IntY4","Sc_One_IntY5",
#                "Sc_One_IntY6","Sc_One_IntY7", "Sc_One_IntY8", "Sc_One_IntY9", "Sc_One_IntY10")
# 
# result_p_sc1_joined <- result_p_sc1 %>% mutate(AssetID = as.character(AssetID)) %>% inner_join(assgnd_p_p_bl[,p_bl_cols], by = "AssetID")
# threshold_p = 1.19999999
# get_sc_year_logic <- function(input, threshold){
#   res <- input %>%
#     mutate(Sc_One_IntY1 = ifelse(Sc_One_POF1 > threshold,0,1),
#            Sc_One_IntY2 = ifelse(Sc_One_POF2 > threshold,0,1),
#            Sc_One_IntY3 = ifelse(Sc_One_POF3 > threshold,0,1),
#            Sc_One_IntY4 = ifelse(Sc_One_POF4 > threshold,0,1),
#            Sc_One_IntY5 = ifelse(Sc_One_POF5 > threshold,0,1),
#            Sc_One_IntY6 = ifelse(Sc_One_POF6 > threshold,0,1),
#            Sc_One_IntY7 = ifelse(Sc_One_POF7 > threshold,0,1),
#            Sc_One_IntY8 = ifelse(Sc_One_POF8 > threshold,0,1),
#            Sc_One_IntY9 = ifelse(Sc_One_POF9 > threshold,0,1),
#            Sc_One_IntY10 = ifelse(Sc_One_POF10 > threshold,0,1)) %>% 
#     as.data.frame()
#   return(res)
#   
# }
# 
# result_p_sc1_joined2 <- get_sc_year_logic(result_p_sc1_joined,threshold_p)
# 
# # bridges >>>
# 
# b_bl_cols <- c("AssetID","Project Name","Sc_One_IntY1","Sc_One_IntY2","Sc_One_IntY3","Sc_One_IntY4","Sc_One_IntY5",
#                "Sc_One_IntY6","Sc_One_IntY7", "Sc_One_IntY8", "Sc_One_IntY9", "Sc_One_IntY10")
# 
# result_b_sc1_joined <- result_b_sc1 %>% mutate(AssetID = paste0(BNO,"_",as.character(ObjectID),"_",as.character(ROAD_NUMBER))) %>% 
#   inner_join(unique(assgnd_p_b_bl[,b_bl_cols]), by = c("AssetID","Project Name"))
# 
# threshold_b = 1
# 
# result_b_sc1_joined2 <- get_sc_year_logic(result_b_sc1_joined,threshold_b)
# 
# # corridors >>>
# 
# c_bl_cols <- c("AssetID","Sc_One_IntY1","Sc_One_IntY2","Sc_One_IntY3","Sc_One_IntY4","Sc_One_IntY5",
#                "Sc_One_IntY6","Sc_One_IntY7", "Sc_One_IntY8", "Sc_One_IntY9", "Sc_One_IntY10")
# assgnd_p_c_bl[,c_bl_cols] <- assgnd_p_c_bl[,c_bl_cols] %>% mutate(AssetID = as.character(AssetID))
# result_c_sc1_joined <- result_c_sc1 %>% mutate(AssetID = as.character(`Asset Number`)) %>% 
#   inner_join(assgnd_p_c_bl[,c_bl_cols], by = "AssetID")
# threshold_c = 2
# 
# result_c_sc1_joined2 <- get_sc_year_logic(result_c_sc1_joined,threshold_c)
# 
# 
# # ITS >>>
# 
# i_bl_cols <- c("AssetID","Project Name","Sc_One_IntY1","Sc_One_IntY2","Sc_One_IntY3","Sc_One_IntY4","Sc_One_IntY5",
#                "Sc_One_IntY6","Sc_One_IntY7", "Sc_One_IntY8", "Sc_One_IntY9", "Sc_One_IntY10")
# result_i_sc1_joined <- result_i_sc1 %>% 
#   #mutate(AssetID = ifelse(!is.na(TCS_ID),as.character(TCS_ID),as.character(LABEL))) %>% 
#   inner_join(assgnd_p_i_bl[,i_bl_cols], by = "Project Name")
# threshold_i = 2
# 
# result_i_sc1_joined2 <- get_sc_year_logic(result_i_sc1_joined,threshold_i)
# 
# 
# cols_rawpm <- c("AssetID","Project Name","Sc_One_POF1", "Sc_One_POF2", "Sc_One_POF3","Sc_One_POF4",
#                 "Sc_One_POF5", "Sc_One_POF6","Sc_One_POF7", "Sc_One_POF8", "Sc_One_POF9","Sc_One_POF10",
#                 "Sc_One_IntY1","Sc_One_IntY2","Sc_One_IntY3","Sc_One_IntY4","Sc_One_IntY5",
#                 "Sc_One_IntY6","Sc_One_IntY7", "Sc_One_IntY8", "Sc_One_IntY9", "Sc_One_IntY10")
# result_all_with_p_merged <- rbind(result_p_sc1_joined2[,cols_rawpm],result_b_sc1_joined2[,cols_rawpm],
#                                   result_c_sc1_joined2[,cols_rawpm],result_i_sc1_joined2[,cols_rawpm])  
# result_all_with_p_merged$Scenario <- "Scenario 1"



# AVAILABILITY SUMMARY # ---------------------------------------------------------------------------
# add in Sc_One_Int Y1 to Y10...
## for the above and those without project names...


# get no investment Sc1_y1 ----------------------------------------------------------------------



path_pp <- "C:/Users/TOsosanya/Desktop/NSW/Asset port"
assgnd_p_p_bl <- read_excel(paste0(path_pp, "/Backlog Reference.xlsx"), range = "A1:FN1596" ,sheet = "Pavement")
assgnd_p_c_bl <- read_excel(paste0(path_pp, "/Backlog Reference.xlsx"), range = "A1:FN4703" ,sheet = "Corridor")
assgnd_p_b_bl <- read_excel(paste0(path_pp, "/Backlog Reference.xlsx"), range = "A1:FN450" ,sheet = "Bridges")
assgnd_p_i_bl <- read_excel(paste0(path_pp, "/Backlog Reference.xlsx"), range = "A1:FN3118" ,sheet = "ITS")


# pavements >>>

p_bl_cols_npi <- c("AssetID","Asset Group","Project Name","ConOps_Rating","POF1","POF2","POF3","POF4","POF5","POF6","POF7","POF8","POF9","POF10",
                   "Sc_One_IntY1","Sc_One_IntY2","Sc_One_IntY3","Sc_One_IntY4","Sc_One_IntY5",
                   "Sc_One_IntY6","Sc_One_IntY7", "Sc_One_IntY8", "Sc_One_IntY9", "Sc_One_IntY10")
assgnd_p_p_bl$`Asset Group` <- "Pavement asset management"
assgnd_p_p_bl <- assgnd_p_p_bl[,p_bl_cols_npi] %>% mutate(AssetID = as.character(AssetID))
threshold_p = 1.19999999
get_sc_year_logic_npi <- function(input, threshold){
  res <- input %>%
    mutate(Sc_One_IntY1 = ifelse(POF1 > threshold,0,1),
           Sc_One_IntY2 = ifelse(POF2 > threshold,0,1),
           Sc_One_IntY3 = ifelse(POF3 > threshold,0,1),
           Sc_One_IntY4 = ifelse(POF4 > threshold,0,1),
           Sc_One_IntY5 = ifelse(POF5 > threshold,0,1),
           Sc_One_IntY6 = ifelse(POF6 > threshold,0,1),
           Sc_One_IntY7 = ifelse(POF7 > threshold,0,1),
           Sc_One_IntY8 = ifelse(POF8 > threshold,0,1),
           Sc_One_IntY9 = ifelse(POF9 > threshold,0,1),
           Sc_One_IntY10 = ifelse(POF10 > threshold,0,1)) %>% 
    as.data.frame()
  return(res)
  
}
result_assgnd_p_p_bl <- get_sc_year_logic_npi(assgnd_p_p_bl,threshold_p)

# bridges >>>

b_bl_cols_npi <- c("AssetID","Asset Group","Project Name","ConOps_Rating","POF1","POF2","POF3","POF4","POF5","POF6","POF7","POF8","POF9","POF10",
                   "Sc_One_IntY1","Sc_One_IntY2","Sc_One_IntY3","Sc_One_IntY4","Sc_One_IntY5",
                   "Sc_One_IntY6","Sc_One_IntY7", "Sc_One_IntY8", "Sc_One_IntY9", "Sc_One_IntY10")
assgnd_p_b_bl$`Asset Group` <- "Bridge asset management"
assgnd_p_b_bl <- assgnd_p_b_bl[,b_bl_cols_npi] %>% mutate(AssetID = as.character(AssetID))
threshold_b = 1
result_assgnd_p_b_bl <- get_sc_year_logic_npi(assgnd_p_b_bl,threshold_b)

# corridors >>>

c_bl_cols_npi <- c("AssetID","Asset Group","Project Name","ConOps_Rating","POF1","POF2","POF3","POF4","POF5","POF6","POF7","POF8","POF9","POF10",
                   "Sc_One_IntY1","Sc_One_IntY2","Sc_One_IntY3","Sc_One_IntY4","Sc_One_IntY5",
                   "Sc_One_IntY6","Sc_One_IntY7", "Sc_One_IntY8", "Sc_One_IntY9", "Sc_One_IntY10")
assgnd_p_c_bl$`Asset Group` <- "Corridor asset management"
assgnd_p_c_bl <- assgnd_p_c_bl[,c_bl_cols_npi] %>% mutate(AssetID = as.character(AssetID))
threshold_c = 2
result_assgnd_p_c_bl <- get_sc_year_logic_npi(assgnd_p_c_bl,threshold_c)
result_assgnd_p_c_bl %>% filter(!is.na(`Project Name`))

# ITS >>>

i_bl_cols_npi <- c("AssetID","Asset Group","Project Name","ConOps_Rating","POF1","POF2","POF3","POF4","POF5","POF6","POF7","POF8","POF9","POF10",
                   "Sc_One_IntY1","Sc_One_IntY2","Sc_One_IntY3","Sc_One_IntY4","Sc_One_IntY5",
                   "Sc_One_IntY6","Sc_One_IntY7", "Sc_One_IntY8", "Sc_One_IntY9", "Sc_One_IntY10")
assgnd_p_i_bl$`Asset Group` <- "ITS asset management"
assgnd_p_i_bl <- assgnd_p_i_bl[,i_bl_cols_npi] %>% mutate(AssetID = as.character(AssetID)) 
threshold_i = 2
result_assgnd_p_i_bl <- get_sc_year_logic_npi(assgnd_p_i_bl,threshold_i)



# merge the above

result_all_npi_merged <- rbind(result_assgnd_p_p_bl[,p_bl_cols_npi],result_assgnd_p_b_bl[,b_bl_cols_npi],
                               result_assgnd_p_c_bl[,c_bl_cols_npi],result_assgnd_p_i_bl[,i_bl_cols_npi])  
result_all_npi_merged$Scenario <- "No Investment"

# hard fix for "Backlog Project 82701 Corridor" and remove the one already marked as NA
result_all_npi_merged <- result_all_npi_merged %>%
  filter(AssetID != "82701") %>% 
  mutate(`Project Name` = ifelse(`Project Name`=="Backlog Project 82701 Corridor",NA,`Project Name`))

write.csv(result_all_npi_merged,"C:/Users/TOsosanya/Desktop/NSW/UVO/Availability summary_No Investment.csv", row.names = FALSE)

# will use this down the line

result_no_project_name <- result_all_npi_merged %>% filter(is.na(`Project Name`))




# get backlog reference Sc1_y1 ----------------------------------------------------------------------

path_pp <- "C:/Users/TOsosanya/Desktop/NSW/Asset port"
assgnd_p_p_bl <- read_excel(paste0(path_pp, "/Backlog Reference.xlsx"), range = "A1:FN1596" ,sheet = "Pavement")
assgnd_p_c_bl <- read_excel(paste0(path_pp, "/Backlog Reference.xlsx"), range = "A1:FN4703" ,sheet = "Corridor")
assgnd_p_b_bl <- read_excel(paste0(path_pp, "/Backlog Reference.xlsx"), range = "A1:FN450" ,sheet = "Bridges")
assgnd_p_i_bl <- read_excel(paste0(path_pp, "/Backlog Reference.xlsx"), range = "A1:FN3118" ,sheet = "ITS")

path_b <- "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Bridges"
path_p <- "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Pavements"
path_c <- "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Corridors"
path_i <- "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/ITS"

result_p_sc1 <- read_excel(paste0(path_p, "/Pavement results post_intervention.xlsx"))
result_b_sc1 <- read_excel(paste0(path_b, "/Bridge results post_intervention.xlsx"))
result_c_sc1 <- read_excel(paste0(path_c, "/Corridor results post_intervention.xlsx"))
result_i_sc1 <- read_excel(paste0(path_i, "/ITS results post_intervention.xlsx"))

result_all_sc1 <- plyr::rbind.fill(result_p_sc1,result_b_sc1,result_c_sc1,result_i_sc1)

# pavements >>>

p_bl_cols <- c("AssetID","Asset Group","Sc_One_IntY1","Sc_One_IntY2","Sc_One_IntY3","Sc_One_IntY4","Sc_One_IntY5",
               "Sc_One_IntY6","Sc_One_IntY7", "Sc_One_IntY8", "Sc_One_IntY9", "Sc_One_IntY10")

result_p_sc1_joined <- result_p_sc1 %>% mutate(AssetID = as.character(AssetID)) %>% inner_join(assgnd_p_p_bl[,p_bl_cols], by = "AssetID")
threshold_p = 1.19999999
get_sc_year_logic <- function(input, threshold){
  res <- input %>%
    mutate(Sc_One_IntY1 = ifelse(Sc_One_POF1 > threshold,0,1),
           Sc_One_IntY2 = ifelse(Sc_One_POF2 > threshold,0,1),
           Sc_One_IntY3 = ifelse(Sc_One_POF3 > threshold,0,1),
           Sc_One_IntY4 = ifelse(Sc_One_POF4 > threshold,0,1),
           Sc_One_IntY5 = ifelse(Sc_One_POF5 > threshold,0,1),
           Sc_One_IntY6 = ifelse(Sc_One_POF6 > threshold,0,1),
           Sc_One_IntY7 = ifelse(Sc_One_POF7 > threshold,0,1),
           Sc_One_IntY8 = ifelse(Sc_One_POF8 > threshold,0,1),
           Sc_One_IntY9 = ifelse(Sc_One_POF9 > threshold,0,1),
           Sc_One_IntY10 = ifelse(Sc_One_POF10 > threshold,0,1)) %>% 
    as.data.frame()
  return(res)
  
}

result_p_sc1_joined2 <- get_sc_year_logic(result_p_sc1_joined,threshold_p)



# bridges >>>

b_bl_cols <- c("AssetID","Asset Group","Project Name","ConOps_Rating","Sc_One_IntY1","Sc_One_IntY2","Sc_One_IntY3","Sc_One_IntY4","Sc_One_IntY5",
               "Sc_One_IntY6","Sc_One_IntY7", "Sc_One_IntY8", "Sc_One_IntY9", "Sc_One_IntY10")

result_b_sc1_joined <- result_b_sc1 %>% mutate(AssetID = paste0(BNO,"_",as.character(ObjectID),"_",as.character(ROAD_NUMBER))) %>% 
  inner_join(unique(assgnd_p_b_bl[,b_bl_cols]), by = c("AssetID","Project Name"))

threshold_b = 1

result_b_sc1_joined2 <- get_sc_year_logic(result_b_sc1_joined,threshold_b)

# corridors >>> make all scY1-10 0 if arl = 1 or 2

# c_bl_cols <- c("AssetID","Sc_One_IntY1","Sc_One_IntY2","Sc_One_IntY3","Sc_One_IntY4","Sc_One_IntY5",
#                "Sc_One_IntY6","Sc_One_IntY7", "Sc_One_IntY8", "Sc_One_IntY9", "Sc_One_IntY10")
# assgnd_p_c_bl[,c_bl_cols] <- assgnd_p_c_bl[,c_bl_cols] %>% mutate(AssetID = as.character(AssetID))
# result_c_sc1_joined <- result_c_sc1 %>% mutate(AssetID = as.character(`Asset Number`)) %>% 
#   inner_join(assgnd_p_c_bl[,c_bl_cols], by = "AssetID")
# threshold_c = 2
# 
# result_c_sc1_joined2 <- get_sc_year_logic(result_c_sc1_joined,threshold_c)

c_bl_cols_npi <- c("AssetID","Asset Group","Project Name","ConOps_Rating","Sc_One_IntY1","Sc_One_IntY2","Sc_One_IntY3","Sc_One_IntY4","Sc_One_IntY5",
                   "Sc_One_IntY6","Sc_One_IntY7", "Sc_One_IntY8", "Sc_One_IntY9", "Sc_One_IntY10")
result_c_sc1$`Asset Group` <- "Corridor asset management"
result_c_sc1_joined <- result_c_sc1 %>% 
  mutate(`AssetID`=`Asset Number`) %>% 
  #left_join(assgnd_p_c_bl[,c_bl_cols_npi],by = c("Asset Number","AssetID")) %>% 
  mutate(Sc_One_IntY1 = ifelse(ARL == "1" | ARL == "2", 0, 1),
          Sc_One_IntY2 = ifelse(ARL == "1" | ARL == "2", 0, 1),
          Sc_One_IntY3 = ifelse(ARL == "1" | ARL == "2", 0, 1),
          Sc_One_IntY4 = ifelse(ARL == "1" | ARL == "2", 0, 1),
          Sc_One_IntY5 = ifelse(ARL == "1" | ARL == "2", 0, 1),
          Sc_One_IntY6 = ifelse(ARL == "1" | ARL == "2", 0, 1),
          Sc_One_IntY7 = ifelse(ARL == "1" | ARL == "2", 0, 1),
          Sc_One_IntY8 = ifelse(ARL == "1" | ARL == "2", 0, 1),
          Sc_One_IntY9 = ifelse(ARL == "1" | ARL == "2", 0, 1),
          Sc_One_IntY10 = ifelse(ARL == "1" | ARL == "2", 0, 1)) %>% 
  as.data.frame()
assgnd_p_c_bl$AssetID <- as.character(assgnd_p_c_bl$AssetID)
result_c_sc1_joined <- result_c_sc1_joined %>% left_join(assgnd_p_c_bl[,c("AssetID","ConOps_Rating")], by = ("AssetID"))
result_c_sc1_joined2 <- result_c_sc1_joined[,c_bl_cols_npi]


# ITS >>>

i_bl_cols <- c("AssetID","Asset Group","Project Name","ConOps_Rating","Sc_One_IntY1","Sc_One_IntY2","Sc_One_IntY3","Sc_One_IntY4","Sc_One_IntY5",
               "Sc_One_IntY6","Sc_One_IntY7", "Sc_One_IntY8", "Sc_One_IntY9", "Sc_One_IntY10")
result_i_sc1_joined <- result_i_sc1 %>% 
  #mutate(AssetID = ifelse(!is.na(TCS_ID),as.character(TCS_ID),as.character(LABEL))) %>% 
  inner_join(assgnd_p_i_bl[,i_bl_cols], by = "Project Name")
threshold_i = 2

result_i_sc1_joined2 <- get_sc_year_logic(result_i_sc1_joined,threshold_i)


cols_rawpm <- c("AssetID","Asset Group","Project Name","ConOps_Rating",
                #"Sc_One_POF1", "Sc_One_POF2", "Sc_One_POF3","Sc_One_POF4",
                #"Sc_One_POF5", "Sc_One_POF6","Sc_One_POF7", "Sc_One_POF8", "Sc_One_POF9","Sc_One_POF10",
                "Sc_One_IntY1","Sc_One_IntY2","Sc_One_IntY3","Sc_One_IntY4","Sc_One_IntY5",
                "Sc_One_IntY6","Sc_One_IntY7", "Sc_One_IntY8", "Sc_One_IntY9", "Sc_One_IntY10")
result_all_with_p_merged <- rbind(result_p_sc1_joined2[,cols_rawpm],result_b_sc1_joined2[,cols_rawpm],
                                  result_c_sc1_joined2[,cols_rawpm],result_i_sc1_joined2[,cols_rawpm]) 

result_all_with_p_merged$Scenario <- "Scenario 1"

final_cols <- c("AssetID","Asset Group", "ConOps_Rating","Project Name", "Sc_One_IntY1", "Sc_One_IntY2", "Sc_One_IntY3", "Sc_One_IntY4", 
                "Sc_One_IntY5", "Sc_One_IntY6", "Sc_One_IntY7","Sc_One_IntY8", "Sc_One_IntY9", "Sc_One_IntY10","Scenario")

final_result_sc1 <- rbind(result_all_with_p_merged,result_no_project_name[,final_cols]) %>% 
  mutate(Scenario = "Scenario 1")

write.csv(final_result_sc1,"C:/Users/TOsosanya/Desktop/NSW/UVO/Availability summary_Scenario 1.csv", row.names = FALSE)

