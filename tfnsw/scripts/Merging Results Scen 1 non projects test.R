library(dplyr)
library(magrittr)
library(DescTools)
library(readxl)
library(tidyr)
library(data.table)
library(janitor)


# corridors - slope and culvert

## The difference between Intervention stragey and minimum level - those replaced using intervention vs year 1,2,3

#test_p <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Pavements/pavement_assets ConOps Calcs.xlsx"),range = "A1:AF1596", sheet = "pavement_assets2")
result_p <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Pavements/Pavement results.xlsx"),range = "A1:HC1596", sheet = "Sheet 1")
path_pp <- "C:/Users/TOsosanya/Desktop/NSW/Asset port"
assgnd_p <- read_excel(paste0(path_pp, "/10.04.05 Tactical Works - RFT Addendum 3 (NF Aligned).xlsx"), range = "A1:F400" ,sheet = "DY 21-23")
unassgnd_p <- read_excel(paste0(path_pp, "/10.04.05 Tactical Works - RFT Addendum 3 (NF Aligned).xlsx"), range = "A1:F400" ,sheet = "DY 24-30")
all_p <- rbind(assgnd_p,unassgnd_p)

result_c <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Corridors/Corridor results.xlsx"),range = "A1:EY9138", sheet = "Sheet 1")
result_b <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Bridges/Bridges results.xlsx"),range = "A1:EY450", sheet = "Sheet 1")
result_i <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/ITS/ITS results.xlsx"),range = "A1:EY3118", sheet = "Sheet 1", guess_max = 5000)


# for all assets
result_p <- result_p %>% select(AssetID,`Project Name`,POF1:POF10,POF1_FMECA_CONOPS:POF10_FMECA_Political_CONOPS) %>% 
  mutate(`Asset Category` = "Pavement")
result_c <- result_c %>% 
  filter(`Asset_Type`=="Culvert"|`Asset_Type`=="Slope Site") %>% 
  select(`Asset Number`,`Asset Category`,`Project Name`,POF1:POF10,POF1_FMECA_CONOPS:POF10_FMECA_Political_CONOPS) %>% 
  rename(AssetID = `Asset Number`)
result_b <- result_b %>% select(BNO,ObjectID,ROAD_NUMBER,`Asset Category`,`Project Name`,POF1:POF10,POF1_FMECA_CONOPS:POF10_FMECA_Political_CONOPS)%>% 
  arrange(`Project Name`) %>% 
  mutate(AssetID = paste0(BNO,"_",ObjectID,"_",ROAD_NUMBER)) %>% 
  mutate(`Asset Category` = "Bridges")
result_i <- result_i %>%
  select(LABEL,`Project Name`,POF1:POF10,POF1_FMECA_CONOPS:POF10_FMECA_Political_CONOPS)%>% 
  mutate(`Asset Category` = "ITS") %>% 
  rename(AssetID = `LABEL`)

result_all <- plyr::rbind.fill(result_p,result_c,result_b,result_i)
result_all$`Asset Category` %>% unique()
# write a script which selects the projects with the maximum POF0 when there are duplicates

# pavement_UP <- all_p %>% 
#   na.omit() %>% 
#   inner_join(result_all, by = "Project Name")

s.t = "nnnnnnnnnn"
m.t = "FALSE"

# select the projects without project name and class as remaining assets_ITS/bridges etc.

pavement_UP <- result_all %>% 
  left_join(all_p, by = "Project Name") %>% 
  filter(is.na(`Project Name`)) %>% 
  mutate_at(c("POF1","POF2","POF3","POF4","POF5","POF6","POF7","POF8","POF9","POF10"),as.numeric) %>% 
  distinct()
pavement_UP$`Asset Category` %>% unique()
pavement_UP$`Project Name` %>% unique()

# load attributes for unit costing
p_up_cost <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Pavements/Copy of Tactical Buildup-26.06.2020.xlsx"),range = "A1:D6", sheet = "Averages") %>% 
  filter(`Works Type` == "Unassigned Projects") %>% select(`Total Cost`) %>% as.list()
c_up_cost <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Corridors/Corridor assets ConOps Calcs.xlsx"),range = "Q2:T22", sheet = "FY_TOTAL") %>% 
  filter(`Works Type` == "Unassigned Projects") %>% select(`Total Cost`) %>% as.list()
b_up_cost <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Bridges/Bridges assets ConOps Calcs.xlsx"),range = "N2:Q7", sheet = "FY_TOTAL") %>% 
  filter(`Works Type` == "Unassigned Projects") %>% select(`Total Cost`) %>% as.list()
i_up_cost <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/ITS/ITS assets ConOps Calcs.xlsx"),range = "N2:Q7", sheet = "FY_TOTAL") %>% 
  filter(`Works Type` == "Unassigned Projects") %>% select(`Total Cost`) %>% as.list()


# column sum

# add s.t,m.t, Total Cost

# assign the minimum total cost as total cost for unassigned projects

# p_UP <- pavement_UP %>% filter(`Asset Category`=="Pavement") %>% mutate(Total_Cost = p_up_cost[[1]])
# c_UP <- pavement_UP %>% filter(`Asset Category`=="Corridor")%>% mutate(Total_Cost = c_up_cost[[1]])
# b_UP <- pavement_UP %>% filter(`Asset Category`=="Bridges")%>% mutate(Total_Cost = b_up_cost[[1]])
# i_UP <- pavement_UP %>% filter(`Asset Category`=="ITS")%>% mutate(Total_Cost = i_up_cost[[1]])

p_UP <- pavement_UP %>% filter(`Asset Category`=="Pavement") %>% mutate(Total_Cost = 0)
c_UP <- pavement_UP %>% filter(`Asset Category`=="Corridor") %>% mutate(Total_Cost = 0)
b_UP <- pavement_UP %>% filter(`Asset Category`=="Bridges") %>% mutate(Total_Cost = 0)
i_UP <- pavement_UP %>% filter(`Asset Category`=="ITS") %>% mutate(Total_Cost = 0)


# sum up all the remainings

get_unassigned_projects <- function(data, U.P, A.C, A.G){
  gup <- data %>%
    select(`Project Name`,everything()) %>% 
    adorn_totals(where = "row", fill = "-", na.rm = TRUE, name = U.P) %>% 
    as.data.table() %>% 
    filter(`Project Name`== U.P) %>% 
    mutate(selection.timing = s.t,must = m.t,`Asset Category` = A.C, `Asset Group` = A.G )
  return(gup)
}

p_UP_2 <- get_unassigned_projects(p_UP, U.P = "Remaining Pavements", A.C = "Pavement", A.G = "Pavement asset management")
c_UP_2 <- get_unassigned_projects(c_UP, U.P = "Remaining Corridors", A.C = "Corridor", A.G = "Corridor asset management")
b_UP_2 <- get_unassigned_projects(b_UP, U.P = "Remaining Bridges", A.C = "Bridges", A.G = "Bridges asset management")
i_UP_2 <- get_unassigned_projects(i_UP, U.P = "Remaining ITS", A.C = "ITS", A.G = "ITS asset management")

all_UP <- rbind(p_UP_2,c_UP_2,b_UP_2,i_UP_2)

# order column names
function3 <- function(all_UP){
  ordered_columns <- c("Asset Category","Asset Group","must","Project Name","selection.timing","Total_Cost",
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
  
  
  all_UP_2 <- all_UP[,ordered_columns] %>% 
    pivot_longer(cols = POF1:POF10_FMECA_Political_CONOPS,
                 names_to = c("measure")
                 #names_pattern = "(.)(.)"
    ) %>% 
    group_by(`Project Name`) %>% 
    mutate(timestep = rep(1:10,11),
           value = round(value,1)) 
  all_UP_2$measure = gsub('[[:digit:]]+', '', all_UP_2$measure)
  all_UP_2 <- all_UP_2 %>% 
    as.data.table()
  
  
  
  
  all_UP_3 <- all_UP_2 %>% ungroup() %>% 
    group_by(`Project Name`, timestep) %>% 
    mutate(measure = "Selected",
           value = 0) %>% 
    distinct() %>% 
    ungroup() %>% 
    as.data.table()
  
  all_UP_4 <- rbind(all_UP_2,all_UP_3) %>% 
    mutate(solution = 0)
  
  all_UP_4 %>% as.data.table()
  
  # add solution = 1. i.e. if an action is taken
  
  all_UP_5 <- all_UP_4 %>%
    mutate(solution = 1,
           #value = ifelse(measure == "Selected",1,0)) 
           value = ifelse(measure == "Selected",1,value)) 
  
  up_all_x <- rbind(all_UP_4,all_UP_5) %>% 
    distinct() %>% 
    #filter(`Asset Group` != "ITS asset management") %>% 
    as.data.table()
  return(up_all_x)
}

up_all_x <- function3(all_UP)

#openxlsx::write.xlsx(up_all_x, file = "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Pavements/Pavement UVO 3.xlsx")



# END



up_names_id <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/UVO/Final-NSW-Scen1.xlsx"),range = "A1:C512", sheet = "id") %>%
  #filter(`Asset Group` != "ITS asset management") %>% 
  filter(grepl('Remaining ',`name`)) %>%
  select(1:2)

up_names_id %>% filter(`name` == "BN 257 Service Br ov Haslams Ck Rehab")
up_all_x %>% filter(grepl('BN 257 Serv',`Project Name`))

up_names_id <- up_names_id %>% left_join(up_all_x, by = c("name"="Project Name")) %>%
  rename(project_name = name) %>%
  select(project,project_name,measure,timestep,value, solution,selection.timing, must,Total_Cost) 


#


######
# Add total cost

# add the total cst bridge etc ----------------- REMAINING ASSETS

up_names_id_2 <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/UVO/Final-NSW-Scen1.xlsx"),range = "A1:C517", sheet = "id") %>% 
  #filter(`Asset Group` != "ITS asset management") %>% 
  select(1:2) %>% 
  filter(grepl('Remaining ',`name`))

function4 <- function(up_names_id_2){
  up_names_id_2 <- up_names_id_2 %>% left_join(up_all_x, by = c("name"="Project Name")) %>% 
    rename(project_name = name) %>% 
    select(project,project_name,`Asset Group`,measure,timestep,value, solution,selection.timing, must,Total_Cost)
  
  up_names_id_3 <- up_names_id_2 %>% left_join(all_UP_2[,c("Asset Group","Project Name")], by = "Asset Group") %>% 
    mutate(Total_Cost_Bridges = ifelse(`Asset Group` == "Bridge asset management",Total_Cost, 0)) %>%
    mutate(Total_Cost_Pavement = ifelse(`Asset Group` == "Pavement asset management", Total_Cost, 0)) %>%
    mutate(Total_Cost_Corridor = ifelse(`Asset Group` == "Corridor asset management", Total_Cost, 0))  %>%
    mutate(Total_Cost_ITS = ifelse(`Asset Group` == "ITS asset management", Total_Cost, 0))
  
  up_names_id_4 <- up_names_id_3 %>% 
    na.omit() %>% 
    select(project,project_name,Total_Cost, Total_Cost_Bridges:Total_Cost_ITS) %>% 
    group_by(project,project_name) %>% 
    rename(Total_Cost1 = Total_Cost) %>%
    rename(Total_Cost_Bridges1 = Total_Cost_Bridges) %>% 
    rename(Total_Cost_Pavement1 = Total_Cost_Pavement) %>% 
    rename(Total_Cost_Corridor1 = Total_Cost_Corridor) %>% 
    rename(Total_Cost_ITS1 = Total_Cost_ITS) %>% 
    distinct()
  
  up_names_id_4 <- up_names_id_4 %>% 
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
  up_names_id_4_3 <- up_names_id_4 %>%
    group_by(project) %>% 
    mutate(timestep = rep(1:10,5),
           value = round(value,1)) %>% 
    mutate(solution = 1) %>% 
    as.data.frame()
  
  up_names_id_4_3$measure = gsub('[[:digit:]]+', '', up_names_id_4_3$measure)
  
  # add solution = 0
  
  up_names_id_4_4 <- up_names_id_4_3 %>% ungroup() %>% 
    group_by(project, timestep) %>% 
    mutate(solution = 0,
           value = 0) %>% 
    distinct() %>% 
    ungroup() %>% 
    as.data.frame()
  
  up_names_id_4_5 <- rbind(up_names_id_4_3,up_names_id_4_4) %>% 
    inner_join(up_names_id[,c("project","selection.timing", "must")], by = "project") %>% 
    distinct()
  
  up_all_xx3 <- rbind(up_names_id[,1:8],up_names_id_4_5) %>% 
    filter(!is.na(measure))
  return(up_all_xx3)
}

up_all_xx3 <- function4(up_names_id_2)

####



#openxlsx::write.xlsx(up_all_xx3, file = "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Pavements/Pavement UVO 3 ids.xlsx")

