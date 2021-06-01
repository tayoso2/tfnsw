# Pivot longer

## The difference between Intervention stragey and minimum level - those replaced using intervention vs year 1,2,3

#test_p <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Pavements/pavement_assets ConOps Calcs.xlsx"),range = "A1:AF1596", sheet = "pavement_assets2")
result_p <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Pavements/Pavement results.xlsx"),range = "A1:HC1596", sheet = "Sheet 1")
path_pp <- "C:/Users/TOsosanya/Desktop/NSW/Asset port"
unassigned_pp <- read_excel(paste0(path_pp, "/10.04.05 Tactical Works - RFT Addendum 3 (NF Aligned).xlsx"), range = "A1:B600" ,sheet = "DY 24-29")

result_c <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Corridors/Corridor results.xlsx"),range = "A1:EY9138", sheet = "Sheet 1")
result_b <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Bridges/Bridges results.xlsx"),range = "A1:EY450", sheet = "Sheet 1")

# for all assets
result_p <- result_p %>% select(AssetID,`Project Name`,POF4:POF10,POF4_FMECA_CONOPS:POF10_FMECA_Political_CONOPS) %>% 
  mutate(`Asset Category` = "Pavement")
result_c <- result_c %>% select(`Asset Number`,`Asset Category`,`Project Name`,POF4:POF10,POF4_FMECA_CONOPS:POF10_FMECA_Political_CONOPS) %>% 
  rename(AssetID = `Asset Number`)
result_b <- result_b %>% select(BNO,`Asset Category`,`Project Name`,POF4:POF10,POF4_FMECA_CONOPS:POF10_FMECA_Political_CONOPS)%>% 
  arrange(`Project Name`) %>% 
  rename(AssetID = `BNO`)
result_b <- result_b[c(1:29,31,37:39,44,50),] # select unique bridge projects

result_all <- rbind(result_p,result_c,result_b)

# write a script which selects the projects with the maximum POF0 when there are duplicates

pavement_UP <- unassigned_pp %>% 
  na.omit() %>% 
  inner_join(result_all, by = "Project Name")

pavement_UP$AssetID %>% unique()
pavement_UP$`Project Name` %>% unique()


# remove duplicate projects retaining the max total impact score

pavement_UP2 <- pavement_UP %>% 
  group_by(`Project Name`) %>% 
  arrange(desc(POF4_FMECA_CONOPS)) %>% 
  mutate(rown = row_number(`Project Name`)) %>% 
  filter(rown == 1)

pavement_UP2 %>% filter(`Project Name` == "VICTORIA RD, ERMINGTON - R0165047000.000BU_20.002") %>% as.data.frame()

pavement_UP2x <- pavement_UP2 %>% 
  mutate_at(c("POF4","POF5","POF6","POF7","POF8","POF9","POF10"),as.numeric) %>% 
  pivot_longer(cols = POF4:POF10_FMECA_Political_CONOPS,
               names_to = c("measure")
               #names_pattern = "(.)(.)"
  ) %>% 
  group_by(AssetID) %>% 
  mutate(timestep = rep(4:10,11),
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
  mutate(timestep = timestep-3) %>% 
  distinct() %>% 
  as.data.table()

openxlsx::write.xlsx(pavement_all, file = "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Pavements/Pavement UVO 3.xlsx")

pnames_id <- read_excel(("C:/Users/TOsosanya/Desktop/NSW/UVO/Sample Final-NSW.xlsx"),range = "A1:B364", sheet = "id")

pnames_id <- pavement_all %>% left_join(pnames_id, by = c("Project Name"="name")) %>% 
  rename(project_name = `Project Name`) %>% 
  select(project,project_name,measure,timestep,value, solution)

openxlsx::write.xlsx(pnames_id, file = "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Pavements/Pavement UVO 3 ids.xlsx")

# # for pavements
# result_p <- result_p %>% select(AssetID,`Project Name`,POF1:POF10,POF1_FMECA_CONOPS:POF10_FMECA_CONOPS)
# result_p <- rbind(result_p)
# 
# pavement_UP <- unassigned_pp %>% 
#   na.omit() %>% 
#   inner_join(result_p, by = "Project Name")
# 
# pavement_UP2 <- pavement_UP %>% 
#   pivot_longer(cols = POF1:POF10_FMECA_CONOPS,
#                names_to = c("measure")
#                #names_pattern = "(.)(.)"
#   ) %>% 
#   group_by(AssetID) %>% 
#   mutate(timestep = c(1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10),
#          value = round(value,0),
#          measure = ifelse(nchar(measure) < 6, "POF","POF_FMECA_CONOPS")) %>% 
#   filter(timestep >= 4) %>% 
#   as.data.table()
# 
# # add selected which serves as UID
# 
# pavement_UP3 <- pavement_UP2 %>% ungroup() %>% 
#   group_by(`Project Name`, timestep) %>% 
#   mutate(measure = "Selected",
#          value = 0) %>% 
#   distinct() %>% 
#   ungroup() %>% 
#   as.data.table()
# 
# pavement_UP4 <- rbind(pavement_UP2,pavement_UP3) %>% 
#   mutate(solution = 0)
# 
# pavement_UP4 %>% as.data.table()
# 
# # add solution = 1. i.e. if an action is taken
# 
# pavement_UP5 <- pavement_UP4 %>%
#   mutate(solution = 1,
#          value = ifelse(measure == "Selected",1,0)) 
# 
# pavement_all <- rbind(pavement_UP4,pavement_UP5) %>% 
#   mutate(timestep = timestep-3) %>% 
#   distinct() %>% 
#   as.data.table()
# 
# openxlsx::write.xlsx(pavement_all, file = "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Pavements/Pavement UVO.xlsx")
