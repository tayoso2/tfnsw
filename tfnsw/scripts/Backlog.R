# Scenario 1 - all FY21-FY23 and then the other project names with nnnyyyyyyy and then remaining assets

# Get the combined project and non proects and POF for different years - what is 70 in POF for Pavements
# Tak ethe non projects and do some justice
# the remainign assets will chaneg overtime
#


library(dplyr)
library(magrittr)
library(DescTools)
library(readxl)
library(tidyr)
library(data.table)
library(janitor)

# Pivot longer

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

result_all <- plyr::rbind.fill(result_p,result_c,result_b,result_i) %>% 
  mutate_at(c("POF1","POF2","POF3","POF4","POF5","POF6","POF7","POF8","POF9","POF10"),as.numeric) %>% 
  select(AssetID,`Asset Category`,everything())
result_all$`Asset Category` %>% unique()

openxlsx::write.xlsx(result_all, file = "C:/Users/TOsosanya/Desktop/NSW/Asset port/Backlog.xlsx")

# write a script which selects the projects with the maximum POF0 when there are duplicates

# pavement_UP <- all_p %>% 
#   na.omit() %>% 
#   inner_join(result_all, by = "Project Name")
