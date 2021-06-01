library(data.table)
library(DescTools)
library(dplyr)
library(magrittr)
library(lubridate)


# starts here -------------------------------------------------------------------------------


path_B <- "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Bridges"
assets1 <- read.csv(paste0(path_B, "/Brdiges, Tunnels and Bridge Sized Culverts Condition Rating.csv"))
assets2 <- assets1 %>% dplyr::select(-c(2,5,6,14)) %>%
  dplyr::filter(`Proposed.Zone` == "River Zone") %>%
  mutate(BNO = as.character(BNO)) %>%
  dplyr::select(-c(2)) %>% 
  mutate(UniqueID = paste0(BNO,"_",Element.Type))

assets2$Insp.Date <- dmy(assets2$Insp.Date)

assets_filtered1 <- assets2 %>% 
  filter(State.1 != "0") 
  


Get_State <- function(x = State.1, State.No = 1){
  for(j in 1:length(x)){

      
      y = x[j] # first row state 1
      fill = integer(y)   
      AssetNumber = seq(1,y,1)
      State = State.No # for state 1
      UID = paste0(BNO,"_",Element.Type)[j]
      fill = cbind(AssetNumber, State, UID) %>% as.data.frame()

    if (j == 1) {
      output.data.test <- fill
    } else
    {
      output.data.test <-
        rbind(output.data.test, fill)
    }
  }
  return(output.data.test)
}

assets2 %>% summarise(State.1 = sum(State.1),
                      State.2 = sum(State.2),
                      State.3 = sum(State.3),
                      State.4 = sum(State.4))


attach(assets_filtered1)
x = NULL
fill = NULL
state1.info <- Get_State(State.1,1)  %>% 
  rename(State.1 = State)


assets_filtered2 <- assets2 %>% 
  filter(State.2 != "0")
x = NULL
fill = NULL
attach(assets_filtered2)
state2.info <- Get_State(State.2,1)  %>% 
  rename(State.2 = State)


assets_filtered3 <- assets2 %>% 
  filter(State.3 != "0")
x = NULL
fill = NULL
attach(assets_filtered3)
state3.info <- Get_State(State.3,1)  %>% 
  rename(State.3 = State)

assets_filtered4 <- assets2 %>% 
  filter(State.4 != "0")
x = NULL
fill = NULL
attach(assets_filtered4)
state4.info <- Get_State(State.4,1) %>% 
  rename(State.4 = State)

state_full <- plyr::rbind.fill(state1.info[,2:3],state2.info[,2:3],state3.info[,2:3],state4.info[,2:3]) %>% 
  mutate(State.1 = 1) %>% # >> changed
  mutate(State.2 = 0) %>% 
  mutate(State.3 = 0) %>% 
  mutate(State.4 = 0) 
str(state_full)
state_full[is.na(state_full)] <- 0
state_full$index <- seq.int(nrow(state_full))
state_full <- state_full%>% 
  dplyr::select(index, UID,State.1,State.2,State.3,State.4)

# how do i deteriorate elements by 0.12% from state1 to state 2, selecting random assets everytime step ------------------------
## 1 - 2

Apply_Transmatrix <- function(state_full = state_full){

  ## 3 - 4
  
  set.seed(round(runif(4)*100,0)[3])
  state_full_3 <- state_full %>% filter(State.3 == "1") # the data from previous step is used
  train.base3 <- sample(1:nrow(state_full_3),(nrow(state_full_3)*0.0035),replace = FALSE)
  traindata.base3 <- state_full_3[train.base3, ]
  testdata.base3 <- state_full_3[-train.base3, ]
  traindata.base3 <- traindata.base3 %>% 
    mutate(State.3 = 0,
           State.4 = 1)
  Three_done <- rbind(testdata.base3,traindata.base3)
  Three_leftover <- state_full %>% anti_join(Three_done, by = "index") # join back the rows that are not in state 1
  Three_done <- rbind(Three_done,Three_leftover)
  
  ## 2 - 3
  
  set.seed(round(runif(4)*100,0)[2])
  state_full_2 <- Three_done %>% filter(State.2 == "1") # the data from previous step is used
  train.base2 <- sample(1:nrow(state_full_2),(nrow(state_full_2)*0.0086),replace = FALSE)
  traindata.base2 <- state_full_2[train.base2, ]
  testdata.base2 <- state_full_2[-train.base2, ]
  traindata.base2 <- traindata.base2 %>% 
    mutate(State.2 = 0,
           State.3 = 1)
  Two_done <- rbind(testdata.base2,traindata.base2)
  Two_leftover <- Three_done %>% anti_join(Two_done, by = "index") # join back the rows that are not in state 1
  Two_done <- rbind(Two_done,Two_leftover)
  
  
  ## 1 - 2
  
  set.seed(round(runif(4)*100,0)[1])
  # set.seed(3)
  state_full_1 <- Two_done %>% filter(State.1 == "1")
  train.base <- sample(1:nrow(state_full_1),(nrow(state_full_1)*0.0012),replace = FALSE)
  traindata.base <- state_full_1[train.base, ]
  testdata.base <- state_full_1[-train.base, ]
  traindata.base <- traindata.base %>% 
    mutate(State.1 = 0,
           State.2 = 1)
  One_done <- rbind(testdata.base,traindata.base)
  One_leftover <- Two_done %>% anti_join(One_done, by = "index") # join back the rows that are not in state 1
  One_done <- rbind(One_done,One_leftover)
  
  return(One_done)
}

T1_x <- Apply_Transmatrix(state_full)
T2_x <- Apply_Transmatrix(T1_x)
T3_x <- Apply_Transmatrix(T2_x)
T4_x <- Apply_Transmatrix(T3_x)
T5_x <- Apply_Transmatrix(T4_x)
T6_x <- Apply_Transmatrix(T5_x)
T7_x <- Apply_Transmatrix(T6_x)
T8_x <- Apply_Transmatrix(T7_x)
T9_x <- Apply_Transmatrix(T8_x)
T10_x <- Apply_Transmatrix(T9_x)


assets2 %>% summarise(State.1 = sum(State.1),
                   State.2 = sum(State.2),
                   State.3 = sum(State.3),
                   State.4 = sum(State.4))
T1_x %>% summarise(State.1 = sum(State.1),
                      State.2 = sum(State.2),
                      State.3 = sum(State.3),
                      State.4 = sum(State.4))
T2_x %>% summarise(State.1 = sum(State.1),
                   State.2 = sum(State.2),
                   State.3 = sum(State.3),
                   State.4 = sum(State.4))
T3_x %>% summarise(State.1 = sum(State.1),
                   State.2 = sum(State.2),
                   State.3 = sum(State.3),
                   State.4 = sum(State.4))
T4_x %>% summarise(State.1 = sum(State.1),
                   State.2 = sum(State.2),
                   State.3 = sum(State.3),
                   State.4 = sum(State.4))
T5_x %>% summarise(State.1 = sum(State.1),
                   State.2 = sum(State.2),
                   State.3 = sum(State.3),
                   State.4 = sum(State.4))
T6_x %>% summarise(State.1 = sum(State.1),
                   State.2 = sum(State.2),
                   State.3 = sum(State.3),
                   State.4 = sum(State.4))
T7_x %>% summarise(State.1 = sum(State.1),
                   State.2 = sum(State.2),
                   State.3 = sum(State.3),
                   State.4 = sum(State.4))
T8_x %>% summarise(State.1 = sum(State.1),
                   State.2 = sum(State.2),
                   State.3 = sum(State.3),
                   State.4 = sum(State.4))
T9_x %>% summarise(State.1 = sum(State.1),
                   State.2 = sum(State.2),
                   State.3 = sum(State.3),
                   State.4 = sum(State.4))
T10_x %>% summarise(State.1 = sum(State.1),
                   State.2 = sum(State.2),
                   State.3 = sum(State.3),
                   State.4 = sum(State.4))

# function 2
source("C:/Users/TOsosanya/Desktop/NSW/tfnsw/construct_BHI_tayo.R")

Transform_Elements <- function(data){
  data1 <- data %>% 
    mutate(c = str_split(UID, "_"),
           bno = purrr::map(c, 1),
           element_type = purrr::map(c, 2)) %>% 
    dplyr::select(-(c)) %>%
    group_by(UID) %>% 
    summarise(state_1 = sum(State.1),
              state_2 = sum(State.2),
              state_3 = sum(State.3),
              state_4 = sum(State.4)) %>% 
    as.data.frame() %>% 
    mutate(c = str_split(UID, "_"),
           bno = purrr::map(c, 1),
           element_type = purrr::map(c, 2),
           bno = as.integer(bno),
           element_type = as.character(element_type)) %>% 
    as.data.frame() %>% 
    dplyr::select(-(c))
  
  data2 <- data1 %>% 
    rename_all(~str_replace_all(., "\\s", "_") %>% tolower(.)) %>% 
    left_join(df[,1:9], by = c("bno","element_type")) %>% 
    dplyr::select(bno, bridge_description, proposed_zone, insp_date, initital, 
           insp_type, element_type, env,   qty_total, state_1, state_2, state_3, state_4) %>% # added
    as_tibble() # added
  
return(data2)
}

T1_x2 <- Transform_Elements(T1_x)
T2_x2 <- Transform_Elements(T2_x)
T3_x2 <- Transform_Elements(T3_x)
T4_x2 <- Transform_Elements(T4_x)
T5_x2 <- Transform_Elements(T5_x)
T6_x2 <- Transform_Elements(T6_x)
T7_x2 <- Transform_Elements(T7_x)
T8_x2 <- Transform_Elements(T8_x)
T9_x2 <- Transform_Elements(T9_x)
T10_x2 <- Transform_Elements(T10_x)

#### ADDITIONS

Get_BHI_Desc <- function(PoFData){
  folder = "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Bridges/"
  source(paste0(folder,"bhi_func.R"))
  bridges_load <- load_bridge_Robj(PoFData)
  bridges_calc <- bridges_load %>% calc_bhi(original_layout = F)
  
  # Cross check against the master table
  master <- read_xlsx(paste0(folder, "Bridge mastersheet.xlsx"))
  
  masterbhi <- master %>%
    dplyr::select(bno = BNO, bhi_code = "BHI CODE", project_name = `Project Name`,
           conops = ConOps_Score) %>%
    mutate(bno = as.numeric(bno),
           bhi = factor(bhi_code, levels = c(5,3,1,0),
                        labels = levels(bridges_calc$bhi),
                        ordered = T),
           project = ifelse(is.na(project_name), F, T))
  
  bridges_out <- bridges_calc %>% 
    group_by(bno, element_type, bhi_score) %>%
    na.omit() %>%
    distinct(bhi) %>%
    dplyr::select(bno, everything()) %>%
    left_join(masterbhi, by = "bno", suffix = c(".element", ".master"))
  
  # BHI aggregated at the bridge level
  bridge_agg <- bridges_out %>%
    group_by(bno) %>%
    summarise(bhi.bridge = min(bhi.element))
  
  # Feed bridge level bhi back in and rename
  bridges_out <- bridges_out %>%
    left_join(bridge_agg, by = "bno") %>%
    dplyr::select(bno, bhi.bridge, element_type, bhi.element, bhi_score, bhi.master, 
           everything())
  
  return(bridges_out)
}

PoFData0 <- Get_BHI_Desc(df)
PoFData1 <- Get_BHI_Desc(T1_x2)
PoFData2 <- Get_BHI_Desc(T2_x2)
PoFData3 <- Get_BHI_Desc(T3_x2)
PoFData4 <- Get_BHI_Desc(T4_x2)
PoFData5 <- Get_BHI_Desc(T5_x2)
PoFData6 <- Get_BHI_Desc(T6_x2)
PoFData7 <- Get_BHI_Desc(T7_x2)
PoFData8 <- Get_BHI_Desc(T8_x2)
PoFData9 <- Get_BHI_Desc(T9_x2)
PoFData10 <- Get_BHI_Desc(T10_x2)

PoFData1 %>% group_by(bhi.bridge) %>% 
  tally()
PoFData5 %>% group_by(bhi.bridge) %>% 
  tally()

######

create_uid <- function(data,column_name,score){
  #columns_pof <- c("bno","bhi.bridge","element_type")
    data1 <- data %>% 
      ungroup() %>% 
    dplyr::select(bno,bhi.bridge,element_type,bhi_score) %>% 
    mutate(UID = paste0(bno,"_",element_type)) %>%
    dplyr::select(UID,bhi.bridge,bhi_score)
    colnames(data1)[which(names(data1) == "bhi.bridge")] <- column_name
    colnames(data1)[which(names(data1) == "bhi_score")] <- score
  return(data1)
}

full_bhi <- create_uid(PoFData0,"POF0","score0") %>%  
  left_join(create_uid(PoFData1,"POF1","score1"), by = c("UID")) %>%
  left_join(create_uid(PoFData2,"POF2","score2"), by = c("UID")) %>%
  left_join(create_uid(PoFData3,"POF3","score3"), by = c("UID")) %>%
  left_join(create_uid(PoFData4,"POF4","score4"), by = c("UID")) %>%
  distinct() %>% 
  left_join(create_uid(PoFData5,"POF5","score5"), by = c("UID")) %>%
  left_join(create_uid(PoFData6,"POF6","score6"), by = c("UID")) %>%
  left_join(create_uid(PoFData7,"POF7","score7"), by = c("UID")) %>%
  left_join(create_uid(PoFData8,"POF8","score8"), by = c("UID")) %>%
  left_join(create_uid(PoFData9,"POF9","score9"), by = c("UID")) %>%
  left_join(create_uid(PoFData10,"POF10","score10"), by = c("UID")) %>%
  distinct() %>% 
  as.data.frame() %>% 
  mutate(c = str_split(UID, "_"),
         bno = purrr::map(c, 1),
         element_type = purrr::map(c, 2),
         bno = as.integer(bno),
         element_type = as.character(element_type),
         POF0 = 0) %>% # >> changed 
  dplyr::select(bno, POF0,POF1,POF2,POF3,POF4,POF5,POF6,POF7,POF8,POF9,POF10) %>% 
  #dplyr::select(bno, POF0:score9) %>% 
  distinct() %>% 
  as.data.frame()


full_bhi2 <- data.frame(lapply(full_bhi, function(x) {
   gsub("Poor", 1.00, x, fixed = T)
}))
full_bhi2 <- data.frame(lapply(full_bhi2, function(x) {
  gsub("Fair", 0.50, x, fixed = T)
}))
full_bhi2 <- data.frame(lapply(full_bhi2, function(x) {
  gsub("Good", 0.25, x, fixed = T)
}))
full_bhi2 <- data.frame(lapply(full_bhi2, function(x) {
  gsub("As-Built", 0.00, x, fixed = T)
}))

#write.csv(full_bhi2,paste0(path_B,"bridge results.csv"), row.names = F)
