library(data.table)
library(DescTools)
library(dplyr)
library(magrittr)
library(lubridate)


# starts here -------------------------------------------------------------------------------


path_B <- "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Bridges"
assets1 <- read.csv(paste0(path_B, "/Brdiges, Tunnels and Bridge Sized Culverts Condition Rating.csv"))
assets2 <- assets1 %>% select(-c(2,5,6,14)) %>%
  dplyr::filter(`Proposed.Zone` == "River Zone") %>%
  mutate(BNO = as.character(BNO)) %>%
  select(-c(2)) %>% 
  mutate(UniqueID = paste0(BNO,"_",Element.Type))

assets2$Insp.Date <- dmy(assets2$Insp.Date)

assets_filtered1 <- assets2 %>% 
  filter(State.1 != "0") 
  


get_state <- function(x = State.1, State.No = 1){
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
  mutate(State.1 = as.integer(as.character(State.1))) %>% 
  mutate(State.2 = as.integer(as.character(State.2))) %>% 
  mutate(State.3 = as.integer(as.character(State.3))) %>% 
  mutate(State.4 = as.integer(as.character(State.4))) 
str(state_full)
state_full[is.na(state_full)] <- 0
state_full$index <- seq.int(nrow(state_full))
state_full <- state_full%>% 
  select(index, UID,State.1,State.2,State.3,State.4)

# how do i deteriorate elements by 0.12% from state1 to state 2, selecting random assets everytime step ------------------------
## 1 - 2

Apply_Transmatrix <- function(state_full = state_full){
  set.seed(round(runif(4)*100,0)[1])
  # set.seed(3)
  state_full_1 <- state_full %>% filter(State.1 == "1")
  train.base <- sample(1:nrow(state_full_1),(nrow(state_full_1)*0.0012),replace = FALSE)
  traindata.base <- state_full_1[train.base, ]
  testdata.base <- state_full_1[-train.base, ]
  traindata.base <- traindata.base %>% 
    mutate(State.1 = 0,
           State.2 = 1)
  One_done <- rbind(testdata.base,traindata.base)
  One_leftover <- state_full %>% anti_join(One_done, by = "index") # join back the rows that are not in state 1
  One_done <- rbind(One_done,One_leftover)
  
  
  ## 2 - 3
  
  set.seed(round(runif(4)*100,0)[2])
  state_full_2 <- One_done %>% filter(State.2 == "1") # the data from previous step is used
  train.base2 <- sample(1:nrow(state_full_2),(nrow(state_full_2)*0.0086),replace = FALSE)
  traindata.base2 <- state_full_2[train.base2, ]
  testdata.base2 <- state_full_2[-train.base2, ]
  traindata.base2 <- traindata.base2 %>% 
    mutate(State.2 = 0,
           State.3 = 1)
  Two_done <- rbind(testdata.base2,traindata.base2)
  Two_leftover <- One_done %>% anti_join(Two_done, by = "index") # join back the rows that are not in state 1
  Two_done <- rbind(Two_done,Two_leftover)
  
  
  ## 3 - 4
  
  set.seed(round(runif(4)*100,0)[3])
  state_full_3 <- Two_done %>% filter(State.3 == "1") # the data from previous step is used
  train.base3 <- sample(1:nrow(state_full_3),(nrow(state_full_3)*0.0035),replace = FALSE)
  traindata.base3 <- state_full_3[train.base3, ]
  testdata.base3 <- state_full_3[-train.base3, ]
  traindata.base3 <- traindata.base3 %>% 
    mutate(State.3 = 0,
           State.4 = 1)
  Three_done <- rbind(testdata.base3,traindata.base3)
  Three_leftover <- Two_done %>% anti_join(Three_done, by = "index") # join back the rows that are not in state 1
  Three_done <- rbind(Three_done,Three_leftover)
  
  return(Three_done)
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

# function 2
source("construct_BHI_tayo.R")

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
    left_join(df[,1:9], by = c("bno","element_type"))
  
return(data2)
}

df
T1_x2 <- Transform_Elements(T1_x)
T2_x2 <- Transform_Elements(T2_x)
T3_x2 <- Transform_Elements(T3_x)



T0_POF0 <- convert_state_to_bhi(df)
T0_POF0 %>% group_by(bhi.bridge) %>% tally()

T1_POF1 <- convert_state_to_bhi(T1_x2)
T1_POF1 %>% group_by(bhi.bridge) %>% tally()

T2_POF2 <- convert_state_to_bhi(T2_x2)
T2_POF2 %>% group_by(bhi.bridge) %>% tally()

T3_POF3 <- convert_state_to_bhi(T3_x2)
T3_POF3 %>% group_by(bhi.bridge) %>% tally()


# tests to check the above work

T1_x2 %>% filter(uid == "258_MGCL")
sum(T1_x2$state_2)

df %>% filter(bno == "258", element_type == "MGCL")
sum(df$state_2)


# output POF0,1,2,3


create_uid <- function(data,column_name){
  #columns_pof <- c("bno","bhi.bridge","element_type")
    data1 <- data %>% 
      ungroup() %>% 
    select(bno,bhi.bridge,element_type) %>% 
    mutate(UID = paste0(bno,"_",element_type)) %>%
    select(UID,bhi.bridge)
    colnames(data1)[which(names(data1) == "bhi.bridge")] <- column_name
  return(data1)
}

full_bhi <- create_uid(T0_POF0,"bhi0") %>%  
  left_join(create_uid(T1_POF1,"bhi1"), by = c("UID")) %>%
  left_join(create_uid(T2_POF2,"bhi2"), by = c("UID")) %>%
  left_join(create_uid(T3_POF3,"bhi3"), by = c("UID")) %>%
  distinct() %>% 
  as.data.frame() %>% 
  mutate(c = str_split(UID, "_"),
         bno = purrr::map(c, 1),
         element_type = purrr::map(c, 2),
         bno = as.integer(bno),
         element_type = as.character(element_type)) %>% 
  dplyr::select(bno, element_type, UID, bhi0, bhi1, bhi2, bhi3) %>% 
  as.data.frame()

write.csv(full_bhi,"bridge results.csv", row.names = F)
