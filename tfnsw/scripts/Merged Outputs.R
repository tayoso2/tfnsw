# col1 = rep.int("I need THIS",26)
# col2 = (1:26) 
# 
# df = as.data.frame(cbind(col1,col2))
# 
# str(df)
# 
# library(stringr)
# 
# df %>%
#   mutate(col1 = as.character(col1)) %>% 
#   mutate(c = str_split(col1, " "),
#        element_type = purrr::map(c, -1))

project1 <- read.csv("C:/Users/TOsosanya/Desktop/NSW/UVO/Scen 1-Project.csv")
project2 <- read.csv("C:/Users/TOsosanya/Desktop/NSW/UVO/Scen 2-Project.csv")
project3 <- read.csv("C:/Users/TOsosanya/Desktop/NSW/UVO/Scen 3-Project.csv")
project4 <- read.csv("C:/Users/TOsosanya/Desktop/NSW/UVO/Scen 4-Project.csv")

project_full <- rbind(project1,project2,project3,project4) %>% distinct()
##write.csv(project_full,"C:/Users/TOsosanya/Desktop/NSW/UVO/All Scen-Project.csv",row.names = F)
