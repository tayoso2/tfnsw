# Title     : Data Clean NSW - Bridges
# Objective : Clean NSW Datasets and process for modelling
# Created by: TOsosanya
# Created on: 02/06/2020

library(dplyr)
library(magrittr)
library(DescTools)
library(readxl)
library(tidyr)
library(lubridate)

### Bridges  -------------------------------------------------------------------------

# load the data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

path_B <- "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Bridges"
assets1 <- read.csv(paste0(path_B, "/Brdiges, Tunnels and Bridge Sized Culverts Condition Rating.csv"))
assets2 <- assets1 %>% select(-c(2,5,6,14)) %>%
  dplyr::filter(`Proposed.Zone` == "River Zone") %>%
  mutate(BNO = as.character(BNO)) %>%
  select(-c(2))

assets2$Insp.Date <- dmy(assets2$Insp.Date)
asset3 <- assets2 %>% mutate(State.1 = State.1/Qty.Total,
                              State.2 = State.2/Qty.Total,
                              State.3 = State.3/Qty.Total,
                              State.4 = State.4/Qty.Total) %>% 
  mutate_if(is.numeric, function(x) round(x, 2))

# change the states the proportions


# add bridges construction year

bridges_assets1 <- read_excel(paste0(path_B, "/Bridge mastersheet.xlsx"), range = "A1:AN450" ,sheet = "original mastersheet")
bridges_assets2 <- bridges_assets1 %>% select(Asset_Type,'CONSTRUCTION_YEAR',BNO) %>% 
  mutate(CONSTRUCTION_YEAR = as.numeric(CONSTRUCTION_YEAR)) %>%
  dplyr::filter(Asset_Type == "Bridge") 
bridges_assets2$CONSTRUCTION_YEAR <- paste0("01-01-",bridges_assets2$CONSTRUCTION_YEAR)
bridges_assets2$CONSTRUCTION_YEAR <- dmy(bridges_assets2$CONSTRUCTION_YEAR)


# merge the two on BNO
# analysis starts here 

data.to.plot.whole <- bridges_assets2 %>% left_join(asset3, by = "BNO") %>% 
  mutate(Age = (Insp.Date - CONSTRUCTION_YEAR) / 365,Env = ifelse(Env == "L",1,ifelse(Env == "M",2,3))) %>%
  mutate(mean.condition.grade = (1*State.1) + (2*State.2) + (3 * State.3) + (4*State.4),
         mean.condition.grade = mean.condition.grade^Env) %>% 
  group_by(Age) %>%
  summarise(mean.condition.grade = mean(mean.condition.grade))

data.to.plot.whole <- bridges_assets2 %>% left_join(asset3, by = "BNO") %>% 
  mutate(Age = (Insp.Date - CONSTRUCTION_YEAR) / 365) %>%
  mutate(mean.condition.grade = (1*State.1) + (2*State.2) + (3 * State.3) + (4*State.4)) %>% 
  group_by(Age) %>%
  summarise(mean.condition.grade = mean(mean.condition.grade))

# data.to.plot.whole <- bridges_assets2 %>% left_join(asset3, by = "BNO") %>% 
#   mutate(Age = Insp.Date - CONSTRUCTION_YEAR) %>%
#   mutate(mean.condition.grade = (0.1*State.1) + (0.33*State.2) + (0.67 * State.3) + (1*State.4),
#          mean.condition.grade = mean.condition.grade) %>% 
#   group_by(Age) %>%
#   summarise(mean.condition.grade = mean(mean.condition.grade))

plot.whole <-
  ggplot(data.to.plot.whole,
         aes(x = Age, y = mean.condition.grade)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Complete asset overview",
       y = "Mean Condition Grade") +
  scale_size_continuous(guide = FALSE)

plot.whole




# make the asset1 -------------------------------------------------------------------------

data.for.Asset1 <- bridges_assets2 %>% left_join(asset3, by = "BNO") %>% 
  mutate(Age = round(interval(CONSTRUCTION_YEAR,Insp.Date) / years(1),0)) %>%
  group_by(Element.Type) %>%
  mutate(Counter = group_indices()) %>% 
  mutate(GroupRowNumber = max(row_number(Counter))) %>% 
  arrange(Element.Type) %>% 
  filter(GroupRowNumber > 2) %>% 
  ungroup() %>% 
  select(Counter,Age,State.1:State.4,Qty.Total) %>% 
  #mutate(Element.Type = 1) %>% # set to 1 and change later
  as.data.frame()

names(data.for.Asset1) <- c("group.index","Age","LGR1/Survey Length","LGR2/Survey Length","LGR3/Survey Length","LGR4/Survey Length",
                            "Length/Total Length")

# data.for.Asset1 <- bridges_assets2 %>% left_join(asset3, by = "BNO") %>% 
#   mutate(Age = round(interval(CONSTRUCTION_YEAR,Insp.Date) / years(1),0),
#          Env = ifelse(Env == "L",0.1,ifelse(Env == "M",0.2,0.5))) %>%
#   #mutate(mean.condition.grade = (1*State.1) + (2*State.2) + (3 * State.3) + (4*State.4),
#   #       mean.condition.grade = mean.condition.grade^Env) %>% 
#   group_by(Element.Type) %>%
#   mutate(Counter = group_indices()) %>% 
#   mutate(GroupRowNumber = max(row_number(Counter))) %>% 
#   arrange(Element.Type) %>% 
#   filter(GroupRowNumber > 2) %>% 
#   ungroup() %>% 
#   select(Counter,Age,State.1:State.4,Env) %>% 
#   #mutate(Element.Type = 1) %>% # set to 1 and change later
#   as.data.frame()
# 
# names(data.for.Asset1) <- c("group.index","Age","LGR1/Survey Length","LGR2/Survey Length","LGR3/Survey Length","LGR4/Survey Length",
#                             "Length/Total Length")

# data.for.Asset2 <- split(data.for.Asset1,data.for.Asset1$Element.Type) 
# How_many_rows <- function(x) { # maximum reading and date - minimum reading and date
#   for (j in 1:length(x)) {
#     test <- data.table(x[[j]])
#     test.output
#     if (j == 1) {
#       output.final <- test.output
#     } else{
#       output.final <- rbind(output.final, test.output)
#     }
#   }
#   return(as.data.table(output.final))
# }
# data.for.Asset3 <- How_many_rows(data.for.Asset2,2)
# data.for.Asset4 <- rbindlist(data.for.Asset3, use.names = TRUE, fill = TRUE)


generate.transition.matricies <-
  function(age.value.data,
           group.index,
           groups = "default",
           initial.state = "default",
           timeout = 10,
           predict.periods = 150,
           lower.start = 0,
           upper.start = 1,
           runs = 1,
           required.period.length = 1,
           age.in.year.1 = 1,
           initial.state.age = 0,
           asset.weighting = "default") {
    #calculates transition matrix for different user determined groups, the groups are assumed to be
    #similar in how they behave over time, the functions fit.markov.chain and predict.future.value.groups
    #are applied itteratively to each set of grouped data.
    #
    #Args:
    #   age.value.data: a data frame containing an age column as the first columns and value groups as the latter
    #       with the worst value group at the very right
    #
    #`  group.index:  a single column data frame containing a row by row index of the group a particular
    #       oberservation is in within age.value.data
    #
    #   groups:   a vector containing the indecies of the groups wishing to itterate over, if no
    #       value is given the default is all groups within the data
    #
    #   initial.state:    initial state of the asset to predict from, may be useful if only going forward from
    #       one asset, if many assets have different initial conditions and ages it is better to
    #       use the whole transition matrix to generate future predictions, default is c(1,0,0,0,...)
    #
    #   timeout: Select timeout time to stop if struggling to fit markov chain, default is 30 seconds
    #
    #   predict.periods:  number of periods after initial start age to predict, defaul is 150
    #
    #   lower.start: Lower bound for randomly generated starting values, prior information
    #       can be used to indicate a range to start from, this speeds up time however
    #       can lead to a local optimum being found
    #
    #   upper.start: Upper bound for randomly generated starting values, prior information
    #       can be used to indicate a range to start from, this speeds up time however
    #       can lead to a local optimum being found
    #
    #   runs: The optimiser algorithm has a potential risk of finding a local optimum,
    #       to overcome this the algorithm can be optimised runs times each time with
    #       different randomly generated starting values accepting the minimum residual
    #       of all the runs
    #
    #   required.period.length: if timestep is not length 1 allows
    #       user defined value
    #
    #   age.in.year.1: allows data only to be produced after current year and modeled forward,
    #       useful if required.period.length is not 1 to generate correct multiples
    #
    #   intial.stage.age: if predicting from an initial age corrects the age index
    #
    #Output: A list containing a series of lists, one list for each group, containing the group
    #       number, a list of transition matrix in matrix form, the rmse of the fitting, and
    #       a transition matrix in vector form, and a data frame containing future predictions
    #       for an asset of the group
    #
    #Error information:
    #   Skipped due to timeout: If the dataset is too small the optimiser algorithm will struggle to
    #       fit a markov chain, in this case an error will display with the problem data entry,
    #       the algorithm will skip that group and carry on
    #
    #   There was an unusual warning: There was likely a warning running the fitting or predicting function
    #       attempt to run these manually with problem data group
    #
    #   All datasets empty: Ensure data is entered as data frame and NOT a tibble
    
    
    #Dealing with default values and argument problems
    if (is.character(groups))
      groups <- unique(group.index)
    if (upper.start < lower.start)
      stop("lower.start must be smaller than upper.start")
    if (is.character(initial.state))
      initial.state <- c(1, integer(ncol(age.value.data) - 2))
    
    #progression bar
    if (length(groups) > 1) {
      progression1 <-
        txtProgressBar(
          title = "Progress Bar",
          min = min(groups),
          max = max(groups),
          style = 3
        )
    }
    
    #creating a lise
    group.fitting <- list()
    
    #binding index column to data and naming
    group.with.value <- cbind(group.index, age.value.data)
    names(group.with.value)[1] <- "group.index"
    names(group.with.value)[2] <- "Age"
    asset.weighting1 <- cbind(group.index, asset.weighting)
    
    #dealing with group size of 1
  
    #iterating through desired groups and running functions
    for (i in groups) {
      temp.func <- function() {
        #filtering out required group data
        filtered.group.with.value <-  group.with.value %>%
          filter(group.index == i, Age > 0) %>%
          select(-group.index)
        
        filtered.weighting <- as.data.frame(asset.weighting) %>%
          filter(group.index == i)
        
        #fitting markov chain
        temp.fit <-
          fit.markov.chain(
            filtered.group.with.value,
            runs = runs,
            start.lower = lower.start,
            start.upper = upper.start,
            progress = FALSE,
            asset.weighting = filtered.weighting[, 1]
          )
        
        #predicting future conditions and adding to a list
        group.fitting[[i]] <<-  list(
          i,
          temp.fit,
          predict.future.value.groups(
            temp.fit[[1]],
            periods = predict.periods,
            initial.state = initial.state ,
            required.period.length = required.period.length ,
            age.in.year.1 = age.in.year.1,
            initial.state.age = initial.state.age
          )
          
        )
      }
      
      #running function to deal with errors caused by problem data
      tryCatch({
        withTimeout(temp.func(), timeout = timeout, onTimeout = "error")
      }, TimeoutException = function(ex) {
        cat(paste(
          "[",
          i,
          "Skipped due to timeout, consider increasing timeout argument]\n"
        ))
      }, error = function(e) {
        cat(paste("Group", i, "Dataset empty\n"))
      }, warning = function(e) {
        cat(paste("There was an unusual warning with group", i))
      })
      
      if (length(groups) > 1)
        setTxtProgressBar(progression1, i)
    }
    group.fitting
  }

source("C:/Users/TOsosanya/Desktop/NSW/Markov_Chain_Transitional_Matrices/Markov_Chain_Transitional_Matrices/Transitional Matrix/Luke Transitional model/transition_matrix_functions.R")

Result <- generate.transition.matricies(data.for.Asset1[,c(2:6)], # Age and the four conditions
                                        data.for.Asset1[,1], # groupindex
                                                           timeout = 100,
                                                           lower.start = 0.8,
                                                           asset.weighting = data.for.Asset1[,7] # sample size or confidence in the condition values - length/total length
)
mat <- round(as.data.frame(Result[[1]][[2]][[1]]),4)
names(mat) <- c("[,1]","[,2]","[,3]","[,4]")
rownames(mat) <- c("[1,]","[2,]","[3,]","[4,]")

mat


# Result.non.tiered.fitting <- generate.transition.matricies(Asset1[1:1000,5:10], # Age and the five conditions
#                                                            Asset1[1:1000,1], # groupindex
#                                                            timeout = 100,
#                                                            lower.start = 0.925,
#                                                            asset.weighting = Asset1[1:1000,13] # length/total length
# )
# mat <- round(as.data.frame(Result.non.tiered.fitting[[1]][[2]][[1]]),4)
# names(mat) <- c("[,1]","[,2]","[,3]","[,4]","[,5]")
# rownames(mat) <- c("[1,]","[2,]","[3,]","[4,]","[5,]")
# 
# kable(mat)


