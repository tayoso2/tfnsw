# Function version of construct_BHI.R

#' Read bridge data frame
#' 
#' \code{read_bridge} loads a sheet from an xlsx file, cleaning the column header
#' and filtering by the zone of interest
#' 
#' @param filepath is a string representing the path of the xlsx file to load
#' @param sheet is the sheet name within the xlsx file
#' @param zone is value of `proposed_zone` to filter on
#' @export
read_bridge <- function(filepath, sheet = "in", zone = "River Zone"){
  fileType <- stringr::str_extract(filepath, "(?<=\\.)\\w+$")
  if(fileType == "xlsx"){
    df <- readxl::read_xlsx(filepath, sheet,
                            .name_repair = function(x) {
                              stringr::str_replace_all(x, "\\s", "_") %>% tolower()
                            })
  } else if(fileType == "csv"){
    df <- readr::read_csv(filepath) %>%
      dplyr::rename_all(~stringr::str_replace_all(., "\\s", "_") %>% tolower(.))
  } else if(fileType == "xls"){
    df <- readxl::read_xls(filepath, sheet,
                            .name_repair = function(x) {
                              stringr::str_replace_all(x, "\\s", "_") %>% tolower()
                            })
    } else {
    stop(paste("File type", fileType, "not allowed. Must be csv or xlsx."))
  }
  reqCols <- c("proposed_zone", "bno", "element_type",
               "state_1", "state_2", "state_3", "state_4")
  if(any(!reqCols %in% names(df))) {
    
   stop(paste("File specified is missing required columns:",
              reqCols[which(!reqCols %in% names(df))]) )
  } else{
    df <- df %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("state")), 
                     ~replace(., is.na(.), 0)) %>%
    dplyr::filter(proposed_zone == zone)
  }
}
 
read_bridge_Robj <- function(object, zone = "River Zone"){
  reqCols <- c("proposed_zone", "bno", "element_type",
               "state_1", "state_2", "state_3", "state_4")
  if(any(!reqCols %in% names(object))) {
    
    stop(paste("File specified is missing required columns:",
               reqCols[which(!reqCols %in% names(object))]) )
  } else{
    df <- object %>%
      dplyr::mutate_at(dplyr::vars(dplyr::contains("state")), 
                       ~replace(., is.na(.), 0)) %>%
      dplyr::filter(proposed_zone == zone)
  }
}

#' Transform bridge dataframe and joins to an element table
#' 
#' \code{compile_bridge} returns a longer form bridge data frame where all state
#' columns are gathered into one, and the element table is joined by element_type
#' 
#' @param df is the bridge data frame returned by \code{read_bridge}
#' @param element_table is the file name for the bridge element reference table
#' @param lookup_table is the file name for the BHI condition lookup table
#' @export
compile_bridge <- function(df, element_table = "tables.csv",
                           lookup_table = "tables.xlsx") {
  # Element information table
  element <- readr::read_csv(paste0(folder, element_table)) %>%
    # Remove character returns in col header
    dplyr::rename_all(~stringr::str_replace(., "\r", "_") %>% tolower(.)) 
  reqCols <- c("element_code", "importance_factor", "bhi_group")
  if(any(!reqCols %in% names(element))){
    stop(paste("File specified is missing required columns:",
               reqCols[which(!reqCols %in% names(element))]) )
  }
  
  # BHI lookup table returning BHI from state, importance and BHI group
  lookup <- readxl::read_xlsx(paste0(folder, lookup_table), sheet = "table2") %>%
    dplyr::rename_all(~stringr::str_replace_all(., "\\s", "_") %>% tolower(.)) %>%
    dplyr::mutate(bhi = factor(bhi, levels = c("Poor", "Fair", "Good", "As-Built"),
                               ordered = T),
                  importance_factor = factor(importance_factor,
                                             levels = c("Low", "Medium", "High"),
                                             ordered = T),
                  bhi_group = factor(bhi_group, levels = LETTERS[1:4])) %>%
    tibble::rowid_to_column("logicgroup") %>%
    tidyr::pivot_longer(contains("_cond"), names_to = "boundary", values_to = "limit_pct",
                        values_drop_na = T) %>%
    tidyr::separate(boundary, sep = "_cond", into = c("boundary", "boundary_state")) %>%
    dplyr::filter(boundary_state <= 4) %>% # State 5 not mentioned in the data
    tidyr::pivot_wider(names_from = boundary, values_from = limit_pct)
  
  reqColsLookup <- c("importance_factor", "bhi_group", "logicgroup", "boundary_state", "bhi", "low", "high")
  if(any(!reqColsLookup %in% names(lookup))){
    stop(paste("File specified is missing required columns:",
               reqColsLookup[which(!reqColsLookup %in% names(lookup))]) )
  }
  
  bridges <- df %>%
    tidyr::pivot_longer(contains("state_"), names_to = "state", 
                        values_to = "elements") %>%
    dplyr::mutate(state = stringr::str_extract(state, "[:digit:]")) %>%
    dplyr::filter(state <= 4) %>%  # State 5 not mentioned in the data
    dplyr::left_join(element, by = c("element_type" = "element_code")) %>%
    dplyr::mutate(importance_factor = factor(importance_factor,
                                      levels = c("Low", "Medium", "High"),
                                      ordered = T),
           bhi_group = factor(bhi_group, levels = LETTERS[1:4])) %>%
    dplyr::group_by(bno, element_type) %>%
    dplyr::mutate(total_states = sum(elements)) %>%
    dplyr::mutate(pct_elements = elements * 100 / total_states) %>%
    dplyr::left_join(lookup, by = c("bhi_group", "importance_factor", 
                                            "state" = "boundary_state"))
  }

compile_bridge_Robj <- function(df) {
  # Element information table
  element <- df %>%
    # Remove character returns in col header
    dplyr::rename_all(~stringr::str_replace(., "\r", "_") %>% tolower(.)) 
  reqCols <- c("element_code", "importance_factor", "bhi_group")
  if(any(!reqCols %in% names(element))){
    stop(paste("File specified is missing required columns:",
               reqCols[which(!reqCols %in% names(element))]) )
  }
  
  # BHI lookup table returning BHI from state, importance and BHI group
  lookup <- readxl::read_xlsx(paste0(folder, lookup_table), sheet = "table2") %>%
    dplyr::rename_all(~stringr::str_replace_all(., "\\s", "_") %>% tolower(.)) %>%
    dplyr::mutate(bhi = factor(bhi, levels = c("Poor", "Fair", "Good", "As-Built"),
                               ordered = T),
                  importance_factor = factor(importance_factor,
                                             levels = c("Low", "Medium", "High"),
                                             ordered = T),
                  bhi_group = factor(bhi_group, levels = LETTERS[1:4])) %>%
    tibble::rowid_to_column("logicgroup") %>%
    tidyr::pivot_longer(contains("_cond"), names_to = "boundary", values_to = "limit_pct",
                        values_drop_na = T) %>%
    tidyr::separate(boundary, sep = "_cond", into = c("boundary", "boundary_state")) %>%
    dplyr::filter(boundary_state <= 4) %>% # State 5 not mentioned in the data
    tidyr::pivot_wider(names_from = boundary, values_from = limit_pct)
  
  reqColsLookup <- c("importance_factor", "bhi_group", "logicgroup", "boundary_state", "bhi", "low", "high")
  if(any(!reqColsLookup %in% names(lookup))){
    stop(paste("File specified is missing required columns:",
               reqColsLookup[which(!reqColsLookup %in% names(lookup))]) )
  }
  
  bridges <- df %>%
    tidyr::pivot_longer(contains("state_"), names_to = "state", 
                        values_to = "elements") %>%
    dplyr::mutate(state = stringr::str_extract(state, "[:digit:]")) %>%
    dplyr::filter(state <= 4) %>%  # State 5 not mentioned in the data
    dplyr::left_join(element, by = c("element_type" = "element_code")) %>%
    dplyr::mutate(importance_factor = factor(importance_factor,
                                             levels = c("Low", "Medium", "High"),
                                             ordered = T),
                  bhi_group = factor(bhi_group, levels = LETTERS[1:4])) %>%
    dplyr::group_by(bno, element_type) %>%
    dplyr::mutate(total_states = sum(elements)) %>%
    dplyr::mutate(pct_elements = elements * 100 / total_states) %>%
    dplyr::left_join(lookup, by = c("bhi_group", "importance_factor", 
                                    "state" = "boundary_state"))
}

#' Reads in bridge file, transforms and joins to element and BHI lookup tables
#' 
#' \code{load_bridge} calls \code{read_bridge} to read in the bridge file, in csv or
#' xls* format and proceeds to gather state columns. It then reads in \code{compile_bridge},
#' which joins the data frame to the element and BHI lookup tables.
#' 
#' @inheritParams read_bridge
#' @inheritParams compile_bridge
#' @export

load_bridge <- function(filepath, sheet = "in", zone = "River Zone",
                        element_table = "tables.csv",
                        lookup_table = "tables.xlsx"){
  df <- read_bridge(filepath, sheet, zone) %>%
    compile_bridge(element_table, lookup_table)
}

load_bridge_Robj <- function(object, zone = "River Zone",
                             element_table = "tables.csv",
                             lookup_table = "tables.xlsx"){
  df <- read_bridge_Robj(object, zone) %>%
    compile_bridge(element_table, lookup_table)
}


#' Calculate Bridge Health Index
#' 
#' \code{calc_bhi} calculates the Bridge Health Index from the data frame provided
#' by \code{load_bridge} (or the two-step process, \code{read_bridge} and 
#' \code{compile_bridge}).
#' 
#' @param df is the prepared bridge data frame outputted by \code{load_bridge}
#' @param original_layout logical indicating whether to mimic the wide layout 
#' of the input file
#' @param bridge_level_bhi logical indicating whether to additionally return the 
#' bridge level BHI not just the element level value
#' @examples
#' bhi <- load_bhi("my_file.xlsx", sheet = "Sheet 2") %>% calc_bhi()
#' bhi <- read_bhi("my_file.csv") %>% compile_bhi() %>% calc_bhi()
#' bhi <- calc_bhi(df, original_layout = F, bridge_level_bhi = T)
#' @export

calc_bhi <- function(df, original_layout = T, bridge_level_bhi = F){
  bridges_bhi <- df %>%
    dplyr::mutate(condition_met = dplyr::case_when(
      low <= pct_elements &  pct_elements  <= high ~ T,
      is.nan(pct_elements) & bhi == "As-Built" ~ T,
      TRUE ~ F
    )) %>%
    dplyr::group_by(bno, element_type, bhi, logicgroup) %>%
    dplyr::mutate(bhi_return = case_when(
      min(condition_met) == 1 ~ as.numeric(bhi), # min is the same as AND - all conditions have to be met
      TRUE ~ 0
    ),
    range = high - low,
    bhi_score = case_when(
      state <= 2 ~ 0,
      range == 0 ~ 0,
      pct_elements == 0 ~ 0,
      TRUE ~ round(condition_met *
                     pmin(exp(((pct_elements- low)/range) * as.numeric(state) - 
                                4 + as.numeric(importance_factor)), 
                          ifelse(state == 3, 0.75, 1)),
                   digits = 2)
    )) %>%
    dplyr::group_by(bno, element_type) %>%
    dplyr::mutate(bhi_score = pmin(sum(bhi_score, na.rm = T), 1)) %>%
    dplyr::filter(bhi_return == max(bhi_return))
  
  if(original_layout){
    message("Transforming to original layout")
    cols <- c("bno", "bridge_description", "proposed_zone", "insp_date",
    "initital", "insp_type", "element_type", "env", "qty_total", "bhi", "bhi_score")
    bridges_bhi <- bridges_bhi %>%
      tidyr::pivot_wider(id_cols = all_of(cols),
                         names_from = state, names_prefix = "state_", 
                         values_from = "elements", names_sort = T,
                         values_fill = 0) %>%
      dplyr::select(all_of(cols),
                    dplyr::contains("state"))
  }
  
  if(bridge_level_bhi){
    message("Calculating bridge level BHI")
    # BHI aggregated at the bridge level
    bridge_agg <- bridges_bhi %>%
      dplyr::group_by(bno) %>%
      dplyr::summarise(bhi.bridge = min(bhi))
    
    # Feed bridge level bhi back in and rename
    bridges_bhi <- bridges_bhi %>%
      dplyr::left_join(bridge_agg, by = "bno") %>%
      dplyr::select(bno, bhi.bridge, element_type, bhi.element = bhi, bhi_score, 
             everything())
  }
  return(bridges_bhi)
}