# Load tidyverse and load data

library(tidyverse)
library(car)
source('~/Documents/IWDA Consultancy 2/data/Dimension construction/Sols_dimensions.R')
setwd("~/Documents/IWDA Consultancy 2/Report chapters")
source('summaryfuncs.R')

# Unlist columns that have somehow become lists
data <- map_df(data, function (x) {
  if(is.list(x)) return(unlist(x))
  return(x)
})

# Read in data
dictionary <- read.csv('dictionary.csv') %>%
  select(Newer.var.name, Label, Options)
  
questions <- read.csv('Questions.csv')

# # Get a question key
# questions$key <- questions %>%
#   transmute(key = ifelse(is.na(.$Variable.name), .$Text, .$Variable.name))
# 
# # Join labels to dictionary


# Define factor function
factorise <- function(data, series, var_list) {
  tryCatch({
    # Initialise useful vars
    series_name <- series
    series_content <- data
    
    # If not in var_list send a warning and return unaltered content
    if (!(series_name %in% var_list$Newer.var.name)) {
      warning(paste0("Could not find ", series_name, " in var_list, did not transform the variable."))
      return(series_content)
    }
    
    # Return unaltered if variable doesn't have factor options
    if (is.na(dictionary[dictionary$Newer.var.name == series_name, 'Options']) | 
        dictionary[dictionary$Newer.var.name == series_name, 'Options'] == ""){
      return(series_content)
    }
  
    # Get variable labels
    var_labels <- var_list %>%
      filter(Newer.var.name == series_name) %>%
      select(Options) %>%
      str_split('\\|') %>%
      unlist()
    
    # Check if column is currently made up of character labels or values, set to character
    if (is.numeric(series_content)) {
      for (i in 1:length(series_content)) {
        if (is.na(series_content[i])) {
          series_content[i] <- NA
        } else if (series_content[i] == 97) {
          series_content[i] <- "Refused to answer"
        } else if (series_content[i] == 94) {
          series_content[i] <- "Don't know"
        } else if (series_content[i] == 98) {
          series_content[i] <- "Privacy interrupted"
        } else if (series_content[i] == 99) {
          series_content[i] <- NA
        } else {
          series_content[i] <- var_labels[as.integer(series_content[i])]
        }
      }
    }
    
    # Change to factor and set levels
    factor_series <- factor(series_content,
                            levels = var_labels,
                            ordered = TRUE)
    
    # If length of series is 1, unlist
    if (length(factor_series) == 1) factor_series <- unlist(factor_series)
    
    # Return the new factorised variable
    return(factor_series)
  }, error = function(err) {
    
    # error handler picks up where error was generated
    print(paste("MY_ERROR:  Error found at", series_name, "returning unaltered content."))
    return(series_content)
  })
}

# Apply this function
data_factored <- map2(.x = data, .y = names(data), ~factorise(.x, .y, dictionary))
lengths <- map_int(data_factored, length)
name_wrong_length <- names(lengths[lengths != 1874])

data_fact_df <- map_df(data_factored, function (x) {
  if (length(x) == 1) {
    return(unlist(x))
  } else {
    return(x)
  }
})

write_csv(data_fact_df, 'PR_final_data.csv')
saveRDS(data_fact_df, 'PR_final_data.rds')
