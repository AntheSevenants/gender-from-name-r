library(stringr) # regex
library(parallel) # more faster

df_male_names <- read.csv("data/male_names.csv", sep = ";")
df_female_names <- read.csv("data/female_names.csv", sep = ";")

# Combine both name dataframes
df_names <-
  merge(
    df_male_names,
    df_female_names,
    by = "name",
    all = TRUE,
    suffixes = c(".male", ".female")
  )
# Replace NAs with 0
df_names$frequency.male <-
  ifelse(is.na(df_names$frequency.male), 0, df_names$frequency.male)
df_names$frequency.female <-
  ifelse(is.na(df_names$frequency.female),
         0,
         df_names$frequency.female)
df_names$frequency <-
  df_names$frequency.male + df_names$frequency.female
df_names$length <- sapply(df_names$name, nchar)
df_names_branching <-
  df_names[order(df_names$frequency, decreasing = TRUE),]
df_names <-
  df_names[order(df_names$length, df_names$frequency, decreasing = TRUE),]

get_gender <- function(name) {
  name <- tolower(name)
  name <- str_replace_all(name, "[^a-z]", "")
  
  df_search_space <- df_names[df_names$length <= nchar(name), ]
  
  return_value <- NA
  
  for (i in 1:nrow(df_search_space)) {
    row <- df_search_space[i, ]
    
    tested_name <- tolower(row[["name"]])
    
    if (nchar(tested_name) > nchar(name)) {
      next
    }
    
    name_detected <-
      str_detect(name, regex(paste0("^", tested_name), ignore_case = TRUE))
    
    if (name_detected) {
      return_value <-
        ifelse(row[["frequency.male"]] > row[["frequency.female"]], "male", "female")
      
      break
    }
  }
  
  return(return_value)
}

get_gender_branching <- function(name, known_start = "") {
  #print(paste0("Starting search:", known_start))
  name <- tolower(name)
  name <- str_replace_all(name, "[^a-z]", "")
  
  if (known_start != "") {
    df_search_space <-
      df_names_branching[str_detect(df_names_branching$name, paste0("^", known_start)) &
                           df_names_branching$name != known_start,]
  } else {
    df_search_space <- df_names_branching
  }
  
  return_value <- NA
  
  if (nrow(df_search_space) == 0) {
    return(NA)
  }
  
  for (i in 1:nrow(df_search_space)) {
    row <- df_search_space[i, ]
    
    tested_name <- tolower(row[["name"]])
    
    if (nchar(tested_name) > nchar(name)) {
      next
    }
    
    name_detected <-
      str_detect(name, regex(paste0("^", tested_name), ignore_case = TRUE))
    
    if (name_detected) {
      further_check <- get_gender_branching(name, known_start = row[["name"]])
      
      if (!is.na(further_check)) {
        return_value <- further_check
      } else {
        return_value <-
          ifelse(row[["frequency.male"]] > row[["frequency.female"]], "male", "female")
      }
      
      break
    }
  }
  
  return(return_value)
}