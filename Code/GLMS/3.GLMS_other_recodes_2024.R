# May 2025
# This R script is used to re-code other imported variables from the LFS that are not education related including AGE, SOC codes for occupational grouping and
# ethnicity, so they are in the correct format for further analysis in the later GLMS scripts. This uses a number of dplyr routines to apply the re-coding.



# Contents:
# 1. Age bands
# 2. Skilled occupations
# 3. Ethnicity

#' @title Other_recodes
#' @description Creates recodes for characteristic variables
#' @param quarter LFS quarter - is set in sapply, is 1:4
#' @param year LFS year - is set in sapply, uses start_year and end_year
#' @return Datasets in environment with AGE_BANDS, SOCHE and ETHNICITY variables
Other_recodes <- function(quarter, year) {
  
  # Get dataset and remove any columns that contain the _old suffix
  dataset<-get(paste0("Q",quarter,"_",year)) %>%
    select(!contains('_old'))
  
  
  # 1. Create age group categories
  
  dataset <- dataset %>%
    mutate(AGE_BANDS = case_when(AGE %in% 16:20 ~ "16-20",
                                 AGE %in% 21:30 ~ "21-30",
                                 AGE %in% 31:40 ~ "31-40",
                                 AGE %in% 41:50 ~ "41-50",
                                 AGE %in% 51:60 ~ "51-60",
                                 AGE %in% 61:64 ~ "61-64")) #Note error in 2016 (age 60 in last bracket)
  

  
  # 2. Create skilled occupations variable (high medium low)
  # Variable name: SOCHE
  # Levels: "High skilled employment", "Medium/low skilled employment" (and NA)
  
  if("SC20MMJ" %in% names(dataset)){
    
    dataset <- dataset %>%
      mutate(SOCHE = case_when(SC20MMJ == "Managers, Directors And Senior Officials" | 
                                 SC20MMJ == "Professional Occupations" | 
                                 SC20MMJ == "Associate Professionaloccupations" ~ "High skilled employment",
                               
                               SC20MMJ == "Administrative And Secretarial Occupations" |
                                 SC20MMJ == "Skilled Trades Occupations" |
                                 SC20MMJ == "Caring, Leisure And Other Service Occupations"|
                                 SC20MMJ == "Sales And Customer Service Occupations" |
                                 SC20MMJ == "Process, Plant And Machine Operatives" |
                                 SC20MMJ == "Elementary Occupations" ~ "Medium/low skilled employment",
                               
                               TRUE ~ SC20MMJ))
  }
  
  
  if("SC10MMJ" %in% names(dataset)){
    
    dataset <- dataset %>%
      mutate(SOCHE = case_when(SC10MMJ == "1  'Managers, Directors And Senior Officials'" | 
                                 SC10MMJ == "2  'Professional Occupations'" | 
                                 SC10MMJ == "3  'Associate Professional And Technical Occupations'" ~ "High skilled employment",
                               
                               SC10MMJ == "4  'Administrative And Secretarial Occupations'" |
                                 SC10MMJ == "5  'Skilled Trades Occupations'" |
                                 SC10MMJ == "6  'Caring, Leisure And Other Service Occupations'"|
                                 SC10MMJ == "7  'Sales And Customer Service Occupations'" |
                                 SC10MMJ == "8  'Process, Plant And Machine Operatives'" |
                                 SC10MMJ == "9  'Elementary Occupations'" ~ "Medium/low skilled employment",
                               
                               TRUE ~ SC10MMJ))
  }
  
  
  if("SC2KMMJ" %in% names(dataset)){
    
    dataset <- dataset %>%
      mutate(SOCHE = case_when(SC2KMMJ == "1  'Managers and Senior Officials'" | 
                                 SC2KMMJ == "2  'Professional occupations'" | 
                                 SC2KMMJ == "3  'Associate Professional and Technical'" ~ "High skilled employment",
                               
                               SC2KMMJ == "4  'Administrative and Secretarial'" |
                                 SC2KMMJ == "5  'Skilled Trades Occupations'" |
                                 SC2KMMJ == "6  'Personal Service Occupations'" |
                                 SC2KMMJ == "7  'Sales and Customer Service Occupations'" |
                                 SC2KMMJ == "8  'Process, Plant and Machine Operatives'" |
                                 SC2KMMJ == "9  'Elementary Occupations'" ~ "Medium/low skilled employment",
                               
                               TRUE ~ SC2KMMJ))
  }
  
  
  # Validation check
  # If any values are not categorised into the three groups (high, medium, low), this will be flagged because have included TRUE line to retain all values that may have been missed
  SOCHE_valid_values <- c("High skilled employment", "Medium/low skilled employment", NA)
  
  # Check if any SOCHE values have not been coded into the three levels 
  additional_values <- setdiff(dataset$SOCHE, SOCHE_valid_values)
  
  # Returns which values have not been included in the recode, then add them into the above code
  if (length(additional_values) > 0) {
    cat(paste0("Warning: These SOCHE values have been excluded from the recode for dataset Q",quarter," ",year," (Action: edit wording in the code):  "))
    print(additional_values)
  }
  
  
  
  
  # 3. Create ethnicity variable
  # This aggregates some ethnicity categories into "Other ethnic group", and has validation check
  # Variable name: ETHNICITY
  # Levels: "White", "Asian / Asian British", "Black / African / Caribbean / Black British", "Other ethnic group" (and NA)
 
  # Note that in May 2024 it was agreed that Chinese students should be re-allocated to the Asian / Asian British Group
  # Therefore they were removed from the 'Other ethnic group' and allocated to Asian / Asian British - see below 
  # Updated code below May 2024
  if("ETH11EW" %in% names(dataset)){
    dataset <- dataset %>%
      mutate(ETHNICITY = case_when((ETH11EW == "Mixed / Multiple ethnic groups" | ETH11EW == "Arab" | ETH11EW == "Other ethnic group.") ~ "Other ethnic group",
                                   (ETH11EW == "Chinese" | ETH11EW =="Asian / Asian British") ~ "Asian / Asian British",
                                   TRUE ~ ETH11EW))
  }
  
  # Validation check (to ensure wording of categories is the same across datasets)
  ETHNICITY_valid_values <- c("White", "Asian / Asian British", "Black / African / Caribbean / Black British", "Other ethnic group", NA)
  
  # Check if any ethnicity values have not been coded into the given levels 
  additional_values <- setdiff(dataset$ETHNICITY, ETHNICITY_valid_values)
  
  # Returns which values have not been coded into the recode, then add them into the above code
  if (length(additional_values) > 0) {
    cat(paste0("Warning: These ETHNICITY values have been excluded from the recode for dataset Q",quarter," ",year," (Action: edit wording in the code):  "))
    print(additional_values)
  }
  
  # Could do a final validation check that all variables have been included in new dataset
  
  assign(paste0("Q",quarter,"_",year),dataset,envir = globalenv())
  
}
