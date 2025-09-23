# May 2025
# R script used to derive the nominal and real (adjusted for inflation) salaries required for the GLMS publication across a number of different
# functions. Note that only salary information for one quarter of each calendar year is taken for each individual on the LFS. The information 
# from the LFS is taken from the GRSSWK variable, which provides  an estimate of the gross weekly earnings for main job for full-time workers only. 
# An annualized estimate of the salary is later calculated based on weighting by the number of weeks in each specific year.

##### Employment rate by demographic 
# Find the median, lower and upper quadrilles of salaries by demographic characteristic,


#' @title Reformat_dataset
#' @description Function that selects the weight and relevant variables, and renames the weight variable to PIWT - see later whether this is needed, if not remove last few codes from function.
#'              It is called below for all four LFS quarters
#' @param dataset Each quarter LFS dataset
#' @return Dataset with only required variables and weight
Reformat_dataset <- function(dataset) {
  # Find weight
  # This has been updated so it works with datasets that have more than one weight variable (e.g. Q1_2022)
  
  #Create a dataframe with just the weight variables
  weight_var <- dataset %>%
    select(contains("PIWT")) 
  
  # Extract numeric portion from variable names
  weight_var <- names(weight_var)
  numeric_values <- as.integer(sub("PIWT", "", weight_var))
  
  # Find most recent year suffix
  max_numeric_value <- max(numeric_values)
  
  # Assigns name of most recent weight variable as a value in environment
  weight_var <- paste0("PIWT", max_numeric_value)
  
  # Select relevant columns
  colnames <- c(weight_var,
                "Graduate_type",
                "AGE_BANDS",
                "SEX",
                "ILODEFR",
                "FTPTWK",
                "GRSSWK",
                "GRSSWK_real")
  
  dataset <- dataset %>%
    select(all_of(colnames))
  
  # This renames weight as PIWT
  dataset <- dataset %>%
    rename(PIWT = all_of(weight_var))
  
  # Restrict to just those with an income weight
  # Therefore, income is not NA and this will not impact the calculation of the medians or means
  dataset <- dataset %>%
    filter(PIWT != 0)
  
  return(dataset)
}



#' @title Produce_demographic_salary
#' @description Function that produces the salary outputs for each demographic. It is called within the Average_salary function
#'              It includes the variable for real salaries
#' @param dataset Binded and reformatted dataset of four LFS quarters
#' @param demographic Chosen demographic/characteristic set when function is called in Average_salary and Grad_breakdown_salary functions
#' @return Dataframe of salary stats for given demographic
Produce_demographic_salary <- function(dataset, demographic){
  
  # Find the weight variable
  # This has been updated so it works with datasets that have more than one weight variable (e.g. Q1_2022)
  
  #Create a dataframe with just the person income weight variables
  weight <- "PIWT"
  
  # Produces stats in a dataframe
  # p25 - first quantile
  # p90 - 90th percentile
  dataset %>%
    group_by_at(demographic) %>%
    summarise(
      p25 = weighted.quantile(GRSSWK, w = !!sym(weight), prob = 0.25),
      p90 = weighted.quantile(GRSSWK, w = !!sym(weight), prob = 0.90),
      # Median = weighted.median(GRSSWK, w = !!sym(weight)),
      Median = weighted.median(GRSSWK, w = !!sym(weight), type = 4, collapse = FALSE), # Adjusted in April 2025 Now applying the linear interpolation approach to derive median value
      #  Mean = weighted.mean(GRSSWK, w = !!sym(weight)),
      p25_real = weighted.quantile(GRSSWK_real, w = !!sym(weight), prob = 0.25),
      p90_real = weighted.quantile(GRSSWK_real, w = !!sym(weight), prob = 0.90),
      Median_real = weighted.median(GRSSWK_real, w = !!sym(weight), type = 4, collapse = FALSE), # Same as above - change to   linear interpolation approach
      #   Mean_real = weighted.mean(GRSSWK_real, w = !!sym(weight)),
      Sample = n(),
      Population = sum(!!sym(weight))) %>%
    as.data.frame() %>%
    mutate(Variable = paste0(demographic, collapse = "_"))
  
  }



#' @title Average_salary
#' @description Function that reformats the datasets, filters by full-time employed, then gets salary outputs using the Produce_demographic_salary function
#' @param year Given year, gets all four quarters. Inputted using start year:end year in sapply in GLMS_run_2023
#' @return Dataframe of headline employment stats
Average_salary <- function(year){
  
  # Select datasets
  dataset1 <- get(paste0("Q1_",year))
  dataset2 <- get(paste0("Q2_",year))
  dataset3 <- get(paste0("Q3_",year))
  dataset4 <- get(paste0("Q4_",year))
  
  dataset1 <- Reformat_dataset(dataset1)
  dataset2 <- Reformat_dataset(dataset2)
  dataset3 <- Reformat_dataset(dataset3)
  dataset4 <- Reformat_dataset(dataset4)
  
  dataset <- rbind(dataset1, dataset2, dataset3, dataset4)
  
  # Restrict to just those in full-time employment
  dataset <- dataset %>% 
    filter(FTPTWK == "Full-time", ILODEFR == "In employment")
  

  # Gross Weekly pay in main job
  
  # Split by Age, Gender and Graduate Type
  all_age_sex <- Produce_demographic_salary(dataset, c("AGE_BANDS", "SEX", "Graduate_type"))
  
  
  # Split by Age and Graduate Type
  all_age <- Produce_demographic_salary(dataset, c("AGE_BANDS", "Graduate_type"))
  # Add extra variable to show this is for any gender
  all_age$SEX <- "Total"
  
  
  # Split by Gender and Graduate Type
  all_sex <- Produce_demographic_salary(dataset, c("SEX", "Graduate_type"))
  # Add extra variable to show this is for any age
  all_sex$AGE_BANDS <- "Total"
  
  # Total graduate type
  all_total <- Produce_demographic_salary(dataset, "Graduate_type")
  
  # Add in an 'agegroup' for the average weekly salaries by graduate type and rename the column headers: this way we can merge the datasets as they'll have the same headers
  all_total$AGE_BANDS <- "Total"
  all_total$SEX <- "Total"
  
  # Append the average salaries for people by graduate type and ageband with those for all ageband
  All_sal <- rbind(all_age, all_total, all_sex , all_age_sex)
  All_sal$Year <- year
  
  # Create flags for accuracy (if sample is less than 30)
  All_sal <- All_sal %>%
    mutate(Accuracy_flag = if_else(Sample <= 30, 1, 0))
  
  # Create flags for suppression (if sample of the combined LFS year data is less than or equal to 4)
  All_sal <- All_sal %>%
    mutate(Suppression_flag = if_else(Sample <= 4, 1, 0))
  
  # Create flags for row emptiness
#  All_sal <- All_sal %>%
#  mutate(empty_flag = if_else(Sample <= 0, 1, 0))
  
  
  # Rename the dataset to reference the year it's run on and output to the global environment
  assign(paste0("Salary_",year), All_sal, envir = globalenv())

}










# Graduate Breakdown salaries (16-64), most recent year only


#' @title Reformat_dataset_grad
#' @description Function that selects the weight and relevant variables, and renames the weight variable to PIWT - see later whether this is needed, if not remove last few codes from function.
#'              It is called below for all four LFS quarters
#' @param dataset Each quarter LFS dataset
#' @return Dataset with only required variables and weight for grad code
Reformat_dataset_grad <- function(dataset){
  
  weight_var <- grep("PIWT", names(dataset), value = TRUE)
  
  # Find weight
  # This has been updated so it works with datasets that have more than one weight variable (e.g. Q1_2022)
  
  # Create a dataframe with just the weight variables
  weight_var <- dataset %>%
    select(contains("PIWT")) # Make sure to check in other datasets as sure there might be cases which have _old suffix
  
  # Extract numeric portion from variable names
  weight_var <- names(weight_var)
  numeric_values <- as.integer(sub("PIWT", "", weight_var))
  
  # Find most recent year suffix
  max_numeric_value <- max(numeric_values)
  
  # Assigns name of most recent weight variable as a value in environment
  weight_var <- paste0("PIWT", max_numeric_value)
  
  # Select relevant columns
  colnames <- c(weight_var,
                "Graduate_type",
                "AGE_BANDS",
                "SEX",
                "ILODEFR",
                "FTPTWK",
                "GRSSWK",
                "INDE07M")
  
  
  dataset <- dataset %>%
    select(all_of(colnames))
  
  # This renames weight as PIWT
  dataset <- dataset %>%
    rename(PIWT = all_of(weight_var))
  
  # Restrict to just those with an income weight
  # Therefore, income is not NA and this will not impact the calculation of the medians or means
  dataset <- dataset %>%
    filter(PIWT != 0)
  
  return(dataset)
}



#' @title Produce_demographic_salary_grad
#' @description Function that produces the salary outputs for each demographic. It is called within the Grad_breakdown_salary - all grads and young grads - function
#'              This does not include real salaries as it is not reported on
#' @param dataset Binded and reformatted dataset of four LFS quarters
#' @param demographic Chosen demographic/characteristic set when function is called in Average_salary and Grad_breakdown_salary functions
#' @return Dataframe of salary stats for given demographic
Produce_demographic_salary_grad <- function(dataset, demographic){
  
  # Find the weight variable
  # This has been updated so it works with datasets that have more than one weight variable (e.g. Q1_2022)
  
  #Create a dataframe with just the weight variables
  weight <- "PIWT"
  
  # Produces stats in a dataframe
  # p25 - first quantile
  # p90 - 90th percentile
  dataset %>%
    group_by_at(demographic) %>%
    summarise(
      p25 = weighted.quantile(GRSSWK, w = !!sym(weight), prob = 0.25),
      p90 = weighted.quantile(GRSSWK, w = !!sym(weight), prob = 0.90),
      # Median = weighted.median(GRSSWK, w = !!sym(weight)),
      Median = weighted.median(GRSSWK, w = !!sym(weight), type = 4, collapse = FALSE), # Approach changed in April 2025 - now using the linear interpolation approach to derive the median salary
      Mean = weighted.mean(GRSSWK, w = !!sym(weight)),
      Sample = n(),
      Population = sum(!!sym(weight))) %>%
    as.data.frame() %>%
    mutate(Variable = paste0(demographic, collapse = "_"))
  
}


#' @title Grad_breakdown_salary
#' @description Function that reformats the datasets, filters by full-time, employed and graduate
#'              Then gets salary outputs using the Produce_demographic_salary_grad function for working age and young grad
#' @param year Given year, gets all four quarters. Inputted using start year:end year in sapply in GLMS_run_2023
#' @return Dataframe of headline employment stats
Grad_breakdown_salary <- function(year){
  
  #select dataset: for example if quarter =1 and year =2016 this will get Q1_2016 and set this to 'dataset
  dataset1 <- get(paste0("Q1_",year))
  dataset2 <- get(paste0("Q2_",year))
  dataset3 <- get(paste0("Q3_",year))
  dataset4 <- get(paste0("Q4_",year))
  
  # Modify datasets using nested. This selects the most up to date income weight and renames it
  dataset1 <- Reformat_dataset_grad(dataset1)
  dataset2 <- Reformat_dataset_grad(dataset2)
  dataset3 <- Reformat_dataset_grad(dataset3)
  dataset4 <- Reformat_dataset_grad(dataset4)
  
  # Bind quarterly dataset to make yearly dataset
  dataset <- rbind(dataset1, dataset2, dataset3, dataset4)
  
  # Restrict to just those in full-time employment
  dataset <- dataset %>% 
    filter(FTPTWK == "Full-time", ILODEFR == "In employment")
  
  # Restrict to just graduates
  dataset <- dataset %>% 
    filter(Graduate_type == "Graduate")
  
  # Repeat for average salary by gender
  sex <- Produce_demographic_salary_grad(dataset, "SEX")
  sex <- rename(sex, Value = SEX)
  # Add extra variable to show this is for any gender
##  sex <- sex %>% filter(sex == "NA", "Total") # new code
  sex <- sex %>% filter(!is.na(Value))
  
  # Average salary by industry category
  industry <- Produce_demographic_salary_grad(dataset,"INDE07M")
  industry <- rename(industry, Value = INDE07M)
  # Add extra variable to show this is for any gender
##  industry <- industry %>% filter(industry == "NA", "Total") # new code
  industry <- industry %>% filter(!is.na(Value))
  
  # Average salary by Sex and industry category
  industry_sex <- Produce_demographic_salary_grad(dataset,c("SEX", "INDE07M"))
  industry_sex <- rename(industry_sex, Value = SEX, Industry = INDE07M)
  # Add extra variable to show this is for any gender
##  industry_sex <- industry_sex %>% filter(industry_sex == "NA", "Total") # new code
  industry_sex <- industry_sex %>% filter(!is.na(Industry), !is.na(Value))  
  
  # Append datasets to each other to create one dataset of graduate salaries for different demographic characteristics
  Grad_sal <- bind_rows(list(sex, 
                             industry,
                             industry_sex))
  
  Grad_sal <- Grad_sal %>%
    mutate(Group = "Graduates: 16-64")
             
                
  # Graduate salaries for young grads (21-30)
  
  # Restrict to just graduates and young population
  dataset <- dataset %>% 
    filter(Graduate_type == "Graduate" & AGE_BANDS == "21-30")
  
  # Repeat for average salary by gender
  sex <- Produce_demographic_salary_grad(dataset, "SEX")
  sex <- rename(sex, Value = SEX)
  sex <- sex %>% filter(!is.na(Value))
  
  # Average salary by industry category
  industry <- Produce_demographic_salary_grad(dataset,"INDE07M")
  industry <- rename(industry, Value = INDE07M)
  industry <- industry %>% filter(!is.na(Value))
  
  # Average salary by Sex and industry category
  industry_sex <- Produce_demographic_salary_grad(dataset,c("SEX", "INDE07M"))
  industry_sex <- rename(industry_sex, Value = SEX, Industry = INDE07M)
  industry_sex <- industry_sex %>% filter(!is.na(Industry), !is.na(Value))  
  
  # Append datasets to each other to create one dataset of graduate salaries for different demographic characteristics
  Young_grad_sal <- bind_rows(list(sex,
                                   industry,
                                   industry_sex))
  
  Young_grad_sal <- Young_grad_sal %>%
    mutate(Group = "Graduates: 21-30") 
  
  
  All_grad_sal <- bind_rows(list(Grad_sal,
                                 Young_grad_sal))
  
  # Reorders with Group at beginning
  All_grad_sal <- All_grad_sal[c("Group", "Value", "p25", "p90", "Median", "Mean", "Sample", "Population", "Industry")]
  
  # Create flags for accuracy (if sample is less than 30)
  All_grad_sal <- All_grad_sal %>%
    mutate(Accuracy_flag = if_else(Sample <= 30, 1, 0))
  
  # Create flags for suppression and empty rows (if sample of the combined LFS year data is less than or equal to 4)
  All_grad_sal <- All_grad_sal %>%
    mutate(Suppression_flag = if_else(Sample <= 4, 1, 0))
  
#  All_grad_sal <- All_grad_sal %>%
#  mutate(empty_flag = if_else(sample <= 0, 1, 0)) # New row for emptiness
  
  # Rename the all_grad_sal dataset to reference the year it's run on and output to the global environment
  assign(paste0("All_grad_sal_",year), All_grad_sal, envir = globalenv())

}


