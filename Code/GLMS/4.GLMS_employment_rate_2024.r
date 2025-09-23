# May 2025
# This R scripts creates a number of different functions required to calculate and generate employment, high-skilled employment, inactivity and
# unemployment rates required in the GLMS series. Quarterly rates are taken and converetd into annualised rates. 



# 1.
# Previous function: Headline_stats_function(all,age_range)
# This finds employment rates, samples sizes and populations, by graduate type and age group
# This function is called within the next function, to get outputs for both the total and young populations

#this function creates the stats
#the next function outputs the rates (but uses function within to create the output)

#all is the grouped data that we want to report on
#age_range is which age group we want to look at 


## Function rewritten using dplyr where possible

#' @title Produce_headline_stats
#' @description Function that produces the main employment rate outputs. It is called within the Employment_rate function for all adults and then young pop
#' @param all Weighted and grouped dataset - created in Employment_rate function
#' @param age_range Either working age "16-64" or young population "21-30" - is set in Employment_rate function
#' @return Dataframe of headline employment stats
Produce_headline_stats <- function(all, age_range) {
  
  # Subset data depending on whether working age ("16-64") and young ("21-30")
  if (age_range == "16-64") {
    all <- all
  }
  else {
    all <- all %>%
      filter(AGE_BANDS == "21-30")
  }
  
  # Find the sample size and number of people in each graduate type population
  full_pop <- all %>%
    group_by(Graduate_type) %>%
    summarise(total = sum(freq), sample = sum(sample))
  
  
  # Creates labels for graduate type
  types <- full_pop["Graduate_type"]
  
  # Make in_employment table by selecting those in employment, grouping by grad type, shows weighted count and sample size
  # Uses left_join to retain all values of Graduate_type even if the returning value for the given breakdown is zero (i.e. if there were no people who were postgrad and inactive, using left join will still keep postgrad as a row)
  in_employment <- all %>%
    filter(ILODEFR == "In employment") %>%
    group_by(Graduate_type) %>%
    summarise(count = sum(freq), sample = sum(sample)) %>%
    left_join(types, by = "Graduate_type")
  
  # Make in_HSemployment table by selecting those in high-skilled employment, grouping by grad type, shows weighted count and sample size
  in_HSemployment <- all %>%
    filter(ILODEFR == "In employment" & SOCHE == "High skilled employment") %>%
    group_by(Graduate_type) %>%
    summarise(count = sum(freq), sample = sum(sample)) %>%
    left_join(types, by = "Graduate_type")
  
  # Make unemployment table by selecting those unemployed, grouping by grad type, shows weighted count and sample size
  unemployed <- all %>%
    filter(ILODEFR == "ILO unemployed") %>%
    group_by(Graduate_type) %>%
    summarise(count = sum(freq), sample = sum(sample)) %>%
    left_join(types, by = "Graduate_type")
  
  # Make inactive table by selecting those inactive, grouping by grad type, shows weighted count and sample size
  inactive <- all %>%
    filter(ILODEFR == "Inactive") %>%
    group_by(Graduate_type) %>%
    summarise(count = sum(freq), sample = sum(sample)) %>%
    left_join(types, by = "Graduate_type")
  
  # Create new data frame with levels of interest
  Headline_stats <- data.frame(in_employment$Graduate_type, #names each grad type
                               in_employment$count/full_pop$total, #Employment_rate each grad type
                               in_employment$sample, #Employed_sample
                               in_employment$count, #Employed_total by grad type
                               in_HSemployment$count/full_pop$total, #HSemployment_rate each grad type
                               in_HSemployment$sample, #HSemployed_sample
                               in_HSemployment$count, #HSemployed_total by grad type
                               unemployed$count/(unemployed$count + in_employment$count), #Unemployment_rate each grad type 
                               unemployed$sample, #Unemployed_sample
                               unemployed$count, #Unemployed_total by grad type
                               inactive$count/full_pop$total, #Inactive_rate each grad type 
                               inactive$sample, #Inactive_sample
                               inactive$count, #Inactive_total by grad type
                               full_pop$sample, #Full_population_sample
                               full_pop$total #Full_population_total
  )
  
  # Labels age range for each grad type
  Headline_stats$Age_range <- age_range
  
  # Column names
  colnames(Headline_stats) <- c("Population",
                                "Employment_rate", "Employed_sample", "Employed_total",
                                "HSemployment_rate", "HSemployed_sample", "HSemployed_total",
                                "Unemployment_rate", "Unemployed_sample", "Unemployed_total",
                                "Inactivity_rate", "Inactive_sample", "Inactive_total",
                                "Full_population_sample", "Full_population_total", "Age_range")
  
  
  # Reorders with age range at beginning
  Headline_stats <- Headline_stats[c("Age_range", "Population",
                                     "Employment_rate", "Employed_sample", "Employed_total", 
                                     "HSemployment_rate", "HSemployed_sample", "HSemployed_total", 
                                     "Unemployment_rate", "Unemployed_sample", "Unemployed_total", 
                                     "Inactivity_rate", "Inactive_sample", "Inactive_total", 
                                     "Full_population_sample", "Full_population_total")]
}



# 2.
## Function rewritten using dplyr where possible

#' @title Employment_rate
#' @description Gets dataset, weights and filters, runs Produce_headline_stats function to get outputs and saves each quarter in environment
#' @param quarter LFS quarter - is set in sapply, is 1:4
#' @param year LFS year - is set in sapply, uses start_year and end_year
#' @return Employment rate headline stats in global environment, labelled by _quarter_year
Employment_rate <- function(quarter, year){
  
  # Get the dataset from the global environment
  dataset <- get(paste0("Q",quarter,"_",year))
  
  # Find the weight variable
  # This has been updated so it works with datasets that have more than one weight variable (e.g. Q1_2022)
  
  #Create a dataframe with just the weight variables
  weight <- dataset %>%
    select(contains("PWT"))
  
  # Extract numeric portion from variable names
  weight <- names(weight)
  numeric_values <- as.integer(sub("PWT", "", weight))
  
  # Find most recent year suffix
  max_numeric_value <- max(numeric_values)
  
  # Assigns name of most recent weight variable as a value in environment
  weight <- paste0("PWT", max_numeric_value)
  
  # Filter dataset only keeping rows where weight is greater than 0
  dataset <- dataset %>%
    filter(dataset[,c(weight)] > 0)
  
  # Groups by age, graduate type, ILO employment status and SOC group: Calculates the weight for the group and sample size
  all <- dataset %>%
    group_by(AGE_BANDS, Graduate_type, ILODEFR, SOCHE) %>%
    summarise(freq = sum(!!sym(weight)), sample = n())
  
  # Run Produce_headline_stats to get outputs for both age groups
  
  # Headline stats for all
  Headline_stats <- Produce_headline_stats(all, "16-64")
  
  # Headline stats for young population
  Headline_stats_young <- Produce_headline_stats(all, "21-30")
  
  Headline_stats <- rbind(Headline_stats, Headline_stats_young)
  
  # This code replaces all NAs with zero (not including the population column) so the mean can be calculated properly 
  Headline_stats <- Headline_stats %>%
    mutate(across(-"Population", ~ replace_na(., 0)))
  
  # Create flags for accuracy (if sample of the quarter is less than 30)
  Headline_stats <- Headline_stats %>%
    mutate(Employment_accuracy_flag = if_else(Employed_sample <= 30, 1, 0),
           HSemployment_accuracy_flag = if_else(HSemployed_sample <= 30, 1, 0),
           Unemployment_accuracy_flag = if_else(Unemployed_sample <= 30, 1, 0),
           Inactive_accuracy_flag = if_else(Inactive_sample <= 30, 1, 0))
  
  assign(paste0("Headline_Q",quarter,"_",year), Headline_stats, envir = globalenv())
  
}


# 3. Create yearly data by combining quarterly datasets for each year (find mean rates and total sample sizes/counts for a year)


#' @title Emp_rate_year_average
#' @description Takes the headline stats for each quarter in the environment and calculates means and sums to get yearly averages
#' @param year LFS year - is set in sapply, uses start_year and end_year
#' @return Employment rate headline yearly stats in global environment, labelled by _year
Emp_rate_year_average <- function(year){
  
  # Select datasets: for example if year=2016, the first line will get Q1_2016 and set this to 'dataset1'
  dataset1 <- get(paste0("Headline_Q1_",year))
  dataset2 <- get(paste0("Headline_Q2_",year))
  dataset3 <- get(paste0("Headline_Q3_",year))
  dataset4 <- get(paste0("Headline_Q4_",year))
  
  year_data <- rbind(dataset1, dataset2, dataset3, dataset4)
  
  # Group combined quarters data by age range and graduate type, work out average/totals for the entire year 
  # na.rm = TRUE excludes any missing values from the calculations
  year_average <- year_data %>%
    group_by(Age_range, Population) %>% 
    summarise(Employment_rate = mean(Employment_rate, na.rm = TRUE),
              Employed_sample = sum(Employed_sample, na.rm = TRUE),
              Employed_total = sum(Employed_total, na.rm = TRUE),
              HSemployment_rate = mean(HSemployment_rate, na.rm = TRUE),
              HSemployed_sample = sum(HSemployed_sample, na.rm = TRUE),
              HSemployed_total = sum(HSemployed_total, na.rm = TRUE),
              Unemployment_rate = mean(Unemployment_rate, na.rm = TRUE),
              Unemployed_sample = sum(Unemployed_sample, na.rm = TRUE),
              Unemployed_total = sum(Unemployed_total, na.rm = TRUE),
              Inactivity_rate = mean(Inactivity_rate, na.rm = TRUE),
              Inactive_sample = sum(Inactive_sample, na.rm = TRUE),
              Inactive_total = sum(Inactive_total, na.rm = TRUE),
              Full_population_sample = sum(Full_population_sample, na.rm = TRUE),
              Full_population_total = sum(Full_population_total, na.rm = TRUE),
              Employment_accuracy_flag = sum(Employment_accuracy_flag, na.rm = TRUE),
              HSemployment_accuracy_flag = sum(HSemployment_accuracy_flag, na.rm = TRUE),
              Unemployment_accuracy_flag = sum(Unemployment_accuracy_flag, na.rm = TRUE),
              Inactive_accuracy_flag = sum(Inactive_accuracy_flag, na.rm = TRUE)
    )
  
  # Create flags for suppression (if sample of the combined LFS year data is less than or equal to 4)
  year_average <- year_average %>%
    mutate(Employment_suppression_flag = if_else(Employed_sample <= 4, 1, 0),
           HSemployment_suppression_flag = if_else(HSemployed_sample <= 4, 1, 0),
           Unemployment_suppression_flag = if_else(Unemployed_sample <= 4, 1, 0),
           Inactive_suppression_flag = if_else(Inactive_sample <= 4, 1, 0))
  
  # Label the year_average table with year
  year_average$Year <- year
  
  # Reorders with year is before flags
  year_average <- year_average[c("Age_range", "Population",
                                 "Employment_rate", "Employed_sample", "Employed_total", 
                                 "HSemployment_rate", "HSemployed_sample", "HSemployed_total", 
                                 "Unemployment_rate", "Unemployed_sample", "Unemployed_total", 
                                 "Inactivity_rate", "Inactive_sample", "Inactive_total", 
                                 "Full_population_sample", "Full_population_total",
                                 "Year",
                                 "Employment_accuracy_flag", "HSemployment_accuracy_flag", "Unemployment_accuracy_flag", "Inactive_accuracy_flag",
                                 "Employment_suppression_flag", "HSemployment_suppression_flag", "Unemployment_suppression_flag", "Inactive_suppression_flag")]
  
  
  
  assign(paste0("Headline_",year), year_average, envir = globalenv())
  
}



########## NEW section
# For skilled employment table/bar chart

# 4.

#' @title Produce_skilled_occ_stats
#' @description Gets outputs for SOCHE by graduate type breakdown and calculates the rates
#' @param dataset dataset, inputted when this function is called in Skilled_occ_rate
#' @return Skilled occ by graduate type
Produce_skilled_occ_stats <- function(all, age_range){
  
  # Subset data depending on whether working age ("16-64") and young ("21-30")
  if (age_range == "16-64") {
    all <- all
  }
  else {
    all <- all %>%
      filter(AGE_BANDS == "21-30")
  }
  
  #Create a dataframe with just the weight variables
  weight <- all %>%
    select(contains("PWT"))
  
  # Extract numeric portion from variable names
  weight <- names(weight)
  numeric_values <- as.integer(sub("PWT", "", weight))
  
  # Find most recent year suffix
  max_numeric_value <- max(numeric_values)
  
  # Assigns name of most recent weight variable as a value in environment
  weight <- paste0("PWT", max_numeric_value)
  
  # Filter dataset only keeping rows where weight is greater than 0
  all <- all %>%
    filter(all[,c(weight)] > 0)
  
  # Groups by graduate type, ILO employment status and SOC group: Calculates the weight for the group and sample size
  all <- all %>%
    group_by(Graduate_type, ILODEFR, SOCHE) %>%
    summarise(population = sum(!!sym(weight)), sample = n())
  
  # Get stats for total employed
  full_pop <- all %>%
    group_by(Graduate_type) %>%
    summarise(sample = sum(sample), total = sum(population))
  
  # Creates labels for graduate type
  types <- full_pop["Graduate_type"]
  
  in_HSemployment <- all %>%
    filter(ILODEFR == "In employment" & SOCHE == "High skilled employment") %>%
    group_by(Graduate_type) %>%
    summarise(sample = sum(sample), total = sum(population)) %>%
    left_join(types, by = "Graduate_type")
  
  # "Medium/low skilled employment"
  in_MLSemployment <- all  %>%
    filter(ILODEFR == "In employment" & (SOCHE == "Medium/low skilled employment")) %>%
    group_by(Graduate_type) %>%
    summarise(sample = sum(sample), total = sum(population)) %>%
    left_join(types, by = "Graduate_type")
  
  # Want to change this to dplyr but not having much luck with Copilot, possibly come back to
  Skilled_occ_breakdown <- data.frame(full_pop$Graduate_type, #names each grad type
                                      in_HSemployment$total/full_pop$total, #HS_empl rate
                                      in_HSemployment$sample,
                                      in_HSemployment$total,
                                      in_MLSemployment$total/full_pop$total, #MLS_empl rate
                                      in_MLSemployment$sample,
                                      in_MLSemployment$total,
                                      full_pop$sample,
                                      full_pop$total)
  
  # Rename columns
  colnames(Skilled_occ_breakdown) <- c("Graduate_type",
                                       "High_skilled_occ_rate", "High_skilled_occ_sample", "High_skilled_occ_total",
                                       "Med_low_skilled_occ_rate", "Med_low_skilled_occ_sample", "Med_low_skilled_occ_total",
                                       "Full_pop_sample", "Full_pop_total")
  
  # This code replaces all NAs with zero (not including the population column) so the mean can be calculated properly 
  Skilled_occ_breakdown <- Skilled_occ_breakdown %>%
    mutate(across(-"Graduate_type", ~ replace_na(., 0)))
  
  # Create flags for accuracy (if sample of the quarter is less than 30)
  Skilled_occ_breakdown <- Skilled_occ_breakdown %>%
    mutate(HS_employment_accuracy_flag = if_else(High_skilled_occ_sample <= 30, 1, 0),
           MLS_employment_accuracy_flag = if_else(Med_low_skilled_occ_sample <= 30, 1, 0))
  
  
  return(Skilled_occ_breakdown)
}


# 5.

#' @title Skilled_occ_rate
#' @description Runs the Produce_skilled_occ_stats function and adds category for age range
#' @param quarter LFS quarter - is set in Skilled_occ_rate_year function as 1, 2, 3 and 4
#' @param year LFS year - is inputted to Skilled_occ_rate_year function - only run on most recent year
#' @return Skilled off rate outputs
Skilled_occ_rate <- function(quarter, year, age_range="16-64"){
  
  # Get dataset
  dataset <- get(paste0("Q",quarter,"_",year))
  
  # Output: A table summarising statistics by skilled occupation
  Skilled_occ_rates <- Produce_skilled_occ_stats(dataset, age_range)
  
  # Add a new column 'Population' with the value "SOCHE"
  Skilled_occ_rates <- Skilled_occ_rates %>%
    mutate(Age_range = age_range)
  
  return(Skilled_occ_rates)
  
}


# 6. 
# Create yearly estimate

#' @title Skilled_occ_rate_year
#' @description Runs the Skilled_occ_rate function on each quarter then calculates means and sums to produce yearly estimates for graduate breakdown
#' @param year LFS year - is only run for most recent year, so takes end_year
#' @return Final output file
Skilled_occ_rate_year<-function(year, age_range){
  
  #select dataset: for example if quarter =1 and year =2016 this will get Q1_2016 and set this to 'dataset'
  dataset1 <- Skilled_occ_rate(1, year, age_range)
  dataset2 <- Skilled_occ_rate(2, year, age_range)
  dataset3 <- Skilled_occ_rate(3, year, age_range)
  dataset4 <- Skilled_occ_rate(4, year, age_range)
  
  year_data <- rbind(dataset1, dataset2, dataset3, dataset4)
  
  # Find the mean and samples of the rates for a year
  year_average <- year_data %>%
    group_by(Age_range, Graduate_type) %>% 
    summarise(HS_employment_rate = mean(High_skilled_occ_rate, na.rm = TRUE),
              HS_employed_sample = sum(High_skilled_occ_sample, na.rm = TRUE),
              HS_employed_total = sum(High_skilled_occ_total, na.rm = TRUE),
              MLS_employment_rate = mean(Med_low_skilled_occ_rate, na.rm = TRUE),
              MLS_employed_sample = sum(Med_low_skilled_occ_sample, na.rm = TRUE),
              MLS_employed_total = sum(Med_low_skilled_occ_total, na.rm = TRUE),
              Full_population_sample = sum(Full_pop_sample, na.rm = TRUE),
              Full_population_total = sum(Full_pop_total, na.rm = TRUE),
              HS_employment_accuracy_flag = sum(HS_employment_accuracy_flag, na.rm = TRUE),
              MLS_employment_accuracy_flag = sum(MLS_employment_accuracy_flag, na.rm = TRUE)
    )
  
  # Create flags for suppression (if sample of the combined LFS year data is less than or equal to 4)
  year_average <- year_average %>%
    mutate(HS_employment_suppression_flag = if_else(HS_employed_sample <= 4, 1, 0),
           MLS_employment_suppression_flag = if_else(MLS_employed_sample <= 4, 1, 0))
  
  assign(paste0("Skilled_occ_rates_",year), year_average, envir = globalenv())
  
}


########## Graduate breakdown

# 7. Create the functions that produce outputs for breakdowns
# Create_demographic_dataset<-function(dataset,demographic) - creates dataset grouped by specific characteristic that is piped into Produce_demographic_stats
# Produce_demographic_stats<-function(dataset, demographic) - produces the output table for each breakdown
# Both functions are then called and given parameters in the next section

# Demographic is whatever we want to break down by

#' @title Create_demographic_dataset
#' @description Creates the dataset for the given demographic/characteristic
#' @param dataset LFS dataset read in when function called in Grad_emp_rate_by_demographic
#' @param demographic demographic read in when function called in Grad_emp_rate_by_demographic
#' @return Weighted dataset grouped by demographic
Create_demographic_dataset <- function(dataset, demographic){
  
  # Find the weight variable
  # This has been updated so it works with datasets that have more than one weight variable (e.g. Q1_2022)
  
  #Create a dataframe with just the weight variables
  weight <- dataset %>%
    select(contains("PWT"))
  
  # Extract numeric portion from variable names
  weight <- names(weight)
  numeric_values <- as.integer(sub("PWT", "", weight))
  
  # Find most recent year suffix
  max_numeric_value <- max(numeric_values)
  
  # Assigns name of most recent weight variable as a value in environment
  weight <- paste0("PWT", max_numeric_value)
  
  # Filter dataset only keeping rows where weight is greater than 0
  dataset <- dataset %>%
    filter(dataset[,c(weight)] > 0)
  
  # Groups by chosen demographic, graduate type, ILO employment status and SOC group: Calculates the weight for the group and sample size
  
  as.data.frame(
    dataset %>%
      group_by_at(vars(demographic, "Graduate_type", "ILODEFR", "SOCHE")) %>%
      summarise(sample=n(), population=sum(!!sym(weight))) %>% 
      ungroup() %>%
      complete(!!sym(demographic), `Graduate_type`, `ILODEFR`, `SOCHE`, fill = list(sample = 0, population = 0)))
  
  
  # as.data.frame(
  #   dataset %>%
  #     group_by_at(vars(demographic, "Graduate_type", "ILODEFR", "SOCHE")) %>%
  #     summarise(sample=n(), population=sum(!!sym(weight))))
  
}



# 8. This is called within graduate breakdown, runs to get breakdowns
# Dataset inputted into the function has been created from Create_demographic_dataset(dataset, demographic) function

#' @title Produce_demographic_stats
#' @description Gets outputs for given demographic by graduate type breakdown and calculates the rates
#' @param dataset dataset created in Create_demographic_dataset function, grouped by demographic
#' @param demographic inputted in when function is called in Grad_emp_rate_by_demographic
#' @return Dataframe of calculated employment rate, sample and total for each grad type
Produce_demographic_stats <- function(dataset, demographic){
  
  # Group by demographic
  full_pop <- dataset %>%
    group_by_at(vars(demographic))
  
  # This gets the overall sample size and weighted count of who is in each level of the demographic variable
  full_pop <- full_pop %>%
    summarise(total = sum(population),
              full_pop.sample = sum(sample))
  
  # This gets breakdowns by certain demographic of those in employment
  in_employment <- dataset %>%
    filter(ILODEFR == "In employment") %>%
    group_by_at(vars(demographic)) %>%
    summarise(employed.count = sum(population),
              employed.sample = sum(sample))
  
  # HS employment
  in_HSemployment <- dataset %>%
    filter(ILODEFR == "In employment" & SOCHE == "High skilled employment") %>%
    group_by_at(vars(demographic)) %>%
    summarise(HSemp.count = sum(population),
              HSemp.sample = sum(sample))
  
  # Unemployment
  unemployed <- dataset %>%
    filter(ILODEFR == "ILO unemployed") %>%
    group_by_at(vars(demographic)) %>%
    summarise(unemp.count = sum(population),
              unemp.sample = sum(sample))
  
  # Inactivity
  inactive <- dataset %>%
    filter(ILODEFR == "Inactive") %>%
    group_by_at(vars(demographic)) %>%
    summarise(inactive.count = sum(population),
              inactive.sample = sum(sample))
  
  # This list stores all of the data sets that will be merged in the next step
  list <- list(full_pop, in_employment, in_HSemployment, unemployed, inactive)
  
  # Uses the merge function to combine the data frames based on a common columns (in this case, demographic)
  # The reduce function allows the merge function to be applied to each element of the list
  total_count <- Reduce(function(x, y) merge(x, y, all.x = TRUE, by = c(demographic)), list, accumulate = FALSE)
  
  # Want to set all NAs to zero before doing the calculation #################### THIS NEEDS ACTIONING!!
  
  
  # Want to change this to dplyr but not having much luck with Copilot, possibly come back to
  Graduate_breakdown <- data.frame(total_count[demographic], #column of demographic breakdowns
                                   total_count$employed.count/total_count$total, 
                                   total_count$employed.sample,
                                   total_count$employed.count,
                                   total_count$HSemp.count/total_count$total,
                                   total_count$HSemp.sample,
                                   total_count$HSemp.count,
                                   total_count$unemp.count/(total_count$unemp.count + total_count$employed.count),
                                   total_count$unemp.sample,
                                   total_count$unemp.count,
                                   total_count$inactive.count/total_count$total,
                                   total_count$inactive.sample,
                                   total_count$inactive.count,
                                   total_count$full_pop.sample,
                                   total_count$total)
  
  # Add new labels to columns
  colnames(Graduate_breakdown)<-c("Breakdown",
                                  "Employment_rate", "Employed_sample", "Employed_total",
                                  "HSemployment_rate", "HSemployed_sample", "HSemployed_total",
                                  "Unemployment_rate", "Unemployed_sample", "Unemployed_total",
                                  "Inactivity_rate", "Inactive_sample", "Inactive_total",
                                  "Full_population_sample", "Full_population_total")
  
  # This code replaces all NAs with zero (not including the first column) so the mean can be calculated properly 
  Graduate_breakdown <- Graduate_breakdown %>%
    mutate(across(-"Breakdown", ~ replace_na(., 0)))
  
  # Create flags for accuracy (if sample of the quarter is less than 30)
  Graduate_breakdown <- Graduate_breakdown %>%
    mutate(Employment_accuracy_flag = if_else(Employed_sample <= 30, 1, 0),
           HSemployment_accuracy_flag = if_else(HSemployed_sample <= 30, 1, 0),
           Unemployment_accuracy_flag = if_else(Unemployed_sample <= 30, 1, 0),
           Inactive_accuracy_flag = if_else(Inactive_sample <= 30, 1, 0))
  
  return(Graduate_breakdown)
  
}



# 9.
# this pipes the breakdown dataset into the variable splits function
# Create_demographic_dataset(dataset,"SEX") %>% 
#   Produce_demographic_stats(.,"SEX") -> Gender_rates2

#' @title Grad_emp_rate_by_demographic
#' @description Produces deompgraphic breakdowns for given characteristics by calling Create_demographic_dataset and Produce_demographic_stats functions
#' @param quarter LFS quarter - is set in Grad_emp_rate_by_demographic_year function as 1, 2, 3 and 4
#' @param year LFS year - is inputted to Grad_emp_rate_by_demographic_year function - possibly only run on most recent year
#' @return Binded list of graduate breakdown outputs for each demographic
Grad_emp_rate_by_demographic <- function(quarter,year){
  
  # Get dataset
  dataset <- get(paste0("Q",quarter,"_",year))
  
  # Select just graduates from dataset
  dataset <- dataset %>%
    filter(Graduate_type == "Graduate")
  
  # Breakdowns
  # Output: A table summarising statistics on graduates by each given characteristic breakdown
  
  # Demographic: "SEX"
  Gender_rates <- Create_demographic_dataset(dataset, "SEX") %>%
    Produce_demographic_stats(., "SEX")
  
  # Add a new column 'Population' with the value "SEX"
  Gender_rates <- Gender_rates %>%
    mutate(Population = "SEX")
  
  
  # Demographic: "ETHNICITY"
  Ethnicity_rates <- Create_demographic_dataset(dataset, "ETHNICITY") %>%
    Produce_demographic_stats(., "ETHNICITY")
  
  # Add a new column 'Population' with the value "ETHNICITY"
  Ethnicity_rates <- Ethnicity_rates %>%
    mutate(Population = "ETHNICITY")
  
  
  # Demographic: "DISEA"
  Disability_rates <- Create_demographic_dataset(dataset, "DISEA") %>%
    Produce_demographic_stats(., "DISEA")
  
  # Add a new column 'Population' with the value "DISEA"
  Disability_rates <- Disability_rates %>%
    mutate(Population = "DISEA")
  
  
  # Demographic: "DEGCLS7"
  Degree_class_rates <- Create_demographic_dataset(dataset, "DEGCLS7") %>%
    Produce_demographic_stats(., "DEGCLS7")
  
  # Add a new column 'Population' with the value "DEGCLS7"
  Degree_class_rates <- Degree_class_rates %>%
    mutate(Population = "DEGCLS7")
  
  
  # Combine all the rates in a table
  quarter <- rbind(Gender_rates, Ethnicity_rates, Disability_rates, Degree_class_rates)
  
  #label the table with the age group it belongs to
  quarter <- quarter %>%
    mutate(Group = "Graduates: 16-64")
  
  return(quarter)
  
}



# 10.
# This creates yearly estimates

#' @title Grad_emp_rate_by_demographic_year
#' @description Runs the Grad_emp_rate_by_demographic function on each quarter then calculates means and sums to produce yearly estimates for graduate breakdown
#' @param year LFS year - think is only run for most recent year, so takes end_year
#' @return Saves graduate breakdown outputs as .csv and .rds file
Grad_emp_rate_by_demographic_year<-function(year){
  
  #select dataset: for example if quarter =1 and year =2016 this will get Q1_2016 and set this to 'dataset'
  dataset1 <- Grad_emp_rate_by_demographic(1, 2024) # These presumably should read as 2024 in this script
  dataset2 <- Grad_emp_rate_by_demographic(2, 2024)
  dataset3 <- Grad_emp_rate_by_demographic(3, 2024)
  dataset4 <- Grad_emp_rate_by_demographic(4, 2024)
  
  year_data <- rbind(dataset1, dataset2, dataset3, dataset4)
  
  # Find the mean and samples of the rates for a year
  year_average <- year_data %>%
    group_by(Group, Population, Breakdown) %>%
    summarise(Employment_rate = mean(Employment_rate, na.rm = TRUE),
              Employed_sample = sum(Employed_sample, na.rm = TRUE),
              Employed_total = sum(Employed_total, na.rm = TRUE),
              HSemployment_rate = mean(HSemployment_rate, na.rm = TRUE),
              HSemployed_sample = sum(HSemployed_sample, na.rm = TRUE),
              HSemployed_total = sum(HSemployed_total, na.rm = TRUE),
              Unemployment_rate = mean(Unemployment_rate, na.rm = TRUE),
              Unemployed_sample = sum(Unemployed_sample, na.rm = TRUE),
              Unemployed_total = sum(Unemployed_total, na.rm = TRUE),
              Inactivity_rate = mean(Inactivity_rate, na.rm = TRUE),
              Inactive_sample = sum(Inactive_sample, na.rm = TRUE),
              Inactive_total = sum(Inactive_total, na.rm = TRUE),
              Full_population_sample = sum(Full_population_sample, na.rm = TRUE),
              Full_population_total = sum(Full_population_total, na.rm = TRUE),
              Employment_accuracy_flag = sum(Employment_accuracy_flag, na.rm = TRUE),
              HSemployment_accuracy_flag = sum(HSemployment_accuracy_flag, na.rm = TRUE),
              Unemployment_accuracy_flag = sum(Unemployment_accuracy_flag, na.rm = TRUE),
              Inactive_accuracy_flag = sum(Inactive_accuracy_flag, na.rm = TRUE)
    )
  
  # Create suppression flag (if less than or equal to 4)
  year_average <- year_average %>%
    mutate(Employment_suppression_flag = if_else(Employed_sample <= 4, 1, 0),
           HSemployment_suppression_flag = if_else(HSemployed_sample <= 4, 1, 0),
           Unemployment_suppression_flag = if_else(Unemployed_sample <= 4, 1, 0),
           Inactive_suppression_flag = if_else(Inactive_sample <= 4, 1, 0))
  
  year_average <- year_average %>%
    mutate(Employment_empty_flag = if_else(Employed_sample <= 4, 1, 0),
           HSemployment_empty_flag = if_else(HSemployed_sample <= 4, 1, 0),
           Unemployment_empty_flag = if_else(Unemployed_sample <= 4, 1, 0),
           Inactive_empty_flag = if_else(Inactive_sample <= 4, 1, 0))
  
  assign(paste0("Graduate_breakdown_",year), year_average, envir = globalenv())
  
}

