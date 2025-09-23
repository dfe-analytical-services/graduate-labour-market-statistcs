# May 2025
# This R script is used to calculate the proportion of workers/employees that work part-time (not the part-time employment rate).
# Note that no information is produced on earnings for part-time workers in the GLMS analysis.


# Tables required

# Agebands 
# Proportion of employees working part time for different age groups, by postgrad, graduate and non-graduate, 2024 
# 
# Working age 
# Proportion of working age employees working part time, by postgrad, graduate and non-graduate, time series 2007-2024 
# 
# Young population 
# Proportion of young pop employees working part time, by postgrad, graduate and non-graduate, time series 2007-2024 
# 
# Gender
# Proportion of employees working part-time, by gender and grad type (i.e. female grad, male grad, female non grad, male non grad), working age, time series 2007-2024 





## Function rewritten using dplyr where possible to apply below:

#' @title PT_produce_headline_stats
#' @description Function that produces the main PT employment rate outputs. It is called within the PT_employment_rate function for all adults and then young pop
#' @param all Weighted and grouped dataset - created in PT_employment_rate function
#' @param age_range Either working age "16-64" or young population "21-30" - is set in PT_employment_rate function
#' @return Dataframe of headline employment stats
PT_produce_headline_stats <- function(all, age_range) {
  
  # Subset data depending on whether working age ("16-64") and young ("21-30")
  if (age_range == "16-64") {
    all <- all
  }
  else {
    all <- all %>%
      filter(AGE_BANDS == "21-30")
  }
  
  # Find the sample size and number of people in each graduate type population
  full_empl_pop <- all %>%
    group_by(Graduate_type) %>%
    summarise(total = sum(freq), sample = sum(sample))
  
  # Creates labels for graduate type
  types <- full_empl_pop["Graduate_type"]
  
  # Make in_PT_employment table by selecting those in PT employment, grouping by grad type, shows weighted count and sample size
  # Uses left_join to retain all values of Graduate_type even if the returning value for the given breakdown is zero (i.e. if there were no people who were postgrad and inactive, using left join will still keep postgrad as a row)
  in_PT_employment <- all %>%
    filter(FTPTWK == "Part-time") %>%
    group_by(Graduate_type) %>%
    summarise(count = sum(freq), sample = sum(sample)) %>%
    left_join(types, by = "Graduate_type")
  
  # Create new data frame with levels of interest
  Headline_stats <- data.frame(in_PT_employment$Graduate_type, #names each grad type
                               in_PT_employment$count/full_empl_pop$total, #PT_employment_rate each grad type
                               in_PT_employment$sample, #PT_employed_sample
                               in_PT_employment$count, #PT_employed_total by grad type
                               full_empl_pop$sample, #Full_employed_pop_sample
                               full_empl_pop$total #Full_employed_pop_total
                               )
  
  # Labels age range for each grad type
  Headline_stats$Age_range <- age_range
  
  # Column names
  colnames(Headline_stats) <- c("Population", "PT_employment_rate", "PT_employed_sample", "PT_employed_total", 
                              "Full_employed_pop_sample", "Full_employed_pop_total", "Age_range")
  
  # Reorders with age range at beginning
  Headline_stats <- Headline_stats[c("Age_range","Population","PT_employment_rate", "PT_employed_sample", "PT_employed_total",
                                     "Full_employed_pop_sample", "Full_employed_pop_total")]
  
}





#' @title PT_employment_rate
#' @description Gets dataset, weights and filters, runs PT_produce_headline_stats function to get outputs and saves each quarter in environment
#' @param quarter LFS quarter - is set in sapply, is 1:4
#' @param year LFS year - is set in sapply, uses start_year and end_year
#' @return PT employment rate headline stats in global environment, labelled by _quarter_year
PT_employment_rate <- function(quarter, year){
  
  # Get the dataset from the global environment
  dataset <- get(paste0("Q",quarter,"_",year))
  
  # Find the weight variable
  # This has been updated to select most recent weight, for datasets that contain more than one weight var

  # Creates a dataframe with just the weight variables
  weight <- dataset %>%
    select(contains("PWT"))
  # Extract numeric portion from variable names
  weight <- names(weight)
  numeric_values <- as.integer(sub("PWT", "", weight))
  max_numeric_value <- max(numeric_values)
  # Return most recent weight
  weight <- paste0("PWT", max_numeric_value)
  
  # Filter dataset only keeping rows where weight is greater than 0
  dataset <- dataset %>%
    filter(dataset[,c(weight)] > 0)
  # Filter dataset so only those in employment
  dataset <- dataset %>%
    filter(ILODEFR == "In employment")
  # Filter dataset to only keep cases where we know FT/PT work status
  dataset <- dataset %>%
    filter(!is.na(FTPTWK))
  
  # Groups by age, graduate type, ILO employment status and SOC group: Calculates the weight for the group and sample size
  all <- dataset %>%
    group_by(AGE_BANDS, Graduate_type, ILODEFR, FTPTWK) %>%
    summarise(freq = sum(!!sym(weight)), sample = n())
  
  # Run Produce_headline_stats to get outputs for both age groups
  
  # Headline stats for all
  Headline_stats <- PT_produce_headline_stats(all,"16-64")
  
  # Headline stats for young population
  Headline_stats_young <- PT_produce_headline_stats(all,"21-30")
  
  Headline_stats_all <- rbind(Headline_stats, Headline_stats_young)
  
  # Create flags for accuracy (if sample of the quarter is less than 30)
  Headline_stats_all <- Headline_stats_all %>%
    mutate(PT_employment_accuracy_flag = if_else(PT_employed_sample <= 30, 1, 0),
           Full_employed_pop_accuracy_flag = if_else(Full_employed_pop_sample <= 30, 1, 0))
  
  assign(paste0("Headline_Q",quarter,"_",year), Headline_stats_all, envir = globalenv())
  
}


# 3. Create yearly data by combining quarterly datasets for each year (find mean rates and total sample sizes/counts for a year)


#' @title PT_emp_rate_year_average
#' @description Takes the headline stats for each quarter in the environment and calculates means and sums to get yearly averages
#' @param year LFS year - is set in sapply, uses start_year and end_year
#' @return Employment rate headline yearly stats in global environment, labelled by _year
PT_emp_rate_year_average <- function(year){
  
  # Select datasets: for example if year=2016, the first line will get Q1_2016 and set this to 'dataset1'
  dataset1 <- get(paste0("Headline_Q1_",year))
  dataset2 <- get(paste0("Headline_Q2_",year))
  dataset3 <- get(paste0("Headline_Q3_",year))
  dataset4 <- get(paste0("Headline_Q4_",year))
  
  year_data <- rbind(dataset1,dataset2,dataset3,dataset4)
  
  # This code replaces all NAs with zero (not including the population column) so the mean can be calculated properly 
  year_data <- year_data %>%
    mutate(across(-"Population", ~ replace_na(., 0)))
  
  # Group combined quarters data by age range and graduate type, work out average/totals for the entire year 
  # na.rm = TRUE excludes any missing values from the calculations
  ##### rerun and check this works bc only giving 1 obs??#####
  year_average <- year_data %>%
    group_by(Age_range, Population) %>% 
    summarise(PT_employment_rate = mean(PT_employment_rate, na.rm = TRUE),
              PT_employed_sample = sum(PT_employed_sample, na.rm = TRUE),
              PT_employed_total = sum(PT_employed_total, na.rm = TRUE),
              Full_employed_pop_sample = sum(Full_employed_pop_sample, na.rm = TRUE),
              Full_employed_pop_total = sum(Full_employed_pop_total, na.rm = TRUE),
              PT_employment_accuracy_flag = sum(PT_employment_accuracy_flag, na.rm = TRUE),
              Full_employed_pop_accuracy_flag = sum(Full_employed_pop_accuracy_flag, na.rm = TRUE),
    )
  
  # Label the year_average table with year
  year_average$Year <- year
  
  # Create flags for suppression (if sample of the combined LFS year data is less than or equal to 4)
  year_average <- year_average %>%
    mutate(PT_employment_suppression_flag = if_else(PT_employed_sample <= 4, 1, 0),
           Full_employed_pop_suppression_flag = if_else(Full_employed_pop_sample <= 4, 1, 0))
  
  # Reorders with year is before flags
  year_average <- year_average[c("Age_range", "Population", "PT_employment_rate", "PT_employed_sample", "PT_employed_total",
                                 "Full_employed_pop_sample", "Full_employed_pop_total", "Year",
                                 "PT_employment_accuracy_flag", "Full_employed_pop_accuracy_flag",
                                 "PT_employment_suppression_flag", "Full_employed_pop_suppression_flag")]
  
  assign(paste0("Headline_",year), year_average, envir = globalenv())
  
}


# The above code has produced weighted counts for part time employees by each grad type, for working age and young pop


# 4. Create the functions that produce outputs for breakdowns
# Create_demographic_dataset<-function(dataset,demographic) - creates dataset grouped by specific characteristic that is piped into Produce_demographic_stats
# Produce_demographic_stats<-function(dataset, demographic) - produces the output table for each breakdown
# Both functions are then called and given parameters in the next section

# Demographic is whatever we want to break down by

#' @title PT_create_demographic_dataset
#' @description Creates the dataset for the given demographic/characteristic
#' @param dataset LFS dataset read in when function called in Grad_emp_rate_by_demographic
#' @param demographic demographic read in when function called in Grad_emp_rate_by_demographic
#' @return Weighted dataset grouped by demographic
PT_create_demographic_dataset <- function(dataset, demographic){
  
  # Find the weight variable
  # This has been updated to select most recent weight, for datasets that contain more than one weight var
  
  # Creates a dataframe with just the weight variables
  weight <- dataset %>%
    select(contains("PWT"))
  # Extract numeric portion from variable names
  weight <- names(weight)
  numeric_values <- as.integer(sub("PWT", "", weight))
  max_numeric_value <- max(numeric_values)
  # Return most recent weight
  weight <- paste0("PWT", max_numeric_value)
  
  # Filter dataset only keeping rows where weight is greater than 0
  dataset <- dataset %>%
    filter(dataset[,c(weight)] > 0)
  # Filter dataset so only those in employment
  dataset <- dataset %>%
    filter(ILODEFR == "In employment")
  # Filter dataset to only keep cases where we know FT/PT work status
  dataset <- dataset %>%
    filter(!is.na(dataset$FTPTWK))
  
  # Groups by chosen demographic, graduate type, ILO employment status and SOC group: Calculates the weight for the group and sample size
  # New addition to ensure the groups which have no people in them are still outputted as a zero
  dataset <- as.data.frame(
    dataset %>%
      group_by_at(vars(demographic, "Graduate_type", "ILODEFR", "FTPTWK")) %>%
      summarise(sample = n(), population = sum(!!sym(weight))) %>%
      ungroup() %>%
      complete(!!sym(demographic), `Graduate_type`, `ILODEFR`, `FTPTWK`, fill = list(sample = 0, population = 0)))
  
  dataset <- dataset %>%
    filter(ILODEFR == "In employment")
  
}






# This is called within graduate breakdown, runs to get breakdowns
# Dataset inputted into the function has been created from Create_demographic_dataset(dataset, demographic) function

#' @title PT_produce_demographic_stats
#' @description Gets outputs for given demographic by graduate type breakdown and calculates the rates
#' @param dataset dataset created in PT_create_demographic_dataset function, grouped by demographic
#' @param demographic inputted in when function is called in PT_emp_rate_by_demographic
#' @return Dataframe of calculated employment rate, sample and total for each grad type
PT_produce_demographic_stats<-function(dataset, demographic){
  
  # Group by demographic
  full_empl_pop <- dataset %>%
    group_by_at(vars(demographic, "Graduate_type")) 
  
  full_empl_pop <- full_empl_pop %>%
    summarise(total = sum(population),
              sample = sum(sample))

  # This gets breakdowns by certain demographic of those in PT employment
  in_PT_employment <- dataset %>%
    filter(FTPTWK == "Part-time") %>%
    group_by_at(vars(demographic, "Graduate_type")) %>%
    summarise(PT_employed.count = sum(population),
              PT_employed.sample = sum(sample)) 
  
  # This list stores all of the data sets that will be merged in the next step
  list <- list(full_empl_pop, in_PT_employment)
  
  # Uses the merge function to combine the data frames based on a common columns (in this case, demographic)
  # The reduce function allows the merge function to be applied to each element of the list
  total_count <- Reduce(function(x, y) merge(x, y, all.x = TRUE, by = c(demographic, "Graduate_type")), list, accumulate = FALSE)
  
  # Want to change this to dplyr but not having much luck with Copilot, possibly come back to
  PT_breakdown <- data.frame(total_count$Graduate_type, #names each grad type
                             total_count[demographic], #column of demographic breakdowns
                             total_count$PT_employed.count/total_count$total, 
                             total_count$PT_employed.sample,
                             total_count$PT_employed.count,
                             total_count$sample,
                             total_count$total)
                                   
  colnames(PT_breakdown) <- c("Graduate_type", "Breakdown","PT_employment_rate","PT_employed_sample",
                                  "PT_employed_total", "Full_employed_pop_sample", "Full_employed_pop_total")
  
  # Create flags for accuracy (if sample of the quarter is less than 30)
  PT_breakdown <- PT_breakdown %>%
    mutate(PT_employment_accuracy_flag = if_else(PT_employed_sample <= 30, 1, 0))

  return(PT_breakdown)
}

#' @title PT_emp_rate_by_demographic
#' @description Produces deompgraphic breakdowns for given characteristics by calling PT_create_demographic_dataset and PT_produce_demographic_stats functions
#' @param quarter LFS quarter - is set in PT_emp_rate_by_demographic_year function as 1, 2, 3 and 4
#' @param year LFS year - is inputted to PT_emp_rate_by_demographic_year function - possibly only run on most recent year
#' @return Binded list of graduate breakdown outputs for each demographic
PT_emp_rate_by_demographic <- function(quarter,year){
  
  # Get dataset
  dataset <- get(paste0("Q",quarter,"_",year))
  
  # Breakdowns
  # Output: A table summarising statistics on graduates by each given characteristic breakdown
  
  # Demographic: "AGE_BANDS"
  Age_band_rates <- PT_create_demographic_dataset(dataset, "AGE_BANDS") %>%
    PT_produce_demographic_stats(., "AGE_BANDS")
  
  # Add a new column 'Population' with the value "AGE_BANDS"
  Age_band_rates <- Age_band_rates %>%
    mutate(Population = "AGE_BANDS")
  
  # Demographic: "SEX"
  Gender_rates <- PT_create_demographic_dataset(dataset, "SEX") %>%
    PT_produce_demographic_stats(., "SEX")
  
  # Add a new column 'Population' with the value "SEX"
  Gender_rates <- Gender_rates %>%
    mutate(Population = "SEX")

  
  # Combine all the rates in a table
  quarter <- rbind(Age_band_rates, Gender_rates)
  
  #label the table with the age group it belongs to
  quarter <- quarter %>%
    mutate(Group = "PT workers: 16-64")
  
  return(quarter)
  
}


# 6.
# This creates yearly estimates

#' @title PT_emp_rate_by_demographic_year
#' @description Runs the PT_emp_rate_by_demographic function on each quarter then calculates means and sums to produce yearly estimates for breakdowns, for PT students
#' @param year LFS year - think is only run for most recent year, so takes end_year
#' @return Saves graduate breakdown outputs as .csv and .rds file
PT_emp_rate_by_demographic_year<-function(year){
  
  #select dataset: for example if quarter =1 and year =2016 this will get Q1_2016 and set this to 'dataset'
  dataset1 <- PT_emp_rate_by_demographic(1,year)
  dataset2 <- PT_emp_rate_by_demographic(2,year)
  dataset3 <- PT_emp_rate_by_demographic(3,year)
  dataset4 <- PT_emp_rate_by_demographic(4,year)
  
  year_data <- rbind(dataset1, dataset2, dataset3, dataset4)
  
  year_data <- year_data %>%
    mutate(across(-c("Group", "Population", "Breakdown"), ~ replace_na(., 0)))
  
  # Find the mean and samples of the rates for a year
  year_average <- year_data %>%
    group_by(Group, Population, Breakdown, Graduate_type) %>% 
    summarise(PT_employment_rate = mean(PT_employment_rate, na.rm = TRUE),
              PT_employed_sample = sum(PT_employed_sample, na.rm = TRUE),
              PT_employed_total = sum(PT_employed_total, na.rm = TRUE),
              Full_employed_pop_sample = sum(Full_employed_pop_sample, na.rm = TRUE),
              Full_employed_pop_total = sum(Full_employed_pop_total, na.rm = TRUE),
              PT_employment_accuracy_flag = sum(PT_employment_accuracy_flag, na.rm = TRUE)
    )
  
  # Label the year_average table with year
  year_average$Year <- year
  
  # Create flags for suppression (if sample of the combined LFS year data is less than or equal to 4)
  year_average <- year_average %>%
    mutate(PT_employment_suppression_flag = if_else(PT_employed_sample <= 4, 1, 0)) %>%
    mutate(PT_employment_empty_flag = if_else(PT_employed_sample <= 0, 1, 0))
  
  # Reorder (check how want to do this, can make it consistent across the script)
  year_average <- year_average[c("Group", "Population", "Breakdown", "Graduate_type",
                                 "PT_employment_rate", "PT_employed_sample", "PT_employed_total",
                                 "Full_employed_pop_sample", "Full_employed_pop_total",
                                 "Year",
                                 "PT_employment_accuracy_flag",
                                 "PT_employment_suppression_flag",
                                 "PT_employment_empty_flag")]
  
  assign(paste0("PT_breakdown_",year),year_average,envir = globalenv())
  
}



