# May 2025 - this script controls the overall running of the GLMS production process and calls up additional R scripts to 
# generate all the relevant data-sets and then the output files covering employment and earnings figures.

# Load packages
library("foreign")
library("dplyr")
library("readxl")
library("magrittr")
library("tidyverse")
library("spatstat.geom")
library("Hmisc")
library("matrixStats")
library("modi")
library("spatstat")
library("data.table")
library("stringi")
library("lubridate")


##########
# Set up #
##########




# Get file path and years updated here before starting to run the code
filepath <- data_folder
start_year = 2007
end_year = 2024


############
# Get data #
############

# Sourcing the script so that functions in the script canbe called in this script
source("Code/1.GLMS_read_LFS_2024.R")

# This only needs to be run once as it creates and saves all of the RDS files
# sapply(start_year:end_year, function(y)sapply(1:4,function(x)CSV_to_RDS(x,y))) # data tables no generated
 
# Run this to get all dataset quarters in the global environment
# Note: Invisible function suppresses unnecessary outputs from the sapply function
invisible(sapply(start_year:end_year, function(y)sapply(1:4,function(x)Read_RDS(x,y))))


###############################
# Cleaning of specific issues #
###############################

#### Fix for issue in Q1_2015
# HIQUAL is made up of numbers rather than the HIQUAL values, so this code assigns value labels to the numbers
# This code uses the value labels of Q1_2016 to recode the numbers as text strings.
# Note: This is not automated as only ran for one quarter, so running the source line below will run the manual code in the script

# library(data.table)

# Check that 2016 data is loaded because value labels are taken from this year
ifelse(!(2016 %in% start_year:end_year) & 2015 %in% start_year:end_year,
       "Need to load 2016 data.",NA)

# Assigns missing values to Q1_2015
ifelse(2015 %in% start_year:end_year & 2016 %in% start_year:end_year,
       source("Code/1.2.GLMS_HIQUAL_Q1_2015_fix_2024.R"),NA)
 
 
# ##### Fix for issue where HIQUAL recode was not recognising ¿
# For the time being, this is manual code which 
# NEEDS TO BE RUN BY OPENING UP THE SCRIPT AND RUNNING IT MANUALLY (can ctrl+A to highlight all code, then ctrl+enter)
source("Code/1.1.GLMS_HIQUAL_fix_functions_2024.R")

# Q1_2015$HIQUAL15 <- haven::as_factor(Q1_2015$HIQUAL15)
####################
# Recode variables #
####################

source("Code/2.GLMS_grad_type_recode_2024.R")
invisible(sapply(start_year:end_year, function(y)sapply(1:4, function(x)Graduate_type_recode(x,y)))) 

source("Code/3.GLMS_other_recodes_2024.R")
invisible(sapply(start_year:end_year, function(y)sapply(1:4, function(x)Other_recodes(x,y))))

###################
# Employment rate #
###################


source("Code/4.GLMS_employment_rate_2024.R")
       
invisible(sapply(start_year:end_year, function(y)sapply(1:4, function(x)Employment_rate(x,y))))

#Create headline stats for each year (mean average of rates in each quarter)
invisible(sapply(start_year:end_year, function(x)Emp_rate_year_average(x)))

list <- lapply(start_year:end_year,function(x)get(paste0("Headline_",x)))

Yearly_employment <- Reduce(rbind,list)

write.csv(Yearly_employment,
          paste0(filepath,data_folder,start_year,"_",end_year,".csv"))

saveRDS(object = Yearly_employment,
        file = paste0(filepath,data_folder,start_year,"_",end_year,".rds"))


# Skilled occupation table (2024 only) - amendments made to extract both working age and young population outputs
adult_pop <- Skilled_occ_rate_year(end_year, age_range="16-64")
young_pop <- Skilled_occ_rate_year(end_year, age_range="21-30")
Skilled_occ_rates_2024 <- rbind(adult_pop, young_pop) 

write.csv(Skilled_occ_rates_2024,
          paste0(filepath, data_folder,end_year,".csv"))

saveRDS(object = Skilled_occ_rates_2024,
        file = paste0(filepath, data_folder,end_year, ".rds"))


#And employment rate for the graduate breakdowns (2024 only)
Grad_emp_rate_by_demographic_year(end_year)

write.csv(Graduate_breakdown_2024,
          paste0(filepath, data_folder,end_year,".csv"))

saveRDS(object = Graduate_breakdown_2024,
        file = paste0(filepath, data_folder,end_year, ".rds"))



#############
# Part-time employment #
#############

source("Code/5.GLMS_part_time_2024.R")

invisible(sapply(start_year:end_year, function(y)sapply(1:4, function(x)PT_employment_rate(x,y))))

invisible(sapply(start_year:end_year,function(x)PT_emp_rate_year_average(x)))

# Creates a list of all of the outputs
list <- lapply(start_year:end_year,function(x)get(paste0("Headline_",x))) 

# Combines list of data frames into single data frame, made up of outputs for all years
Yearly_PT_employment <- Reduce(rbind,list)

write.csv(Yearly_PT_employment,
          paste0(filepath, data_folder,start_year,"_",end_year,".csv"))

saveRDS(object = Yearly_PT_employment,
        file = paste0(filepath, data_folder,start_year,"_",end_year,".rds"))


# For age and sex breakdowns
# Age bands - only need most recent year (but outputs as time series with sex)
# Sex - need time series

invisible(sapply(start_year:end_year,function(x)PT_emp_rate_by_demographic_year(x))) #This takes v long to run

list <- lapply(start_year:end_year,function(x)get(paste0("PT_breakdown_",x)))

Breakdowns_PT_employment <- Reduce(rbind,list)

write.csv(Breakdowns_PT_employment,
          paste0(filepath, data_folder,start_year,"_",end_year,".csv"))

saveRDS(object = Breakdowns_PT_employment,
        file = paste0(filepath, data_folder,start_year,"_",end_year,".rds"))




##########
# Salary #
##########

##### Inflation
# Note this is not a function, so running the below code will run the code within that script
# This script creates a new variable GRSSWK_real which adjusts for inflation
source("Code/6.GLMS_inflation_2024.R")

source("Code/7.GLMS_salary_2024.R")

# Note: Salaries script may return some warning messages about outliers in tables
# but the tables have still been created

invisible(sapply(start_year:end_year, function(y)Average_salary(y)))

list <- lapply(start_year:end_year,function(x)get(paste0("Salary_",x)))

Yearly_salaries <- Reduce(rbind,list)

write.csv(Yearly_salaries,
          paste0(filepath, data_folder,start_year,"_",end_year,".csv"))

saveRDS(object = Yearly_salaries,
        file = paste0(filepath, data_folder,start_year,"_",end_year,".rds"))

#And salaries for the graduate breakdowns, end year only
Grad_breakdown_salary(end_year) 


write.csv(All_grad_sal_2024,
          paste0(filepath, data_folder,end_year,".csv"))

saveRDS(object = All_grad_sal_2024,
        file = paste0(filepath, data_folder,end_year,".rds"))



###############
# EES outputs #
###############


#labels for data file for EES platform timeseries i.e 2007 - 2024 should be 200724
label_EES <-as.numeric(paste0(start_year,stri_sub(end_year,-2,-1)))

#### Convert data files for EES
# OPEN THESE SCRIPTS AND RUN THE CODE MANUALLY (ctrl+a highlights it all, then ctrl+enter to run)
# Outputs save in Outputs/EES_csv

# Time series, employment and salaries
source("Code/8.1.EES_time_series_2024.R")

# Graduate breakdowns for employment and salary
source("Code/8.2.EES_grad_breakdown_2024.R")

# Part-time
source("Code/8.3.EES_part_time_2024.R")




