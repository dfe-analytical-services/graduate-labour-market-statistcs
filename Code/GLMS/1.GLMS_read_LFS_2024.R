# May 2025
# This R script reads in the raw LFS data for each quarter between 2007 and 2024 (the latest available) and extracts only the variables required for the 
# GLMS analysis and finally generate RDS format data-sets for further analysis in the production process
# The code now uses the haven package to read in the LFS data as it appears to work more effectively than the foreign package

# GLMS_read_LFS_2024.R

#
# Note: This code is based on code used pre-2023 publication, from the script GLMS_from_LFS_data.R.
# Purpose: This code takes the SPSS files and converts them all to RDS, then reads them into the R environment.
# Changes made: Code has been left as functions because it runs through each quarter and year (lots of datasets). Code has been made more simple and clear.
#               Changes to names of functions have been documented, and where functions from previous code have been omitted.

####################################################
# Previous function: Read_LFS
# New function: CSV_to_RDS

#' @title CSV_to_RDS
#' @description Reads in csv dataset, selects variables of interest, makes a cut of the data, saves as RDS file.
#'              This only needs to be run once at the begining of analysis to create RDS datasets.
#' @param quarter LFS quarter - is set in sapply, is 1:4
#' @param year LFS year - is set in sapply, uses start_year and end_year
#' @return Dataset in RDS format is saved in specified area
CSV_to_RDS <- function(quarter, year) {
  
  dataset <- haven::read_sav(paste0(data_folder, year, " Q", quarter, ".sav"), encoding = "ISO-8859-1")  #### encoding = "UTF-8")  
  # Select needed variables (grep finds variable names within dataset containing that argument, because names vary depending on year)
  colnames<-c("COUNTRY","SEX","AGE",
              names(dataset[grep("PWT", names(dataset))]), 
              names(dataset[grep("PIWT", names(dataset))]),
              names(dataset[grep("HIQUAL", names(dataset))]),
              "HIGHO",
              names(dataset[grep("DEGREE", names(dataset))]),
              "SC2KMMJ", "SC10MMJ","ETH11EW", "SC20MMJ",
              "SNGDEGB","SNGHD","FDSNGDEG","ILODEFR","DISEA","DEGCLS7",
              "FTPTWK","GRSSWK","INDE07M","UALA","CASENO","GOVTOF2","GRSSWK2","HOURPAY","YERQAL3","YERQAL2")
  
  # Check whether all in dataset
  available_columns<-colnames[which(colnames %in% names(dataset))]
  
  # Make cut of data
  dataset_cut <- dataset[,available_columns]
  
  dataset_cut <- haven::as_factor(dataset_cut) # need to convert some variables to factors like HIGHO, HIQUAL and SOC codes for further R scripts in GLMS series to run correctly - this is because of the 'haven labelling' issue
  # Note that the filter for domicile is set to England, rather than its numerical value of 1 imported from SPSS
  # Restrict to population in England aged 16-64
  dataset_cut <- dataset_cut[which(dataset_cut$COUNTRY=="England" & dataset_cut$AGE>15 & dataset_cut$AGE<65),] # domicile can stay as England and has been converted to a factor for the filtering process
  
  # Save as RDS file
  saveRDS(object = dataset_cut, file = paste0(filepath,data_folder,"Q",quarter,"_",year,".rds"))
  
} 


####################################################
# Previous function: readLFS_reweight
# Changes: This is legacy code that has been removed because no data files have _reweighted suffix.


####################################################
# Previous function: read_LFS_from_project
# New function: Read_RDS
# Changes: Have removed the error messages as don't think they are necessary, but note they are in previous code if needed in the future.
#          Removed code that deals with reweighting in 2020 legacy issue.

#' @title Read_RDS
#' @description Reads in RDS dataset for each year, loads it to global environment with appropriate name
#' @param quarter LFS quarter - is set in sapply, is 1:4
#' @param year LFS year - is set in sapply, uses start_year and end_year
#' @return Dataset in RDS format in global environment
Read_RDS <- function(quarter, year) {
  
  dataset <- readRDS(paste0(filepath, data_folder,"Q",quarter,"_",year,".rds"))
  
  # Creates name of dataset based on what year and quarter
  assign(paste0("Q", quarter, "_", year), dataset, envir = globalenv())
  
}

