# May 2025
# This R script is used to process the LFS data from the HIGHQUAL, HIGHO and DEGREE related variables and categorise the individuals into one of the 
# graduate groups of postgraduate, graduate, non-graduate and not in scope (these are excluded from the GLMS analysis)


#' @title Graduate_type_recode
#' @description Cleans variables used to create graduate type groupings, then recodes them into Graduate, Post grad, Non grad, Not in scope
#'              Note warning messages that can come up, identifies where recode may have not worked how we want
#' @param quarter LFS quarter - is set in sapply, is 1:4
#' @param year LFS year - is set in sapply, uses start_year and end_year
#' @return Datasets in environment with Graduate_type variable
Graduate_type_recode <- function(quarter, year) {
  
  ###############
  # Set up code #
  ###############
  
  # Packages
  library(dplyr)
  library(tidyr)
  
  # Get dataset
  dataset <- get(paste0("Q",quarter,"_",year))
  
  # Get correct HIQUAL variable
  HIQUAL_var <- names(dataset[grep("HIQUAL", names(dataset))])[1]
  
  
  # Checks data is right type, if it is numeric, turns it to a factor
  if(all(class(dataset[,c(HIQUAL_var)])=="numeric")){ # NEW INSERTION OF ALL FUNCTION HERE - MARCH 2025
    #add in error warning if not a factor/string variable
    paste0("Warning: dataset Q",quarter," ",year," is missing factor levels in HIQUAL")
 }
  
  # Turns to a factor
  if(all(class(dataset[,c(HIQUAL_var)])=="numeric")){ # NEW INSERTION OF ALL FUNCTION HERE - MARCH 2025
    dataset[,c(HIQUAL_var)] <- as.factor(dataset[,c(HIQUAL_var)])
 #    dataset[,c(HIQUAL_var)] <- haven::as_factor(dataset[,c(HIQUAL_var)])
    } 
  
  
  ####################################
  # Cleaning qualification variables #
  ####################################
  
  # DEGREE71-75 cleaning
  # DEGREE7: Type of degree already held
  # New levels: "Higher degree", "First degree", "Foundation degree", Other" (and NA)
  
  # Note: This is a multi-response variable, so this code creates a single variable
  # If any is higher degree, set as higher - if any is first degree and none higher degree, set as first 
  # - if any is foundation degree and non higher or first, set as foundation - if neither foundation first or higher degree, set to other.  
  # Make sure there are no more than 5 "DEGREE7..." variables in newer datasets
  dataset <- dataset %>%
    mutate(DEGREE7_recode = case_when((DEGREE71=="higher degree" | DEGREE72=="higher degree" | DEGREE73=="higher degree" | DEGREE74=="higher degree" | DEGREE75=="higher degree") ~ "Higher degree",
                                      
                                      (DEGREE71=="first degree" | DEGREE72=="first degree" | DEGREE73=="first degree" | DEGREE74=="first degree" | DEGREE75=="first degree") &
                                        (DEGREE71!="higher degree" | DEGREE72!="higher degree" | DEGREE73!="higher degree" | DEGREE74!="higher degree" | DEGREE75!="higher degree") ~ "First degree",
                                      
                                      (DEGREE71=="foundation degree" | DEGREE72=="foundation degree" | DEGREE73=="foundation degree" | DEGREE74=="foundation degree" | DEGREE75=="foundation degree") &
                                        (DEGREE71!="first degree" | DEGREE72!="first degree" | DEGREE73!="first degree" | DEGREE74!="first degree" | DEGREE75!="first degree") &
                                        (DEGREE71!="higher degree" | DEGREE72!="higher degree" | DEGREE73!="higher degree" | DEGREE74!="higher degree" | DEGREE75!="higher degree") ~ "Foundation degree",
                                      
                                      (DEGREE71!="first degree" | DEGREE72!="first degree" | DEGREE73!="first degree" | DEGREE74!="first degree" | DEGREE75!="first degree") & 
                                        (DEGREE71!="higher degree" | DEGREE72!="higher degree" | DEGREE73!="higher degree" | DEGREE74!="higher degree" | DEGREE75!="higher degree") &
                                        (DEGREE71!="foundation degree" | DEGREE72!="foundation degree" | DEGREE73!="foundation degree" | DEGREE74!="foundation degree" | DEGREE75!="foundation degree") ~ "Other"))
  
  
  # This version of the recode is used for setting those where we aren't sure of what higher degree they have but we know that any of DEGREE7 is first degree, so we can be sure they have a first degree
  # Level: "Any first degree" (and NA), only need one level to use as a flag
  dataset <- dataset %>%
    mutate(DEGREE7_recode_first = case_when((DEGREE71=="first degree" | DEGREE72=="first degree" | DEGREE73=="first degree" | DEGREE74=="first degree" | DEGREE75=="first degree") ~ "Any first degree"))
  
  
  # HIGHO cleaning
  # Question: Type of higher degree
  # New levels: higher degree qualifications, "Not known" (and NA)
  dataset <- dataset %>%
    mutate(HIGHO_recode = case_when(HIGHO =="Dont know" | HIGHO == "NA" | HIGHO == "-9" ~ "Not known",
                                    TRUE ~ HIGHO))
  
  
  # New code inserted below to replace the routine above in March 2025. This runs correctly and the HIQUAL_recode to be generated and allocate unknown values using the original variable HIQUAL_var
  dataset <- dataset %>%
    mutate(HIQUAL_recode = !!sym(HIQUAL_var),
           HIQUAL_recode = ifelse(HIQUAL_recode %in% c("Don.t know", "Don't know", "Did not know", "Don¿t know", "Don?t know", "DK", "-9", "-8", "Don¿t know"),  "Not known", as.character(HIQUAL_recode)),
           HIQUAL_recode = factor(HIQUAL_recode, levels = c("Not known", levels(!!sym(HIQUAL_var)))))
  
  
  

  # HIQUAL Post grad, Graduate, Non grad cleaning
  # Builds off of HIQUAL_recode above and can therefore use dplyr functions so code is more clear
  # Levels: "Higher degree", "Other graduate quals", "First or foundation degree", "Other quals", "Not known" (and NA)
  
  dataset <- dataset %>%
    mutate(HIQUAL_recode = case_when(HIQUAL_recode == "Higher degree" | HIQUAL_recode == "Higher Degree" ~ "Higher degree", #if HIGHO is valid
                                     
                                     HIQUAL_recode == "Degree Apprenticsehip" | #spelling incorrect in dataset
                                       HIQUAL_recode == "Degree Apprenticeship" |
                                       HIQUAL_recode == "Graduate Apprenticsehip"| #spelling incorrect in dataset
                                       HIQUAL_recode == "Degree Apprenticeship" |
                                       HIQUAL_recode == "Level 8 Diploma"|
                                       HIQUAL_recode == "Level 8 Certificate"|
                                       HIQUAL_recode == "Level 8 Award"|
                                       HIQUAL_recode == "RQF level 8 qualifications"|
                                       HIQUAL_recode == "Level 7 Certificate"|
                                       HIQUAL_recode == "Level 7 Diploma"|
                                       HIQUAL_recode == "RQF level 7 qualifications"|
                                       HIQUAL_recode == 'Level 7 Award'|
                                       HIQUAL_recode == 'Level 6 Award'|
                                       HIQUAL_recode == 'Level 6 Certificate'|
                                       HIQUAL_recode == 'Level 6 Diploma'|
                                       HIQUAL_recode == "RQF level 6 qualifications" ~ "Other graduate quals", #if DEGREE7 is "First degree" or "Higher degree"
                                     
                                     HIQUAL_recode == "First degree/foundation degree"|
                                       HIQUAL_recode == "First/Foundation degree" |
                                       HIQUAL_recode == "First or Foundation Degree" #different wording in HIQUAL22 Instead of a slash, they have used 'or' (watch out for this in future data)
                                     ~ "First or foundation degree", #if DEGREE7 is "First degree" (or DEGREE4)
                                     
                                     HIQUAL_recode == "HNC/HND Higher" |
                                       HIQUAL_recode == "Teaching further education" |
                                       HIQUAL_recode == "Teaching .further education" | #in 2012 datasets, added to code 2023 
                                       HIQUAL_recode == "Teaching secondary education" |
                                       HIQUAL_recode == "Teaching .secondary education" | #in 2012 datasets, added to code 2023
                                       HIQUAL_recode == "Teaching primary education" |
                                       HIQUAL_recode == "Teaching .primary education" | #in 2012 datasets, added to code 2023
                                       HIQUAL_recode == "Teaching foundation stage" |
                                       HIQUAL_recode == "Teaching .foundation stage" | #in 2012 datasets, added to code 2023
                                       HIQUAL_recode == "Teaching level not stated" |
                                       HIQUAL_recode == "Teaching .level not stated" | #in 2012 datasets, added to code 2023
                                       HIQUAL_recode == "Teaching ¿secondary education" | #these are not being recognised by this code, but have been manually changed in the 1.5 cleaning script
                                       HIQUAL_recode == "Teaching ¿primary education" | #these are not being recognised, but have been manually changed in the 1.5 cleaning script
                                       HIQUAL_recode == "Teaching ¿level not stated" | #these are not being recognised, but have been manually changed in the 1.5 cleaning script
                                       HIQUAL_recode == "Teaching ¿further education" | #these are not being recognised, but have been manually changed in the 1.5 cleaning script
                                       HIQUAL_recode == "Teaching ¿foundation stage" | #these are not being recognised, but have been manually changed in the 1.5 cleaning script
                                       HIQUAL_recode == "Nursing etc" |
                                       HIQUAL_recode == "RSA/OCR/Cambridge technical Level 4" |
                                       HIQUAL_recode == "Other higher education below degree level" |
                                       HIQUAL_recode == "SVQ level 4" |
                                       HIQUAL_recode == "RQF level 4 qualifications" |
                                       HIQUAL_recode == "Higher Apprenticeship" |
                                       HIQUAL_recode == "Modern, Technical, & Professional Apprenticeship" |
                                       HIQUAL_recode == "Advanced Higher (Scotland)" |
                                       HIQUAL_recode == "RQF level 3 qualifications" |
                                       HIQUAL_recode == "SVQ level 3 (SCQF level 7)" |
                                       HIQUAL_recode == "OND/ONC National" |
                                       HIQUAL_recode == "Advanced or Progression Diploma (14-19)" |
                                       HIQUAL_recode == "Advanced Welsh Baccalaureate" |
                                       HIQUAL_recode == "Advanced Welsh Bac'te" | #in Q4 2010 and 2007
                                       HIQUAL_recode == "International Baccalaureate" |
                                       HIQUAL_recode == "Scottish Baccalaureate" |
                                       HIQUAL_recode == "GNVQ/GSVQ Advanced" |
                                       HIQUAL_recode == "RQF level 5 qualifications" |
                                       HIQUAL_recode == "A level or equivalent" |
                                       HIQUAL_recode == "RSA/OCR/Techni level 3" |
                                       HIQUAL_recode == "Advanced Apprenticeship" |
                                       HIQUAL_recode == "Foundation Apprenticeship" |
                                       HIQUAL_recode == "Scottish 6th year certificate" |
                                       HIQUAL_recode == "National Scottish Higher" |
                                       HIQUAL_recode == "Access Qualifications" |
                                       HIQUAL_recode == "AS level or equivalent" |
                                       HIQUAL_recode == "Trade Apprenticeship" |
                                       HIQUAL_recode == "RQF level 2 qualifications" |
                                       HIQUAL_recode == "SVQ level 2" |
                                       HIQUAL_recode == "Intermediate Apprenticeship" |
                                       HIQUAL_recode == "Intermediate Welsh Bac" |
                                       HIQUAL_recode == "Intermediate Welsh Bac'te" | #in Q4 2010, added to code 2023
                                       HIQUAL_recode == "National Scott Intermediate 2" |
                                       HIQUAL_recode == "GNVQ/GSVQ Intermediate" |
                                       HIQUAL_recode == "RSA/OCR/ Cambridge Technical level 2" |
                                       HIQUAL_recode == "Higher Diploma (14-19)" |
                                       HIQUAL_recode == "Scottish National level 5" |
                                       HIQUAL_recode == "O Level/GCSE Grades A* - C and equivalents" |
                                       HIQUAL_recode == "RQF Level 1 Qualifications" |
                                       HIQUAL_recode == "SVQ Level 1" |
                                       HIQUAL_recode == "Foundation Welsh Bac" |
                                       HIQUAL_recode == "GNVQ/GSVQ Foundation Level" |
                                       HIQUAL_recode == "RSA/OCR/ Cambridge Technical Level 1" |
                                       HIQUAL_recode == "Scottish National Level 4" |
                                       HIQUAL_recode == "Scottish National level 4" | #in 2023 datasets, added to code 2023
                                       HIQUAL_recode == "GCSE < Grade C, CSE < Grade 1 and equivalents" |
                                       HIQUAL_recode == "SCOTVEC National Certificate modules" |
                                       HIQUAL_recode == "National Scottish Intermediate 1" |
                                       HIQUAL_recode == "National Scottish Intermediate 2" | #in 2022 datasets, added to code 2023 
                                       HIQUAL_recode == "14-19 Diploma" |
                                       HIQUAL_recode == "YT/YTP Certificate" |
                                       HIQUAL_recode == "Scottish National Level 3" |
                                       HIQUAL_recode == "Scottish National < Level 3" |
                                       HIQUAL_recode == "Basic Skills Qualifications" |
                                       HIQUAL_recode == "National Scottish Access Level" |
                                       HIQUAL_recode == "Entry Level Qualifications" |
                                       HIQUAL_recode == "Key/Core/ Essential/ functional Skills" |
                                       HIQUAL_recode == "Other RSA/OCR/Cambridge Technicals" |
                                       HIQUAL_recode == "Other Qualifications" |
                                       HIQUAL_recode == "No qualifications" |
                                       HIQUAL_recode == "NVQ level 5" |
                                       HIQUAL_recode == "Other degree" |
                                       HIQUAL_recode == "Other Degree" |
                                       HIQUAL_recode == "NVQ level 4" |
                                       HIQUAL_recode == "Level 6 Diploma" |
                                       HIQUAL_recode == "Level 6 Certificate" |
                                       HIQUAL_recode == "Diploma in higher education" |
                                       HIQUAL_recode == "Level 5 Diploma" |
                                       HIQUAL_recode == "Level 5 Certificate" |
                                       HIQUAL_recode == "HNC/HND/BTEC higher etc" |
                                       HIQUAL_recode == "Teaching Ð further education" |
                                       HIQUAL_recode == "Teaching Ð secondary education" |
                                       HIQUAL_recode == "Teaching Ð primary education" |
                                       HIQUAL_recode == "Teaching Ð foundation stage" |
                                       HIQUAL_recode == "Teaching Ð level not stated" |
                                       HIQUAL_recode == "Nursing etc" |
                                       HIQUAL_recode == "RSA higher diploma" |
                                       HIQUAL_recode == "Other higher education below degree" |
                                       HIQUAL_recode == "Level 4 Diploma" |
                                       HIQUAL_recode == "Level 4 Certificate" |
                                       HIQUAL_recode == "Level 5 Award" |
                                       HIQUAL_recode == "NVQ level 3" |
                                       HIQUAL_recode == "Advanced/Progression (14-19) Diploma" |
                                       HIQUAL_recode == "Level 3 Diploma" |
                                       HIQUAL_recode == "A-level or equivalent" |
                                       HIQUAL_recode == "RSA advanced diploma" |
                                       HIQUAL_recode == "OND/ONC/BTEC/SCOTVEC National etc" |
                                       HIQUAL_recode == "City & Guilds Advanced Craft/Part 1" |
                                       HIQUAL_recode == "Scottish 6 year certificate/CSYS" |
                                       HIQUAL_recode == "SCE higher or equivalent" |
                                       HIQUAL_recode == "AS-level or equivalent" |
                                       HIQUAL_recode == "Level 3 Certificate" |
                                       HIQUAL_recode == "Level 4 Award" |
                                       HIQUAL_recode == "NVQ level 2 or equivalent" |
                                       HIQUAL_recode == "Intermediate Welsh Baccalaureate"|
                                       HIQUAL_recode == "RSA diploma" |
                                       HIQUAL_recode == "City & Guilds Craft/Part 2" |
                                       HIQUAL_recode == "BTEC/SCOTVEC First or General diploma etc" |
                                       HIQUAL_recode == "Higher (14-19) Diploma" |
                                       HIQUAL_recode == "Level 2 Diploma" |
                                       HIQUAL_recode == "Level 2 Certificate" |
                                       HIQUAL_recode == "O-level, GCSE grade A*-C or equivalent" |
                                       HIQUAL_recode == "Level 3 Award" | 
                                       HIQUAL_recode == "Foundation Welsh Baccalaureate" |
                                       HIQUAL_recode == "Foundation (14-19) Diploma" |
                                       HIQUAL_recode == "Level 1 Diploma" |
                                       HIQUAL_recode == "CSE below grade 1, GCSE below grade C" |
                                       HIQUAL_recode == "BTEC/SCOTVEC First or General certificate" |
                                       HIQUAL_recode == "SCOTVEC modules" |
                                       HIQUAL_recode == "RSA other" |
                                       HIQUAL_recode == "Scottish Nationals Level 3" |
                                       HIQUAL_recode == "Scottish Nationals below Level 3" |
                                       HIQUAL_recode == "City & Guilds foundation/Part 1" |
                                       HIQUAL_recode == "Level 1 Certificate" |
                                       HIQUAL_recode == "Level 2 Award" |
                                       HIQUAL_recode == "skills qualification" |
                                       HIQUAL_recode == "Basic skills qualification"|
                                       HIQUAL_recode == "Entry level qualification"|
                                       HIQUAL_recode == "Entry level Diploma" |
                                       HIQUAL_recode == "Entry level Certificate" |
                                       HIQUAL_recode == "Level 1 Award" |
                                       HIQUAL_recode == "Entry level Award" |
                                       HIQUAL_recode == "Other qualification" |
                                       HIQUAL_recode == "NVQ level 1 or equivalent" |
                                       HIQUAL_recode == "NVQ level 1 or equivalent" |
                                       HIQUAL_recode == "RSA/OCR/Cambridge Technical level 3" |
                                       HIQUAL_recode == "NVQ level 5 (old) qualifications" |
                                       HIQUAL_recode == "O Level/GCSE A*-C and equivalents" |
                                       HIQUAL_recode == "A Level or equivalent" |
                                       HIQUAL_recode == "Other qualifications" |
                                       HIQUAL_recode == "Diploma in HE" |
                                       HIQUAL_recode == "Nursing" |
                                       HIQUAL_recode == "Teaching - Further Education" |
                                       HIQUAL_recode == "GCSE < C/CSE < 1 and equivalents" |
                                       HIQUAL_recode == "Other qualifications" |
                                       HIQUAL_recode == "Teaching - not stated" |
                                       HIQUAL_recode == "Basic Skills qualifications" |
                                       HIQUAL_recode == "AS Level or equivalent" |
                                       HIQUAL_recode == "GNVQ/GSCQ Intermediate" |
                                       HIQUAL_recode == "Key/Core/Essential/Functional Skills" |
                                       HIQUAL_recode == "Teaching - Foundation Stage" |
                                       HIQUAL_recode == "Other HE below Degree level" |
                                       HIQUAL_recode == "RQF Level 1 qualifications" |
                                       HIQUAL_recode == "RSA/OCR/Cambridge Technical level 1" |
                                       HIQUAL_recode == "RSA/OCR/Cambridge Technical other" |
                                       HIQUAL_recode == "Advanced or Progression Diploma" |
                                       HIQUAL_recode == "RSA/OCR/Cambridge Technical level 2" |
                                       HIQUAL_recode == "Trade apprenticeship" |
                                       HIQUAL_recode == "GNVQ/GSVQ intermediate" |
                                       HIQUAL_recode == "GNVQ/GSVQ advanced" |
                                       HIQUAL_recode == "Level 2  Certificate" |
                                       HIQUAL_recode == "Level 6 Award" |
                                       HIQUAL_recode == "Teaching - Secondary Education" |
                                       HIQUAL_recode == "Scottish 6th Year Certificate" |
                                       HIQUAL_recode == "GNVQ/GSVQ foundation level" |
                                       HIQUAL_recode == "Key skills qualification" |
                                       HIQUAL_recode == "YT/YTP certificate" |
                                       HIQUAL_recode == "Level 2  Diploma" |
                                       HIQUAL_recode == "Level 1  Certificate" |
                                       HIQUAL_recode == "Entry level  Certificate" |
                                       HIQUAL_recode == "Scottish National Level 5" |
                                       HIQUAL_recode == "Access qualifications" |
                                       HIQUAL_recode == "Level 1  Diploma" |
                                       HIQUAL_recode == "Level 2  Award" |
                                       HIQUAL_recode == "Level 1  Award"|
                                       HIQUAL_recode == "RSA/OCR/Cambridge Technical level 4" |
                                       HIQUAL_recode == "Entry Level qualifications" |
                                       HIQUAL_recode == "Teaching - Primary Education" |
                                       HIQUAL_recode == "Diploma (14-19)" |
                                       HIQUAL_recode == "SVQ level 2 qualifications" |
                                       HIQUAL_recode == "SVQ level 3 qualifications" |
                                       HIQUAL_recode == "Scottish National below level 3" |
                                       HIQUAL_recode == "Entry level  Diploma" |
                                       HIQUAL_recode == "Entry level  Award" |
                                       HIQUAL_recode == "Scottish National below level 3" |
                                       HIQUAL_recode == "SVQ level 4 qualifications" |
                                       HIQUAL_recode == "O level, GCSE grade A-C or equivalent" |
                                       HIQUAL_recode == "No qualif" |
                                       HIQUAL_recode == "CSE below grade1,GCSE below grade c" |
                                       HIQUAL_recode == "Other qualif" |
                                       HIQUAL_recode == "OND,ONC,BTEC etc, national" |
                                       HIQUAL_recode == "City & Guilds advanced craft/part 1" |
                                       HIQUAL_recode == "HNC,HND,BTEC etc higher" |
                                       HIQUAL_recode == "Teaching, primary educ" |
                                       HIQUAL_recode == "Teaching, further educ" |
                                       HIQUAL_recode == "A,S level or equivalent" |
                                       HIQUAL_recode == "City & Guilds craft/part 2" |
                                       HIQUAL_recode == "Diploma in higher educ" |
                                       HIQUAL_recode == "Other higher educ below degree" |
                                       HIQUAL_recode == "Teaching, secondary educ" |
                                       HIQUAL_recode == "YT,YTP certificate" |
                                       HIQUAL_recode == "City & Guilds Foundation/Part 1" |
                                       HIQUAL_recode == "BTEC,SCOTVEC first/general diploma etc" |
                                       HIQUAL_recode == "GNVQ,GSVQ foundation level" |
                                       HIQUAL_recode == "Basic Skills Qualif" |
                                       HIQUAL_recode == "International Bac'te" |
                                       HIQUAL_recode == "Teaching, level not stated" |
                                       HIQUAL_recode == "Key Skills Qualif" |
                                       HIQUAL_recode == "SCE Higher or equivalent" |
                                       HIQUAL_recode == "BTEC,SCOTVEC first/general certificate" |
                                       HIQUAL_recode == "Scottish CSYS" |
                                       HIQUAL_recode == "Entry Level qualif" |
                                       HIQUAL_recode == "Foundation Welsh Bac'te" | 
                                       HIQUAL_recode == "Modern/Technical/Professional Apprentice" ~ "Other quals",
                      # The last of the above was added in April 2025 after a QA of the code               
                                     TRUE ~ HIQUAL_recode)) 
  
  
  # Validation check to ensure all HIQUAL levels are categorised
  HIQUAL_valid_values <- c("First or foundation degree", "Higher degree", "Not known", "Other graduate quals", "Other quals", NA)
  
  # Check if any HIQUAL values have not been coded into the five levels 
  additional_values <- setdiff(dataset$HIQUAL_recode, HIQUAL_valid_values)
  
  # Returns which values have not been coded into the recode, then add them into the above code
  if (length(additional_values) > 0) {
    cat(paste0("Warning: These additional HIQUAL values have been excluded from the recode for dataset Q",quarter," ",year," (Action: add them into the code):  "))
    print(additional_values)
  }
  
  
  
  ##################
  # Grad type code #
  ##################
  
  # Note: This code that creates the Graduate_type variable uses base R rather than dplyr functions
  # This is because it was easier to append different combinations of different variables onto it
  
  # Creates blank variable for Graduate_type, sets all to NA
  dataset$Graduate_type <- NA
  
  ########################################
  # Post grad
  
  dataset$Graduate_type[dataset$HIQUAL_recode == "Higher degree" &
                          (dataset$HIGHO_recode != "Not known" & !is.na(dataset$HIGHO_recode))] <- "Post grad" #where HIGHO is a valid response
  
  dataset$Graduate_type[dataset$HIQUAL_recode == "Other graduate quals" &
                          dataset$DEGREE7_recode == "Higher degree"] <- "Post grad" #because these people are not asked HIGHO
  
  
  ########################################
  # Graduate
  
  if("DEGREE71" %in% names(dataset)){
    dataset$Graduate_type[(dataset$HIQUAL_recode == "Other graduate quals" | dataset$HIQUAL_recode == "First or foundation degree") &
                            dataset$DEGREE7_recode == "First degree"] <- "Graduate"}
  
  # Setting those where HIQUAL is "Higher degree", but HIGHO is not known or NA, and any DEGREE7 is first degree
  # This is because we don't have enough info to know what their higher degree is, but we know they at least have a first degree
  dataset$Graduate_type[dataset$HIQUAL_recode == "Higher degree" &
                          (dataset$HIGHO_recode == "Not known"| is.na(dataset$HIGHO_recode)) &
                          dataset$DEGREE7_recode_first == "Any first degree"] <- "Graduate" #TO CHECK: If including DEGREE4, does the equivalent need to be made?
  
  
  ########################################
  # Non Grad
  
  dataset$Graduate_type[dataset$HIQUAL_recode == "Other quals"] <- "Non grad"
  
  # Setting those with foundation degree only to non grad
  if("DEGREE71" %in% names(dataset)){
    dataset$Graduate_type[(dataset$HIQUAL_recode == "Other graduate quals" | dataset$HIQUAL_recode == "First or foundation degree") &
                            dataset$DEGREE7_recode == "Foundation degree"] <- "Non grad"} # only do this for foundation degree, because we can't be sure of the level of the other DEGREE7 options
  
  
  ########################################
  # Not in scope
  
  # All
  dataset$Graduate_type[dataset$HIQUAL_recode == "Not known" | is.na(dataset$HIQUAL_recode)] <- "Not in scope"
  
  # Next bits of code set where there is ambiguity about who is in what group
  
  # Post grad not in scope
  dataset$Graduate_type[dataset$HIQUAL_recode == "Higher degree" &
                          dataset$DEGREE7_recode_first != "Any first degree" & #this keeps in those who we can't be sure have higher but definitely have first degree
                          (dataset$HIGHO_recode == "Not known" | is.na(dataset$HIGHO_recode))] <- "Not in scope"
  
  dataset$Graduate_type[dataset$HIQUAL_recode == "Other graduate quals" &
                          dataset$DEGREE7_recode_first != "Any first degree" & #this keeps in those who we can't be sure have higher but definitely have first degree
                          (dataset$DEGREE7_recode == "Other" | is.na(dataset$DEGREE7_recode) | dataset$HIGHO_recode == "Not known" | is.na(dataset$HIGHO_recode))] <- "Not in scope" #Pretty sure HIGHO is only asked of those who said HIQUAL "Higher degree", so could maybe remove last two arguments from this line?
  
  # Grad not in scope
  if("DEGREE71" %in% names(dataset)){
    dataset$Graduate_type[(dataset$HIQUAL_recode == "First or foundation degree" | dataset$HIQUAL_recode == "Other graduate quals") &
                            (dataset$DEGREE7_recode == "Other" | is.na(dataset$DEGREE7_recode))] <- "Not in scope"} #those in DEGREE7 who we can't be sure of their grad level (values: grad membership of prof institute, other, dont know)
  
  # Those left over
  dataset$Graduate_type[is.na(dataset$Graduate_type)]<-"Not in scope"
  
  
  # Every case in the dataset should now be categorised in Graduate_type
  # Validation check to flag that the number of valid responses in Graduate_type equals the total number of people in the dataset
  if (sum(!is.na(dataset$Graduate_type)) == nrow(dataset)) {
    print("Sample size of the graduate type variable (excl NAs) equals the sample size of the total dataset.")
  } else {
    print("Sample size of the graduate type variable (excl NAs) does not equal the sample size of the total dataset. Therefore, not all people have not been included.")
  }
  
  assign(paste0("Q",quarter,"_",year),dataset,envir = globalenv())
  
}






























