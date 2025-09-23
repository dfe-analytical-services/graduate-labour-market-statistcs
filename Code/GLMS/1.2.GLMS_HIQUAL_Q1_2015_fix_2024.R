# May 2025 
# This is a short R script that adds missing labels to the LFS HIGHQUAL variable to the Q1 2015 data-set using available labels from the Q1 2016 data-set

# Note that Older versions of HIQUAL15 don't have value labels- just numbers- but we can use the value labels of Q1_2016 to recode the numbers as text strings
# Note that HIQUAL15 for the Q1 2015 data needs to be converted to a factor for consistency with other quarterly data tables in the GLMS series


HIQUAL15 <- seq(1:85)

HIQUAL15_values <- levels(Q1_2016$HIQUAL15)

HIQUAL15_ref <- tibble(HIQUAL15_values, HIQUAL15)

Q1_2015 <- left_join(Q1_2015, HIQUAL15_ref, by = "HIQUAL15")

Q1_2015 <- Q1_2015 %>% 
  select(-HIQUAL15)

setnames(Q1_2015, "HIQUAL15_values", "HIQUAL15")

Q1_2015$HIQUAL15 <- haven::as_factor(Q1_2015$HIQUAL15) # Convert to factor variable for consistency with other variables throughout data-sets






