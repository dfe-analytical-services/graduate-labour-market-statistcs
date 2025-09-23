# May 2025
# R script to read in the ONS quarterly deflators series using the ONS CPI quarterly inflation series to use for the generation of the real salaries series for
# GLMS. This then reflects the impact on salaries due to inflation, based on a baseline of 2007 at the start of the GLMS series.


# Apply deflators

# Deflators downloaded from: https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/l522/mm23

# Read in deflators
# Note: need to wrap in as.data.frame, because otherwise read_xlsx reads in a tibble, which does not function with the subsequent code
deflators <- as.data.frame(read_xlsx(data_folder/"CPIH - March 2025.xlsx",
                       sheet = 1,
                       skip = 8,
                       col_names=FALSE))

# Rename columns
names(deflators) <- c('period', 'deflator2015')

# Calculate 2007 deflators based on value of full year 2007
# Note: May decide that this needs to be changed for 2023
deflators$deflator2007 <- deflators$deflator2015 * 100 / deflators$deflator2015[deflators$period =="2007"]

# keep only those for quarters from 2007 to 2024 (as only these are needed for GLMS analysis)
deflators <- tail(deflators, -113)
deflators <- head(deflators, 72)


# Apply deflators to create new variable

Q1_2007$GRSSWK_real <- Q1_2007$GRSSWK * 100 / deflators[1,3]
Q2_2007$GRSSWK_real <- Q2_2007$GRSSWK * 100 / deflators[2,3]
Q3_2007$GRSSWK_real <- Q3_2007$GRSSWK * 100 / deflators[3,3]
Q4_2007$GRSSWK_real <- Q4_2007$GRSSWK * 100 / deflators[4,3]

Q1_2008$GRSSWK_real <- Q1_2008$GRSSWK * 100 / deflators[5,3]
Q2_2008$GRSSWK_real <- Q2_2008$GRSSWK * 100 / deflators[6,3]
Q3_2008$GRSSWK_real <- Q3_2008$GRSSWK * 100 / deflators[7,3]
Q4_2008$GRSSWK_real <- Q4_2008$GRSSWK * 100 / deflators[8,3]

Q1_2009$GRSSWK_real <- Q1_2009$GRSSWK * 100 / deflators[9,3]
Q2_2009$GRSSWK_real <- Q2_2009$GRSSWK * 100 / deflators[10,3]
Q3_2009$GRSSWK_real <- Q3_2009$GRSSWK * 100 / deflators[11,3]
Q4_2009$GRSSWK_real <- Q4_2009$GRSSWK * 100 / deflators[12,3]

Q1_2010$GRSSWK_real <- Q1_2010$GRSSWK * 100 / deflators[13,3]
Q2_2010$GRSSWK_real <- Q2_2010$GRSSWK * 100 / deflators[14,3]
Q3_2010$GRSSWK_real <- Q3_2010$GRSSWK * 100 / deflators[15,3]
Q4_2010$GRSSWK_real <- Q4_2010$GRSSWK * 100 / deflators[16,3]

Q1_2011$GRSSWK_real <- Q1_2011$GRSSWK * 100 / deflators[17,3]
Q2_2011$GRSSWK_real <- Q2_2011$GRSSWK * 100 / deflators[18,3]
Q3_2011$GRSSWK_real <- Q3_2011$GRSSWK * 100 / deflators[19,3]
Q4_2011$GRSSWK_real <- Q4_2011$GRSSWK * 100 / deflators[20,3]

Q1_2012$GRSSWK_real <- Q1_2012$GRSSWK * 100 / deflators[21,3]
Q2_2012$GRSSWK_real <- Q2_2012$GRSSWK * 100 / deflators[22,3]
Q3_2012$GRSSWK_real <- Q3_2012$GRSSWK * 100 / deflators[23,3]
Q4_2012$GRSSWK_real <- Q4_2012$GRSSWK * 100 / deflators[24,3]

Q1_2013$GRSSWK_real <- Q1_2013$GRSSWK * 100 / deflators[25,3]
Q2_2013$GRSSWK_real <- Q2_2013$GRSSWK * 100 / deflators[26,3]
Q3_2013$GRSSWK_real <- Q3_2013$GRSSWK * 100 / deflators[27,3]
Q4_2013$GRSSWK_real <- Q4_2013$GRSSWK * 100 / deflators[28,3]

Q1_2014$GRSSWK_real <- Q1_2014$GRSSWK * 100 / deflators[29,3]
Q2_2014$GRSSWK_real <- Q2_2014$GRSSWK * 100 / deflators[30,3]
Q3_2014$GRSSWK_real <- Q3_2014$GRSSWK * 100 / deflators[31,3]
Q4_2014$GRSSWK_real <- Q4_2014$GRSSWK * 100 / deflators[32,3]

Q1_2015$GRSSWK_real <- Q1_2015$GRSSWK * 100 / deflators[33,3]
Q2_2015$GRSSWK_real <- Q2_2015$GRSSWK * 100 / deflators[34,3]
Q3_2015$GRSSWK_real <- Q3_2015$GRSSWK * 100 / deflators[35,3]
Q4_2015$GRSSWK_real <- Q4_2015$GRSSWK * 100 / deflators[36,3]

Q1_2016$GRSSWK_real <- Q1_2016$GRSSWK * 100 / deflators[37,3]
Q2_2016$GRSSWK_real <- Q2_2016$GRSSWK * 100 / deflators[38,3]
Q3_2016$GRSSWK_real <- Q3_2016$GRSSWK * 100 / deflators[39,3]
Q4_2016$GRSSWK_real <- Q4_2016$GRSSWK * 100 / deflators[40,3]

Q1_2017$GRSSWK_real <- Q1_2017$GRSSWK * 100 / deflators[41,3]
Q2_2017$GRSSWK_real <- Q2_2017$GRSSWK * 100 / deflators[42,3]
Q3_2017$GRSSWK_real <- Q3_2017$GRSSWK * 100 / deflators[43,3]
Q4_2017$GRSSWK_real <- Q4_2017$GRSSWK * 100 / deflators[44,3]

Q1_2018$GRSSWK_real <- Q1_2018$GRSSWK * 100 / deflators[45,3]
Q2_2018$GRSSWK_real <- Q2_2018$GRSSWK * 100 / deflators[46,3]
Q3_2018$GRSSWK_real <- Q3_2018$GRSSWK * 100 / deflators[47,3]
Q4_2018$GRSSWK_real <- Q4_2018$GRSSWK * 100 / deflators[48,3]

Q1_2019$GRSSWK_real <- Q1_2019$GRSSWK * 100 / deflators[49,3]
Q2_2019$GRSSWK_real <- Q2_2019$GRSSWK * 100 / deflators[50,3]
Q3_2019$GRSSWK_real <- Q3_2019$GRSSWK * 100 / deflators[51,3]
Q4_2019$GRSSWK_real <- Q4_2019$GRSSWK * 100 / deflators[52,3]

Q1_2020$GRSSWK_real <- Q1_2020$GRSSWK * 100 / deflators[53,3]
Q2_2020$GRSSWK_real <- Q2_2020$GRSSWK * 100 / deflators[54,3]
Q3_2020$GRSSWK_real <- Q3_2020$GRSSWK * 100 / deflators[55,3]
Q4_2020$GRSSWK_real <- Q4_2020$GRSSWK * 100 / deflators[56,3]

Q1_2021$GRSSWK_real <- Q1_2021$GRSSWK * 100 / deflators[57,3]
Q2_2021$GRSSWK_real <- Q2_2021$GRSSWK * 100 / deflators[58,3]
Q3_2021$GRSSWK_real <- Q3_2021$GRSSWK * 100 / deflators[59,3]
Q4_2021$GRSSWK_real <- Q4_2021$GRSSWK * 100 / deflators[60,3]

Q1_2022$GRSSWK_real <- Q1_2022$GRSSWK * 100 / deflators[61,3]
Q2_2022$GRSSWK_real <- Q2_2022$GRSSWK * 100 / deflators[62,3]
Q3_2022$GRSSWK_real <- Q3_2022$GRSSWK * 100 / deflators[63,3]
Q4_2022$GRSSWK_real <- Q4_2022$GRSSWK * 100 / deflators[64,3]

Q1_2023$GRSSWK_real <- Q1_2023$GRSSWK * 100 / deflators[65,3]
Q2_2023$GRSSWK_real <- Q2_2023$GRSSWK * 100 / deflators[66,3]
Q3_2023$GRSSWK_real <- Q3_2023$GRSSWK * 100 / deflators[67,3]
Q4_2023$GRSSWK_real <- Q4_2023$GRSSWK * 100 / deflators[68,3]

Q1_2024$GRSSWK_real <- Q1_2024$GRSSWK * 100 / deflators[69,3]
Q2_2024$GRSSWK_real <- Q2_2024$GRSSWK * 100 / deflators[70,3]
Q3_2024$GRSSWK_real <- Q3_2024$GRSSWK * 100 / deflators[71,3]
Q4_2024$GRSSWK_real <- Q4_2024$GRSSWK * 100 / deflators[72,3]
