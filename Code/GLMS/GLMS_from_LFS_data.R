#changes made in Feb 2020 to import new SOC 2020 code
#----Load Libraries----
rm(list=ls())
library("foreign")
library("plyr")
library("dplyr")
library("readxl")
library("magrittr")
library("tidyverse")
library("spatstat.geom")
library("Hmisc")
library("matrixStats")
library("haven") 


#----Reading in SPSS files-----
# This function reads in the quarterly spss files, selects the relevant columns and converts them to rds


readLFS<-function(quarter, year){
  
  #Latest data
  test<-read.spss(paste0("//vmt1pr-spss2a/LFS/Master/Quarterly datasets/2004 onwards/",year," Q",quarter,".sav"),to.data.frame=TRUE)
  
  
  #Select variables needed
  
  colnames<-c("COUNTRY","SEX","AGE", names(test[grep("PWT", names(test))]), 
              names(test[grep("PIWT", names(test))]),
              names(test[grep("HIQUAL", names(test))]),"HIGHO",
              names(test[grep("DEGREE", names(test))]),"SC2KMMJ",
              "SC10MMJ","ETH11EW", "SC20MMJ",
              "SNGDEGB","SNGHD","FDSNGDEG","ILODEFR","DISEA","DEGCLS7",
              "FTPTWK","GRSSWK","INDE07M","UALA","CASENO","GOVTOF2","GRSSWK2","HOURPAY","YERQAL3","YERQAL2")
  
  #not all variables are in each dataset
  available_columns<-colnames[which(colnames %in% names(test))]
  
  test2 <- test[,available_columns]
  
  
  #restrict to population in England aged 16-64
  test2 <- test2[which(test2$COUNTRY=="England" & test2$AGE>15 & test2$AGE<65),]
  
  saveRDS(object = test2, file = paste0(filepath, "Rds_datasets/Q",quarter, "_", year, ".rds"))
  
  
  
}

# Pre 2022 publication comment:
#The Q1 and Q2 , 2020 SPSS files have been re-weighted as a consequence of operational changes in collecting the LFS 
#due to Covid-19. These file have the suffix _reweighted in their PWT/PIWT variable. This function
#reads theses files into R, saves the relevant variables and saves them to the shared area.


readLFS_reweight<-function(quarter, year){
  
  #Latest data
  
  test<-read.spss(paste0("//vmt1pr-spss2a/LFS/Master/Quarterly datasets/2004 onwards/",year," Q",quarter,"_reweighted_Oct20.sav"),to.data.frame=TRUE)
  
  
  #Select variables needed
  
  colnames<-c("COUNTRY","SEX","AGE", names(test[grep("PWT", names(test))]), 
              names(test[grep("PIWT", names(test))]),
              names(test[grep("HIQUAL", names(test))]),"HIGHO",
              names(test[grep("DEGREE", names(test))]),"SC2KMMJ",
              "SC10MMJ","ETH11EW", "SC20MMJ",
              "SNGDEGB","SNGHD","FDSNGDEG","ILODEFR","DISEA","DEGCLS7",
              "FTPTWK","GRSSWK","INDE07M","UALA","CASENO","GOVTOF2","GRSSWK2","HOURPAY","YERQAL3","YERQAL2")
  
  #not all variables are in each dataset
  available_columns<-colnames[which(colnames %in% names(test))]
  
  test2 <- test[,available_columns]
  
  
  #restrict to population in England aged 16-64
  test2 <- test2[which(test2$COUNTRY=="England" & test2$AGE>15 & test2$AGE<65),]
  
  #Re-writes the Q1 and Q2 2020 datsets with the reweighted ones    
  #saveRDS(object = test2, file = paste0(filepath, "Rds_datasets/Q",quarter, "_", year, ".rds"))
  
  saveRDS(object = test2, file = paste0(filepath, "Rds_datasets/Q",quarter, "_", year, ".rds"))
  
  
  
  
}


#----Load .rds datasets and check that they have read well into R---------


#For these files manually convert them to .CSV and then use script ? to then covert to .rds 

read_LFS_from_project<-function(quarter, year){
  
  test3<-readRDS(paste0(filepath, "Rds_datasets/Q", quarter, "_", year, ".rds"))
  assign(paste0("Q", quarter, "_", year), test3,envir = globalenv())
  
  
  if(dim(test3)[1]==0)#add in error warning 
    stop(return(paste0("Warning: dataset Q",quarter," ",year," has not read in well from SPSS so first convert manually to .csv")))
  
  
  
}
