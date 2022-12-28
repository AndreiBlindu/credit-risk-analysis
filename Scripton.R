# Clear the environment 
rm(list=ls())

# Calling the libraries
library(DescTools)
library(ggplot2)
library(dplyr)
library(readxl)
library(Hmisc)
library(pastecs)
library(corrplot)

#############################################################################
# DATA LOADING
#############################################################################

# We have a xlsx file with multiple sheets so we have to join them into a single dataframe
setwd("/Users/andreiblindu/Desktop/Università/Laurea Magistrale/Corsi/2° anno/1° semestre/Financial Data Science/Project/credit-risk-analysis/")
getwd()
multiplesheets <- function(fname) {
  
  # getting info about all excel sheets
  sheets <- readxl::excel_sheets(fname)
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x))
  data_frame <- lapply(tibble, as.data.frame)
  
  # assigning names to data frames
  names(data_frame) <- sheets
  
  # print data frame
  print(data_frame)
}

Complete_Data <- multiplesheets("Complete_Data.xlsx")

Dataset <- rbind(Complete_Data$`1-20k`, Complete_Data$`20k-40k`)
Dataset <- rbind(Dataset, Complete_Data$`40k-60k`)
Dataset <- rbind(Dataset, Complete_Data$`60k-80k`)
Dataset <- rbind(Dataset, Complete_Data$`80k-100k`)
Dataset <- rbind(Dataset, Complete_Data$`100k-121k`)

# View dataset
View(Dataset)
# View summary and column names
summary(Dataset)
colnames(Dataset)

#############################################################################
# DATA PRE-PROCESSING
#############################################################################
# Remove missing values with na.omit
data <- na.omit(Dataset)
colnames(data)
summary(data)

# We want the MScores to be a binary variables with
# value 0 for the levels from A to B (AAA, AA, A, BBB, BB, B)
# value 1 for the levels from C to D (CCC,CC, C and D)
unique(data$MScore.2020) # first things first, see all the possible values of the column
# binarize
data1 <- data
data1$MScore.2020 <- ifelse(data$MScore.2020 >= "C", 1, 0)
data1$MScore.2019 <- ifelse(data$MScore.2019 >= "C", 1, 0)
data1$MScore.2018 <- ifelse(data$MScore.2018 >= "C", 1, 0)
data1$MScore.2017 <- ifelse(data$MScore.2017 >= "C", 1, 0)
data1$MScore.2016 <- ifelse(data$MScore.2016 >= "C", 1, 0)
data1$MScore.2015 <- ifelse(data$MScore.2015 >= "C", 1, 0)

colnames(data1)

column_list <- c( 'Turnover_Prev', 
                   'EBIT_Prev', 
                   'PLTax_Prev', 
                   'Region', 
                   'Country', 
                   'NACE code',
                   'Sector 1',
                   'Sector 2',
                   'Leverage_Prev', 
                   'ROE_Prev', 
                   'TAsset_Prev',
                   'MScore_Curr',
                   'MScore_Prev')

df <- data1

# Rename the columns
names(df)[names(df) == "Turnover.2019"] <- "Turnover_Prev"
names(df)[names(df) == "EBIT.2019"] <- "EBIT_Prev"
names(df)[names(df) == "PLTax.2019"] <- "PLTax_Prev"
names(df)[names(df) == "MScore.2020"] <- "MScore_Curr"
names(df)[names(df) == "MScore.2019"] <- "MScore_Prev"
names(df)[names(df) == "Leverage.2019"] <- "Leverage_Prev"
names(df)[names(df) == "ROE.2019"] <- "ROE_Prev"
names(df)[names(df) == "TAsset.2019"] <- "TAsset_Prev"

# Keep only the columns we need
df <- df[column_list]
colnames(df)

for (y in 2019:2016) {
  d <- data1
  # Rename the columns
  names(d)[names(d) == paste("Turnover.", y-1, sep = "")] <- "Turnover_Prev"
  names(d)[names(d) == paste("EBIT.", y-1, sep = "")] <- "EBIT_Prev"
  names(d)[names(d) == paste("PLTax.", y-1, sep = "")] <- "PLTax_Prev"
  names(d)[names(d) == paste("MScore.", y, sep = "")] <- "MScore_Curr"
  names(d)[names(d) == paste("MScore.", y-1, sep = "")] <- "MScore_Prev"
  names(d)[names(d) == paste("Leverage.", y-1, sep = "")] <- "Leverage_Prev"
  names(d)[names(d) == paste("ROE.", y-1, sep = "")] <- "ROE_Prev"
  names(d)[names(d) == paste("TAsset.", y-1, sep = "")] <- "TAsset_Prev"
  
  # Keep only the columns we need
  d <- d[column_list]
  
  # Bind dataframe to the one from the previous year
  df <- rbind(df, d)
  
  nrow(df)
}
nrow(df)
colnames(df)
