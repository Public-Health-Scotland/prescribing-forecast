options(scipen = 999)

# 1. Load packages, key variables and data ----

## 1.1. Load packages ----
library(tidyverse)
library(lubridate)
library(forecast)
library(ggplot2)
library(plotly)
library(openxlsx)
library(readxl)
library(officer)
library(glue)
library(devtools)
library(urca)
library(phsmethods)
library(tibble)
library(bizdays)
library(doParallel)
library(foreach)
library(ggtime)
library(tsibble)
library(feasts)

## 1.2. Initialise variables and file paths ----
'%!in%' <- function(x,y)!('%in%'(x,y))

path <- '/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast'
setwd('/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast')

# 2. Read in data and some processing ----
data <- read.csv(paste0(glue('shiny/forecasts/data/Historical Data.csv')), check.names = FALSE)

data$`Claim PD Paid GIC excl. BB` <- as.double(gsub(",", "", data$`Claim PD Paid GIC excl. BB`))
#data$`Paid BNF Chapter Description`[data$`Paid BNF Chapter Description` %in% c("", NA)] <- "BLANK CHAPTER" # assign NAs to any blank values
#data$`Age Band (patient age at paid date)`[data$`Age Band (patient age at paid date)` %in% c("", NA)] <- "BLANK AGE BAND" # assign NAs to any blank values

board_data <- data %>%
  #bnf_demographic <- data %>%
  mutate(`Paid Date` = dmy(`Paid Date`)) %>%
  filter(`Paid Date` > '2009-12-31') #%>% # filter data for 2010 onwards so those values can be used as lags for 2011, where there was introduction of free prescriptions
# aggregate data to exclude BNF chapter and age band - comment out if wanting full dataframe
# group_by(`Disp Health Board Name`, `Paid Date`) %>%
# summarise(`Claim PD Number of Paid Items` = sum(`Claim PD Number of Paid Items`),
#           `Claim PD Paid GIC excl. BB` = sum(`Claim PD Paid GIC excl. BB`)) %>%
# ungroup()

rm(data)

## 2.1. Aggregate and join Scotland data ----
scotland_data <- board_data %>%
  group_by(`Paid Date`) %>%
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>%
  mutate(`Disp Health Board Name` = 'SCOTLAND') %>%
  select(`Disp Health Board Name`, everything())

board_data <- board_data %>%
  bind_rows(scotland_data) %>%
  mutate(`Cost per item` = `Claim PD Paid GIC excl. BB` / `Claim PD Number of Paid Items`) %>%
  # double check data is arranged by board and date 
  arrange(`Disp Health Board Name`, `Paid Date`)

board_data$`Cost per item`[board_data$`Cost per item` %in% c("", NA, NaN)] <- 0 # assign NAs to any blank values

items <- board_data %>%
  select(-`Claim PD Paid GIC excl. BB`, -`Cost per item`) %>%
  filter(`Disp Health Board Name` == 'NHS AYRSHIRE & ARRAN') %>%
  select(`Paid Date`, `Claim PD Number of Paid Items`)

## 2.2 Create healthboard and date variables ----
healthboards <- unique(board_data$`Disp Health Board Name`) 
dates <- unique(board_data$`Paid Date`)

## 3. LSTM model ----

## 3.1. Install packages ----
#install.packages("keras3")
#install.packages("tensorflow")
#install.packages('TSLSTMplus')

library(keras3)
library(tensorflow)
library(TSLSTMplus)

items_ts <- ts(items$`Claim PD Number of Paid Items`, start = 2010, frequency = 12)

# The value of tsLag here cuts out the first x number of time-series observations to create the number of lag columns
# i.e. tsLag = 2 will create two lag columns for the previous month and then the month before that, the first two observations will not have two previous months worth of data
items_lstm <- ts.prepare.data(items_ts, tsLag = 12)



