### Prophet Forecast
### Honours Project
### Date last updated: 24/02/2026
### Author: Liam Rooney
### Model: v1.0

### Changes made to previous model version (v...):
### 1. 
### 
###    

### Key notes for running this script:
### Once the variables at lines 48, 49, 50, 56, 70, 72 and 74 have been defined by the user: 
### 1. Click Source --> Source as Workbench Job --> define the memory parameters
### 2. Sit back and let the forecast work its magic! 

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

## 2.2 Create healthboard and date variables ----
healthboards <- unique(board_data$`Disp Health Board Name`) 
dates <- unique(board_data$`Paid Date`)

# 3. EDA ----

## 3.1. Data preparation ----

scotland_data <- board_data %>%
  filter(`Disp Health Board Name` == 'SCOTLAND')

items <- scotland_data %>%
  select(`Paid Date`, `Claim PD Number of Paid Items`)

items_ts <- ts(items$`Claim PD Number of Paid Items`, start = c(2010, 1), frequency = 12)

gic <- scotland_data %>%
  select(`Paid Date`, `Claim PD Paid GIC excl. BB`)

gic_ts <- ts(gic$`Claim PD Paid GIC excl. BB`, start = c(2010, 1), frequency = 12)

cpi <- scotland_data %>%
  select(`Paid Date`, `Cost per item`)

cpi_ts <- ts(cpi$`Cost per item`, start = c(2010, 1), frequency = 12)

## 3.2. Time-series plots ----

items_tsibble <- items %>%
  mutate(Month = yearmonth(`Paid Date`)) %>%
  tsibble(index = 'Month') 

items_sp <- items_tsibble %>%
  gg_season(`Claim PD Number of Paid Items`, labels = 'both')

#ggsave('shiny/forecasts/eda/images/items_season_plot_12_2025.png')

items_ssp <- items_tsibble %>%
  gg_subseries(`Claim PD Number of Paid Items`)

#ggsave('shiny/forecasts/eda/images/items_subseries_season_plot_12_2025.png')

gic_tsibble <- gic %>%
  mutate(Month = yearmonth(`Paid Date`)) %>%
  tsibble(index = 'Month')

gic_sp <- gic_tsibble %>%
  gg_season(`Claim PD Paid GIC excl. BB`, labels = 'both')

#ggsave('shiny/forecasts/eda/images/gic_season_plot_12_2025.png')

gic_ssp <- gic_tsibble %>%
  gg_subseries(`Claim PD Paid GIC excl. BB`)

#ggsave('shiny/forecasts/eda/images/gic_subseries_season_plot_12_2025.png')

cpi_tsibble <- cpi %>%
  mutate(Month = yearmonth(`Paid Date`)) %>%
  tsibble(index = 'Month')

cpi_sp <- cpi_tsibble %>%
  gg_season(`Cost per item`, labels = 'both')

#ggsave('shiny/forecasts/eda/images/cpi_season_plot_12_2025.png')

cpi_ssp <- cpi_tsibble %>%
  gg_subseries(`Cost per item`)

#ggsave('shiny/forecasts/eda/images/cpi_subseries_season_plot_12_2025.png')

## 3.3. Scatterplots ----

library(GGally)

## Measure correlation coefficients between items, gic, cpi
## Scatterplot matrix created
data_corr <- scotland_data %>%
  ggpairs(columns = 3:5)

ggsave('shiny/forecasts/eda/images/coefficient_plot_12_2025.png')

## 3.4. Lag plots ----

items_lag <- scotland_data %>%
  mutate(month_new = yearmonth(`Paid Date`)) %>%
  tsibble(index = 'month_new', frequency = 'month') %>%
  ggtime::gg_lag(`Claim PD Number of Paid Items`, geom = "point")

gic_lag <- scotland_data %>%
  mutate(month_new = yearmonth(`Paid Date`)) %>%
  tsibble(index = 'month_new', frequency = 'month') %>%
  ggtime::gg_lag(`Claim PD Paid GIC excl. BB`, geom = "point")

cpi_lag <- scotland_data %>%
  mutate(month_new = yearmonth(`Paid Date`)) %>%
  tsibble(index = 'month_new', frequency = 'month') %>%
  ggtime::gg_lag(`Cost per item`, geom = "point")

## 3.5. Autocorrelation plots ----
items_autocorr <- scotland_data %>%
  mutate(month_new = yearmonth(`Paid Date`)) %>%
  tsibble(index = 'month_new', frequency = 'month') %>%
  ACF(`Claim PD Number of Paid Items`) %>%
  autoplot()
  
gic_autocorr <- scotland_data %>%
  mutate(month_new = yearmonth(`Paid Date`)) %>%
  tsibble(index = 'month_new', frequency = 'month') %>%
  ACF(`Claim PD Paid GIC excl. BB`) %>%
  autoplot()

cpi_autocorr <- scotland_data %>%
  mutate(month_new = yearmonth(`Paid Date`)) %>%
  tsibble(index = 'month_new', frequency = 'month') %>%
  ACF(`Cost per item`) %>%
  autoplot()


