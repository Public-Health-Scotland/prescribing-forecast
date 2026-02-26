####################### Setup #######################
options(scipen = 999)

# Shiny packages ----
library(shiny)
library(shinycssloaders)
library(shinythemes)
library(bslib)
library(bsicons)
library(cookies)
library(login)
library(bs4Dash)
library(rintrojs)

# Data wrangling packages ----
library(dplyr)
library(magrittr)
library(tidyverse)
library(openxlsx)

# Plotting packages ----
library(plotly)
library(ggplot2)
library(ggiraph)

# Miscellaneous ----
library(tidyverse)
library(lubridate)
library(forecast)
library(ggplot2)
library(ggtime)
library(plotly)
library(openxlsx)
library(readxl)
library(officer)
library(glue)
library(devtools)
library(urca)
library(phsmethods)
library(tibble)
library(tsibble)
library(feasts)
library(GGally)

# PHS styling packages ----
library(phsstyles)

# Deployment ----
library(rsconnect)
library(shinymanager)

# Load core functions ----
source("shiny/functions/core_functions.R")

## Plotting ----
# Style of x and y axis
xaxis_plots <- list(
  title = FALSE,
  tickfont = list(size = 14),
  titlefont = list(size = 14),
  showline = TRUE,
  fixedrange = TRUE
)

yaxis_plots <- list(
  title = FALSE,
  rangemode = "tozero",
  fixedrange = TRUE,
  size = 4,
  tickfont = list(size = 14),
  titlefont = list(size = 14)
)

# Buttons to remove from plotly plots
bttn_remove <- list(
  'select2d',
  'lasso2d',
  'zoomIn2d',
  'zoomOut2d',
  'autoScale2d',
  'toggleSpikelines',
  'hoverCompareCartesian',
  'hoverClosestCartesian'
)

'%!in%' <- function(x,y)!('%in%'(x,y))

# LOAD IN DATA HERE ----

############## Forecast ############## 
forecast <- readRDS('shiny/forecasts/sarima/output/forecast-run-2.rds') %>%
  filter(Historical_Data == "12 months of historical data",
         Year == 2025,
         F_Horizon == 48,
         Arima_Error == FALSE) %>%
  select(-c(Historical_Data, Year, F_Horizon, Arima_Error))

############## Volume Data ############## 

## Number of Paid Items forecast
forecast_items <- forecast %>%
  mutate(FY = extract_fin_year(Date)) %>%
  filter(Type == 'Claim PD Number of Paid Items') %>%
  group_by(Board) %>%
  mutate(YoY = (Measure - lag(Measure, 12)) / lag(Measure, 12) * 100) %>%
  ungroup()

## Number of Paid Items per working day forecast
forecast_items_wd <- forecast %>%
  mutate(FY = extract_fin_year(Date)) %>%
  filter(Type == 'Paid Items per Working Day') %>%
  group_by(Board) %>%
  mutate(YoY = (Measure - lag(Measure, 12)) / lag(Measure, 12) * 100) %>%
  ungroup()

############## Cost Data ############## 

## Calculate GIC for post-training data
gic <- forecast %>%
  mutate(FY = extract_fin_year(Date)) %>%
  filter(Type %in% c('Claim PD Number of Paid Items', 'Cost per item'),
         Date > '2024-06-30') %>%
  group_by(Board, Date, FY) %>%
  summarise(Measure = prod(Measure), # multiplies number of paid items and cost per item to get gross ingredient cost
            Forecast = prod(Forecast),
            Lower_80 = prod(Lower_80),
            Upper_80 = prod(Upper_80),
            Lower_95 = prod(Lower_95),
            Upper_95 = prod(Upper_95)) %>%
  ungroup()

## Final GIC dataframe
forecast_gic <- forecast %>%
  mutate(FY = extract_fin_year(Date)) %>%
  filter(Type %in% c('Claim PD Number of Paid Items', 'Cost per item'),
         Date < '2024-07-01') %>%
  pivot_wider(names_from = 'Type',
              values_from = 'Measure') %>%
  mutate(Measure = `Claim PD Number of Paid Items` * `Cost per item`) %>%
  select(Board, Date, FY, Measure, everything()) %>%
  select(-`Claim PD Number of Paid Items`, -`Cost per item`) %>%
  bind_rows(gic) %>%
  arrange(Date) %>%
  group_by(Board) %>%
  mutate(YoY = (Measure - lag(Measure, 12)) / lag(Measure, 12) * 100) %>%
  ungroup() 

## Cost per Item
forecast_cpi <- forecast %>%
  mutate(FY = extract_fin_year(Date)) %>%
  filter(Type == 'Cost per item') %>%
  group_by(Board) %>%
  mutate(YoY = (Measure - lag(Measure, 12)) / lag(Measure, 12) * 100) %>%
  ungroup() 

############## Trend Monitoring Data ############## 

## List all Excel files in the directory
file_list <- list.files(path = "shiny/data/Items per prescription/", pattern = "\\.xlsx$", full.names = TRUE)

## Read all files into a list of data frames
data_list <- map(file_list, ~read.xlsx(.x, startRow = 4, cols = 2:7, sep.names = ' '))

## Combine all data frames into one
combined_data <- bind_rows(data_list) %>%
  mutate(`Paid Date` = as.Date(`Paid Date`, origin = "1899-12-30")) %>%
  arrange(`Paid Date`)

scotland_data <- combined_data %>%
  group_by(`Paid Date`, `Paid Calendar Year`) %>%
  summarise(`Claim PD Number of Paid Items` = sum(`Claim PD Number of Paid Items`),
            `No of Prescriptions` = sum(`No of Prescriptions`)) %>%
  ungroup() %>%
  mutate(`Disp Health Board Name` = 'SCOTLAND',
         `Avg No of Items per prescription` = `Claim PD Number of Paid Items` / `No of Prescriptions`) %>%
  select(`Disp Health Board Name`, everything())

combined_data <- combined_data %>%
  rbind(scotland_data) %>%
  arrange(`Paid Date`)

## compare figures with working days
combined_data_wd <- combined_data

business_days <- readRDS('shiny/forecasts/data/business_days_lookup.rds') %>%
  dplyr::rename(`Paid Date` = Date)

combined_data_wd <- combined_data_wd %>%
  left_join(business_days, by = 'Paid Date') %>%
  mutate(#`Number of Paid Items per working day` = `Claim PD Number of Paid Items` / Business_Days,
    `No of Prescriptions per working day` = `No of Prescriptions` / Business_Days) #%>%
#mutate(`Number of items per prescription per working day` = `Number of Paid Items per working day` / `No of Prescriptions per working day`)

## 1.4. SARIMA model v2.1 - run 1 ----
# sarima_v2.1 <- readRDS('shiny/forecasts/sarima/output/forecast-run-1.rds') %>%
#   filter(Historical_Data == "12 months of historical data",
#          Year == 2025,
#          F_Horizon == 48,
#          Arima_Error == FALSE) %>%
#   select(-c(Historical_Data, Year, F_Horizon, Arima_Error))

sarima_v2.1_performance  <- readRDS('shiny/forecasts/sarima/output/forecast-performance-2026-01-14.rds')

############## EDA ############## 
eda_data <- read.csv('shiny/forecasts/data/Historical Data.csv', check.names = FALSE) %>%
  mutate(`Paid Date` = dmy(`Paid Date`)) %>%
  filter(`Paid Date` > '2009-12-31') 

eda_data$`Claim PD Paid GIC excl. BB` <- as.double(gsub(",", "", eda_data$`Claim PD Paid GIC excl. BB`))

scotland_data <- eda_data %>%
  group_by(`Paid Date`) %>%
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>%
  mutate(`Disp Health Board Name` = 'SCOTLAND') %>%
  select(`Disp Health Board Name`, everything())

eda_data <- eda_data %>%
  bind_rows(scotland_data) %>%
  mutate(`Cost per item` = `Claim PD Paid GIC excl. BB` / `Claim PD Number of Paid Items`) %>%
  # double check data is arranged by board and date 
  arrange(`Disp Health Board Name`, `Paid Date`)

rm(scotland_data)

# eda_data_tsibble <- eda_data %>%
#   # create new column for index
#   mutate(month_new = yearmonth(`Paid Date`)) %>%
#   tsibble(index = 'month_new')

############## Variables ############## 
healthboards <- unique(forecast_items$Board)
financial_years <- unique(forecast_items$FY)

# # LOGIN----
# # get protection status for live app
# Protected <- readRDS("data/Protect.rds")
# 
# # Set password if required for deployed app
# # Whether to password protect the app - set in deployment script
# password_protect <- Protected

# Create a custom theme
my_theme <- bs_theme(
  version = 5,                # Bootstrap 5
  primary = "#3F3685",        # Change primary button colour
  secondary = "#0078D4"       # Change secondary button colour
)















