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
library(plotly)
library(openxlsx)
library(readxl)
library(officer)
library(glue)
library(devtools)
library(fpp)
library(urca)
library(phsmethods)
library(tibble)

# PHS styling packages ----
library(phsstyles)

# Load core functions ----
source("functions/core_functions.R")

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

# LOAD IN DATA HERE ----
'%!in%' <- function(x,y)!('%in%'(x,y))

#path <- '/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast'

## 1.1. Forecasted data ----
### Healthboard code lookup

## read aggregated forecasts
forecast_items <- readRDS('data/aggregated-forecast-2025-11-04-wd.rds') %>%
  mutate(FY = extract_fin_year(Date)) %>%
  filter(Type == 'Claim PD Number of Paid Items') %>%
  group_by(Board) %>%
  mutate(YoY = (Measure - lag(Measure, 12)) / lag(Measure, 12) * 100) %>%
  ungroup()

healthboards <- sort(unique(forecast_items$Board))
financial_years <- unique(forecast_items$FY)

forecast_items_wd <- readRDS('data/aggregated-forecast-2025-11-04-wd.rds') %>%
  mutate(FY = extract_fin_year(Date)) %>%
  filter(Type == 'Paid Items per Working Day') %>%
  group_by(Board) %>%
  mutate(YoY = (Measure - lag(Measure, 12)) / lag(Measure, 12) * 100) %>%
  ungroup()

# have to multiply cost per item by items
gic <- readRDS('data/aggregated-forecast-2025-11-04-wd.rds') %>%
  mutate(FY = extract_fin_year(Date)) %>%
  filter(Type %in% c('Claim PD Number of Paid Items', 'Cost per item'),
         Date > '2024-06-30') %>%
  group_by(Board, Date, FY) %>%
  summarise(Measure = prod(Measure),
            Forecast = prod(Forecast),
            Lower_80 = prod(Lower_80),
            Upper_80 = prod(Upper_80),
            Lower_95 = prod(Lower_95),
            Upper_95 = prod(Upper_95)) %>%
  ungroup()

# join tables
forecast_gic <- readRDS('data/aggregated-forecast-2025-11-04-wd.rds') %>%
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

# cost per item
forecast_cpi <- readRDS('data/aggregated-forecast-2025-11-04-wd.rds') %>%
  mutate(FY = extract_fin_year(Date)) %>%
  filter(Type == 'Cost per item') %>%
  group_by(Board) %>%
  mutate(YoY = (Measure - lag(Measure, 12)) / lag(Measure, 12) * 100) %>%
  ungroup() 

healthboards_agg <- unique(forecast_items$Board)
healthboards_agg <- sort(healthboards_agg[healthboards_agg %!in% c('ARGYLL & CLYDE HEALTH BOARD', 'DUMMY SCOTLAND HB')])
healthboards_agg <- c('SCOTLAND', healthboards_agg[healthboards_agg != 'SCOTLAND'])

# ## read forecast performance
# forecast_performance <- readRDS(paste0(path, '/shiny/data/forecast-2025-10-23-wd.rds')) %>%  
#   select(type, everything())
# 
# ## bind items per prescriptions extracts
# test <- read.xlsx('/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/Items per prescription/Forecasting_data_Jul_Aug_Sep_2023.xlsx',
#                   startRow = 4, cols = 2:7, sep.names = ' ')

# List all Excel files in the directory
file_list <- list.files(path = "/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/Items per prescription/", pattern = "\\.xlsx$", full.names = TRUE)

# Read all files into a list of data frames
data_list <- map(file_list, ~read.xlsx(.x, startRow = 4, cols = 2:7, sep.names = ' '))

# Optionally combine all data frames into one
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

business_days <- readRDS('forecasts/data/business_days_lookup.rds') %>%
  dplyr::rename(`Paid Date` = Date)

combined_data_wd <- combined_data_wd %>%
  left_join(business_days, by = 'Paid Date') %>%
  mutate(#`Number of Paid Items per working day` = `Claim PD Number of Paid Items` / Business_Days,
         `No of Prescriptions per working day` = `No of Prescriptions` / Business_Days) #%>%
  #mutate(`Number of items per prescription per working day` = `Number of Paid Items per working day` / `No of Prescriptions per working day`)

# plot_ly(
#   data = combined_data %>% filter(`Disp Health Board Name` == 'NHS GREATER GLASGOW & CLYDE'),
#   x = ~`Paid Date`,
#   y = ~`Avg No of Items per prescription`,
#   name = 'Actual data',
#   type = 'scatter',
#   mode = 'lines',
#   line = list(color = phs_colours('phs-blue'))
# )

## 1.2. Measure forecast error as more data is added ----
historical_error <- readRDS('data/forecast_22_23_24.rds')

### 1.2.1. Create time series ----
he_ts <- historical_error %>%
  select(board, year, forecast_error)


## 1.3. Cost per item analysis ----
# scotland_cost_per_item <- data %>%
#   group_by(Date) %>%
#   summarise(`Claim PD Number of Paid Items` = sum(`Claim PD Number of Paid Items`),
#             `Claim PD Paid GIC excl. BB` = sum(`Claim PD Paid GIC excl. BB`)) %>%
#   ungroup() %>%
#   mutate(Board = 'SCOTLAND') %>%
#   select(Board, everything())
#   # mutate(`Cost per item` = `Claim PD Paid GIC excl. BB` / `Claim PD Number of Paid Items`) %>%
#   # select(Board, Date, `Cost per item`)
# 
# cost_per_item <- data %>%
#   select(Board, Date, `Claim PD Number of Paid Items`, `Claim PD Paid GIC excl. BB`) %>%
#   bind_rows(scotland_cost_per_item) %>%
#   mutate(`Cost per item` = `Claim PD Paid GIC excl. BB` / `Claim PD Number of Paid Items`) %>%
#   select(Board, Date, `Cost per item`) %>%
#   arrange(Date)

## 1.4. SARIMA model v2.1 - run 1 ----
sarima_v2.1 <- readRDS('forecasts/sarima/output/forecast-run-1.rds')

sarima_v2.1_performance  <- readRDS('forecasts/sarima/output/forecast-performance-2025-11-18.rds')





