####################### Setup #######################
options(scipen = 999)

# Shiny packages ----
library(shiny)
library(shinycssloaders)
library(bslib)
library(bsicons)

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

path <- '/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast'

## 1.1. Forecasted data ----
### Healthboard code lookup
hb_code <- read_excel(glue('{path}/lookups/hb_code_lookup.xlsx'))

data <- rbind(
  read_excel(glue('{path}/data/healthboard/HB_Phasings_04_08.xlsx')),
  read_excel(glue('{path}/data/healthboard/HB_Phasings_09_13.xlsx')),
  read_excel(glue('{path}/data/healthboard/HB_Phasings_14_18.xlsx')),
  read_excel(glue('{path}/data/healthboard/HB_Phasings_19_24.xlsx'))
) %>%
  mutate(Date = ceiling_date(as.Date(paste('1', `Paid Financial Month`, `Paid Financial Year`), format = '%d %m %Y'), unit = 'month') - days(1)) %>%
  left_join(hb_code, by = 'Disp Health Board Code') %>%
  mutate(FY = extract_fin_year(Date)) %>%
  select(Board = `Disp Health Board Name`, Date, FY, `Claim PD Number of Paid Items`, `Claim PD Paid GIC excl. BB`, Phasings)

forecasted_data <- readRDS(paste0(path, '/shiny/data/new_forecasted_data.rds'))

number_of_items <- readRDS(paste0(path, '/shiny/data/number_of_items.rds'))

results_comparison <- readRDS(paste0(path, '/shiny/data/results_list.rds'))

## scotland
scotland_data <- rbind(
  read_excel(glue('{path}/data/scotland/Scotland_Phasings_04_08.xlsx')),
  read_excel(glue('{path}/data/scotland/Scotland_Phasings_09_13.xlsx')),
  read_excel(glue('{path}/data/scotland/Scotland_Phasings_14_18.xlsx')),
  read_excel(glue('{path}/data/scotland/Scotland_Phasings_19_24.xlsx'))
) %>%
  #mutate(Date = ceiling_date(as.Date(paste('1', `Paid Financial Month`, `Paid Financial Year`), format = '%d %m %Y'), unit = 'month') - days(1)) %>%
  mutate(Date = as.Date(`Paid Date`),
         Board = 'SCOTLAND') %>%
  #left_join(hb_code, by = 'Disp Health Board Code') %>%
  mutate(FY = extract_fin_year(Date)) %>%
  select(Board, Date, FY, `Claim PD Number of Paid Items`, `Claim PD Paid GIC excl. BB`, Phasings)

scotland_results <- readRDS(paste0(path, '/shiny/data/scotland_results_list.rds'))

financial_years <- unique(forecasted_data$FY)
healthboards <- unique(number_of_items$Board)

## read aggregated forecasts
forecast <- readRDS(paste0(path, '/shiny/data/aggregated_forecast.rds')) %>%
  mutate(FY = extract_fin_year(Date))

forecast_wd <- readRDS(paste0(path, '/shiny/data/aggregated_forecast_wd.rds')) %>%
  mutate(FY = extract_fin_year(Date)) %>%
  group_by(Board) %>%
  mutate(YoY = (`Paid Items per Working Day` - lag(`Paid Items per Working Day`, 12)) / lag(`Paid Items per Working Day`, 12) * 100) %>%
  ungroup()

healthboards_agg <- unique(forecast$Board)
healthboards_agg <- healthboards_agg[healthboards_agg %!in% c('ARGYLL & CLYDE HEALTH BOARD', 'DUMMY SCOTLAND HB')]

## read forecast performance
forecast_performance <- readRDS(paste0(path, '/shiny/data/performance_working_day.rds')) %>%
  mutate(Type = 'Paid Items') %>%
  filter(!(board == 'SCOTLAND'))

forecast_wd_performance <- readRDS(paste0(path, '/shiny/data/best_performance_list.rds')) %>%
  mutate(Type = 'Working Day')

forecast_performance <- forecast_performance %>%
  rbind(forecast_wd_performance) %>%
  select(Type, everything())

## bind items per prescriptions extracts
test <- read.xlsx('/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/Items per prescription/Forecasting_data_Jul_Aug_Sep_2023.xlsx',
                  startRow = 4, cols = 2:7, sep.names = ' ')

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

business_days <- readRDS(paste0(path, '/shiny/data/business_days_lookup.rds')) %>%
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
historical_error <- readRDS(paste0(path, '/shiny/data/forecast_22_23_24.rds'))

### 1.2.1. Create time series ----
he_ts <- historical_error %>%
  select(board, year, forecast_error)













