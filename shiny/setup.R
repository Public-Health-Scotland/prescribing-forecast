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
library(fabletools)
library(GGally)

# PHS styling packages ----
library(phsstyles)

# Deployment ----
library(rsconnect)
library(shinymanager)

# Load core functions ----
source("functions/core_functions.R")

## Plotting ----
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

# Key variables to load files
run_number = 4
latest_year = 2026
new_fy = FALSE # this should be true if updating in May/June following PIS load of March data, completing full financial year
#forecast_performance_date = '2026-05-24' # in the format YYYY-MM-DD

# Format FY in 4 digit e.g. 2627
fy_short <- paste0(
  substr(latest_year, 3, 4),
  as.character(as.numeric(substr(latest_year, 3, 4)) + 1)
)

# LOAD IN DATA HERE ----

############## Forecast ############## 
forecast <- readRDS(paste0('forecasts/sarima/output/run ', run_number, '/forecast-run-', run_number,'.rds')) %>%
  filter(Historical_Data == "12 months of historical data",
         Year == latest_year,
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

forecast_items_quarterly <- forecast %>%
  mutate(FY = extract_fin_year(Date),
         quarter_date = qtr_end(Date)) %>%
  filter(Type == 'Claim PD Number of Paid Items') %>%
  group_by(Board, quarter_date, FY, Type) %>%
  mutate(Date = max(Date, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Board, Date, FY, Type) %>%
  summarise(Measure = sum(Measure),
            Forecast = sum(Forecast),
            Lower_80 = sum(Lower_80),
            Upper_80 = sum(Upper_80),
            Lower_95 = sum(Lower_95),
            Upper_95 = sum(Upper_95)) %>%
  ungroup()

## Number of Paid Items per working day forecast
forecast_items_wd <- forecast %>%
  mutate(FY = extract_fin_year(Date)) %>%
  filter(Type == 'Paid Items per Working Day') %>%
  group_by(Board) %>%
  mutate(YoY = (Measure - lag(Measure, 12)) / lag(Measure, 12) * 100) %>%
  ungroup()

############## Cost Data ############## 

forecast_gic <- forecast %>%
  mutate(FY = extract_fin_year(Date)) %>%
  filter(Type == 'Claim PD Paid GIC excl. BB') %>%
  group_by(Board) %>%
  mutate(YoY = (Measure - lag(Measure, 12)) / lag(Measure, 12) * 100) %>%
  ungroup()

forecast_gic_quarterly <- forecast %>%
  mutate(FY = extract_fin_year(Date),
         quarter_date = qtr_end(Date)) %>%
  filter(Type == 'Claim PD Paid GIC excl. BB') %>%
  group_by(Board, quarter_date, FY, Type) %>%
  mutate(Date = max(Date, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Board, Date, FY, Type) %>%
  summarise(Measure = sum(Measure),
            Forecast = sum(Forecast),
            Lower_80 = sum(Lower_80),
            Upper_80 = sum(Upper_80),
            Lower_95 = sum(Lower_95),
            Upper_95 = sum(Upper_95)) %>%
  ungroup()

## Cost per Item
forecast_cpi <- forecast %>%
  mutate(FY = extract_fin_year(Date)) %>%
  filter(Type == 'Cost per item') %>%
  group_by(Board) %>%
  mutate(YoY = (Measure - lag(Measure, 12)) / lag(Measure, 12) * 100) %>%
  ungroup() 

## Phasings - uncomment this code once full financial year of data is in
if(new_fy == TRUE) {
  
  forecast_phasings <- forecast_gic %>%
    filter(Date > "2010-03-31") %>%
    group_by(Board, FY, Type) %>%
    mutate(FY_Spend = sum(Measure),
           FY_Spend_Forecast = sum(Forecast)) %>%
    ungroup() %>%
    mutate(Phasings_Obs = Measure / FY_Spend * 100,
           Phasings_Forecast = Forecast / FY_Spend_Forecast * 100) %>%
    select(-Measure, -Forecast, -YoY, -Lower_80, -Lower_95, -Upper_80, -Upper_95) %>%
    dplyr::rename(Measure = Phasings_Obs,
                  Forecast = Phasings_Forecast) %>%
    select(Board, Date, Type, Measure, Forecast, everything()) %>%
    mutate(Type = "Phasings")
  
  ### Save file for 26/27
  write.xlsx(forecast_phasings, glue('data/Phasings/{fy_short} Phasings.xlsx'))
  
} else {
  forecast_phasings <- read_excel(glue('data/Phasings/{fy_short} Phasings.xlsx'))
}

############## Trend Monitoring Data ############## 

## List all Excel files in the directory
file_list <- list.files(path = "data/Supplementary Trend Data", pattern = "\\.xlsx$", full.names = TRUE)

## Read all files into a list of data frames
data_list <- map(file_list, ~read.xlsx(.x, startRow = 2, cols = 2:7, sep.names = ' '))

## Combine all data frames into one
combined_data <- bind_rows(data_list) %>%
  unique() %>%
  mutate(`Paid Date` = as.Date(`Paid Date`, origin = "1899-12-30"),
         `Avg No of Items per prescription` = `Claim PD Number of Paid Items` / `No of Prescriptions`) %>%
  arrange(`Paid Date`)

scotland_data <- combined_data %>%
  group_by(`Paid Date`, `Paid Calendar Year`) %>%
  summarise(`Claim PD Number of Paid Items` = sum(`Claim PD Number of Paid Items`),
            `No of Prescriptions` = sum(`No of Prescriptions`)) %>%
  ungroup() %>%
  mutate(`Presc Health Board Name` = 'SCOTLAND',
         `Avg No of Items per prescription` = `Claim PD Number of Paid Items` / `No of Prescriptions`) %>%
  select(`Presc Health Board Name`, everything())

combined_data <- combined_data %>%
  rbind(scotland_data) %>%
  arrange(`Paid Date`)

## compare figures with working days
combined_data_wd <- combined_data

business_days <- readRDS('data/Business Days/business_days_lookup.rds') %>%
  dplyr::rename(`Paid Date` = Date)

combined_data_wd <- combined_data_wd %>%
  left_join(business_days, by = 'Paid Date') %>%
  mutate(#`Number of Paid Items per working day` = `Claim PD Number of Paid Items` / Business_Days,
    `No of Prescriptions per working day` = `No of Prescriptions` / Business_Days) #%>%
#mutate(`Number of items per prescription per working day` = `Number of Paid Items per working day` / `No of Prescriptions per working day`)

############## Performance ############## 
forecast_performance <- read_excel('forecasts/sarima/output/forecast-performance.xlsx')
  
accuracy <- forecast_performance %>%
  # create column with forecast parameters
  mutate(Model = paste0("(", p, ", 1, ", q, ")(", P, ", 1, ", Q, ")[12]")) %>%
  select(`Prescribing Health Board` = board, Measure = type, Model, `Run Number` = run,
         Version = version, `Average MAPE` = forecast_error)

############## Variables ############## 
healthboards <- unique(forecast_items$Board)
financial_years <- unique(forecast_items$FY)
measures <- unique(accuracy$Measure)
run_numbers <- unique(accuracy$`Run Number`)

performance_hbs <- c("All boards", healthboards[healthboards %!in% "SCOTLAND"]) # Scotland forecast is aggregated, so no need for it here. 
                                                                                # Need all boards option for filter

# Create a custom theme
my_theme <- bs_theme(
  version = 5,                # Bootstrap 5
  primary = "#3F3685",        # Change primary button colour
  secondary = "#0078D4"       # Change secondary button colour
)





