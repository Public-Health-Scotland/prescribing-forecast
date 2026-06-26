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
library(scales)

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

# Key variables to load files
run_number = 4
latest_year = 2026
forecast_performance_date = '2026-05-24' # in the format YYYY-MM-DD

# LOAD IN DATA HERE ----

############## Forecast ############## 
forecast <- readRDS(paste0('shiny/forecasts/sarima/output/forecast-run-', run_number,'.rds')) %>%
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
# forecast_phasings <- forecast_gic %>%
#   filter(Date > "2010-03-31") %>%
#   group_by(Board, FY, Type) %>%
#   mutate(FY_Spend = sum(Measure),
#          FY_Spend_Forecast = sum(Forecast)) %>%
#   ungroup() %>%
#   mutate(Phasings_Obs = Measure / FY_Spend * 100,
#          Phasings_Forecast = Forecast / FY_Spend_Forecast * 100) %>%
#   select(-Measure, -Forecast, -YoY, -Lower_80, -Lower_95, -Upper_80, -Upper_95) %>%
#   dplyr::rename(Measure = Phasings_Obs,
#                 Forecast = Phasings_Forecast) %>%
#   select(Board, Date, Type, Measure, Forecast, everything()) %>%
#   mutate(Type = "Phasings")
# 
# ### Save file for 26/27
# write.xlsx(forecast_phasings, 'shiny/data/2627 Phasings.xlsx')

### Phasings - leave uncommented once new file has been saved
forecast_phasings <- read_excel('shiny/data/2627 Phasings.xlsx')

############## Trend Monitoring Data ############## 

## List all Excel files in the directory
file_list <- list.files(path = "shiny/data/Items per prescription/", pattern = "\\.xlsx$", full.names = TRUE)

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

business_days <- readRDS('shiny/forecasts/data/business_days_lookup.rds') %>%
  dplyr::rename(`Paid Date` = Date)

combined_data_wd <- combined_data_wd %>%
  left_join(business_days, by = 'Paid Date') %>%
  mutate(#`Number of Paid Items per working day` = `Claim PD Number of Paid Items` / Business_Days,
    `No of Prescriptions per working day` = `No of Prescriptions` / Business_Days) #%>%
#mutate(`Number of items per prescription per working day` = `Number of Paid Items per working day` / `No of Prescriptions per working day`)

############## Performance ############## 
forecast_performance <- read_excel('shiny/forecasts/sarima/output/forecast-performance.xlsx')

############## EDA ############## 
eda_data <- read.csv('shiny/forecasts/data/Historical Data.csv', check.names = FALSE) %>%
  mutate(`Paid Date` = dmy(`Paid Date`)) %>%
  filter(`Paid Date` > '2009-12-31') 

eda_data$`Claim PD Paid GIC excl. BB` <- as.double(gsub(",", "", eda_data$`Claim PD Paid GIC excl. BB`))

scotland_data <- eda_data %>%
  group_by(`Paid Date`) %>%
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>%
  mutate(`Presc Health Board Name` = 'SCOTLAND') %>%
  select(`Presc Health Board Name`, everything())

eda_data <- eda_data %>%
  bind_rows(scotland_data) %>%
  mutate(`Cost per item` = `Claim PD Paid GIC excl. BB` / `Claim PD Number of Paid Items`) %>%
  # double check data is arranged by board and date 
  arrange(`Presc Health Board Name`, `Paid Date`)

rm(scotland_data)

# eda_data_tsibble <- eda_data %>%
#   # create new column for index
#   mutate(month_new = yearmonth(`Paid Date`)) %>%
#   tsibble(index = 'month_new')

############## per 1,000 list size (weighted and non-weighted) ############## 
list_sizes <- read.xlsx('shiny/data/population/List Sizes.xlsx') %>%
  mutate(quarter_date = as.Date(quarter_date, origin = "1899-12-30")) %>%
  dplyr::rename(`Paid Date` = quarter_date,
                `Presc Health Board Name` = Board) %>%
  select(`Presc Health Board Name`, everything()) %>%
  left_join(eda_data, by = c('Presc Health Board Name', 'Paid Date')) %>%
  select(`Presc Health Board Name`, `Paid Date`, `Claim PD Number of Paid Items`, `Claim PD Paid GIC excl. BB`, `Cost per item`, everything()) %>%
  mutate(
    Items_1000_LS = `Claim PD Number of Paid Items` / `Non-Weighted` * 1000,
    Items_1000_Weighted_LS = `Claim PD Number of Paid Items` / Weighted * 1000,
    GIC_1000_LS = `Claim PD Paid GIC excl. BB` / `Non-Weighted` * 1000,
    GIC_1000_Weighted_LS = `Claim PD Paid GIC excl. BB` / Weighted * 1000,
    CPI_1000_LS = `Cost per item` / `Non-Weighted` * 1000,
    CPI_1000_Weighted_LS = `Cost per item` / Weighted * 1000
  )

plot <- plot_ly(
  data = list_sizes,
  x = ~`Paid Date`,
  y = ~Items_1000_LS,
  color = ~`Presc Health Board Name`,
  type = 'scatter',
  mode = 'lines'
)

plot <- plot_ly(
  data = list_sizes %>% filter(!(`Presc Health Board Name` == "SCOTLAND")),
  x = ~`Paid Date`,
  y = ~Items_1000_Weighted_LS,
  color = ~`Presc Health Board Name`,
  type = 'scatter',
  mode = 'lines'
)

# Add a bold version of one specific line (e.g., 'NHS Greater Glasgow & Clyde')
plot <- plot %>%
  add_trace(
    data = subset(list_sizes, `Presc Health Board Name` == "SCOTLAND"),
    x = ~`Paid Date`,
    y = ~Items_1000_Weighted_LS,
    type = 'scatter',
    mode = 'lines',
    line = list(width = 4,
                color = "#3F3685"),      # ← Bold effect
    name = "SCOTLAND"
  )

# Monthly trend extraction for Scotland
timeseries_items_trend <- forecast_items %>%
  filter(Board == "SCOTLAND") %>%
  arrange(Date) %>%
  mutate(
    month = months(Date),
    month = factor(month, levels = c(
      "January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December"
    ))
  ) %>%
  filter(!is.na(Measure)) %>%
  group_by(month) %>%
  summarise(Average = mean(Measure), .groups = "drop") %>%
  arrange(month)

seasonality_items_chart <- ggplot(
  timeseries_items_trend,
  aes(x = month, y = Average)) +
  geom_line(group = 1, color = phs_colours("phs-purple"), linewidth = 1.5) +
  #geom_point() +
  scale_y_continuous(labels = comma) +
  labs(title = "Seasonality chart showing mean number of paid items in Scotland across each month from 2010 to 2025",
       x = "Month",
       y = "Mean number of paid items") +
  theme(plot.title = element_text(size = 18),
        axis.title.x = element_text(size = 14), # X-axis title
        axis.title.y = element_text(size = 14), # Y-axis title
        axis.text.x = element_text(size = 12), # X-axis labels
        axis.text.y = element_text(size = 12)) # Y-axis labels

timeseries_gic_trend <- forecast_gic %>%
  filter(Board == "SCOTLAND",
         Date < "2026-01-01") %>%
  arrange(Date) %>%
  mutate(
    month = months(Date),
    month = factor(month, levels = c(
      "January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December"
    ))
  ) %>%
  #filter(!is.na(Measure)) %>%
  group_by(month) %>%
  summarise(Average = mean(Measure), .groups = "drop") %>%
  arrange(month)

seasonality_gic_chart <- ggplot(
  timeseries_gic_trend,
  aes(x = month, y = Average)) +
  geom_line(group = 1, color = phs_colours("phs-purple"), linewidth = 1.5) +
  #geom_point() +
  scale_y_continuous(labels = comma) +
  labs(title = "Seasonality chart showing mean Gross Ingredient Cost (£) in Scotland across each month from 2010 to 2025",
       x = "Month",
       y = "Mean Gross Ingredient Cost (£)") +
  theme(plot.title = element_text(size = 18),
        axis.title.x = element_text(size = 14), # X-axis title
        axis.title.y = element_text(size = 14), # Y-axis title
        axis.text.x = element_text(size = 12), # X-axis labels
        axis.text.y = element_text(size = 12)) # Y-axis labels

correlation_items_trend <- forecast_items %>%
  filter(Board == "SCOTLAND") %>%
  arrange(Date) %>%
  filter(Date < "2026-01-01") %>%
  select(Date, Board, `Number of Paid Items` = Measure)

correlation_gic_trend <- forecast_gic %>%
  filter(Board == "SCOTLAND") %>%
  arrange(Date) %>%
  filter(Date < "2026-01-01") %>%
  select(Date, Board, `Gross Ingredient Cost (£)` = Measure)

correlation <- left_join(correlation_items_trend, correlation_gic_trend)

correlation_plot <- correlation %>%
  ggpairs(columns = 3:4,
          lower = list(continuous = wrap("points", colour = phs_colours("phs-purple"))),
          diag  = list(continuous = wrap("densityDiag", fill = phs_colours("phs-purple"), colour = phs_colours("phs-purple"))),
          upper = list(continuous = wrap("cor", colour = phs_colours("phs-purple")))
  ) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 14)  # variable names on panels
  )


# Define a scaling factor
scale_factor <- max(correlation$`Number of Paid Items`, na.rm = TRUE) / 
  max(correlation$`Gross Ingredient Cost (£)`, na.rm = TRUE)

# pivot_corr <- correlation %>%
#   pivot_longer(cols = 3:4,
#                names_to = "Variable",
#                values_to = "Value")

ggplot(correlation, aes(x = Date, y = `Number of Paid Items`)) +
  geom_line(colour = phs_colours("phs-purple"), size = 1) +
  labs(title = "Monthly trend chart showing number of paid items in Scotland from 2010 to 2025",
       x = "Month",
       y = "Number of Paid Items") +
  scale_y_continuous(labels = comma) +
  theme(plot.title = element_text(size = 18),
        axis.title.x = element_text(size = 14), # X-axis title
        axis.title.y = element_text(size = 14), # Y-axis title
        axis.text.x = element_text(size = 12), # X-axis labels
        axis.text.y = element_text(size = 12)) # Y-axis labels
  
ggplot(correlation, aes(x = Date, y = `Gross Ingredient Cost (£)`)) +
  geom_line(colour = phs_colours("phs-purple"), size = 1) +
  labs(title = "Monthly trend chart showing Gross Ingredient Cost (£) in Scotland from 2010 to 2025",
       x = "Month",
       y = "Gross Ingredient Cost (£)") +
  scale_y_continuous(labels = comma) +
  theme(plot.title = element_text(size = 18),
        axis.title.x = element_text(size = 14), # X-axis title
        axis.title.y = element_text(size = 14), # Y-axis title
        axis.text.x = element_text(size = 12), # X-axis labels
        axis.text.y = element_text(size = 12)) # Y-axis labels

cpi <- forecast_cpi %>%
  filter(Board == "SCOTLAND",
         Date > "2011-03-31" & Date < "2026-01-01") %>%
  select(Date, Measure) %>%
  arrange(Date)

ggplot(cpi,
       aes(x = Date, y = Measure)) +
  geom_line(colour = phs_colours("phs-purple"), size = 1) +
  labs(title = "Monthly trend chart showing average cost per item in Scotland from 2010 to 2025",
       x = "Month",
       y = "Average cost per item") +
  scale_y_continuous(labels = comma) +
  theme(plot.title = element_text(size = 18),
        axis.title.x = element_text(size = 14), # X-axis title
        axis.title.y = element_text(size = 14), # Y-axis title
        axis.text.x = element_text(size = 12), # X-axis labels
        axis.text.y = element_text(size = 12)) # Y-axis labels

acf_pacf_items <- forecast_items %>%
  filter(Board == "NHS AYRSHIRE & ARRAN") %>%
  arrange(Date) %>%
  filter(Date > "2011-01-01" & Date < "2026-01-01") %>%
  select(Date, `Number of Paid Items` = Measure) %>%
  mutate(Month = yearmonth(Date)) %>%
  as_tsibble(index = Month) %>%
  mutate(
    diff_1 = difference(`Number of Paid Items`, 1),
    diff_12 = difference(diff_1, 12)
  ) %>%
  gg_tsdisplay(diff_12)


autoplot(acf_pacf_items, `Number of Paid Items`)

acf_pacf_items %>%
  ACF(`Number of Paid Items`) %>%
  autoplot()

acf_pacf_items %>%
  gg_tsdisplay(difference(`Number of Paid Items`, 12),
               plot_type = 'partial', lag = 36) +
  labs(title="Seasonally differenced", y="")

test_mape <- rbind(`forecast-performance-2026-05-28` %>% select(board, type, MAPE, run), forecast_performance %>% select(board, type, MAPE, run))

t1 <- test_mape %>%
  filter(!board == "SCOTLAND") %>%
  group_by(run, type) %>%
  summarise(avg = mean(MAPE))

##### Read in machine learning model results
ml_preds <- read_excel("model_outputs.xlsx", sheet = "Predictions") %>%
  mutate(time = ymd(time))

ml_preds_new <- ml_preds %>%
  select(Board, Date = time, Type = Target, Measure = Actual, Predicted, Model) %>%
  pivot_wider(names_from = Model, values_from = Predicted)

forecast_comp <- readRDS(paste0('shiny/forecasts/sarima/output/forecast-run-999.rds')) %>%
  filter(Historical_Data == "12 months of historical data",
         Year == 2025,
         F_Horizon == 48,
         Arima_Error == FALSE) %>%
  select(-c(Historical_Data, Year, F_Horizon, Arima_Error))

model_comp <- forecast_comp %>%
  select(Board, Date, Type, Measure, SARIMA = Forecast) %>%
  filter(Type %in% c("Claim PD Number of Paid Items", "Claim PD GIC excl. BB")) %>%
  left_join(ml_preds_new)
  

############## Variables ############## 
healthboards <- unique(forecast_items$Board)
financial_years <- unique(forecast_items$FY)

# Create a custom theme
my_theme <- bs_theme(
  version = 5,                # Bootstrap 5
  primary = "#3F3685",        # Change primary button colour
  secondary = "#0078D4"       # Change secondary button colour
)















