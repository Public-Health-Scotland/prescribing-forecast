### Initial forecasting script for September 2025
### Honours Project
### Date created: 01/08/2025
### Author: Liam Rooney

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

## 1.2. Initialise variables and file paths ----
'%!in%' <- function(x,y)!('%in%'(x,y))

path <- '/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast'

## 1.3. Load in data ----

### Healthboard code lookup
hb_code <- read_excel(glue('{path}/lookups/hb_code_lookup.xlsx'))

### Full dataset and joined with hb code lookup
data <- read_excel(glue('{path}/data/20250617 All data no HBPs.xlsx')) %>%
  left_join(hb_code, by = 'Disp Health Board Code') %>%
  select(`Disp Health Board Name`, everything(), -`Disp Health Board Code`)

### Split into two different data frames
items_data <- data %>%
  filter(`Disp Health Board Name` == 'NHS GREATER GLASGOW & CLYDE') %>%
  select(`Paid Date`, `Number of Paid Items`, `PD Paid GIC excl. BB`)

# gic_data <- data %>%
#   select(-`Number of Paid Items`, -Phasings)

# 2. Forecast ----

## 2.1. Frequency of a time series ----
y <- ts(items_data,
        start = year(min(items_data$`Paid Date`)),
        frequency = 12)

### Autoplot for items
autoplot(y[, "Number of Paid Items"])

### Autoplot for GIC
autoplot(y[, "PD Paid GIC excl. BB"])

## 2.2. Seasonal plots ----

### Line chart
#### Items
ggseasonplot(y[, "Number of Paid Items"], year.labels=TRUE, year.labels.left=TRUE) +
  ylab('Number of Paid Items') +
  ggtitle('Seasonal plot: number of paid items in GG&C')

#### GIC
ggseasonplot(y[, "PD Paid GIC excl. BB"], year.labels=TRUE, year.labels.left=TRUE) +
  ylab('GIC') +
  ggtitle('Seasonal plot: GIC in GG&C')

### Polar chart
#### Items
ggseasonplot(y[, "Number of Paid Items"], polar=TRUE) +
  ylab('Number of Paid Items') +
  ggtitle('Seasonal plot: number of paid items in GG&C')

#### GIC
ggseasonplot(y[, "PD Paid GIC excl. BB"], polar=TRUE) +
  ylab('GIC') +
  ggtitle('Seasonal plot: GIC in GG&C')

### Seasonal subseries plot
#### Items
ggsubseriesplot(y[, "Number of Paid Items"]) +
  ylab('Number of Paid Items') +
  ggtitle('Seasonal subseries plot: number of paid items in GG&C')

#### GIC
ggsubseriesplot(y[, "PD Paid GIC excl. BB"]) +
  ylab('GIC') +
  ggtitle('Seasonal subseries plot: GIC in GG&C')

## 2.3. Scatterplots ----
### Facet wrapped plot for both items and GIC
autoplot(y[, c('Number of Paid Items', 'PD Paid GIC excl. BB')], facets = TRUE) +
  xlab('Year') + ylab('') +
  ggtitle('Time series trend in GGC for both items and GIC')

### Study relationship of items against GIC
qplot(`Number of Paid Items`, `PD Paid GIC excl. BB`, data = as.data.frame(y))


## 2.4. Create forecasts ----
### Set training data
time_series <- items_data %>%
  select(-`PD Paid GIC excl. BB`)

y2 <- ts(time_series,
        start = year(min(time_series$`Paid Date`)),
        frequency = 12)

meanf(y2[, 'Number of Paid Items'], h = 12) 
rwf(y2[, 'Number of Paid Items'], h = 12) 
snaive(y2[, 'Number of Paid Items'], h = 12) 

# Apply all methods into one
items_data_2 <- window(y2, start = 2004, end = 2010)

autoplot(items_data_2[, 'Number of Paid Items']) +
  autolayer(meanf(items_data_2[, 'Number of Paid Items'], h = 11),
            series = 'Mean', PI = FALSE) +
  autolayer(rwf(items_data_2[, 'Number of Paid Items'], h = 11),
            series = 'Naïve', PI = FALSE) +
  autolayer(snaive(items_data_2[, 'Number of Paid Items'], h = 11),
            series = 'Seasonal naïve', PI = FALSE) +
  ggtitle('Forecasts for number of items in GG&C') +
  xlab('Year') + ylab('Number of items') +
  guides(colour = guide_legend(title = 'Forecast'))

# Apply non seasonal methods
autoplot(items_data_2[, 'Number of Paid Items']) +
  autolayer(meanf(items_data_2[, 'Number of Paid Items'], h = 11),
            series = 'Mean', PI = FALSE) +
  autolayer(rwf(items_data_2[, 'Number of Paid Items'], h = 11),
            series = 'Naïve', PI = FALSE) +
  autolayer(rwf(items_data_2[, 'Number of Paid Items'], drift = TRUE, h = 11),
            series = 'Drift', PI = FALSE) +
  ggtitle('Forecasts for number of items in GG&C') +
  xlab('Year') + ylab('Number of items') +
  guides(colour = guide_legend(title = 'Forecast'))


## 2.5. Transformations and adjustments ----
dframe <- cbind(Monthly = items_data_2[, 'Number of Paid Items'],
                DailyAverage = items_data_2[, 'Number of Paid Items']/monthdays(items_data_2[, 'Number of Paid Items']))

autoplot(dframe, facet = TRUE)
## page 62





