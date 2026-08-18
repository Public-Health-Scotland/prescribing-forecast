# ############## EDA Server ######################

# Putting EDA code in here to remove it from setup file
eda_data <- read.csv('data/Time-Series Data/Historical Data.csv', check.names = FALSE) %>%
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
list_sizes <- read.xlsx('data/List Sizes/List Sizes.xlsx') %>%
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

## Turn dataframe from setup into tsibble
eda_data_tsibble <- reactive({
  
  df <- eda_data %>%
    mutate(month_new = yearmonth(`Paid Date`)) %>%
    filter(`Presc Health Board Name` == input$eda_board) %>%
    # create new column for index
    tsibble(index = 'month_new') 
  
})

## Time-series plots ----

### Items
output$items_ts <- renderPlot({
  
  eda_data_tsibble() %>%
    gg_season(`Claim PD Number of Paid Items`, labels = 'both') +
    scale_y_continuous(labels = scales::comma) +
    labs(
      x = "Month"
    )
  
}, res = 96)

### GIC
output$gic_ts <- renderPlot({
  
  eda_data_tsibble() %>%
    gg_season(`Claim PD Paid GIC excl. BB`, labels = 'both') +
    scale_y_continuous(labels = scales::comma) +
    labs(
      x = "Month"
    )
  
}, res = 96)

### CPI
output$cpi_ts <- renderPlot({
  
  eda_data_tsibble() %>%
    gg_season(`Cost per item`, labels = 'both') +
    scale_y_continuous(labels = scales::comma) +
    labs(
      x = "Month"
    )
  
}, res = 96)

## Sub-series plots ----
output$items_ss <- renderPlot({
  
  eda_data_tsibble() %>%
    gg_subseries(`Claim PD Number of Paid Items`)
  
}, res = 96)

### GIC
output$gic_ss <- renderPlot({
  
  eda_data_tsibble() %>%
    gg_subseries(`Claim PD Paid GIC excl. BB`)
  
}, res = 96)

### CPI
output$cpi_ss <- renderPlot({
  
  eda_data_tsibble() %>%
    gg_subseries(`Cost per item`)
  
}, res = 96)

## Scatterplots ----
output$corr_scatter <- renderPlot({
  
  eda_data_tsibble() %>%
    GGally::ggpairs(columns = 3:5)
  
}, res = 96)

## Lag plots ----

### Items
output$items_lag <- renderPlot({
  
  eda_data_tsibble() %>%
    gg_lag(`Claim PD Number of Paid Items`, geom = "point")  
  
}, res = 96)

### GIC
output$gic_lag <- renderPlot({
  
  eda_data_tsibble() %>%
    gg_lag(`Claim PD Paid GIC excl. BB`, geom = "point")

}, res = 96)

### CPI
output$cpi_lag <- renderPlot({
  
  eda_data_tsibble() %>%
    gg_lag(`Cost per item`, geom = "point")
  
}, res = 96)



## Export data syntax and button ----
### Download handler
# output$downloadData_pp_wd <- downloadHandler(
#   filename = function() {
#     paste("PrescriptionWD-", Sys.Date(), ".csv", sep = "")
#   },
#   content = function(file) {
#     write.csv(pp_wd(), file, row.names = FALSE)
#   }
# )
# 
# output$downloadData_items_pp <- downloadHandler(
#   filename = function() {
#     paste("NumberOfPaidItemsPrescription-", Sys.Date(), ".csv", sep = "")
#   },
#   content = function(file) {
#     write.csv(pp_wd(), file, row.names = FALSE)
#   }
# )

##### This code generated images used in final report for this project.
##### It is not included in the EDA tab as of yet, but development could
##### be done to integrate it to help monitor trend.

##### Any data sources prefixed as forecast_... are taken from the setup file, 
##### make sure you've ran this before running this code for checking


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
  scale_y_continuous(labels = scales::comma) +
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
  scale_y_continuous(labels = scales::comma) +
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
  GGally::ggpairs(columns = 3:4,
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
  scale_y_continuous(labels = scales::comma) +
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
  scale_y_continuous(labels = scales::comma) +
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
  scale_y_continuous(labels = scales::comma) +
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
  as_tsibble(index = Month) #%>%
# mutate(
#   diff_1 = difference(`Number of Paid Items`, 1),
#   diff_12 = difference(diff_1, 12)
# ) %>%
# gg_tsdisplay(diff_12)


autoplot(acf_pacf_items, `Number of Paid Items`)

acf_pacf_items %>%
  ACF(`Number of Paid Items`) %>%
  autoplot()

acf_pacf_items %>%
  gg_tsdisplay(difference(`Number of Paid Items`, 12),
               plot_type = 'partial', lag = 36) +
  labs(title="Seasonally differenced", y="")

# test_mape <- rbind(`forecast-performance-2026-05-28` %>% select(board, type, MAPE, run), forecast_performance %>% select(board, type, MAPE, run))
# 
# t1 <- test_mape %>%
#   filter(!board == "SCOTLAND") %>%
#   group_by(run, type) %>%
#   summarise(avg = mean(MAPE))

##### Read in machine learning model results
ml_preds <- read_excel("model_outputs.xlsx", sheet = "Predictions") %>%
  mutate(time = ymd(time))

ml_preds_new <- ml_preds %>%
  select(Board, Date = time, Type = Target, Measure = Actual, Predicted, Model) %>%
  pivot_wider(names_from = Model, values_from = Predicted)

forecast_comp <- readRDS(paste0('forecasts/sarima/output/run 999/forecast-run-999.rds')) %>%
  filter(Historical_Data == "12 months of historical data",
         Year == 2025,
         F_Horizon == 48,
         Arima_Error == FALSE) %>%
  select(-c(Historical_Data, Year, F_Horizon, Arima_Error))

model_comp <- forecast_comp %>%
  select(Board, Date, Type, Measure, SARIMA = Forecast) %>%
  filter(Type %in% c("Claim PD Number of Paid Items", "Claim PD GIC excl. BB")) %>%
  left_join(ml_preds_new)


