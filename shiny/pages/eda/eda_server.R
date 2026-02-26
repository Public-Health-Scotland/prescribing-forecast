
# ############## Results Server ######################

palette <- phs_colour_values

## Turn dataframe from setup into tsibble
eda_data_tsibble <- reactive({
  
  df <- eda_data %>%
    mutate(month_new = yearmonth(`Paid Date`)) %>%
    filter(`Disp Health Board Name` == input$eda_board) %>%
    # create new column for index
    tsibble(index = 'month_new') 
  
})

## Time-series plots ----

### Items
output$items_ts <- renderPlot({
  
  eda_data_tsibble() %>%
    gg_season(`Claim PD Number of Paid Items`, labels = 'both')
  
}, res = 96)

### GIC
output$gic_ts <- renderPlot({
  
  eda_data_tsibble() %>%
    gg_season(`Claim PD Paid GIC excl. BB`, labels = 'both')
  
}, res = 96)

### CPI
output$cpi_ts <- renderPlot({
  
  eda_data_tsibble() %>%
    gg_season(`Cost per item`, labels = 'both')
  
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
    ggpairs(columns = 3:5)
  
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



