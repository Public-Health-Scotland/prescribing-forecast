####################### Core functions #######################

# 1. Add n linebreaks function ----

#' @param n Number of linebreaks required
#' 
#' @returns HTML line breaks
#' 
#' @examples
#' linebreaks(1)

linebreaks <- function(n) {
  HTML(strrep(br(), n))
}

# 2. Remove warnings from icons ----

#' @param icon_name Name of icon from https://use.fontawesome.com
#' 
#' @returns Icon to be included with warnings removed
#' 
#' @examples
#' icon = icon_no_warning_fn("code-compare")

icon_no_warning_fn = function(icon_name) {
  icon(icon_name, verify_fa = FALSE)
}

# 3. Create and format data table ----

#' @param input_data_table R dataframe that you want to format
#' 
#' @returns Formatted datatable to put in dashboard
#' 
#' @examples
#' make_table(forecast_items)

make_table <- function(input_data_table, rows_to_display = 5) {
  # Take out underscores in column names for display purposes
  table_colnames <- gsub("_", " ", colnames(input_data_table))

  dt <- DT::datatable(
    input_data_table,
    style = 'bootstrap',
    class = 'table-bordered table-condensed',
    rownames = FALSE,
    filter = "top",
    colnames = table_colnames,
    options = list(
      pageLength = rows_to_display,
      scrollX = FALSE,
      scrollY = FALSE,
      dom = 'tip',
      autoWidth = TRUE,
      # style header
      initComplete = htmlwidgets::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#C5C3DA', 'color': '#3F3685'});",
        "$(this.api().table().row().index()).css({'background-color': '#C5C3DA', 'color': '#3F3685'});",
        "}"
      )
    )
  )

  return(dt)
}

# 4. Make chart function ----

#' @param data Name of reactive dataframe, don't pass into function with reactive brackets.
#' @param measure Number of Paid Items, Gross Ingredient Cost (£), Cost per item
#' @param aggregation Monthly, quarterly (lowercase)
#' @param filter Name of input parameter to be passed.
#' 
#' @returns Rendered plotly chart for use in forecasting dashboard.
#' 
#' @examples
#' make_chart(items_first_board_data,
#'            "Number of Paid Items",
#'            "monthly",
#'            reactive(input$first_board))
#' 

make_chart <- function(data, measure, aggregation, filter) {
  
  # Capitalisation of Gross Ingredient Cost in chart title should always be maintained
  if (measure == "Gross Ingredient Cost (£)") {
    measure_title <- measure
  } else {
    measure_title <- str_to_lower(measure)
  }
  
  renderPlotly({
    
    plot <- plot_ly(
      data(),
      x = ~Date,
      y = ~Measure,
      name = 'Actual data',
      type = 'scatter',
      mode = 'lines') %>%
      add_trace(y = ~Forecast,
                name = 'Forecast',
                line = list(dash = 'dot')) %>%
      add_ribbons(y = ~Forecast,
        ymin = ~Lower_95, ymax = ~Upper_95,
        fillcolor = 'rgba(255, 0, 0, 0.2)',
        line = list(color = 'rgba(255, 0, 0, 0)'),
        name = '95% CI') %>%
      add_ribbons(y = ~Forecast,
        ymin = ~Lower_80, ymax = ~Upper_80,
        line = list(color = 'rgba(0,0,0,0)'),
        fillcolor = 'rgba(100,100,200,0.2)',
        name = '80% CI') %>%
      # Update title and axes
      layout(title = paste("Forecasting", aggregation, measure_title, "in", filter()),
             xaxis = list(title = '<b>Date</b>',
                          rangeslider = list(visible = TRUE,          # Enable the range slider
                                             bgcolor = phs_colours('phs-magenta-30'),   # Background color of the range slider
                                             bordercolor = phs_colours('phs-magenta'),    # Border color
                                             borderwidth = 2)
             ),
             yaxis = list(title = paste0("<b>", measure, "</b>")),
             font = list(family = 'Arial'),
             legend = list(
               orientation = "h",
               x = 0.5,
               y = -0.7,
               xanchor = "center",
               yanchor = "top"
             ))
    
    plot
    
  })
  
}

# 5. Filter data function ----

#' @param forecast_data Desired forecast dataframe passed from setup file.
#' @param input_filter Name of input parameter to be passed.
#' 
#' @returns Reactive dataframe that is filtered by user using dropdown.
#' 
#' @examples
#' filter_data(forecast_items,
#'             reactive(input$first_board))

filter_data <- function(forecast_data, input_filter) {
  
  reactive({
    
    table <- forecast_data %>%
      filter(Board == input_filter())
    
  })
  
}

# 6. Year-on-year change plot function ----

#' @param data Name of reactive dataframe, don't pass into function with reactive brackets.
#' @param measure Number of Paid Items, Gross Ingredient Cost (£), Cost per item
#' @param filter Name of input parameter to be passed.
#' 
#' @returns Rendered plotly year-on-year percentage change chart for use in forecasting dashboard.
#' 
#' @examples
#' make_yoy_chart(gic_yearly_change,
#'                "Gross Ingredient Cost (£)",
#'                reactive(input$cost_board))
#' 

make_yoy_chart <- function(data, measure, filter) {
  
  # Capitalisation of Gross Ingredient Cost in chart title should always be maintained
  if (measure == "Gross Ingredient Cost (£)") {
    measure_title <- measure
  } else {
    measure_title <- str_to_lower(measure)
  }
  
  renderPlotly({
    
    plot <- plot_ly(
      data(),
      x = ~Date,
      y = ~YoY,
      type = 'scatter',
      mode = 'lines',
      fill = 'tozeroy'
    ) %>%
      layout(title = paste("Year-on-year percentage change in", measure, "in", filter()),
             xaxis = list(title = 'Month'),
             yaxis = list(title = 'Percentage change (%)'),
             font = list(family = 'Arial'))
    
    plot
    
  })
  
}
