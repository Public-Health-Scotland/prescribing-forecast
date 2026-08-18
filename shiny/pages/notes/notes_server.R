####################### Notes Page - Server #######################

## Heading and introductory text ####
output$info <-  renderUI({
  
  div(
    fluidRow(
      
      h1("PHS Prescribing Forecasting Model"),
      
      p("Welcome to PHS' very own prescribing forecasting model!"),
      p("This new and improved output was developed following a number of short life working group meetings, which were used to gather feedback on the previous version of the output on things like model accuracy and stakeholder suitability, and to then gather stakeholder requirements to ensure the work is tailored to those who require its information."),
      p("The decision was taken to use R to build the new version of the forecasting model, as this would provide the tools to gain much more control and flexibility in what can be applied to the model compared to Tableau."),
      p("Predictions seen in this dashboard have been generated using a SARIMA (Seasonal Auto-Regressive Moving Average) model. Further information can be found by navigating to the 'Model details' tab across the top of the page."),
      p("Please refer to the 'Feedback' tab and fill in the embedded Microsoft Form if you run into any problems, if you have any further questions around how to navigate the dashboard or if you have any useful feedback and/or suggestions that could be incorporated here."),
      p("Forecasts were last generated in May 2026, with updates scheduled to happen quarterly following the most recent data load e.g. February 2026 update will include quarter ending December 2025."),
      tags$p(tags$i("This piece of work has been devised, created, developed and iteratively refined to inform an ongoing disseration project as Glasgow Caledonian University. Your involvement in this piece of work as an end-user is greatly appreciated and will go a long way to making this project a success -", tags$b("thank you!")))
      
    ) #fluidrow
  ) # div
}) # renderUI

## Using the dashboard ####
output$using_dashboard <-  renderUI({
  
    fluidRow(
      
      h1("How to use the dashboard..."),
      
      p("See each of the sections below for instructions on how to interact with the dashboard."),
      
      h2("Filters"),
      
      p(strong("Volume, Cost and Monitoring"), "tabs will have a filter where users can choose a board's data to view."),
      
      p(
        tags$img(
        src = "images/Filters.png",
        style = "width: 250px"
        )
        ),
      
      p("Only the", strong("Volume and Cost"), "tabs will contain a filter where users can toggle between viewing the forecast, and the year-on-year change in the measure of choice (e.g. cost per item)."),
      
      p(
        tags$img(
          src = "images/View Filter.png",
          style = "width: 250px"
        )
      ),
      
      p("Forecasts for ", strong("Number of Paid Items and Gross Ingredient Cost"), "will also feature a filter where users can choose to aggregate the forecasts to either monthly or quarterly."),
      
      p(
        tags$img(
          src = "images/Aggregation.png",
          style = "width: 250px"
        )
      ),
      
      p("The forecast", strong("Comparator"), "tab features four filters: the first dropdown to filter data for the chart on the left, the second for the chart on the right, the third for choosing the forecast you're interested in and the fourth for the level of aggregation you require."),
      
      p(
        tags$img(
          src = "images/Comparator.png",
          style = "width: 1400px"
        )
      ),
      
      h2("Chart interaction"),
      
      p("Each graph within this dashboard has been created using Plotly, which contains a number of different opportunities for users to interact."),
      
      p(
        tags$img(
          src = "images/Date Slider.png",
          style = "width: 750px"
        )
      ),
      
      p("Use the slider underneath the x-axis to shorten or lengthen the time-series in view - the white bars at either side can be used."),
      
      p("A single click on a legend entry (e.g. 95% CI) will hide it from the graph, whereas a double click will isolate the trace and hide each of the other entries."),
      
      h2("Data in tabular format"),
      
      p("Click the download data button to view an Excel workbook with the chart's background data."),
      
      p(
        tags$img(
          src = "images/Download Data.png",
          style = "width: 250px"
        )
      )
      
    ) #fluidrow
  
}) # renderUI

## Dashboard Updates ####

## Text explaining tab ####
output$dashboard_updates <- renderUI({
  
  fluidRow(
    
    h1("Latest updates to dashboard"),
    
    p("This tab will detail any changes made to dashboard functionality and UI (if any), at each refresh."),
    
    ## Add new section each time if anything to update on
    
    tags$b("February 2026:"),
    p(linebreaks(0.5)),
    tags$ul(
      style = "margin-left: 20px; list-style-type: disc;",
      tags$li("New tab added for listing updates to dashboard"),
      tags$li("Quarterly forecast aggregation added for items and GIC"),
      tags$li("Download data file names now including board name")
    ),
    
    ## Add new section each time if anything to update on
    
    tags$b("May 2026:"),
    p(linebreaks(0.5)),
    tags$ul(
      style = "margin-left: 20px; list-style-type: disc;",
      tags$li("New tab added for comparing forecasts of two boards"),
      tags$li("Phasings value added back in based on gross ingredient cost forecast"),
      tags$li("Links and tabs updated to remove mention of 'pharmacy' and replace with 'prescribing'")
    ),
    
    tags$b("August 2026:"),
    p(linebreaks(0.5)),
    tags$ul(
      style = "margin-left: 20px; list-style-type: disc;",
      tags$li("Performance tab added to allow users to see accuracy measurements (still in development)")
    )
    
  )
  
})

## Model Updates ####

## Text for each model version ####
model1 <- div(
  
    fluidRow(
      
      tags$b("Last updated: June 2025"),
      tags$b("Platform: Tableau"),
      
      p(linebreaks(0.5)),
      
      # Bullet pointed list with heading
      tags$b("Key features:"),
      tags$ul(
        style = "margin-left: 20px; list-style-type: disc;",
        tags$li("Automatic exponential smoothing"),
        tags$li("Splits for each board within Tableau"),
        tags$li("Results formatted in Excel"),
        tags$li("Accuracy template created to measure predicted versus observed")
        )
      
    )
  )
  
model2_0 <- div(
  
  fluidRow(
    
    tags$b("Last updated: September 2025"),
    tags$b("Platform: R"),
    
    p(linebreaks(0.5)),
    
    # Bullet pointed list with heading
    tags$b("Key features:"),
    tags$ul(
      style = "margin-left: 20px; list-style-type: disc;",
      tags$li("SARIMA model developed and evaluated"),
      tags$li("Each board's best fit model chosen"),
      tags$li("Forecasts for number of paid items and gross ingredient cost"),
      tags$li("Year-on-year change charts"),
      tags$li("Shiny dashboard implemented with trend monitoring and ability for users to download data")
    )
    
  )
)

model2_1 <- div(
  
  fluidRow(
    
    tags$b("Last updated: May 2026"),
    tags$b("Platform: R"),
    
    p(linebreaks(0.5)),
    
    # Bullet pointed list with heading
    tags$b("Key features:"),
    tags$ul(
      style = "margin-left: 20px; list-style-type: disc;",
      tags$li("Time series data now starting from 2011 (free prescriptions)"),
      tags$li("Cost per item forecast now included"),
      tags$li("Feedback form added to dashboard"),
      tags$li("Prescribing Health Board used instead of Dispensing Health Board in data extracts")
      )
  )
)

model2_2 <- div(
  
  fluidRow(
    
    tags$b("Last updated: August 2026"),
    tags$b("Platform: R"),
    
    p(linebreaks(0.5)),
    
    # Bullet pointed list with heading
    tags$b("Key features:"),
    tags$ul(
      style = "margin-left: 20px; list-style-type: disc;",
      tags$li("Data extract updated to match Schedule 5 excl. England report; excluding English prescribing and dispensing, only including community pharmacy and GP prescribing"),
      tags$li("Parameter grid search updated"),
      tags$li("Increased model stability, less computational time")
    )
    
  )
  
)

output$model_updates <-  renderUI({
  
  div(
    fluidRow(
      
      h1("Version History"),
      
      p("See below details of each model version throughout development."),
      
      tags$b("Next scheduled refresh: August/September 2026"),
      
      p(linebreaks(1)),
      
    ), #fluidrow
    
    navset_card_tab(
      nav_panel(
        "PharmPredict 1",
        fluidRow(
          "First generated in the mid 2010s PharmaPredict 1 was the original model developed, using Tableau's built in exponential smoothing function."
        ),
        layout_column_wrap(
          width = 1/2,
          height = 300,
          card(full_screen = TRUE, card_header("v1.0"), model1)
          )
        ),
      nav_panel(
        "PharmPredict 2",
        fluidRow(
          "PharmaPredict 2 represents the latest developments into the prescribing forecast, first developed in September 2025 following extensive consultations with a dedicated short life working group."
        ),
        layout_column_wrap(
          width = 1/2,
          height = 600,
          card(full_screen = TRUE, card_header("v2.0"), model2_0),
          card(full_screen = TRUE, card_header("v2.1"), model2_1),
          card(full_screen = TRUE, card_header("v2.2"), model2_2)
        )      
        )
    )
    
  ) # div
}) # renderUI

## Sharing of outputs ####
output$share_outputs <-  renderUI({
  
  div(
    fluidRow(
      
      h1("Sharing outputs outwith the dashboard"),
      
      p("The sharing of outputs from this dashboard is encouraged, however anything in relation to the forecasts and their values e.g. charts or data tables, must be supplemented with the correct notes."),
      p("Please see below supporting narrative that should be distributed alongside any forecast outputs."),
      
      div(
        style = "position: relative;
                 border: 1px solid #ddd;
                 border-radius: 8px;           
                 padding: 15px 50px 15px 15px;
                 background-color: #f9f9f9;
                 margin-top: 5px;
                 max-width: 95%",
        
        actionButton(
          inputId = "copy_btn",
          label = tags$i(class = "fa-solid fa-copy"),
          title = "Copy text",
          style = "position: absolute;
           top: 10px;
           right: 10px;
           border: none;
           background: transparent;
           cursor: pointer;
           font-size: 16px;
           color: #555;
           padding: 0;
           min-width: auto;",
          class = "btn btn-link"
        ),
        
        # Text content
        div(
          id = "copy_text_block",
          
          tags$p(
            tags$i(
              "Please note that predictions detailed in this table/chart should be treated as such, ",
              strong("they are not an indication of fact."),
              " Predictions are informed using a SARIMA (Seasonal Auto-Regressive Integrated Moving Average) model using historical data, capturing trend and seasonality patterns through its parameter definitions. Please see the ",
              tags$a(href = "https://scotland.shinyapps.io/phs-prescribing-forecast/", 
                     "Prescribing Forecast", target = "_blank"),
              " dashboard for further information."
            )
          ),
          
          tags$p(
            tags$i(
              "Confidence intervals in the context of SARIMA forecasts quantifies the uncertainty of the predicted values. For example, a ",
              strong("95% confidence interval"),
              " suggests that the true value is ",
              strong("expected to lie within this range 95% of the time."),
              " An ",
              strong("80% confidence interval"),
              " suggests that the true value is ",
              strong("expected to lie within this range 80% of the time."),
              " These intervals help to communicate the reliability of predictions. Confidence interval ranges are likely to widen with each incremental time point, reflecting the higher level of uncertainty attributed to long-term predictions."
            )
          )
        )
      ),
      
      # div(
      #   id = "copy_text_block",
      #   
      #   tags$p(
      #     tags$i(
      #       "Please note that predictions detailed in this table/chart should be treated as such, ",
      #       strong("they are not an indication of fact."),
      #       " Predictions are informed using a SARIMA (Seasonal Auto-Regressive Integrated Moving Average) model using historical data, capturing trend and seasonality patterns through its parameter definitions. Please see the ",
      #       tags$a(href = "https://scotland.shinyapps.io/phs-prescribing-forecast/", 
      #              "Prescribing Forecast", target = "_blank"),
      #       " dashboard for further information."
      #     )
      #   ),
      #   
      #   tags$p(
      #     tags$i(
      #       "Confidence intervals in the context of SARIMA forecasts quantifies the uncertainty of the predicted values. For example, a ",
      #       strong("95% confidence interval"),
      #       " suggests that the true value is ",
      #       strong("expected to lie within this range 95% of the time."),
      #       " An ",
      #       strong("80% confidence interval"),
      #       " suggests that the true value is ",
      #       strong("expected to lie within this range 80% of the time."),
      #       " These intervals help to communicate the reliability of predictions. Confidence interval ranges are likely to widen with each incremental time point, reflecting the higher level of uncertainty attributed to long-term predictions."
      #     )
      #   )
      # ),
      
      #actionButton("copy_btn", "Copy text")
    
    ) #fluidrow
  ) # div
}) # renderUI

observeEvent(input$copy_btn, {
  
  shinyjs::runjs("
    var el = document.getElementById('copy_text_block');
    var html = el.innerHTML;

    var blob = new Blob([html], { type: 'text/html' });
    var data = [new ClipboardItem({ 'text/html': blob })];

    navigator.clipboard.write(data);
  ")
  
  showNotification('Copied to clipboard', type = 'message')
})

