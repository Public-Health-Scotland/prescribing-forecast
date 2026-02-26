####################### Model Information Page #######################


# tagList(
#   ## Heading and introductory text
#   fluidRow(
#     h1("Model details"),
#     HTML(paste("In this iteration of the forecasting model, the SARIMA (Seasonal Auto-Regressive Integrated Moving Average) statistical model has been used to generate the forecasts seen within this dashboard.",
#                "<br>The model can be defined in the format below:</br>",
#                "<p style='text-align: center;'><strong>SARIMA(p, d, q)(1, 0, 1)[12]</strong></p>",
#                "Each SARIMA model has two parts to it - a seasonal part (the second set of brackets) and a non-seasonal (first set of brackets) part. Values in the non-seasonal part are denoted as p, d and q, while the seasonal part is the same but denoted in upper-case. 'p' deals with the auto-regressive part of the mode, 'q' quantifies the number of times the data is differenced to achieve stationarity, and 'p' is the moving average component of the model. The value in the square brackets defines the time series period, which in this case is monthly.<br>", 
#                "Following model testing on the time series data used, the seasonal part was able to be quantified as (1, 0, 1) while the non-seasonal part was particularly tricky to define. To deal with this problem, the model first runs with the seasonal and periodic parts of the model pre-defined while looping through each possible combination of the three non-seasonal parameters (values of p and d between 0 and 9, q between 0 and 1).<br>",
#                "Say for example, the model is required to forecast twelve months from June 2025 for NHS Ayrshire & Arran. The model will use time series data until June 2024 and generate forecasts for the following twenty-four months. This is so predicted values for the first twelve months can be compared to real, observed data. This comparison will calculate an absolute average error between the two sets of data, and at the end of the looping process take the model with the lowest calculated error as the best fit. Computing the model in this way means each board will have a model that best fits their data at the time of running (based on the time series boundaries). Scotland figures can then be calculated once each board's model has been generated, by aggregating the forecasted values and its confidence intervals - ensuring the variability in each board is taken into account.",
#                
#                sep="<br/>"),
#          collapse = "<br><br>")  
#     )
# )


tagList(
  ## Heading and introductory text
  fluidRow(
    h1("Model details"),
    
    column(10,
    
      p("In this iteration of the forecasting model, the SARIMA (Seasonal Auto-Regressive Integrated Moving Average) statistical model has been used to generate the forecasts seen within this dashboard."),
      
      p("The model can be defined in the format below:"),
      div(
        strong("SARIMA(p, d, q)(1, 0, 1)[12]"),
        style = "text-align: left;"
      ),
      
      p(linebreaks(0.5)),
      
      p("Each SARIMA model has two parts to it — a seasonal part (the second set of brackets) and a non-seasonal (first set of brackets) part. Values in the non-seasonal part are denoted as p, d and q, while the seasonal part is the same but denoted in upper-case. 'p' deals with the auto-regressive part of the mode, 'q' quantifies the number of times the data is differenced to achieve stationarity, and 'p' is the moving average component of the model. The value in the square brackets defines the time series period, which in this case is monthly."),
      
      p("Following model testing on the time series data used, the seasonal part was able to be quantified as (1, 0, 1) while the non-seasonal part was particularly tricky to define. To deal with this problem, the model first runs with the seasonal and periodic parts of the model pre-defined while looping through each possible combination of the three non-seasonal parameters (values of p and d between 0 and 9, q between 0 and 1)."),
      
      p("Say for example, the model is required to forecast twelve months from June 2025 for NHS Ayrshire & Arran. The model will use time series data until June 2024 and generate forecasts for the following twenty-four months. This is so predicted values for the first twelve months can be compared to real, observed data. This comparison will calculate an absolute average error between the two sets of data, and at the end of the looping process take the model with the lowest calculated error as the best fit. Computing the model in this way means each board will have a model that best fits their data at the time of running (based on the time series boundaries). Scotland figures can then be calculated once each board's model has been generated, by aggregating the forecasted values and its confidence intervals - ensuring the variability in each board is taken into account.")
  
      )
  
  )
)