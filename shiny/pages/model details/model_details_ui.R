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
    
      p("The forecasting model used within this dashboard is based on a Seasonal Auto-Regressive Integrated Moving Average (SARIMA) framework. SARIMA was selected following a comparison of traditional statistical forecasting techniques and modern machine learning approaches, including XGBoost, LightGBM, Random Forest, and hybrid SARIMA/machine learning models. Across both prescribing activity and gross ingredient cost measures, SARIMA provided the most consistent combination of predictive accuracy, interpretability, reproducibility and ease of maintenance."),       
      
      p("The model can be defined in the format below:"),
      div(
        strong("SARIMA(p, 1, q)(P, 1, Q)[12]"),
        style = "text-align: left;"
      ),
      
      p(linebreaks(0.5)),
      
      p("The model consists of two components: a non-seasonal component represented by (p, d, q) and a seasonal component represented by (P, D, Q). The parameters p and P define the autoregressive elements of the model, while q and Q define the moving average elements. The values d and D represent the degree of differencing applied to remove trend and seasonality from the data. The value in the square brackets defines the seasonal period and is set to 12 to reflect the annual cycle present within monthly prescribing data."),
      
      p("Exploratory analysis of the historical prescribing data identified both long-term trend and annual seasonality. Investigation of trend charts, seasonal plots, lag plots and autocorrelation diagnostics indicated that first-order differencing and seasonal differencing were required to achieve stationarity. As a result, the differencing parameters were fixed at ", tags$i("d"), " = ", tags$i("1"), " and ", tags$i("d"), " = ", tags$i("1"), " for all models. The model selection process then focused on identifying the most appropriate autoregressive and moving average parameters for each NHS Board and measure being forecast."),
      
      p("To achieve this, an automated grid search is run for every board and measure. Non-seasonal parameters p and q are evaluated across values ranging from 0 to 4, while seasonal parameters P and Q are evaluated across values ranging from 0 to 1. These ranges were chosen to provide sufficient flexibility to capture short-term fluctuations and annual seasonal effects, while maintaining interpretability and minimising unnecessary computational complexity."),
      
      p("For example, if forecasts are being produced using data available up to June 2025, the model is first trained on historical observations before generating forecasts for a twelve-month validation period. Forecast values are then compared against the observed data and a percentage error is calculated for each parameter combination. The model producing the lowest validation error is selected as the best-fitting specification for that NHS Board and measure. This process is repeated automatically across all NHS Boards, ensuring forecasts are tailored to local prescribing patterns whilst maintaining a consistent methodology across Scotland."),
      
      p("Once the grid search has identified the best-fitting parameters, forecasts and confidence intervals are generated at board level before being aggregated to produce Scotland-level forecasts. This approach ensures that local variation is captured within the national forecast and allows the model to be updated on a quarterly basis without requiring manual parameter selection or intervention.")
  
      )
  
  )
)