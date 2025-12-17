####################### Results Comparison Page #######################


tagList(
  
  ## Heading and introductory text
  fluidRow(
    h1("PHS Pharmacy Forecasting Model"),
    
    p("Welcome to PHS' very own pharmacy forecasting model!"),
    p("This new and improved output was developed following a number of short life working group meetings, which were used to gather feedback on the previous version of the output on things like model accuracy and stakeholder suitability, and to then gather stakeholder requirements to ensure the work is tailored to those who require its information."),
    p("The decision was taken to use R to build the new version of the forecasting model, as this would provide the tools to gain much more control and flexibility in what can be applied to the model compared to Tableau."),
    p("Predictions seen in this dashboard have been generated using a SARIMA (Seasonal Auto-Regressive Moving Average) model. Further information can be found by navigating to the 'Model details' tab across the top of the page."),
    p("Please refer to the 'Feedback' tab and fill in the embedded Microsoft Form if you run into any problems, if you have any further questions around how to navigate the dashboard or if you have any useful feedback and/or suggestions that could be incorporated here."),
    tags$p(tags$i("This piece of work has been devised, created, developed and iteratively refined to inform an ongoing disseration project as Glasgow Caledonian University. Your involvement in this piece of work as an end-user is greatly appreciated and will go a long way to making this project a success -", tags$b("thank you!")))
  )
  
)
