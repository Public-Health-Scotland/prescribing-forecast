library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Export Data Example"),
  sidebarLayout(
    sidebarPanel(
      downloadButton("downloadData", "Download Data")
    ),
    mainPanel(
      tableOutput("dataTable")
    )
  )
)

# Define Server
server <- function(input, output) {
  # Sample data
  data <- mtcars
  
  # Render table
  output$dataTable <- renderTable({
    data
  })
  
  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data, file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
