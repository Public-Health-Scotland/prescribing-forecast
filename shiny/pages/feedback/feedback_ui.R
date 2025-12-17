####################### Embedded Feedback Form #######################


tagList(

  fluidRow(
    h1("Feedback form"),
    p("Please use the Microsoft Form embedded below to ask questions or give feedback about the application and its content to the developer."),
    p("If you face any issues with the form, please email ",
      tags$a(href = "mailto:liam.rooney@phs.scot", "liam.rooney@phs.scot.")),
    p(linebreaks(1))
  ),
  
  fluidRow(
    # Embed Microsoft Form via iframe
    tags$iframe(
      src = "https://forms.office.com/e/bjUKrazLh0?embed=true",
      width = "100%",
      height = "800",
      frameborder = "0",
      marginheight = "0",
      marginwidth = "0"
    )
  )
  
  )