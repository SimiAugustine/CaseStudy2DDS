
library(shiny)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar panel for inputs ----
    sidebarLayout(
      sidebarPanel(
        sliderInput("years", "Select Total working years",
                    min = 0, max = 40, value = c(1, 10))
      ),
    
      mainPanel(
        plotOutput("scatter_plot"),
        tableOutput("summary_table")
      )
    )
  )
  