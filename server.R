library(shiny)
library(dplyr)
library(ggplot2)
library(aws.s3)

# Set your AWS credentials (replace with your own values)
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAYDHJABOVWCTRSW5V",
           "AWS_SECRET_ACCESS_KEY" = "W3wLGutQ+LLjGoITNRgs1OkId07M4vSlMUoMDP9q",
           "AWS_DEFAULT_REGION" = "us-east-2")

# Define server logic
server <- function(input, output) {
  
  aws.s3::get_bucket("ddsproject1")
  
  # read and write from ojbect
  #Read in CaseStudy2-Data.csv
  CaseStudy2 = s3read_using(FUN = read.csv,
                            bucket = "ddsproject1",
                            object = "CaseStudy2-data.csv")

 
  # Filtered data based on years from UI
  filtered_data <- reactive({
    CaseStudy2 %>%
      filter(TotalWorkingYears >= input$years[1] & TotalWorkingYears <= input$years[2])
  })
  
  print (head(filtered_data))
  # Scatter plot
  output$scatter_plot <- renderPlot({
    ggplot(data = filtered_data(), aes(x = Age, y = MonthlyIncome, color = Attrition)) +
      geom_point() +
      labs(title = "Scatter Plot of MonthlyIncome vs. Age (Filtered)")
  })
  
  # Summary table
  # Summary table
  output$summary_table <- renderTable({
    filtered_data1 <- filtered_data() %>%
      filter(Attrition %in% c("Yes", "No"),
             Age >= 25 & Age <= 40)
    summary_data <- filtered_data1 %>%
      group_by(Attrition) %>%
      summarise(Count = n(),
                Average_Age = mean(Age),
                Average_Income = mean(MonthlyIncome))
    summary_data
  })
  
}

