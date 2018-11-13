library(shiny)
ui <- fluidPage(
  
  # App title ----
  titlePanel("Multiplicative Index Creation by Customer"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Simple integer interval ----
      sliderInput("loud1", "Loudness of Yelling Customer #1:",
                  min = 0, max = 1,
                  value = 0.5, step = 0.1),
      
      # Input: Decimal interval with step value ----
      sliderInput("care1", "How Much We Care about Customer #1:",
                  min = 0, max = 1,
                  value = 0.5, step = 0.1),
      
      
      sliderInput("loud2", "Loudness of Yelling Customer #2:",
                  min = 0, max = 1,
                  value = 0.5, step = 0.1),
      
      sliderInput("care2", "How Much We Care about Customer #2:",
                  min = 0, max = 1,
                  value = 0.5, step = 0.1)
      
      
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table summarizing the values entered ----
      tableOutput("values")
      
    )
  )
)

server <- function(input, output) {
  
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
    x = data.frame(
      Name = c("Loudness of Yelling",
               "How Much We Care"
      ),
      Value = c(input$loud1,
                input$care1),
      stringsAsFactors = FALSE)
    x$Value = x$Value * 5
    Total_Index = sum(x$Value)
    as.data.frame(Total_Index)
    
    
    
  })
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)


