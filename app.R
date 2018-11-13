library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Inspired Energy: Minimizing Late Days Optimization"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----

      downloadButton("downloadData", "Download Example Data"),
      
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(First_Five = "head",
                               All = "all"),
                   selected = "head")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      textOutput("header"),
      # Output: Data file ----
      tableOutput("contents")
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$header <- renderText({
    req(input$file1)
    
    print("Algorithm Results: Below is the table with a prioritized production schedule optimized by minimizing late days")
  })
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep)
        library(dplyr)
        current_date = Sys.Date()
        df$finish_shifts = round(df$time_to_make / 8)
        df$finish_date = as.Date(current_date) + df$finish_shifts 
        df$due_date_diff = difftime(df$finish_date,df$due_date)
        df$scaled_diff = scale(df$due_date_diff)
        df$rank = df$client_importance + df$scaled_diff
        df = df[order(df$rank, decreasing = TRUE),]
        df = df %>% 
          select(product, rank, due_date)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  output$downloadData <- downloadHandler(
    filename <- function() {
      paste("output", "csv", sep=".")
    },
    
    content <- function(file) {
      file.copy("example_data.csv", file)
    },
    contentType = "csv"
  )
  
}

shinyApp(ui = ui, server = server)
