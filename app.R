library(shiny)
library(readxl)
library(janitor)
# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Inspired Energy: Minimizing Late Days Optimization"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose Excel File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".xlsx")),
      
      # Horizontal line ----

      downloadButton("downloadData", "Download Example Data"),
      
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      sliderInput("weight_size", "Weight of Item Size:",
                  min = 0, max = 1,
                  value = .5),
      
      
      
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
      # Output: Data file ----
      dataTableOutput("contents")
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$header <- renderText({
    req(input$file1)
    
    print("Algorithm Results: Below is the table with a prioritized production schedule optimized by minimizing late days")
  })
  output$contents <- renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read_xlsx(input$file1$datapath,sheet = "Raw Report with Extra Columns")
        library(dplyr)
        time_estimates = read.csv("Time_Estimates.csv", header = TRUE)
        
        current_date = as.Date("2018-11-24")
        
        #determine what type of product this is
        df$Item_time =  toupper(substr(df$`Item ID`, 2, 2))
        
        #merge with estimated times
        
        estimated_times = read.csv("Time_Estimates.csv")
        df = merge(df, estimated_times, by.x = "Item_time", by.y = "Letter")
        
        #estimate date completed
        new_df = df %>% 
          clean_names() %>% 
          mutate(estimated_shifts = ((as.numeric(time_avg) * as.numeric(qty_remaining) + 624.5) /60)/8, 
                 estimated_days = estimated_shifts / 2,
                 date = current_date,
                 estimated_finish_date = as.Date(current_date) + estimated_days,
                 due_date_diff = difftime(estimated_finish_date, ship_by))
                 
        new_df = new_df[order(new_df$due_date_diff, decreasing = TRUE),]
          
        new_df = new_df %>% 
          select(line_description, qty_remaining, ship_by, estimated_finish_date, estimated_shifts, estimated_days, date)
          
        
        #calculate date difference
        
        # df$finish_shifts = round(df$time_to_make / 8)
        # df$finish_date = as.Date(current_date) + df$finish_shifts 
        # df$due_date_diff = difftime(df$finish_date,df$due_date)
        # df$scaled_diff = scale(df$due_date_diff)
        # df$rank = df$client_importance + df$scaled_diff
        # df = df[order(df$rank, decreasing = TRUE),]
        # df = df %>% 
        #   select(product, rank, due_date)
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      head(new_df)
    }
    else {
      new_df
    }
    
  })
  
  output$downloadData <- downloadHandler(
    filename <- function() {
      paste("output", "xlsx", sep=".")
    },
    
    content <- function(file) {
      file.copy("Example_data.xlsx", file)
    },
    contentType = "xlsx"
  )
  
}

shinyApp(ui = ui, server = server)
