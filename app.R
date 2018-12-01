library(shiny)
library(readxl)
library(janitor)
library(dplyr)
library(ggplot2)
# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Inspired Energy: Minimizing Late Days Optimization and Clustering"),
  
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
      checkboxInput("clustered", "Clustered Output", FALSE),
      
      sliderInput("weight_qty", "Weight of Order Size:",
                  min = 0, max = 1,
                  value = .5),
      sliderInput("weight_value", "Weight of Order Revenue:",
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
                 due_date_diff = scale(difftime(estimated_finish_date, ship_by)),
                 qty_scaled = scale(qty_ordered),
                 value_scaled = scale(value),
                 rank = (qty_scaled * input$weight_qty) + (value_scaled * input$weight_value) + due_date_diff)
        #weight on quantity ordered and weight
        
        new_df = new_df[order(new_df$rank, decreasing = TRUE),]
        
        new_df$due_date_diff = new_df$due_date_diff[,1]
        new_df$qty_scaled = new_df$qty_scaled[,1]
        new_df$value_scaled = new_df$value_scaled[,1]
        new_df$rank =  new_df$rank[,1]
      
        
        clustered =new_df %>% 
          mutate(gr = cut_number(rank, n = 20)) %>% 
          group_by(gr, core_pack, actual_ship_week) %>% 
          summarise(clustering = paste0(line_description, collapse = ","),
                    num_products = n()) %>% 
          ungroup() %>% 
          select(-gr)
                   
        new_df = new_df %>% 
          select(line_description, qty_remaining, ship_by, estimated_shifts, date, rank)                                                      
          
        
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head" & input$clustered == "FALSE") {
      head(new_df)
    }
    else if(input$disp == "head" & input$clustered == "TRUE"){
      head(clustered)
    }
    else if(input$disp == "all" & input$clustered == "TRUE"){
      clustered
    }
    else{
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
