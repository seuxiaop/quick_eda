library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
    # App title ----
  titlePanel("Uploading Files"),
  
  fluidRow(
    column(2,
           fileInput("file1", "Choose CSV File",
                     multiple = TRUE,
                     #width = "50%",
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv"))
          ),
    column(2,
           radioButtons("sep", "Separator",
                        inline =TRUE,
                        choices = c(Comma = ",",
                                    Semicolon = ";",
                                    Tab = "\t"),
                        selected = ","),
           checkboxInput("header", "Include Header", TRUE) 
    )
  ),

  fluidRow(
    
    tabsetPanel( type = "tabs",
                 
                 tabPanel("View Data",
                          fluidRow(
                            
                            column(2,
                                   selectInput('si1', 'Select a Variable', choices = "No Data Available", selected = "No Data Available", multiple = T)
                            ),
                            
                            column(2, 
                                   radioButtons("disp", "Display",
                                                inline = TRUE,
                                                choices = c(Head = "head",
                                                            Random = "random"),
                                                selected = "head")
                                   ),
                            column(2, 
                                   sliderInput("nbr_of_rec", "Number of Records",
                                               1, 500, 10, step = 1)
                                   
                                   )
                          ),
                          DT::dataTableOutput("contents"),
                          verbatimTextOutput("summary1")
                          
                          ),
                 tabPanel("Data Summary"),
                 tabPanel("Table")
      
      
      
    )
    
    
  )
  
  # Sidebar layout with input and output definitions ----
  
)