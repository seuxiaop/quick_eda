
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
options(shiny.maxRequestSize = 300*1024^2) # max file size 300 MB

shinyServer(function(input, output, session) {
   
  mydata <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                  header = input$header,
                  sep = input$sep)
    updateSelectInput(session, "si1", choices = c("All", colnames(df)), selected = "All")
    return(df)
  })
  
  
  

   output$contents <- DT::renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    df <- mydata()
    if(input$si1 != "No Data Available"){
      if(input$disp == "head") {
        
        if(input$si1 == "All" ){
          return(head(df, input$nbr_of_rec))
        }else{
         
         x <- as.data.frame(df[,input$si1])
         colnames(x) <- input$si1
         return(head(x, input$nbr_of_rec))
        }
        
      }
      else {
        if(input$si1 == "All" ){
          return( df[sample(nrow(df), size = input$nbr_of_rec ),]   )
        }else{
          x <- as.data.frame(df[sample(nrow(df), size = input$nbr_of_rec),input$si1])
          colnames(x) <- input$si1
          return(x )
        }
      }
    }
    
    
  })
 
  output$summary1 <- renderPrint({
      
      df <- mydata()
      x <- print(summary(df))  
    
  })
  


})
