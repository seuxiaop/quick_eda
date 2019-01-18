
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
options(shiny.maxRequestSize = 300*1024^2) # max file size 300 MB


source("plot functions.R")

shinyServer(function(input, output, session) {
   
  mydata <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                  header = input$header,
                  sep = input$sep)
    updateSelectInput(session, "si1", choices = c("All", colnames(df)), selected = "All")
    updateSelectInput(session, "si2", choices = colnames(df), selected = colnames(df)[1])
    return(df)
  })
  

  

   output$contents <- DT::renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)
    df <- mydata()
    checker <- ifelse(length(input$si1) > 0, input$si1[1] != "No Data Available", F)

    if(checker){
      
      
      
      
      
      #df <- mydata()
      if(input$disp == "head") {
        
        if("All" %in% input$si1){
          return(head(df, input$nbr_of_rec))
        }else{
         
         x <- as.data.frame(df[,input$si1])
         colnames(x) <- input$si1
         return(head(x, input$nbr_of_rec))
        }
        
      }
      else {
        if("All" %in% input$si1 ){
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
      
    
     
     if(is.null(input$file1)){
       cat("No Data Avaiable")
     }else{
       checker <- ifelse(length(input$si1) > 0, input$si1[1] != "No Data Available", F)
       if(checker){
         df <- mydata()
         if("All" %in% input$si1){
           print(summary(df))
         }else{
           
           x <- as.data.frame(df[,input$si1])
           colnames(x) <- input$si1
           print(summary(x))
         }

       }
       
     }

  })
  
  output$placeholder <- renderPrint({
    
    if(is.null(input$file1)){
      cat("No Data Avaiable")
    }
  })

  output$freqplot <- renderPlot({
    
    req(input$file1)
    df1 <- mydata()
    checker <- ifelse(length(input$si2) > 0, input$si2 != "No Data Available", F)
    if(checker){
      
      x <- df1[, input$si2]
      x_name <- input$si2
      
      
      my_freq_plot(x, x_name)

    }

  })

  output$summary2 <- renderPrint({
    
    
    
    if(is.null(input$file1)){
      cat("No Data Avaiable")
    }else{
      req(input$file1)
      df1 <- mydata()
      checker <- ifelse(length(input$si2) > 0, input$si2 != "No Data Available", F)
      if(checker){
        
        x <- df1[, input$si2]
        x_name <- input$si2
        n_x <- length(x)
        
        cat("Number of Observations: ", length(x),"\n")
        cat("Number/Pct. of NA Observations: ", length(x[is.na(x)]), " / ", round(length(x[is.na(x)])/length(x)*100, 1), "%\n" )
        
        if(is.numeric(x)){
          x <- x[!is.na(x)]
          x_IQR <- quantile(x, 0.75) - quantile(x, 0.25)
          cat("Interquartile Range: ",x_IQR, "\n")
          cat("IQR Lower Limit: ", quantile(x, 0.25) - 1.5*x_IQR , "\n")
          cat("IQR Upper Limit: ", quantile(x, 0.75) + 1.5*x_IQR , "\n")
          x_outliers <- x[x > quantile(x, 0.75) + 1.5*x_IQR | x < quantile(x, 0.25) - 1.5*x_IQR ]
          cat("Number/Pct. of outliers: ", length(x_outliers)," / ", round(length(x_outliers)/n_x * 100, 1), "%\n")
          
          
          
        }
        
        
      }
      
    }
    
  })  
  
  
})
