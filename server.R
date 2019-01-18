
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(entropy)
library(dplyr)
library(shinycssloaders)

options(shiny.maxRequestSize = 300*1024^2) # max file size 300 MB
options(spinner.color.background="#F5F5F5") # set spinner backgroud

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
  
  mydata_statistic <- reactive({
    req(input$file1)
    df <- mydata()
    na_cnt_df <- df1 %>% summarise_all(na_cnt )
    na_pct_df <- df1 %>% summarise_all(na_pct )
    outlier_cnt_df <- df1 %>% summarise_all(outlier_cnt )
    outlier_pct_df <- df1 %>% summarise_all(outlier_pct )
    data_entropy_df <- df1 %>% summarise_all(data_entropy )
    
    summary_df <- data.frame(var_name = colnames(df1),
                             na_cnt = as.numeric(na_cnt_df),
                             na_pct = as.numeric(na_pct_df),
                             outlier_cnt = as.numeric(outlier_cnt_df), 
                             outlier_pct = as.numeric(outlier_pct_df),
                             data_entropy = as.numeric(data_entropy_df)
    )
    
    colnames(summary_df) <- c("Variable Names",
                              "Nbr. of NAs",
                              "Pct. of NAs",
                              "Nbr. of Outliers",
                              "Pct. of Outliers",
                              "Entropy"
    )
    
    return(summary_df)
    
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
   
   output$contents1 <- DT::renderDataTable({
     
     # input$file1 will be NULL initially. After the user selects
     # and uploads a file, head of that data file by default,
     # or all rows if selected, will be shown.
     
     req(input$file1)
     summary_df <- mydata_statistic()
     
     
    # display with format
     
    datatable(summary_df) %>% 
    formatPercentage(c("Pct. of NAs", "Pct. of Outliers"),2) %>%
    formatRound(c("Entropy"),2)
    
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
  
  output$placeholder1 <- renderPrint({
    
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
      
      
      my_freq_plot(x, x_name,n_bins = input$nbr_of_bin)

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
          cat("Number/Pct. of total outliers: ", length(x_outliers)," / ", round(length(x_outliers)/n_x * 100, 1), "%\n")
          x_outliers_ub <- x[x > quantile(x, 0.75) + 1.5*x_IQR ]
          cat("Number/Pct. of UB outliers: ", length(x_outliers_ub)," / ", round(length(x_outliers_ub)/n_x * 100, 1), "%\n")
          x_outliers_lb <- x[x < quantile(x, 0.25) - 1.5*x_IQR ]
          cat("Number/Pct. of LB outliers: ", length(x_outliers_lb)," / ", round(length(x_outliers_lb)/n_x * 100, 1), "%\n")
          
        }
        
        
      }
      
    }
    
  })  
  
  
})
