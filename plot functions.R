# # ## load test data ##
#  df1 <- read.csv("application_test.csv")
# # ## this script create a histgram for numerical variables 
# # # 
#   x <- df1$EXT_SOURCE_2
#   x_name <- "EXT_SOURCE_2"
  
my_freq_plot <- function(x, x_name, min_unique = 20, x_limit = NULL){
  
  if(is.numeric(x) & length(unique(x)) > min_unique ){
    
    ## remove NA
    x <- x[!is.na(x)]
    x_IQR <- quantile(x, 0.75) - quantile(x, 0.25)
    if(is.null(x_limit)){
      x_limit <- c(quantile(x, 0.25) - 1.5*x_IQR, quantile(x,0.75) + 1.5*x_IQR)
    }
    x_plot <- data.frame( v_names = x[x>=x_limit[1] & x<=x_limit[2]])
    colnames(x_plot) <- x_name
    x_avg <- mean(x_plot[,x_name])
    p <- ggplot(x_plot, aes(x=!!ensym(x_name))) + 
      geom_histogram(binwidth = x_IQR/10, color="black", fill="lightblue") +
      geom_density(aes(y=..count..* x_IQR/10)) +
      geom_vline(aes(xintercept=x_avg, color = "Avg. Line"),linetype="dashed", size=2) + 
      scale_color_manual(values =c( "black")) + 
      labs(color="") 
    return(p)
  }else{
    
    x <- as.character(x)
    x[is.na(x)] <- "N/A"
    x <- table(x)
    x_plot <- data.frame( v_names = names(x), y = as.numeric(x))
    colnames(x_plot) <- c(x_name,"count")
    x_plot <- head(x_plot[order(-x_plot$count), ], min_unique)
    x_plot[,x_name] <- factor(  x_plot[,x_name], levels =  x_plot[,x_name])
    
   p  <- ggplot(x_plot, aes(x=!!ensym(x_name) , y = count)) + 
         geom_bar(stat = "Identity",  color="black",fill="lightblue", width = 0.5)
    
   return(p)
  }
  
  
}