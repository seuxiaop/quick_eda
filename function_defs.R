# df1 <- read.csv("application_test.csv")
# 
# x <- df1$FLAG_DOCUMENT_2
# x_name <- "EXT_SOURCE_2"

## function definitions are here
 
na_cnt <- function(x){
  return(sum(is.na(x)))
}

na_pct <- function(x){
  return(sum(is.na(x))/length(x))
}

outlier_cnt <- function(x){
  if(is.numeric(x)){
    x <- x[!is.na(x)]
    x_IQR <- quantile(x, 0.75) - quantile(x, 0.25)
    x_limit <- c(quantile(x, 0.25) - 1.5*x_IQR, quantile(x,0.75) + 1.5*x_IQR)
    return(length(x[x<x_limit[1] | x>x_limit[2]]))
  }
  return(NA)
} 

outlier_pct <- function(x){
  if(is.numeric(x)){
    n <- length(x)
    x <- x[!is.na(x)]
    x_IQR <- quantile(x, 0.75) - quantile(x, 0.25)
    x_limit <- c(quantile(x, 0.25) - 1.5*x_IQR, quantile(x,0.75) + 1.5*x_IQR)
    return(length(x[x<x_limit[1] | x>x_limit[2]])/n)
  }
  return(NA)
} 

data_entropy <- function(x){
  
  if(is.numeric(x)){
    nbr_na <- length(x[is.na(x)])
    x <- x[!is.na(x)]
    x_IQR <- quantile(x, 0.75) - quantile(x, 0.25)
    x_limit <- c(quantile(x, 0.25) - 1.5*x_IQR, quantile(x,0.75) + 1.5*x_IQR)
    x_plot <- x[x>=x_limit[1] & x<=x_limit[2]]
    
    if(length(unique(x)) >98){
      x_tab <- discretize(x, numBins = 98)
    }else{
      x_tab <- table(x)
    }
    x_tab <- c(x_tab, "L_outliers" = length(x[x<x_limit[1]]), "U_outliers" = length(x[x>x_limit[2]]), "NAs" = nbr_na)
    return(entropy(x_tab))
  }else{
    x <- as.character(x)
    x[is.na(x)] <- "N/A"
    return(entropy(table(x)))
  }
  
}


## estimate the information that the variable contains

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








