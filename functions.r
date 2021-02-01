# this function shows a plot of all the missing (NA) data in a dataframe: ----------

library(ggplot2)
library(reshape2)

plot_missings<-function(x){
  
  x%>%
    is.na%>%
    melt%>%
    ggplot(data = .,
           aes(x = Var2, 
               y = Var1))+
    geom_raster(aes(fill = value))+
    scale_fill_grey(name = "",
                    labels = c("Present", "Missing"))+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
    labs(x = "Variables in Dataset", y = "Cases")
}


# summary of missing data by column percentages: -----------------------------------

missPerc<-function(df){
	colSums((is.na(df)/nrow(df))*100)
	}

# NA columns:	----------------------------------------------------------------------

# na_columns() takes a dataframe (with IDs as the first column), and creates duplicate variables ending in _NA for each variable.

# na_columns_repl() creates duplicate variables ending in _NA, as na_columns() does, but has an extra argument
# which takes a vector of values representing missing data codes, and removes these from the original columns
# but keeps them in the new _NA columns.
# The result is a data frame with a set of columns with true measured values (so can call e.g. mean, sd, etc on them)
# and a set of columns which has the missing data codes (so can tabulate/analyze these if necessary).
# Missing codes will not be inadvertently included as measured values and/or won't have to be explicitly filtered out every time.

library(tidyverse)

na_columns<-function(data){
  
  for (indexCol in 2:ncol(data)){
    
    columnName<-colnames(data)[indexCol]
    varname<-paste0(columnName, "_NA")
    
    data<-mutate(data, !!varname := data[[indexCol]])
    
  }
  
  data
  
}


na_columns_repl<-function(data, values){
  
  for (indexCol in 2:ncol(data)){
    
    columnName<-colnames(data)[indexCol]
    varname<-paste0(columnName, "_NA")
    
    data<-mutate(data, !!varname := data[[indexCol]])
    
  }
  
  for (indexCol in 1:ncol(data)){
    
    for (indexRow in 1:nrow(data)){
      
      columnName<-colnames(data)[indexCol]
      dataValue<-data[indexRow,indexCol] 
      
      if (!grepl(".+_NA", columnName)){
        
        if ((dataValue %in% values)){
          
          data[indexRow,indexCol]<-NA
          
        }
        
      }
      
    }
    
  }
  
  for (indexCol in 1:ncol(data)){
    
    for (indexRow in 1:nrow(data)){
      
      columnName<-colnames(data)[indexCol]
      dataValue<-data[indexRow,indexCol] 
      
      if (grepl(".+_NA", columnName)){
        
        if (!(dataValue %in% values)){
          
          data[indexRow,indexCol]<-NA
          
        }
        
      }
      
    }
    
  }  
  
  data
  
} 

# example (888 and 999 are codes for missing data - 888 = did not finish, 999 = not provided):

df <- data.frame(id_no = c(1,2,3,4,5,6,7,8,9,10),
                 age = c(45,32,44,23,47,999,45,999,50,38),
                 score = c(23,24,26,30,888,999,999,999,30,28),
                 score_2 = c(24,24,888,30,22,20,999,999,30,999))

na_columns(df)

na_columns_repl(df, c(888,999))
na_columns_repl(df, c(23))