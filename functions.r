# this function shows a plot of all the missing (NA) data in a dataframe: ####

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


# summary of missing data by column percentages: ####

missPerc<-function(df){
	colSums((is.na(df)/nrow(df))*100)
	}
	
# the three functions below combined take a dataframe (with IDs as the first column), and create duplicate variables ending in _NA for each variable. (na_columns function)
# once this has been done, the dataframe and a vector of values corresponding to missing data (e.g. 99, NA, etc.) are passed as arguments to the na_values function.
# this function loops over each column ending in _NA, keeps the missing values, and changes everything else to NA. 
# once this has been done, the same dataframe and vector of values are passed to the non_na_values function. 
# this function loops over each column that doesn't end in _NA, keeps all the real values and changes all the missing values to NA. 
# the purpose of these 3 functions is to facilitate data input into Nesstar, which won't accept categorical values along with interval data in the same variable, but also to assist with analysis.
# now, analysis can be carried out on the original data without having to remove or overlook missing codes, or risk errors resulting from inadvertently including missing values in analysis as real values. 
# if for any reason the missing values need to be analysed for any reason, they are in their own separate variables and can be easily selected and analysed without having to deal with real values. 


# function to create extra columns:

na_columns<-function(data, ...){
  
  for (indexCol in 2:ncol(data)){
    
    columnName<-colnames(data)[indexCol]
    varname<-paste0(columnName, "_NA")
    
    data<-mutate(data, !!varname := data[[indexCol]])
    
  }
  
  return(data)
  
} 

# function to amend _NA column values:

na_values<-function(data, values, ...){
  
  for (indexCol in 1:ncol(data)){
    
    for (indexRow in 1:nrow(data)){
      
      columnName<-colnames(data)[indexCol]
      dataValue<-data[indexRow,indexCol] 
      
      if (!grepl(".+_NA", columnName)){
        
        next
        
      } else {
        
        if(!(dataValue %in% values)){
          data[indexRow,indexCol]<-NA
        }
        
      }
      
    }
    
  }
  
  return(data)
}


# function to amend non-_NA column values:

non_na_values<-function(data, values, ...){
  
  for (indexCol in 1:ncol(data)){
    
    for (indexRow in 1:nrow(data)){
      
      columnName<-colnames(data)[indexCol]
      dataValue<-data[indexRow,indexCol] 
      
      if (grepl(".+_NA", columnName)){
        
        next
        
      } else {
        
        if((dataValue %in% values)){
          data[indexRow,indexCol]<-NA
        }
        
      }
      
    }
    
  }
  
  return(data)
}
