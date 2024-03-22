# Function for spliting offshoot data

colspliter <- function(Data, Col_Index, Split_Mark = ",", Calculate_Mean = FALSE, Calculate_Num_of_Elements = FALSE){
  # Data is original dataframe
  # Col_index - index number of columns wit data to split
  # Split_Mark - how are different numbers separated
  # Calculate_Mean - for elements instead of just spliting data
  # Calculate_Num_of_Elements - create new column with number of elements
  
  for(i in Col_Index){
    wynik = rep(0,nrow(Data))
    Cal_No_Elements = rep(0,nrow(Data))
    Cal_Mean = rep(0,nrow(Data))
    
    for(ii in 1:nrow(Data)){
      wektor <- unlist(strsplit(as.character(Data[ii,i]), split = Split_Mark, fixed=T))
      wektor <- as.numeric(wektor)
      Cal_No_Elements[[ii]] <- length(wektor)
      if(length(wektor) == 1){
        if(is.na(wektor)){  Cal_No_Elements[[ii]]  = 0}
      }
      
      
      if(Calculate_Mean == FALSE){
        wynik[ii] <- list(wektor) # classic pathway
        
      } else{
        Cal_Mean <- mean(wektor)
        wynik[ii] <- Cal_Mean
      }
      
    }
    
    Data$New <- wynik
    names(Data)[names(Data) == 'New'] <- colnames(Data)[i]
    
    if(Calculate_Num_of_Elements == TRUE){
      Data$N_Elements <- Cal_No_Elements
      names(Data)[names(Data) == 'N_Elements'] <- paste0("Num_of_elements_&_", colnames(Data)[i])
    }
    
  }
  
  Data <- Data[-c(Col_Index)]
  
  return(Data)
}