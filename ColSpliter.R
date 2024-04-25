# Function for spliting the offshoot data

colspliter <- function(Data, Col_Index, Split_Mark = ",", Split_Into_List = TRUE, Calculate_Mean = FALSE, Calculate_Num_of_Elements = FALSE, Calculate_Sum_of_Elements = FALSE){
  # Data is original dataframe
  # Col_index - index number of columns wit data to split
  # Split_Mark - how are different numbers separated
  # Split_Into_List - [BASIC] - change character into a list
  # Calculate_Mean - create new column with mean value of splited elements
  # Calculate_Num_of_Elements - create new column with number of elements
  # Calculate_Sum_of_Elements - create new column with sum of splited elements
  
  for(i in Col_Index){
    wynik           = rep(0,nrow(Data))
    Cal_Mean        = rep(0,nrow(Data))
    Cal_No_Elements = rep(0,nrow(Data))
    Cal_Sum         = rep(0,nrow(Data))
    
    for(ii in 1:nrow(Data)){
      wektor <- unlist(strsplit(as.character(Data[ii,i]), split = Split_Mark, fixed=T))
      wektor <- as.numeric(wektor)
      
      # basic split
      if(Split_Into_List == TRUE){
        wynik[ii] <- list(wektor)
      }
      
      # optional
      if(Calculate_Mean == TRUE){
        Cal_Mean[ii] <- mean(wektor)
      }
      
      if(Calculate_Num_of_Elements == TRUE){
        Cal_No_Elements[[ii]] <- length(wektor)
        if(length(wektor) == 1){
          Cal_No_Elements[[ii]]  = 0
          if(wektor>0){  Cal_No_Elements[[ii]]  = 1}
      }}
      
      if(Calculate_Sum_of_Elements == TRUE){
        Cal_Sum[ii] <- sum(wektor)
      }
      
    }
    
    
    # saving results
    if(Split_Into_List == TRUE){
      Data$New <- wynik
      names(Data)[names(Data) == 'New'] <- colnames(Data)[i]
    }
    
    if(Calculate_Mean == TRUE){
      Data$Cal_Mean <- Cal_Mean
      names(Data)[names(Data) == 'Calculate_Mean'] <- paste0("Mean_&_", colnames(Data)[i])
    }
    
    if(Calculate_Num_of_Elements == TRUE){
      Data$Cal_No_Elements <- Cal_No_Elements
      names(Data)[names(Data) == 'Cal_No_Elements'] <- paste0("No_Elements_&_", colnames(Data)[i])
    }
    
    if(Calculate_Sum_of_Elements == TRUE){
      Data$Cal_Sum <- Cal_Sum
      names(Data)[names(Data) == 'Cal_Sum'] <- paste0("Sum_&_", colnames(Data)[i])
    } 
      
  
  }
  # after all loops droping org cols
  Data <- Data[-c(Col_Index)]
  
  return(Data)
}
