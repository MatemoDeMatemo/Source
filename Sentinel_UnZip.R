########### Step 1 # Unzip and rename Sentinel filies ###################################################################################################

# 18.11.2023
# Select a folder with freshly downloaded ziped maps from Sentinel2. Select a destination folder for unzpied folders. Add a label if necessary.

# # Input
# x   <- "C:/Users/mateu/Desktop/Test_Funkcji"        # path to a folder with zip files
# y   <- "D:/Mapy_Sentinel"                           # path to the folder where we will store the created maps
# z   <- "_PL"                                          # Add notation to the file: for example "PL" if map is for Poland
# 
# Sentinel_UnZip(x = x, y = y, z = "_PL")


Sentinel_UnZip <- function(x, y, z = "") {
  
  #### List Files and pick those with zip extension
  
  Zip_Files <- list.files(x, full.names = TRUE)             # pick all files
  Zip_Files <- Zip_Files[grepl(".zip", Zip_Files)]          # select those with .zip
  
  
  #### For each file, unzip it, rename it in new folder y
  
  outDir  <- y
  OldWd <- getwd()
  setwd(y)
  
  for (i in Zip_Files) {
    
    # unziping
    
    print(i)
    zipFile <- i
    unzip(zipFile, exdir = outDir)
    
    # Get the date
    
    Date_Start <- unlist(gregexpr("_20", zipFile))[1] # wszystkie obserwacje z sentinela to rok 20%% wiec tak wyszukuje
    Date_Raw <- substring(zipFile, first=Date_Start + 1, last= Date_Start + 8)
    
    Date_Name <- rep("_", times = 10)
    Date_Name[c(1:4, 6:7, 9:10)] <- unlist(strsplit(Date_Raw, ""))
    Date_Name <- paste(Date_Name, collapse = "")
    
    # Change the name
    
    file.rename(list.files(pattern= Date_Raw), paste0(Date_Name, z,".SAFE"))
    print(paste0(Date_Name, z, "_DONE"))
  }
  setwd <- OldWd
}