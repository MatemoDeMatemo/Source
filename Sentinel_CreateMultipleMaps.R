########### Step 2 # Create Maps  ###################################################################################################

# 18.11.2023
# Select a folder with unziped data from Sentinel2. Create multispectral maps for 10m resolution. Specify if you want to overwrite the older maps

# # Input
# y     <- "D:/Mapy_Sentinel"
# crop  <-  ""
# #crop  <- c(720000, 753847, 4397464, 4414508) # Example of crooping coords
# OverWrite <- FALSE
# 
# Sentinel_CreateMultipleMaps(y = y, OverWrite = TRUE)


Sentinel_CreateMultipleMaps <- function(y, crop = "", OverWrite = FALSE) {
  
  OldWd <- getwd()
  library(terra)
  # Find folders with a raw Sentinel data
  
  Sentinel_Folders <- list.files(y, full.names = TRUE)             # pick all files
  Sentinel_Folders <- Sentinel_Folders[grepl(".SAFE", Sentinel_Folders)]          # select those with .zip
  
  # for each folder create a map 
  
  for (i in Sentinel_Folders) {
    
    Sentinel_Path <- "D:/Mapy_Sentinel/2018_05_06.SAFE/GRANULE/L2A_T33UXU_A014991_20180506T100214/IMG_DATA/R10m"
    Sentinel_Path <- paste0(i, "/GRANULE/")
    Sentinel_Path <- paste0(Sentinel_Path, list.files(Sentinel_Path), "/IMG_DATA/R10m")
    list.files(Sentinel_Path)
    
    # Extracting the date from i
    Sentinel_Date_Start <- unlist(gregexpr("/20", i))[1]
    Sentinel_Date_End <- unlist(gregexpr(".SAFE", i))[1]
    Sentinel_Date <- substring(i, first= Sentinel_Date_Start + 1, last= Sentinel_Date_End -1 )
    
    ## Map Creation
    setwd(Sentinel_Path)
    images10m <- list.files(Sentinel_Path)
    images10m <- images10m[grepl("B02|B03|B04|B08|AOT|WVP",images10m)] # lacze warstwy RGB i near infrared
    senstack_10 <- rast(images10m)
    
    print(paste0("Creating Map for a date:   ", Sentinel_Date, "      CRS is:   ", substring(crs(senstack_10), first = 1, last = 40)))
    
    # Croping if needed
    
    if (length(crop) > 1) { senstack_10 <-  crop(senstack_10, crop) }
    
    # Better band names
    sen_ms <-  senstack_10
    names(sen_ms) <- substr(names(sen_ms),
                            nchar(names(sen_ms))-6, # from the 6th-last position... Bierzemy dane pomiedzy ostatnia 6 i 4 litera
                            nchar(names(sen_ms))-4)
    
    # Save Raster 
    
    writeRaster(sen_ms,
                #paste0(imagepath,".tif"),
                paste0(y, "/", Sentinel_Date,".tif"),
                overwrite=OverWrite)
    print(paste0("Raster saved:  ", Sentinel_Date))
  }
  setwd <- OldWd
  
}
