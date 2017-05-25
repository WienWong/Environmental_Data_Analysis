# Om Mani Padme Hum !
# Acquire GPS coordinates and time information during the city measurement on May kth 2017
# 2017-03-12, 1st built, Weihua Wang
# mon and dy are the specific month and day, respectively. 

acquireCoor <- function(mon,dy) {
  library(stringr)       # Import 'stringr' library
  source("./dirpath.R")  # Invoke a user-defined R function 

  dirpath <- dirpath(mon,dy,"gps")
  filepath <- paste(dirpath, "GPSInfo.txt", sep = "") # Generate the GPS log file path
  options(digits=9)      # Keep digits precision
  
  datRaw = read.table(filepath)
  posLat <- datRaw[,3]   # extract the latitude
  posLon <- datRaw[,4]   # extract the longitude
  daytime <- datRaw[,9]  # extract the day time  
  
  # Read GPS log.txt file
  # A priori coordinate of the city is approximately at c(36.6512 Lat,117.1201 Lon) thus outliers are outside of a small range around this coordinates.
  # Why outliers? Because malfunction of the GPS tracker at certain time can record abnormal coordinate pairs! 
  LAT=36.6512; LON=117.1201; offset=3
  if ( min(posLon)<LON-offset | max(posLon)>LON+offset | min(posLat)<LAT-offset | max(posLat)>LAT+offset ){
    print("Outliers exist.")
    idx = which( posLon<LON-offset | posLon>LON+offset | posLat<LAT-offset | posLat>LAT+offset )
    
    for (tt in idx) {
      print( paste("Outlier at row: ", as.character(tt), sep = " " ) ) 
      }
      
    coordinate <- matrix(data=NA, nrow=length(posLon), ncol=3) # create a NA matrix, then create a data frame
      coordinate[, 1] <- posLon   # longitude
      coordinate[, 2] <- posLat   # latitude
      coordinate[, 3] <- daytime  # day time
      coor <- data.frame(coordinate)  # need data frame not matrix format
      names(coor) <- c('longitude','latitude','time') # add names 
      
      remove=toString(idx)
      remove=str_split(remove,", ")[[1]] # str_split(remove,", ") gives a list, but we have to access the correct list element by [[1]]
      # remove=unlist(str_split(remove,", "))  # this also works fine by using 'stringr' package.
      
      coor <- coor[ !(rownames(coor) %in% remove), ]  # remove outliers   
    } else{
      print("No outliers.")
      # If no outliers, then extract the position coordinates and time info directly.
      coordinate <- matrix(data=NA, nrow=length(posLon), ncol=3)
      coordinate[, 1] <- posLon   # longitude
      coordinate[, 2] <- posLat   # latitude
      coordinate[, 3] <- daytime  # day time
      coor <- data.frame(coordinate)  # need data frame not matrix format
      names(coor) <- c('longitude','latitude','time') # add names 
    }

  return(coor)
}

## Test code
# coor0508 <- acquireCoor(5,8) 
# coor0514 <- acquireCoor(5,14) 
