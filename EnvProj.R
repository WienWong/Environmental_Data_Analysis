
library(ggplot2)                                             # Import 'ggplot2' library
library(ggmap)                                               # Import 'ggmap' library for Google Maps
library(grid)                                                # For function "textGrob"
library(gridExtra)                                           # Plot sub-images together

getwd()                                                      # Check the default working directory
datRaw = read.table("/PATH_TO_THE_TARGET_FILE/datNew.txt")   # Generate the log file path
options(digits=9)                                            # Keep digits precision
names(datRaw) <- c("Latitude","Longitude","NO2","SO2","Wind")# Rename the observation variables

source("./centercal.R")                                      # Invoke a user-defined function 'centercal.R'
center <- centercal(datRaw$Latitude, datRaw$Longitude)
lat=center[2]; lon=center[1]                                 # The beginning coor. pairs for wind arrow

zoomfactor=15                                                # This is the best zoomin factor for this driving route meausrement.
mp <- ggmap( get_map(location = center, zoom=zoomfactor,     # Use the Google 'satellite' map
            maptype="satellite") )

title <- "NO2 and SO2 concentration along the driving route"

mp + 
  geom_point(data=datRaw, aes(x=Longitude, y=Latitude), colour='red') + 
  ggtitle("Driving route")                                   # Driving route plot

windir = mean(datRaw$Wind)                                   # Mean wind direction in degree
latend = windArrow(windir)[1]; lonend = windArrow(windir)[2] # The ending coor. pairs for wind arrow                 

# NO2 concentration along the driving route with 'sizer' turned on
p1 <- mp + geom_point(data=datRaw, aes(x=Longitude, y=Latitude, colour=NO2, size=NO2)) + 
  scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red")) +
  geom_segment(data = datRaw, aes(y = lat, x = lon, yend = latend, xend = lonend), arrow = arrow() )

# SO2 concentration along the driving route with 'sizer' turned on
p2 <- mp + geom_point(data=datRaw, aes(x=Longitude, y=Latitude, colour=SO2, size=SO2)) + 
  scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red")) +
  geom_segment(data = datRaw, aes(y = lat, x = lon, yend = latend, xend = lonend), arrow = arrow() )

title1 = textGrob(title, gp=gpar(fontsize=18,fontface="bold"))
grid.arrange(p1,p2,top=title1,layout_matrix = matrix(c(1,2),
              ncol=2,byrow=TRUE))                             # Plot two sub-images together

