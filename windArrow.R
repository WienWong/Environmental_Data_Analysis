windArrow <- function(windir){
    
  windir = windir*pi/180           # changes into radians
  offx = 0.003*sin(windir + pi)
  offy = 0.003*cos(windir + pi)
  lat = center[2]; lon = center[1] # latend=center[2]+0.01; lonend=center[1]+0.01;
  latend = center[2] + offy      
  lonend = center[1] + offx
  
  return(c(latend,lonend))
}
