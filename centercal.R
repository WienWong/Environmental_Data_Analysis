centercal <- function(latv,lonv){
  # Om Mani Padme Hum !
  # Map center calculation based on longitude and latitude vector.
  # 2017-02-02, 1st built, Wang Weihua
  
  center <- c( ( max(lonv) + min(lonv) )/2, ( max(latv) + min(latv) )/2 )  # estimate the google map center
  
  return(center)
}
