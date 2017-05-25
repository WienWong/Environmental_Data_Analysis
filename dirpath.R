dirpath <- function(month,day,SW) {

  # Om Mani Padme Hum !
  # This function will generate a directory path for the concentration measurement.
  # 2017-03-14, 1st built, 2017-05-01, 2nd modified, by Wang Weihua
  # SW stands for 'switch', thus the concentration folder ("conc") or GPS folder ("gps") can be selected.
  
  if (SW=="conc") {
    if (1<=day & day<=9){
      tmpath <- paste('/PATH_TO_THE_FOLDER/170', toString(month), '0', toString(day), '/', sep="")
    } else if (10<=day & day<=31){
      tmpath <- paste('/PATH_TO_THE_FOLDER/170', toString(month), toString(day), '/', sep="")
    }
    return(tmpath)
  } else if (SW=="gps") {
    if (1<=day & day<=9){
      tmpath <- paste( "/PATH_TO_THE_FOLDER/170", toString(month), '0', toString(day), "/", sep="")
    } else if (10<=day & day<=31){
      tmpath <- paste( "/PATH_TO_THE_FOLDER/170", toString(month), toString(day), "/", sep="")
    }
    return(tmpath) 
  }
  
}
