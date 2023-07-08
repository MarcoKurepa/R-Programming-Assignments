complete <- function(directory, id=1:332){
  df <- data.frame(id = numeric(),nobs = numeric(),stringsAsFactors = FALSE)
  
  for(x in id){
    
    csv <- read.csv(paste(directory,"/", sprintf("%03d", x),".csv", sep=""))
    
    id_value <- x
    nobs_value <- sum(!is.na(csv["sulfate"]) & !is.na(csv["nitrate"]))
    
    new_row <- data.frame(id = id_value, nobs = nobs_value)
    
    df <- rbind(df, new_row)
  }
  
  df
}
