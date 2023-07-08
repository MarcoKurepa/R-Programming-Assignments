pollutantmean <- function(directory, pollutant, id = 1:332){
  pollutantmean<-0
  pollutantlength<-0
  
  for(x in id){
    
    csv <- read.csv(paste(directory,"/", sprintf("%03d", x),".csv", sep=""))
    
    pollutantvect <- csv[pollutant]
    pollutantvect <- pollutantvect[!is.na(pollutantvect)]
    
    pollutantmean<-pollutantmean+sum(pollutantvect)
    
    pollutantlength<-pollutantlength+length(pollutantvect)
  }
  
  pollutantmean<-pollutantmean/pollutantlength
  pollutantmean
}
