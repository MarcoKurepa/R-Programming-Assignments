corr <- function(directory, threshold = 0) {
  files <- list.files(directory, full.names = TRUE)
  
  correlations <- numeric()
  
  for (file in files) {
    
    data <- read.csv(file)
    
    if (sum(complete.cases(data)) > threshold) {
      
      sulfate <- data$sulfate
      nitrate <- data$nitrate
      
      if (sum(complete.cases(data.frame(sulfate, nitrate))) > 0) {
        
        correlation <- cor(sulfate, nitrate, use = "pairwise.complete.obs")
        
        correlations <- c(correlations, correlation)
      }
    }
  }
  
  correlations
}


