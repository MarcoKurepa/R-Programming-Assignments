rankall <- function(outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!outcome %in% valid_outcomes) {
    stop("invalid outcome")
  }
  
  outcome_column <- switch(
    outcome,
    "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
    "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )
  
  data[[outcome_column]] <- suppressWarnings(as.numeric(data[[outcome_column]]))
  
  result <- data.frame(hospital = character(), state = character(), stringsAsFactors = FALSE)
  
  for (state in unique(data$State)) {
    filtered_data <- data[data$State == state, ]
    filtered_data <- filtered_data[!is.na(filtered_data[[outcome_column]]) & filtered_data[[outcome_column]] != "Not Available", ]
    
    if (num == "best") {
      rank_value <- 1
    } else if (num == "worst") {
      rank_value <- nrow(filtered_data)
    } else {
      rank_value <- as.integer(num)
      if (is.na(rank_value) || rank_value < 1 || rank_value > nrow(filtered_data)) {
        rank_value <- NA
      }
    }
    
    if (!is.na(rank_value)) {
      sorted_data <- filtered_data[order(filtered_data[[outcome_column]], filtered_data$Hospital.Name), ]
      ranked_hospital <- sorted_data$Hospital.Name[rank_value]
      
      result <- rbind(result, data.frame(hospital = ranked_hospital, state = state, stringsAsFactors = FALSE))
    } else {
      result <- rbind(result, data.frame(hospital = NA, state = state, stringsAsFactors = FALSE))
    }
  }
  
  return(result)
}