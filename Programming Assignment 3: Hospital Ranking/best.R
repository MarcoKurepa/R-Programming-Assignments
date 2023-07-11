best <- function(state, outcome) {
  
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  
  valid_states <- unique(data$State)
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!state %in% valid_states) {
    stop("invalid state")
  }
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
  
  filtered_data <- data[data$State == state, ]
  filtered_data <- filtered_data[!is.na(filtered_data[[outcome_column]]) & filtered_data[[outcome_column]] != "Not Available", ]
  
  lowest_rate <- min(filtered_data[[outcome_column]])
  best_hospital <- filtered_data[filtered_data[[outcome_column]] == lowest_rate, ]
  return(best_hospital$Hospital.Name)
}
