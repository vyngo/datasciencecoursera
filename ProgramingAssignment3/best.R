
best <- function(state, outcome) {
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  for(i in c(11,17,23)){
    data[,i] <- as.numeric(data[,i])
  }
  
  ## Check that state and outcome are valid
  if(!validateStateName(state, data)){
    stop("invalid state")
  }
  if(!validateOutcome(outcome)){
    stop("invalid outcome")
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  col <- getColumnIndexForOutCome(outcome)
  data_state <- data[data$State == state,]
  min_val <- min(data_state[, col], na.rm = TRUE)
  ret <- sort(data_state[data_state[, col] == min_val,"Hospital.Name"])
  ret[1]
}

validateStateName <- function(state, data){
  state_name <- unique(data[,7])
  (state %in% state_name)
}

validateOutcome <- function(outcome){
  outcome_name <- c("heart attack", "heart failure", "pneumonia")
  (outcome %in% outcome_name)
}

getColumnIndexForOutCome <- function(outcome){
  if("heart attack" == outcome){
    11
  }else if("heart failure" == outcome){
    17
  }else {
    23
  }
}