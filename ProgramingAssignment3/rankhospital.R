rankhospital <- function(state, outcome, num = "best") {
  
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
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  if("best" == num){
    num <- 1
  }
  col <- getColumnIndexForOutCome(outcome)
  data_state <- data[data$State == state,]
  list_index <- order(data_state[, col], data_state[, "Hospital.Name"], na.last = NA)
  ret <- data_state[list_index,"Hospital.Name"]
  if("worst" == num){
    return(ret[length(ret)]) 
  }
  if(num > length(ret)){
    return(NA)
  }
  ret[num]
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