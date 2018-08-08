rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  col <- getColumnIndexForOutCome(outcome)
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data[,col] <- as.numeric(data[,col])
  data <- data[!is.na(data[,col]),]
  
  ## Check outcome are valid
  if(!validateOutcome(outcome)){
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  data <- data[]
  if("best" == num){
    num <- 1
  }
  states <- sort(getListState(data$State))
  h_names = c()
  for(state in states){
    data_state <- data[data$State == state,]
    list_index <- order(data_state[, col], data_state[, "Hospital.Name"], na.last = NA)
    ret <- data_state[list_index,"Hospital.Name"]
    if("worst" == num){
      h_names = c(h_names, ret[length(ret)]) 
    }else if(num > length(ret)){
      h_names = c(h_names, NA) 
    }else {
      h_names = c(h_names,  ret[num]) 
    }
  }
  data.frame(hospital = h_names, state = states)
}

getListState <- function(states){
  unique(states[!is.na(states)])
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