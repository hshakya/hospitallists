best <- function(state, outcome){
  outcome_list = c("heart attack", "heart failure", "pneumonia")
  if   (outcome %in% outcome_list == FALSE){
    stop("invalid state")
  }
  x <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  state_list <- x$State
  states <- unique(state_list)
  if (state %in% state_list == FALSE){
    stop("invalid state")
  }
  data <- x[c(2,7,11,17,23)]
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  data <- data[data$State == state & data[ ,outcome]!= "Not Available",]
  value <- data[ ,outcome]
  row_num <- which.min(value)
  data[row_num,]$Hospital.Name
  }