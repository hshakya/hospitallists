rankhospital <- function(state, outcome, num = "best"){
  ##reading the data
  x <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  ##validating outcome
  outcome_list = c("heart attack", "heart failure", "pneumonia")
  if   (outcome %in% outcome_list == FALSE){
      stop("invalid outcome")
  }
  ##validating num
  if( num != "best" && num != "worst" && num%%1 != 0 ) 
      stop("invalid num")
  ##validating state
  state_list <- x$State
  states <- unique(state_list)
  if (state %in% state_list == FALSE)
      stop("invalid state")
  data <- x[c(2,7,11,17,23)]
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  data <- data[data$State == state & data[ ,outcome]!= "Not Available",]
  
  ##ordering data
  data[outcome] <- as.data.frame(sapply(data[outcome], as.numeric))
  data <- data[order(data$Hospital.Name, decreasing = FALSE), ]
  data <- data[order(data[outcome], decreasing = FALSE), ]
  #process
  value <- data[ ,outcome]
  if(num == "best"){
      row_num <- which.min(value)
  }
  else if(num == "worst"){
      row_num <- which.max(value)
  }
  else{
      row_num <- num
  }
  data[row_num,]$Hospital.Name
}
