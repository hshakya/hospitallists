rankall <- function(outcome, num = "best"){
  ##reading the data
  x <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  ##validating outcome
  outcome_list = c("heart attack", "heart failure", "pneumonia")
  if   (outcome %in% outcome_list == FALSE){
      stop("invalid state")
  }
  ##validating num
  if( num != "best" && num != "worst" && num%%1 != 0 ) 
      stop("invalid num")
  hosp <- x$Hospital.Name
  data <- x[c(2,7,11,17,23)]
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  #data <- data[data$State == state & data[ ,outcome]!= "Not Available",]
  state_list <- x$State
  states <- unique(state_list)
  newdata <- data.frame("hospital"=character(), "state"=character())
  for (st in states){
      hosp <- rankhospital(st, outcome, num)
      newdata <- rbind(newdata, data.frame(hospital=hosp, state=st))
  }
  newdata <- newdata[order(newdata['state'], decreasing = FALSE),] 
newdata

  }