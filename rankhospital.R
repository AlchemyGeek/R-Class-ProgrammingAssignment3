#
# rankhospital - Returns the hospital name with certain rank
# in the 30 day death ranked hospital list for certain outcome
# 
# if more than one ties, return the first alphabetically.
#
# state - State (2 letters)
# outcome - "heart attack", "heart failure", "pneumonia"
# num - "best", "worse", or rank
#

rankhospital <- function(state,outcome,num="best")
{
  ## Read file
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  ## Check outcome argument and select column number with data
  if( outcome == "heart attack" )
    mortality.col = match("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                          names(data))
  else if( outcome == "heart failure" )
    mortality.col = match("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                          names(data))
  else if( outcome == "pneumonia")
    mortality.col = match("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",
                          names(data))
  else
    stop("invalid outcome")
  
  # This is an assert in case column names don't exist in data frame
  stopifnot(!is.na(mortality.col))
  
  ## Check for valid state
  if( !state %in% data[]$State )
    stop("invalid state")
  
  ## Filter data for State
  data <- data[data$State == state,]
  
  #Find hospital name column
  hospname.col = match("Hospital.Name",names(data))
  stopifnot(!is.na(hospname.col))
  
  ## Convert collumn to numeric
  data[,mortality.col] <- as.numeric(data[,mortality.col])
  
  ## Remove NA Values
  data <- data[!is.na(data[,mortality.col]),]
  
  ## Sort data frame with mortality rate, hospital name
  data <- data[order(data[,mortality.col],data[,hospname.col]),]

  ## Return hospital name based on requested rank
  if( nrow(data) == 0 )
    return(NA)
  else if ( num == "best")
    return(data[1,hospname.col])
  else if( num == "worst" )
    return(data[nrow(data),hospname.col])
  else if ( is.numeric(num) && num > 0 && num <= nrow(data) )
    return(data[num,hospname.col])
  
  ## Invald num request
  NA
}
