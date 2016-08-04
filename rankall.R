##
## rankall - Returns data frame with the hospital name with certain rank
## in the 30 day death ranked hospital list for certain outcome for all states
## 
## if more than one ties, return the first alphabetically.
## If no hospital in the state in the "num" rank return NA
##
## outcome - "heart attack", "heart failure", "pneumonia"
## num - "best", "worse", or rank
##

rankall <- function(outcome,num="best")
{
  ## Read file
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  ## Create vector with all states sorted
  states <- sort(unique( data[]$State ))
  
  ## Initialize results
  result <- data.frame("hospital"=character(),"state"=character())
  
  ## For each state
  for( state in states ) {
    ## Find ranked hospital
    hospital <- rankinghospital(state,outcome,num)
    
    ## Add row to results
    result <- rbind(result,data.frame("hospital"=hospital,"state"=state))
  }
  
  result
}
