best <- function(state, outcome) {
## Iter001 - get error checking
## read in the outcome-of-care-measures dataframe  
filtered<-read.csv("outcome-of-care-measures.csv",colClasses="character")
## split the data on the state 2 letter identifier
hh<-split(filtered,filtered$State)
## list the valid names in the data set error flagged if not
namit<-names(hh)
## check the input state variable is in this set - 
if(!(state %in% namit)){stop("invalid state")}
## check the input outcome label against a vector of valid labels
## error flagged if not in the valid set
if(!(outcome %in% c("heart attack","heart failure","pneumonia"))){stop("invalid outcome")}

## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with lowest 30-day death
## rate
}