rankhospital <- function(state, outcome, num="best") {
## Iter001 - lots of indexing issues 
## read in the outcome-of-care-measures dataframe  
filtered<-read.csv("outcome-of-care-measures.csv",colClasses="character",na.strings="Not Available")

## establish the outcomes to be valid and their associated column numbers:
validOuts<- list("heart attack"=11L,"heart failure"=17L,"pneumonia"=23L) 
## split the data on the state 2 letter identifier
hh<-split(filtered,filtered$State)
## list the valid names in the data set::error flagged if not
namit<-names(hh)
## check the input state variable is in this set - 
if(!(state %in% namit)){stop("invalid state")}
## check the input outcome label against a vector of valid labels
## error flagged if not in the valid set
if(!(outcome %in% names(validOuts))){stop("invalid outcome")}
## Explicitly state the outcome required
outy<-validOuts[outcome]
#print(outy[1])
## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with lowest 30-day death
## Get the required state results as a dataframe
otherData<-transform(hh[state])
thisData<-otherData[complete.cases(otherData[,as.integer(outy[1])]),]
#thisData#[as.numeric(outy[1])]

#print(thisData[11])
## The required data has to be explicitly coerced to numeric!
## This also produces NA conversions, throwing a warning which must ber supressed
## use order to 1) order the numerical values of the variable 2) alphabetically by hospital name 

suppressWarnings(thatData<-thisData[order(as.numeric(thisData[,as.numeric(outy[1])]),thisData[,2]),])

off<-thatData[,2]

if(num=="best"){return(head(off, n=1))}
if(num=="worst"){return(tail(off, n=1))}

leng<-length(off)
if(num>leng){return(NA)}
off[as.numeric(rank)]

}
