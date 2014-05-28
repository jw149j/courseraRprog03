rankall <- function(outcome, num="best") {
## Iter001 - lots of indexing issues 
## read in the outcome-of-care-measures dataframe  
filtered<-read.csv("outcome-of-care-measures.csv",colClasses="character",na.strings="Not Available")

## establish the outcomes to be valid and their associated column numbers:
validOuts<- list("heart attack"=11L,"heart failure"=17L,"pneumonia"=23L) 
## check the input outcome label against a vector of valid labels
## error flagged if not in the valid set
if(!(outcome %in% names(validOuts))){stop("invalid outcome")}
## split the data on the state 2 letter identifier
hh<<-split(filtered,filtered$State)
## list the valid names in the data se::error flagged if not
namit<-names(hh)
sort(namit)
cloo<-lapply(namit,rankByState,validOuts[outcome],num)
#floo<-transform(cloo)
cloo
howmany <- length(cloo)
retty <- data.frame("hospital"=character(0),"state"=character(0))
for(i in 1:howmany){
#  print( class(cloo[i]))
   retty<-rbind(retty,transform(cloo[i]))
  # retty<-tmp
}

retty

## Explicitly state the outcome required
#outy<-validOuts[outcome]
#print(outy[1])
## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with lowest 30-day death
## Get the required state results as a dataframe
#otherData<-transform(hh[state])
#thisData<-otherData[complete.cases(otherData[,as.integer(outcome[1])]),]
#thisData#[as.numeric(outy[1])]

#print(thisData[11])
## The required data has to be explicitly coerced to numeric!
## This also produces NA conversions, throwing a warning which must ber supressed
## use order to 1) order the numerical values of the variable 2) alphabetically by hospital name 

#suppressWarnings(thatData<-thisData[order(as.numeric(thisData[,as.numeric(outy[1])]),thisData[,2]),])

#off<-thatData[,2]

#if(num=="best"){return(head(off, n=1))}
#if(num=="worst"){return(tail(off, n=1))}

#leng<-length(off)
#if(num>leng){return(NA)}
#off[as.numeric(rank)]
} 


rankByState<-function(stateData,outcome,num){
  #print(c(stateData,outcome)) 
  thisData<-transform(hh[stateData])
 # thisData<-otherData[complete.cases(otherData[,as.integer(outcome)]),]
  suppressWarnings(thatData<-thisData[order(as.numeric(thisData[,as.integer(outcome)]),thisData[,2]),]) 
 # return()
#  <-transform(stateData)
#  thisData<-otherData[complete.cases(otherData[,as.integer(outcome[1])]),]
#  
  off<-thatData[,2]
  leng<-length(off)  
  if(num=="best"){retn<-(head(off, n=1))}
  else if(num=="worst"){retn<-(tail(off, n=1))}
  else if(num>leng){retn<-NA}
  else{retn<-off[as.numeric(num)]}
  gh<-data.frame("hospital"=retn,"state"=stateData)
  row.names(gh)<-gh[,2] 
  return(gh)
  
}