## The function read_outcome reads the measure for the outcome on hospital data 
## and stores it in a datatable
## it also converts the outcome for "heart attack", "heart failure" and "pneumonia" 
## to numeric values
## for simplification purposes, only the interest columns are saved. These are:
##    outcome[,2] -> hospital name
##    outcome[,7] -> state
##    outcome[,11] -> heart attack death rates
##    outcome[ ,17] -> heart failure death rates
##    outcome[, 23] -> pneumonia death rates
read_outcome <- function() {
  ## reads csv outcome mesures
  ## the file outcome-of-care-measures.csv must be on working directory
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  ## keeps the columns of interest
  outcome <-outcome[,c(2,7,11,17,23)]
  ## converts the variables to numeric
  outcome[,3]<-suppressWarnings(as.numeric(outcome[,3]))
  outcome[,4]<-suppressWarnings(as.numeric(outcome[,4]))
  outcome[,5]<-suppressWarnings(as.numeric(outcome[,5]))
  ## renames the column names
  colnames(outcome)<-c("Hospital.Name","State","heart attack","heart failure",
                       "pneumonia")
  ## output the file
  outcome
}

## The hospitalrank function sort a list of hospitals based in a given an outcome on
## a selected state
hospitalrank<-function(data,state,outcome){
  ## selects the state to be analysed
  n_out <- data[data["State"]==state,]
  ## removes NA for an outcome
  n_out <- n_out[!is.na(n_out[,outcome]),]
  ## order and selects the outcome to be analysed
  n_out <- n_out[order(n_out[outcome],n_out["Hospital.Name"]),]
}

## The rankhospital function takes as argument the state, the outcome and a number
## and returns the name of the hospital on that position
rankhospital <- function(state,outcome,num="best"){
  ## read outcome data
  data<-read_outcome()
  ## check that state and outcome are valid
  valid_outcomes<-c('pneumonia','heart attack','heart failure')
  valid_states<-unique(data["State"])
  if (!isTRUE(valid_outcomes[valid_outcomes==outcome]==outcome)) {
    stop("invalid otcome")
  }
  if (!isTRUE(valid_states[valid_states==state]==state)) {
    stop("invalid state")
  }
  ## return hospital name in that state with the given rank 30-day death rate
  rank<-hospitalrank(data,state,outcome)
  if (num=='best'){
    head(rank[,1],1)
  } else if (num=='worst'){
    tail(rank[,1],1)
  } else rank[num,1]
}