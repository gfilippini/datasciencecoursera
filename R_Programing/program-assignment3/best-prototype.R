
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
  outcome[,3]<-as.numeric(outcome[,3])
  outcome[,4]<-as.numeric(outcome[,4])
  outcome[,5]<-as.numeric(outcome[,5])
  ## renames the column names
  colnames(outcome)<-c("Hospital.Name","State","Heart_Attack","Heart_Failure",
                        "Pneumonia")
  ## output the file
  outcome
}

## The first_hospital function gives the name of the best hospital in a specific 
## outcome for a selected state. The function receives 3 parameters: the state 
## to be analysed, the outcome and the data were the hospital information is.
first_hospital <- function(data,state,outcome) {
  ## selects the state to be analysed
  n_out <- data[data["State"]==state,]
  ## selects the outcome to be analysed
  n_out <- n_out[order(n_out[outcome],n_out["Hospital.Name"]),]
  return(n_out[1,"Hospital.Name"])
}

## The validate_args function makes the validation on the arguments passed to
## the function best
validate_args <- function(data,state,outcome){
  valid_outcomes<-c('pneumonia','heart attack','heart failure')
  valid_states<-unique(data["State"])
  isTRUE(valid_outcomes[valid_outcomes==outcome]==outcome)
  isTRUE(valid_states[valid_states==state]==state)
}