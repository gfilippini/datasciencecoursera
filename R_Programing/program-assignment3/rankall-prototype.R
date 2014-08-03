
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

## The hospitalrank function sorts a list of hospitals based in a given an outcome on
## a selected state.
## it also output the hospital name on ranking position the user inputed
hospitalrank<-function(data,state,outcome,num){
  ## selects the state to be analysed
  n_out <- data[data["State"]==state,]
  ## removes NA for an outcome
  n_out <- n_out[!is.na(n_out[,outcome]),]
  ## order and selects the outcome to be analysed
  n_out <- n_out[order(n_out[outcome],n_out["Hospital.Name"]),]
  ## output the position on the selected order
  if (num=='best'){
    head(n_out[,1],c(1,2))
  } else if (num=='worst'){
    tail(n_out[,1],c(1,2))
  } else n_out[num,c(1,2)]
}

