
## This function reads the measure for the outcome on hospital data and stores it 
## in a datatable
## it also converts the outcome for "heart attack", "heart failure" and "pneumonia" 
## to numeric values
read_outcome <- function() {
  ## reads csv outcome mesures
  ## the file outcome-of-care-measures.csv must be on working directory
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  ## converts the variables to numeric
  outcome[,11]<-as.numeric(out[,11])
  outcome[,17]<-as.numeric(out[,17])
  outcome[,23]<-as.numeric(out[,23])
  outcome
}
