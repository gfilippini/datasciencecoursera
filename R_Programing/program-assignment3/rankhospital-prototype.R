
## The hospitalrank function sort a list of hospitals based in a given an outcome on
## a selected state
hospitalrank<-function(data,state,outcome){
  ## selects the state to be analysed
  n_out <- data[data["State"]==state,]
  ## selects the outcome to be analysed
  n_out <- n_out[order(n_out[outcome],n_out["Hospital.Name"]),]
}