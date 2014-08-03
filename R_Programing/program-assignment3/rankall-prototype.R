
## The hospitalrank function sort a list of hospitals based in a given an outcome on
## a selected state
hospitalrank<-function(data,outcome){
  ## removes NA for an outcome
  n_out <- data[!is.na(data[,outcome]),]
  ## order and selects the outcome to be analysed
  n_out <- n_out[order(n_out[outcome],n_out["Hospital.Name"]),]
}

rankstate<-function(data,state,outcome){
  ## splits the dataframe by state
  split_data<-split(data,data["State"])
  ## for each split, brings the ranked hospital on the position using the hospitalrank
  ## function
  ##t<-lapply(split_data, function(hospitalrank(data,state,outcome))
}