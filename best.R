best<-function(state,outcome){
  data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  if(!any(state==data$State)){
    stop("Invalid State")
  }
  else
    if((outcome %in% c("heart attack","heart failure", "pneumonia"))==FALSE){
      stop("Invalid Outcome")
    }
  o2<-subset(data,State==state)
  if(outcome=="heart attack"){
    cnum<-11
  }
  else if(outcome=="heart failure"){
      cnum<-17
    }
  else{
      cnum<-23
    }
  tosort<-which(as.numeric(o2[,cnum])==min(as.numeric(o2[,cnum]),na.rm=TRUE))
  h<-o2[tosort,2]
  h<-sort(h)
  return(h[1])
  
}
best("TX", "heart attack")
