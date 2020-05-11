rankhospital <- function(state,outcome,num="best"){
 data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
 if(!any(state == data$State)){
   stop("Invalid State")
 }
 else if((outcome %in% c("heart attack","heart failure","pneumonia"))==FALSE)
 {
   stop("Invalid Outcome")
 }
 subdata<-subset(data,State==state)
 if(outcome=="heart attack"){
   ncol<-11
 }
 else if(outcome=="heart failure"){
   ncol<-17
 }
 else if(outcome=="pneumonia"){
   ncol<-23
 }
 subdata[,ncol]<-as.numeric(subdata[,ncol])
 ox<-subdata[order(subdata[,ncol],subdata[,2]),]
 ox<-ox[(!is.na(ox[,ncol])),]
 if(num=="best"){
   num<-1
 }
 else if(num=="worst"){
   num<-nrow(ox)
 }
 return(ox[num,2])
}

