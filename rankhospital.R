rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  sdata<-data[which(data$State==state),]
  if (nrow(sdata)==0)
  {
    stop("invalid state")
  }
  out<-chartr(" ",".", outcome)
  colnames(sdata)=tolower(colnames(sdata))
  outname<-paste("hospital.30.day.death..mortality..rates.from",out,sep=".")
  if(is.null(sdata[[outname]]))
  {
    stop("invalid outcome")
  }
  sdata[[outname]]<-as.numeric(sdata[[outname]])
 #sdata=sdata[[outname]][!is.na(sdata[[outname]]),]
  sdata=sdata[complete.cases(sdata[[outname]]),]
  hos<-sdata[order(sdata[[outname]],sdata$hospital.name),]
   if(num=="worst")
     return(hos$hospital.name[nrow(hos)])
   else
     if(num=="best")
     {
       num<-1
     }
     if(num>nrow(sdata))
     {
       return(NA)
     }
     return(hos$hospital.name[num])
}