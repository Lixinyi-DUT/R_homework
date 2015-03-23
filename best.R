best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  sdata<-data[which(data$State==state),]
  if (nrow(sdata)==0)
  {
    stop("invalid state")
  }
  out<-chartr(" ",".", outcome)
  colnames(sdata)=tolower(colnames(sdata))
  outname<-paste("hospital.30.day.death..mortality..rates.from",out,sep=".")
  odata<-sdata[[outname]]
  if(is.null(odata))
  {
   stop("invalid outcome")
  }
  n=which.min(odata)
  sdata$hospital.name[n]
}