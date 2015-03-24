rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  out<-chartr(" ",".", outcome)
  colnames(data)<-tolower(colnames(data))
  outname<-paste("hospital.30.day.death..mortality..rates.from",out,sep=".")
  if(is.null(data[[outname]]))
  {
    stop("invalid outcome")
  }
  data[[outname]]<-as.numeric(data[[outname]])
  data<-data[complete.cases(data[[outname]]),]
  d<-split(data,data$state)
  getrank<-function(hos,outname,r){
    hos<-hos[order(hos[[outname]],hos$hospital.name),]
    if(r=="worst")
    {
      return(c(hos$hospital.name[nrow(hos)],hos$state[nrow(hos)]))
    }
    if(r=="best")
    {
     r<-1
    }
    if(r>nrow(hos))
      return(c(NA,hos$state[1]))
    else
      return(c(hos$hospital.name[r],hos$state[r]))
  }
  re<-lapply(d,getrank,outname=outname,r=num)
  result<-t(as.data.frame(re))
  colnames(result)<-c("hospital","state")
  result<-as.data.frame(result)
}