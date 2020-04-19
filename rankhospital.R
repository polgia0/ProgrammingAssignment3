rankhospital<-function(state,outcome,num="best"){
  ## Read outcome data
  M<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  ## Check that state and outcome are valid
  factor_state<-factor(M$State)
  levels_state<-levels(factor_state)
  if (sum(levels_state==state)==0){
    return("invalid state")
  }
  if(outcome!="heart attack" & outcome!="heart failure" & outcome!="pneumonia"){
    return("invalid outcome")
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  M<-M[M$State==state,c(2,11,17,23)]
  suppressWarnings(M<-cbind(M[,1],as.data.frame(lapply(M[,2:4],as.numeric))) )
  M<-na.omit(M)
  if(outcome=="heart attack"){
    df<-M[,c(1,2)]
  }
  if(outcome=="heart failure"){
    df<-M[,c(1,3)]
  }
  if(outcome=="pneumonia"){
    df<-M[,c(1,4)]
  }
  df<-df[order(df[,2],df[,1]),]
  if(num=="best"){
      as.character(df[1,1])
  }
  if(num=="worst"){
    as.character(df[nrow(df),1])
  }
  else{
    num<-as.numeric(num)
    if (is.na(num)){
      return("invalid num")
    }
    if (num<nrow(df)){
        as.character(df[num,1])
    }else{
        as.character('NA')
    }
  }
}