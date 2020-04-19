rankall <- function(outcome, num = "best") {
  ## Read outcome data
  M<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  ## Check that outcome is valid
  if(outcome!="heart attack" & outcome!="heart failure" & outcome!="pneumonia"){
    return("invalid outcome")
  }
  ## For each state, find the hospital of the given rank
  M<-M[,c(2,7,11,17,23)]
  suppressWarnings(M<-cbind(M[,1],as.factor(M[,2]),as.data.frame(lapply(M[,3:5],as.numeric))) )
  names(M)<-c("Name","State","heart attack","heart failure","heart failure")
  if(outcome=="heart attack"){
    df<-M[,c(1,2,3)]
  }
  if(outcome=="heart failure"){
    df<-M[,c(1,2,4)]
  }
  if(outcome=="pneumonia"){
    df<-M[,c(1,2,5)]
  }
  names(df)<-c("Name","State","Outcome")
  df<-na.omit(df)
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  level_state<-levels(df$State)
  if(num=="best"){
     num<-1
     Hname<-c()
     for (st in level_state) {
          dfs<-df[df$State==st,c("Name","Outcome")]
          dfs<-dfs[order(dfs$Outcome,dfs$Name),]
          if(nrow(dfs)>0){
            Hname<-c(Hname,as.character(dfs[dfs$Outcome==num,"Name"]))
          }else{
            Hname<-c(Hname,NA)
          }
     }
  }
  if(num=="worst"){
    Hname<-c()
    for (st in level_state) {
      dfs<-df[df$State==st,c("Name","Outcome")]
      dfs<-dfs[order(dfs$Outcome,dfs$Name),]
      if(nrow(dfs)>0){
        Hname<-c(Hname,as.character(dfs[nrow(dfs),"Name"]))
      }else{
        Hname<-c(Hname,NA)
      }
    }
  }
  else{
    Hname<-c()
    for (st in level_state) {
      dfs<-df[df$State==st,c("Name","Outcome")]
      dfs<-dfs[order(dfs$Outcome,dfs$Name),]
      if(nrow(dfs)>0){
          if (num<=nrow(dfs)){
            Hname<-c(Hname,as.character(dfs[num,"Name"]))
          } else {
            Hname<-c(Hname,NA)
          }
      }else{
        Hname<-c(Hname,NA)
      }
    }
  }
  data.frame(hospital=Hname,state=level_state)
}