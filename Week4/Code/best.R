best <- function(state,outcome){
        df<- read.csv("outcome-of-care-measures.csv",na.strings="Not Available", stringsAsFactors=FALSE)
        outcomes<-c("heart attack"=11,"heart failure"=17,"pneumonia"=23)
        if(!any(df$State==state)){
                return("invalid state")
        }
        if(!(outcome %in% names(outcomes))){
                return("invalid outcome")
        }
        
        df<-df[,c(2,7,outcomes[outcome])]
        names(df)<-c("Hospital","State","Outcome")
        good<-complete.cases(df)
        df<-df[good,]
        df<- subset(df,State==state)
        i<- which(df$Outcome==min(df$Outcome))
        best_hospitals<-sort(df$Hospital[i])
        best_hospitals[1]
}