rankhospital <- function(state,outcome,num="best"){
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
        df<-df[order(df$Outcome,df$Hospital),]
        worst<- nrow(df)
        rank<- c("best"=1,"worst"=worst)
        if(num %in% names(rank)){
                num<-rank[[num]]
        }
        return(df$Hospital[num])
        
}