rankall <- function(outcome,num="best"){
        df<- read.csv("outcome-of-care-measures.csv",na.strings="Not Available", stringsAsFactors=FALSE)
        outcomes<-c("heart attack"=11,"heart failure"=17,"pneumonia"=23)
        if(!(outcome %in% names(outcomes))){
                return("invalid outcome")
        }
        
        df<-df[,c(2,7,outcomes[outcome])]
        names(df)<-c("hospital","state","Outcome")
        good<-complete.cases(df)
        df<-df[good,]
        list<- split(df,as.factor(df$state))
        list_sorted<-lapply(list, function(x) x[order(x$Outcome,x$hospital),])
        if(num =="best"){
                return(data.frame(t(sapply(list_sorted, function(x) x[1,c(1,2)]))))
        } else if(num =="worst"){
                return(data.frame(t(sapply(list_sorted, function(x) x[nrow(x),c(1,2)]))))
        }
        data.frame(t(sapply(list_sorted,function(x) x[num,c(1,2)])))
        
}