corr <- function(directory,threshold=0){
        files<- paste(directory,"/",list.files(directory),sep="")
        v<-c()
        for (i in 1:length(files)){
                file<-read.csv(files[i])
                good<-complete.cases(file)
                file<-file[good,]
                if(nrow(file) > threshold){
                       corr <- cor(file$nitrate,file$sulfate) 
                       v<-c(v,corr)
                }
        }
        v
}