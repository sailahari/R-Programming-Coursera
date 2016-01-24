complete <- function(directory,id=1:332){
        files<- paste(directory,"/",list.files(directory),sep="")
        v<-c()
        for (i in id){
                file<-read.csv(files[i])
                good<-complete.cases(file)
                v<-c(v,nrow(file[good,]))
        }
        data.frame(id=id,nobs=v)
}