pollutantmean<-function(directory,pollutant,id=1:332){
        files<-paste(directory,"/",list.files(directory),sep="")
        v<-c()
        for (i in id){
            file<-read.csv(files[i])
            v<-c(v,file[,pollutant])
        }
        mean(v,na.rm=TRUE)
}