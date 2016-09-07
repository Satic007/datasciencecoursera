corr <- function(directory, threshold = 0){
        if(grep("specdata",directory) == 1){
                dir <- "./specdata"
        }
        
        #Storing the available files in the directory to list
        listfiles <- list.files(dir)
        #counting number of files
        tot_files <- length(listfiles)
       
        corr <-numeric()
        
        #Looping through all the files
        for(i in 1:tot_files){
                #Retrieving the file path0
                fl_path <- paste(dir,listfiles[i],sep="/")
                #Storing the file data in a data frame
                dtfrm <- read.csv(fl_path)
                #omitting NA records
                dtfrm_na <- na.omit(dtfrm)
                #calculating total number of rows with good data
                thr_rows <- nrow(dtfrm_na)
                
                #Verifying the threshold limit
                if(thr_rows > threshold){
                        #calculating correlation
                        corr = c(corr,cor(dtfrm_na$sulfate,dtfrm_na$nitrate))
                        
                }
        }
        
        return(corr)
        
}