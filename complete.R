complete <- function(directory, id = 1:332){
        
        # Verifying the directory argument is actual directory or not
        d <- grep("specdata",directory)
        if (d == 1){
                dir <- ("./specdata")
        }
        #Storing the list of files in list vector
        filelist <- list.files(dir)
        
        #creating an empty dataframe to hold two numeric vectors
        df <-data.frame(id= numeric(0), nobs = numeric(0))
        
         for (i in id){
                #Getting file name path
                path <- paste(dir,filelist[i],sep="/")
                
                #storing the file info in a dataframe
                dtfrm <- read.csv(path)
                
                #ommitting NA rows from data frame
                dtfrm_na <- na.omit(dtfrm)
                
                ## Following can also be used
                ##dtfrm_na <- dtfrm[!is.na(dtfrm["sulfate"]),] --> Single column filteration
                ##dtfrm_na <- dtfrm[!is.na(dtfrm["nitrate"]),] 
               
                ## Another way by using complete.cases
                ##dtfrm_na <- dtfrm[complete.cases(dtfrm$sulfate),] --> Single column filteration
                ##dtfrm_na <- dtfrm[complete.cases(dtfrm$nitrate),]
                
                
                #calculating number of rows in new data frame
                nob <- nrow(dtfrm_na)
                
                #creating a new row 
                newrow <- data.frame(id=i,nobs=nob)
                
                #appending the storage data frame with new row 
                df <- rbind(df,newrow)
              
         }
        #returning the data frame
        df
}