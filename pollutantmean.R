pollutantmean <- function(directory, pollutant, id = 1:330){
        
        # Verifying the directory argument is actual directory or not
        d <- grep("specdata",directory)
        if (d == 1){
                dir <- ("./specdata")
        }
        
        #Storing the list of files in list vector
        filelist <- list.files(dir)

        #reading the first file and creating an empty data fram
        data <- read.csv(paste(dir,filelist[id[1]],sep="/"))
        data <- head(data,0)

        #Looping through based on the supplied arguments for the files
        for (i in id){
                #storing the file name in a vector
                flname = paste(dir,filelist[i],sep="/")
                #creating a dataframe from the file
                dtfrm <- read.csv(flname)
                #appending all the files by binding to one file for easy calculations by looping through
                data <- rbind(data,read.csv(flname))
        }
        
        #calculating the total number of rows
        rows <-nrow(data)
        
        #calculating the mean value of the pollutant from appended file
        pollumean <- mean(data[[pollutant]], na.rm = TRUE)
        
        #returning the vector with mean value of pollutant
        pollumean
        
        
}