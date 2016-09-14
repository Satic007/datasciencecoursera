best <- function(state, outcome) {
        #Selecting Directory where FIle resides
        dir <- ("./ProgrammingAssignment3")
        #Storing state argument value in a vector
        st <- state
        
        # Creating a character list vector for desired arguments in upper case
        outcomes <- c("HEART ATTACK","HEART FAILURE", "PNEUMONIA")
        
        #Converting the passed outcome argument to upper case
        outcome <- toupper(outcome)
        #print(outcome)
        
        #Stroing the indices of columns for which data to be retrieved
        colindx <-c(11,17,23)
        
        #Creating a data frame
        fldata <- read.csv("outcome-of-care-measures.csv")
        #Stroing unique values of State column in a vector
        stnm <- levels(fldata$State)
        
        #Matching the state argument with unique state values 
        stfnd <- match(st,stnm)
        #print(stfnd)
        
        #Verifying if Valid State is passed as an argument or not
        if(is.na(stfnd)){
                stop("Invalid State")
                
        }
        
        #Verifying if Valid outcome is passed as an argument or not using %in% operator
        if(!outcome %in% outcomes){
                #message("Invalid outcome")
                stop("Invalid Outcome")
        }
        
        
        i <- colindx[match(outcome,outcomes)]
        print(i)
        
        hosp_data <- fldata[fldata$State == st,c(2,i)]
        hosp_data[,2] <- as.numeric(as.character(hosp_data[,2]))
        hosp_data <- na.omit(hosp_data)
        names(hosp_data) <- c("name", "death")
        min_dth <- min(hosp_data$death)
        hosp <- hosp_data[hosp_data$death == min_dth,]$name
        return(as.character(sort(hosp)[1]))
        
        
        
}