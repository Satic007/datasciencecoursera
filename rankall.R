rankall <- function(outcome, num="best"){
        dir <- ("./ProgrammingAssignment3")
        # Creating a character list vector for desired arguments in upper case
        outcomes <- c("HEART ATTACK","HEART FAILURE", "PNEUMONIA")
        
        #Converting the passed outcome argument to upper case
        outcome <- toupper(outcome)
        if (!outcome %in% outcomes) { stop("Invalid outcome")}
       
         #Stroing the indices of columns for which data to be retrieved, 
        #should match sequence of outcomes vector
        col <-c(11,17,23)
        
        #Creating a data frame
        hospdata <- read.csv("outcome-of-care-measures.csv")
        
        i <- col[match(outcome,outcomes)]
        i <- as.numeric(i)
        
        print("Printing the type of outcome index")
        print(i)
        #creating a data frame
        df <- data.frame(st=character(0), hosp=character(0))
        
       states <- levels(hospdata$St)
        
        for (s in states){
                
                print("State Name")
                print(s)
                
                hospdata_st <- hospdata[hospdata$State == s,c(2,i)]
                hospdata_st[,2] <- as.numeric(as.character(hospdata_st[,2]))
                hospdata_st <- na.omit(hospdata_st)
                
                numrows <- nrow(hospdata_st)
                print("number of records in State " )
                print(numrows)
                
                if (num=="best"){
                        idx <- "1"
                }else if (num == "worst"){
                        idx <- numrows
                }else {idx <- num}
                
                print("Order of the outcome to search")
                print(idx)
                
                names(hospdata_st) <- c("name","death")
                
                hospdata_ord <- order(hospdata_st$death)
                print("Lenght of the Order")
                print(length(hospdata_ord))
                or <- hospdata_ord[idx]
                
                print("Printing order index")
                print(or)
                
                hosp <- hospdata_st[hospdata_ord[1],]$name
                print("Printing Hospital Name Before sorting")
                print(hosp)
                
                hospname <- as.character(sort(hosp)[1])
                
                print("Printing Final Sorted Hospital Name")
                print(hospname)
                
                newrow <- data.frame(st = s, hosp = hospname)
                
                df <- rbind(df,newrow)
                
        }
        
       # return(df)
        
        
        
}