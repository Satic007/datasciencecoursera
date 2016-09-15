rankhospital <- function (state, outcome, num = "best") {
        dir <- ("./ProgrammingAssignment3")
        
        # Creating a character list vector for desired arguments in upper case
        outcomes <- c("HEART ATTACK","HEART FAILURE", "PNEUMONIA")
        #Converting the passed outcome argument to upper case
        outcome <- toupper(outcome)
        #print(outcome)
        #Stroing the indices of columns for which data to be retrieved, 
        #should match sequence of outcomes vector
        col <-c(11,17,23)
        
        #Creating a data frame
        hospdata <- read.csv("outcome-of-care-measures.csv")
        states <- levels(hospdata$State)
        
        outcome <- toupper(outcome)
        
        if (!state %in% states) { stop("Invalid State")}
        if (!outcome %in% outcomes) { stop("Invalid outcome")}
        
        i <- col[match(outcome,outcomes)]
        #print(i)
        
        hospdata_st <- hospdata[hospdata$State == state, c(2,i)]
        hospdata_st[,2] <- as.numeric(as.character(hospdata_st[,2]))
        hospdata_st <- na.omit(hospdata_st)
        numrows <- nrow(hospdata_st)
        if (num=="best"){
                idx <- "1"
        }else if (num == "worst"){
                idx <- numrows
        }else {idx <- num}
        
        idx <- as.numeric(idx)
        #print(idx)
        
        names(hospdata_st) <- c("name","death")
        hospdata_ord <- order(hospdata_st$death)
        #print(hospdata_ord[idx])
        hosp <- hospdata_st[hospdata_ord[idx],]$name
        return(as.character(sort(hosp)[1]))
}