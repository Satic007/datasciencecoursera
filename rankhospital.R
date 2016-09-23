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
        states <- as.character(levels(hospdata$State))
        

        if (!state %in% states) { stop("Invalid State")}
        if (!outcome %in% outcomes) { stop("Invalid outcome")}
        
        i <- col[match(outcome,outcomes)]
        #print(i)
        
        hospdata_st <- hospdata[hospdata$State == state, c(2,i)]
        hospdata_st[,2] <- as.numeric(as.character(hospdata_st[,2]))
        hospdata_st <- na.omit(hospdata_st)
        numrows <- nrow(hospdata_st)

        names(hospdata_st) <- c("name","death")

        #Sorting unique death values        
        #rnk <- sort(unique(hospdata_st$death))
        
        #Sorting death values        
        rnk <- sort(hospdata_st$death)

        highrnk <- as.numeric(rnk[length(rnk)])


        if (num=="best"){
                idx <- c("1")
        }else if (num == "worst"){
                idx <- highrnk
        }else {idx <- rnk[as.numeric(num)]}
        
        idx <- as.numeric(idx)
        #print(idx)
        
        
        hospdata_ord <- hospdata_st[hospdata_st$death == idx,]
        #hospdata_ord <- order(hospdata_st$death, -hospdata_st$name)
        print(hospdata_ord)
        #hosp <- sort(hospdata_ord$name, decreasing = TRUE)
        #return(as.character(hosp[1]))
        ## USE traceback in console to find the location of error
}