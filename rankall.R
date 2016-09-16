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
  
  #creating a data frame
  df <- data.frame(st=character(0), hosp=character(0))
  
  states <- levels(hospdata$St)
  
  for (s in states){
    
    hospdata_st <- hospdata[hospdata$State == s,c(2,i)]
    hospdata_st[,2] <- as.numeric(as.character(hospdata_st[,2]))
    hospdata_st <- na.omit(hospdata_st)
    
    numrows <- nrow(hospdata_st)

    
    if (num=="best"){
      idx <- "1"
    }else if (num == "worst"){
      idx <- numrows
    }else {idx <- num}
    
    idx <- as.numeric(idx)
    
    names(hospdata_st) <- c("name","death")
    
    hospdata_ord <- order(hospdata_st$death)

    or <- hospdata_ord[idx]
    
    hosp <- hospdata_st[hospdata_ord[idx],]$name

    hospname <- as.character(sort(hosp)[1])
    
    newrow <- data.frame(st = s, hosp = hospname)
    
    df <- rbind(df,newrow)
    
  }
  
  return(df)
 
}