myfunction <- function(x) {
	y <- rnorm(100)
	mean(y)
}

second <- function(x) {
	x + rnorm(length(x))
}

crazy <- function() {
        x <<- 3.14
        print(x)
        {
                print(x)
                x <- 42
                print(x)
                
        }
        print(x)
}


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        #set <- function(){
        #       x <<- y
        #        m <<- NULL
        # }
        get <- function() x
        
        ix <- solve(x)

        #message( "Calculating Mean of Matrix")
        #get <- function() x
        mean(x)
        
}


prdfiles <- function(){
        prd <- read.csv("SLS_STG_PRODS_Missing_In_Files_v2.csv")
        #Retrieiving DISTINCT values of COUNTRY_CODE
        prd_cntry <- levels(prd$COUNTRY_CODE)
        #Looping over the number of countries and subsetting the data accordingly
        for (name in prd_cntry){
                #print(name)
                #Creating data frame by subsetting the data based on Country Code
                prddata <- prd[prd$COUNTRY_CODE == name,]
                #Creating a new file for country code with extension as csv
                newfile <- paste(paste("SLS_STG_PRODS_Missing_In_Files",name,sep="_"),"csv",sep=".")
                #print(newfile)
                #Writing a new csv file for each country data and suppressing the row number
                write.csv(prddata,newfile, row.names = FALSE)
        }
        
}

Cleaning <- function() {
  #===========================
  ID_Data <- read.csv("FSS06hid.csv")
  ID_Data1 <- ID_Data[, c(1,37)]
  ID_Dt <- ID_Data1[ID_Data1$VAL %in% "24",]
  nrow(ID_Dt)
  #===========================
  ngap <- read.xlsx("NGAP.xlsx" , rowIndex = 18 : 23, colIndex = 7:15, sheetIndex = 1, header = TRUE)
  sum(ngap$Zip*ngap$Ext,na.rm = T)
  
  #===========================
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv", dest = "pid.csv")
  dt <- fread("pid.csv")
  # Different ways of calculating mean col value grouping by SEX
  sapply(split(DT$pwgtp15,DT$SEX),mean)
  tapply(DT$pwgtp15,DT$SEX,mean)
  mean(DT$pwgtp15,by=DT$SEX)
  rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]
  # Below one gives correct way, remaining else gives only values
  DT[,mean(pwgtp15),by=SEX]
  mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)
  #============================= 
  doc <- xmlTreeParse("restaurants.xml" , useInternalNodes = TRUE)
  class(doc)
  class(rootNode)
  rootNode <- xmlRoot(doc)
  names(rootNode) # Gives Parent Root Node
  xmlName(rootNode) #Gives Root Name
  xmlName(rootNode[1])
  rootNode[[1]]
  rootNode[[1]][[1]]
  rootNode[[1]][[1]][[1]]
  
  xmlSApply(rootNode[[1]], xmlName)
  xmlSApply(rootNode[[1]][[1]], xmlName)
  xmlSApply(rootNode[[1]][[1]], xmlName, xmlValue)
  
  rest_zip <- xpathSApply(rootNode,"/response//row[zipcode=21231]", xmlValue) # Filters the record and gives all values of the node
  xpathSApply(rootNode[[1]],"//zipcode", xmlValue) # Gives al values of xipcode element
  #=================================================
  ucsdb <- dbConnect(MySQL(), user="genome", host = "genome-mysql.cse.ucsc.edu") # connecing to the server
  result <- dbGetQuery(ucsdb,"show databases;"); dbDisconnect(ucsdb) # Always close connection once work is done
  hg19 <- dbConnect(MySQL(), user="genome", db="hg19", host = "genome-mysql.cse.ucsc.edu") # connecting to specfic database
  allTables <- dbListTables(hg19)
  length(allTables)
  allTables[1:4] # fetching only first 4 tables
  dbListFields(hg19,"affyU133Plus2") # Listing the fields of table affyU13Plus2
  dbGetQuery(hg19,"SELECT COUNT(*) FROM affyU133Plus2")
  affydata <- dbReadTable(hg19,"affyU133Plus2")
  head(affydata)
  qry <- dbSendQuery(hg19,"select * from affyU133Plus2 where mismatches between 1 and 3")
  affmiss <- fetch(qry);quantile(affmiss$misMatches)
  affyMissSmall <- fetch(qry, n=10); dbClearResult(qry) # Always clear the result 
}
