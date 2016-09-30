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
  #============================= Reading XML file
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
	
  #=================================================MySQL========================================
  
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
  
  #========================Connecting SQL Server
	install.packages("RODBC")
	library(RODBC)
	dbhandle <- odbcDriverConnect('driver={SQL Server};server=USEAGAN6469D;databas=G360_Landing;trusted_connection=true')
	res <- sqlQuery(dbhandle, 'select top 10 * from information_schema.tables')
	res <- sqlQuery(dbhandle, 'select top 10 Product_code from G360_Landing.dbo.G360_Product_Stg')
	
	#==== Using DSN
	myconn <- odbcConnect("G360_Landing")
	ftch <- sqlFetch(myconn,"G360_PRODUCT_STG") # data is fetched into a data frame
  		
	
  #=======================HDF5=================
  source("http://bioconductor.org/biocLite.R")
  biocLite("rhdf5") # 
  library(rhdf5) # This will install packages from Bioconductor, used for genomics , but has good BigData packages
  created <- h5createFile("example.h5")
  created
  will return [1] TRUE
  
  #creating groups 
created <- h5createGroup("example.h5", "boo")
created <- h5createGroup("example.h5", "foo")
created <- h5createGroup("example.h5", "baa")
created <- h5createGroup("example.h5", "foo/foobaa")
h5ls("exmple.h5") # Lists the groups created

#creating data and writing
> A <- matrix(1:10, nr=5, nc=2)
> h5write(A,"example.h5","foo/A")
> B <- array(seq(0.1,2.0, by=0.1), dim = c(5,2,2))
> attr(B,"scale") <- "liter"
> h5write(B, "example.h5", "foo/foobaa/B")
> h5ls("example.h5")

#writing a data frame to h5 file
df <- data.frame(1L:5L,seq(0.1,length.out = 5), c("ab","cde","abdc","a","s"), stringsAsFactors = FALSE)
 h5write(df,"example.h5","df")
> h5ls("example.h5")

#Reading
> readA <- h5read("example.h5","foo/A")
> readA
     [,1] [,2]
[1,]    1    6
[2,]    2    7
[3,]    3    8
[4,]    4    9
[5,]    5   10
#writing chunks
> h5write(c(12,13,14),"example.h5", "foo/A", index = list(1:3,1)) # writing to first 3 defined as index and to firs column
> h5read("example.h5","foo/A")

     [,1] [,2]
[1,]   12    6
[2,]   13    7
[3,]   14    8
[4,]    4    9
[5,]    5   10

#================Reading from Web
> con <- url("https://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en")
> htmlcode <- readLines(con)
close(con)
htmlcode # will return html code

install.packages("XML") # To read XML data
> library(XML)
> url <- "https://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en"
> html <- htmlTreeParse(url, useInternalNodes = T)

 install.packages("httr") # for better reading the web
 > library(httr)
> html2 <- GET(url)
> content2 <- content(html2, as = "text")
> parsedHTML <- htmlParse(content2,asText = TRUE)
> xpathSApply(parsedHTML,"//title", xmlValue)

#Accessing webpages which needs authentication
> pg1 <- GET("http://httpbin.org/basic-auth/user/passwd")
> pg1
Response [http://httpbin.org/basic-auth/user/passwd]
  Date: 2016-09-24 17:37
  Status: 401
  Content-Type: <unknown>
<EMPTY BODY>
#Add credentials this time
> pg2 <- GET("http://httpbin.org/basic-auth/user/passwd", authenticate("user","passwd"))
> pg2
Response [http://httpbin.org/basic-auth/user/passwd]
  Date: 2016-09-24 17:37
  Status: 200
  Content-Type: application/json
  Size: 47 B
{
  "authenticated": true, 
  "user": "user"
}
# Reading from API
	#==============Twitter App 
	#http://www.LearnDS.com

	#Consumer Key (API Key) qgIRwEodVwqQRNfBSZyHoVTy8
	#Consumer Secret (API Secret) WOiUNh7NXLZfOQzS648yh5aVJOUv1VyJaTYjDudcTfVctlSnG8 

	#Request token URL https://api.twitter.com/oauth/request_token
	#Authorize URL https://api.twitter.com/oauth/authorize
	#Access token URL https://api.twitter.com/oauth/access_token 

	#Access Token 95123719-oct1aFo2SVIurG1kOv3GtISJxO1N1mkSo4HAXoM5A
	#Access Token Secret gw2LUis39SMidG7BpuxvNUDisW6vnnJcPbM5XJxxPTpKc 
	#==============Accessing Twitteer
	> myapp <- oauth_app("twitter", key ="qgIRwEodVwqQRNfBSZyHoVTy8", secret = "WOiUNh7NXLZfOQzS648yh5aVJOUv1VyJaTYjDudcTfVctlSnG8")
	> sig <- sign_oauth1.0(myapp, "95123719-oct1aFo2SVIurG1kOv3GtISJxO1N1mkSo4HAXoM5A", "gw2LUis39SMidG7BpuxvNUDisW6vnnJcPbM5XJxxPTpKc")
	> homeTL = GET("https://api.twitter.com/1.1/statuses/home_timeline.json",sig)
	> json1 = content(homeTL)
	> json2 = jsonlite::fromJSON(toJson(json1))

	#==============Accessing Github
	myapp <- oauth_app("github", "8f4dfc041e89690553a7","4efca62ba6850c444824d9de200db1383e257ef4") # pass key(clientid) and secret
	github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
	gtoken <- config(token = github_token)
	req <- GET("https://api.github.com/users/jtleek/repos", gtoken) # accessing specific repo
	stop_for_status(req)
	content(req)
	
	# When using sqldf package, detach RMySQL package
	detach("package:RMySQL", unload = TRUE)
	install.packages("sqldf")
	library(sqldf)
	acs <- read.csv("pid.csv", header = TRUE, sep = ",")
	sqldf("select pwgtp1 from acs where AGEP < 50")
	sqldf("select distinct AGEP from acs")
	
	#Reading from HTML
	doc <- url("http://biostat.jhsph.edu/~jleek/contact.html")
	lns <- readLines(doc)
	nchar(lns[10]) # reading number of characters 
	
	#Reading Fixed width file
	ff <- read.fwf(file = url("http://www.cpc.ncep.noaa.gov/data/indices/wksst8110.for"),skip=4,
+                widths=c(12, 7, 4, 9, 4, 9, 4, 9, 4))
	
	#====================Summarizing the data==================
	> fileurl <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
	> download.file(fileurl, destfile = "restaurants.csv")
	> restData <- read.csv("restaurants.csv")
	> summary(restData) # summarizes the data
	
	> str(restData) # Gives information of data frame
	
	> quantile(restData$councilDistrict)  # Provides quantile of data
	> quantile(restData$councilDistrict, probs = c(0.5,.87)) # Filter the probability of probability
	
	> table(restData$zipCode, useNA = "ifany") # creating a table and using argument to show NA ; 'arg' should be one of “no”, “ifany”, “always”
	> table(restData$councilDistrict, restData$zipCode) # creating two dimensional table 
	
	> sum(is.na(restData$councilDistrict)) # sum of missing NA values
	> any(is.na(restData$councilDistrict)) # Checks if ANY values is NA
	> all(restData$zipCode>0) # searches a column for specific critieria in whole data frame
	
	> colSums(is.na(restData)) # Searches all the columns in df for NA and summarizes each column
	> all(colSums(is.na(restData))==0) # Applying the above in a condition, returns TRUE or FALSE
	
	> table(restData$zipCode==21212) # Gives counts of records for a column with specific condition
	> table(restData$zipCode %in% c("21212")) # Using IN operator
	> table(restData$zipCode %in% c("21212","21213")) # multiple condition
	FALSE  TRUE 
 	1268    59 
	
	> restData[restData$zipCode %in% c("21212"),] # Filtering the data frame
	
	> data("UCBAdmissions")
	> DF <- as.data.frame(UCBAdmissions) # creating a data frame from existing data set
	> summary(DF)
	> xtabs(Freq ~ Gender + Admit, data=DF) # summarize data between cross tabs; creates two dimension table
	
	> xt <- xtabs(Freq ~ Gender + Dept + Admit, data=DF) #using multiple cross reference
	> ftable(xt) # ftable used to organize the jumbled up data
	
	> xt <- xtabs(Freq ~., data=DF) #using cross reference of one column with rest
	
	> warpbreaks$replicate <- rep(1:9, len=54) # replicate the data set
	> xt1 = xtabs(breaks ~.,data = warpbreaks)
	> ftable(xt1)
	
	#Finding the sie of the data
	
	> fake = rnorm(1e5)
	> object.size(fake)
	800040 bytes
	> print(object.size(fake), units = "Mb")
	0.8 Mb
	
	#======================New variable creation=============================================
	# creating sequences
	> s1 <- seq(1,10, by=3) # mention min and max and by for amt of skip
	> s2 <- seq(1,10, length=3) # mention the length of the vector
	> x <- c(1,2,3,5,85); seq(along=x) # creating sequence based on the vector values
	
	#creating new variable
	> restData$Nearme <- restData$neighborhood %in% c("Roland Park","Homeland")
	> table(restData$Nearme)
	
	
	# creatinga binary variable for same above condition
	> restData$zipWrong <- ifelse(restData$zipCode<0, TRUE, FALSE)
	> table(restData$zipWrong, restData$zipCode<0)

		FALSE TRUE
	  FALSE  1326    0
	  TRUE      0    1
	
	# Creating categorical variables
	> restData$zipGroups = cut(restData$zipCode, breaks = quantile(restData$zipCode))
	> table(restData$zipGroups)

	(-2.123e+04,2.12e+04]  (2.12e+04,2.122e+04] (2.122e+04,2.123e+04] (2.123e+04,2.129e+04] 
			  337                   375                   282                   332 

	> table(restData$zipGroups, restData$zipCode)	
	#The same thing can be achieved using Hmisc package
	> library(Hmisc)
	> restData$zipGroups = cut2(restData$zipCode, g=4)
	> table(restData$zipGroups)

	[-21226,21205) [ 21205,21220) [ 21220,21227) [ 21227,21287] 
		   338            375            300            314 		
	
	#===================== Reshaping the data=============
	# Melting data frames
	> library(reshape2)
	#Melt function will create new data frame with ids with measurent varialbe. Out 32 records mtchars, the new data frame 
	# will have 64 records 32 with mpg and 32 with hp 				       
	> carMelt <- melt(mtcars, id=c("carname","gear","cyl"), measure.vars = c("mpg","hp"))
	> head(carMelt)
	> tail(carMelt)
	
	# dcast function to recast the data in particular data set
	> cylData <- dcast(carMelt, cyl ~ variable)
	> cylData # Dsiplays number of records for each cylinder type
	  cyl mpg hp
	1   4  11 11
	2   6   7  7
	3   8  14 14				       
			  
	> cylData <- dcast(carMelt, cyl ~ variable, mean) # Returns mean value
	> cylData
	  cyl      mpg        hp
	1   4 26.66364  82.63636
	2   6 19.74286 122.28571
	3   8 15.10000 209.21429
	
	
	
	# Summing up
	> spIns <- split(InsectSprays$count, InsectSprays$spray)
	> sprcount <- lapply(spIns, sum)
	> unlist(sprcount)				       
	
	#Another way using plyr package				       
	> ddply(InsectSprays,.(spray), plyr::summarize, sum=sum(count))
	#If multiple packages have same funtion, explicitly  mentione the function name of the package viz.., plyr::summarize				       
	
	#Another usage of ddply function				       
	> spraySums <- ddply(InsectSprays,.(spray), plyr::summarize, sum=ave(count,FUN=sum))
	> dim(spraySums)
	[1] 72  2
	> head(spraySums)
	  spray sum
	1     A 174
	2     A 174
	3     A 174
	4     A 174
	5     A 174
	6     A 174				       
				       
	#==================Using deplyr Package============
	chicago <- readRDS("chicago.rds")
	dim(chicago)
	str(chicago)
	names(chicago)
	
	install.packages("dplyr")	
	library(dplyr)				       
	head(select(chicago, city:dptp))				       
	head(select(chicago, -(city:dptp)))
	
	#Filtering the records				       
	chic.f <- filter(chicago, pm25tmean2>30)
	head(chic.f, 2)
	chic.f <- filter(chicago, pm25tmean2>30 & tmpd >80)
	chicago <- arrange(chicago, date)
	head(chicago)
	tail(chicago)
	
	#Arranging the data				       
	chicago <- arrange(chicago, desc(date))
				       
	#Renaming the columns
	chicago <- rename(chicago, pm25 = pm25tmean2, dewpoint = dptp)
	head(chicago)
				       
	#Adding new columns			       
	chicago <- mutate(chicago, pm25detrend = pm25 - mean(pm25, na.rm = true))
	chicago <- mutate(chicago, pm25detrend = pm25 - mean(pm25, na.rm = TRUE))
	head(select(chicago, pm25,pm25detrend))
	
        chicago <- mutate(chicago, tempcat = factor(1 * (tmpd >80), labels = c("cold","hot")))
	select(chicago, tempcat="hot")
	head(filter(chicago, tempcat == "hot"))				       
	hotcold <- group_by(chicago, tempcat)
				       
	#Summarizing the data				       
	summarise(hotcold, pm25= mean(pm25), o3=max(o3tmean2), no2=median(no2tmean2))
	summarise(hotcold, pm25= mean(pm25, na.rm = TRUE), o3=max(o3tmean2), no2=median(no2tmean2))
				       
	chicago <- mutate(chicago, year=as.POSIXlt(date)$year + 1900)
	years <- group_by(chicago, year)
	summarise(years, pm25= mean(pm25, na.rm = TRUE), o3=max(o3tmean2), no2=median(no2tmean2))	
				       
	#=================AGGREGATING DATA ECOLAB PROD FILE ===================================================
	> prd <- read.csv(SLS_STG_PRODS_Missing_In_Files_09272016.csv)
	> prd <- rename(prd, SRC = SOURCE_SYSTEM_NAME, CNTRY = COUNTRY_CODE, PROD = ï..Product_code)				       
	> DT <- data.table(prd)
	> table(DT$CNTRY) # table will gives the count of records based on each column value as list				       
	#Different ways to retrieve number of records for each country like group by in SQL
	> data.frame(table(DT$CNTRY)) # Creates a data frame with columns Var1 Freq
	# Using plyr package ddply function. Split the data frame 'DT' by column 'CNTRY' and applying funtion nrow, returning data frame
	#apply functions are packages made instead of for loops				       
	> ddply(DT,.(CNTRY), nrow) 
	#aggregate is the core R function
	> aggregate(cbind(count=PROD)~CNTRY, data = DT, FUN = function(x){NROW(x)})
	# Counting and aggregating
	> count(DT,"CNTRY")	# Read about count			       
	
					       
	#===================================END OF ECOLAB===================================================================				       

	# ===============Merging Data
	> fileurl1 = "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
	> fileurl2 = "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
	> download.file(fileurl1, destfile = "reviews.csv")	
	> download.file(fileurl2, destfile = "solutions.csv")
	
	> reviews <- read.csv("reviews.csv"); solutions <- read.csv("solutions.csv")
	#merge function is kind of FULL OUTER JOIN. all= TURE will keep column value as NA , if no match found
	> mergedata1 <- merge(reviews, solutions, by.x="solution_id", by.y = "id",all = TRUE)	
	
	> intersect(names(solutions), names(reviews))
	[1] "id"        "start"     "stop"      "time_left"
	> mergedata2 <- merge(reviews, solutions, all = TRUE)
	> head(mergedata2)				    
		
	#Using Join Command				       
	> df3 = data.frame(id=sample(1:10), z= rnorm(10))
	> df2 = data.frame(id=sample(1:10), y= rnorm(10))
	> df1 = data.frame(id=sample(1:10), x= rnorm(10))
	> arrange(join(df1,df2),id)				       
	> dfList <- list(df1, df2, df3)
	> join_all(dfList)				       
				       
	# Quiz
				       
	#1
	> id_hs$SALE <- ifelse(id_hs$ACR==3 & id_hs$AGS==6, TRUE, FALSE)	
	> which(id_hs$SALE)				       
	#3			       
	> gdp <- read.csv("GDP_Data.csv", skip = 3, blank.lines.skip = TRUE)
	> gdp <- gdp[gdp, c(1,2,4)]				       
	> gdp <- rename(gdp, CNTRY=X)
	> gdp_cntry <- merge(gdp, cntry1, by.x = "cntry", by.y = "CountryCode", all = FALSE)				       
	> gdp_cntry[,"Ranking"] <- as.numeric(as.character(gdp_cntry[,"Ranking"]))				       
	> gdp_cntry_mtch <- filter(gdp_cntry, Ranking > 0)
	> gdp_cntry_mtch <- arrange(gdp_cntry_mtch, desc(Ranking))
	> arrange(gdp_cntry_mtch,desc(Ranking))[13,"Economy"]	# Getting 13th records of descending order			       
		## Alternate, using fread to read large data
	> gdp <- fread("GDP.csv", skip = 4, nrows = 190, select = c(1, 2, 4, 5), col.names = c("CountryCode", "Rank", "Economy", "Total"))				       

	> view(gdp) # will show the data in a view				       
				       
	#4
	> ddply(gdp_cntry_mtch_inc, . (inc_grp), plyr::summarize, mean= mean(Ranking))				       
	# Getting mean of all Income groups
	> tapply(gdp_cntry_mtch$Ranking, gdp_cntry_mtch$Income.Group, mean)				       
	#5
	#cut() function divides a numeric vector into different ranges. 				       
	> gdp_cntry_mtch$gdp_rnks2 <- cut(gdp_cntry_mtch$Ranking, breaks=5)
	> table(gdp_cntry_mtch$gdp_rnks2, gdp_cntry_mtch$`Income.Group`) # making a table versus Income Group	
				       
### ================================Week 4 == 
	#======================Editing Text variable=======================================
	> names(cameraData)
	[1] "address"      "direction"    "street"       "crossStreet"  "intersection" "Location.1"  
	> tolower(names(cameraData))
	[1] "address"      "direction"    "street"       "crossstreet"  "intersection" "location.1"  	
	> names(cameraData) <-tolower(names(cameraData))				       
	> splitNames <- strsplit(names(cameraData),"\\.")
	> splitNames[[6]]
	[1] "Location" "1"				       
	> firstElement <- function(x){x[1]}
	> sapply(splitNames,firstElement)
	[1] "address"      "direction"    "street"       "crossstreet"  "intersection" "location" 				       
	> names(cameraData) <- sapply(splitNames,firstElement)
	> names(cameraData)
	[1] "address"      "direction"    "street"       "crossstreet"  "intersection" "location" 				       
				       
	> names(reviews)
	[1] "id"          "solution_id" "reviewer_id" "start"       "stop"        "time_left"   "accept" 				       
	
	> sub("_","", names(reviews),) # substitutes the value
	[1] "id"         "solutionid" "reviewerid" "start"      "stop"       "timeleft"   "accept"   				       
				       
	> tstNm <- "this_is_a_test"
	> sub("_","", tstNm,)
	[1] "thisis_a_test"
	> gsub("_","", tstNm,)
	[1] "thisisatest"
	
	> grep("Alameda", cameraData$intersection) #Find occurences 
	[1]  4  5 36

	> table(grep("Alameda",cameraData$intersection))
	 4  5 36 
	 1  1  1 
	
	> table(grepl("Alameda",cameraData$intersection)) #grepl --> TRUE and FALSE of occurances
	FALSE  TRUE 
	   77     3 			       
	> grep("Alameda", cameraData$intersection, value = TRUE)
	[1] "The Alameda  & 33rd St"   "E 33rd  & The Alameda"    "Harford \n & The Alameda"				       
				       
	> nm <- c("Jeffrey Leek")
	> nchar(nm)
	[1] 12			
	> paste(substr(nm,1,7),"Stark")
	[1] "Jeffrey Stark"
	> paste0(substr(nm,1,7),"Stark")
	[1] "JeffreyStark"
	> paste(substr(nm,1,7),"Stark", sep = ".")
	[1] "Jeffrey.Stark"
	> str_trim("Jeff              ")
	[1] "Jeff"				       
#=========================WK4 Quiz
	url = "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv" #Removed https as issues with download method="curl"
	> download.file(url, destfile = "hid.csv", method = "curl") # This is throwing error Status = 127
	> download.file(url, destfile = "hid.csv")				       
	> ls <- strsplit(names(hid), split = "wgtp")
	> ls[123]				       
	#Finding average of GDP
	> gdp <- read.csv("GDP_Data.csv", skip = 4, blank.lines.skip = TRUE, na.strings = "NA")
	> gdp <- gdp[,c(1,2,4,5)]
	> names(gdp) <- c("CNTRY_CD","RANK","CNTRY_NM","GDP")				       
	> gdp1 <- na.omit(gdp)
	> gdp1$GDP <- gsub(",","", gdp1$GDP)
	> gdp1[,"GDP"] <- as.numeric(as.character(gdp1$GDP))
	> mean(gdp1$GDP)
	[1] 377652.4		

	> gdp_cntry <- merge(gdp1, cntry, by.x = "CNTRY_CD", by.y = "CountryCode")
	#Finding number of records with Fiscal year end of June
	> length(grep("Fiscal year end: June*", gdp_cntry$Special.Notes))
	[1] 13				       
				       
	> install.packages("quantmod")
	> library(quantmod)
	> amzn <- getSymbols("AMZN", auto.assign = FALSE)
	> sampleTimes = index(amzn)				       
	> y <- data.frame(sampleTimes)
	#Finding records in year 2012				       
	> x <- y[format(y$sampleTimes,"%Y")=="2012",]
	> length(x)
	[1] 250		
	#Finding recording in 2012 on Mondays
	> p <- y[weekdays(y$sampleTimes)=="Monday" & format(y$sampleTimes,"%Y")=="2012",]
	> length(p)
	[1] 47				       

	#Alternate to finding recordings
	> yr2012 <- grepl("2012-*",sampleTimes)
	> s2012 <- subset(sampleTimes,yr2012)
	> day <- format(s2012,"%A")
	> table(yr2012)
	yr2012
	FALSE  TRUE 
	 2204   250 
	> table(day)
	day
	   Friday    Monday  Thursday   Tuesday Wednesday 
	       51        47        51        50        51 				       
}
