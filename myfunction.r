myfunction <- function(x) {
	y <- rnorm(100)
	mean(y)
}
#library(lubridate)-- useful date packages
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
        prd <- read.csv("SLS_STG_PRODS_Missing_In_Files_09272016.csv")
        #Retrieiving DISTINCT values of COUNTRY_CODE
        prd_cntry <- levels(prd$COUNTRY_CODE)
        #Looping over the number of countries and subsetting the data accordingly
        for (name in prd_cntry){
                #print(name)
                #Creating data frame by subsetting the data based on Country Code
                prddata <- prd[prd$COUNTRY_CODE == name,]
                #Creating a new file for country code with extension as csv
                newfile <- paste(paste(paste("SLS_STG_PRODS_Missing_In_Files",name,sep="_"),Sys.Date(), sep="_"),"csv",sep=".")
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
  
  #==============Explarotary Analysis====================
  pollution <- read.csv("avgpm25.csv", colClasses = c("numeric","character", "factor","numeric","numeric"))
  summary(pollution$pm25)
  boxplot(pollution$pm25, col = "green")
  hist(pollution$pm25, col = "purple")
  rug(pollution$pm25) # plots all point of dataset underneath 
  hist(pollution$pm25, col = "purple", breaks = 100)
  boxplot(pollution$pm25, col = "green")
  abline(h=12) # horizontal line at level 12, which is average of national level
  hist(pollution$pm25, col = "purple")
  abline(v= median(pollution$pm25), col="blue", lwd=4) # adding horizontal line; overlaying features
  #Barplot used to graphically summarize the categorical data
  barplot(table(pollution$region), col = "wheat", main = "Number of Counties in Each Region")
  #Two Dimension graphs
  hist(subset(pollution,region=="west")$pm25, col = "magenta")
  hist(subset(pollution,region=="east")$pm25, col = "pink")
 
  #scatter plot
  with(pollution, plot(latitude,pm25))
  abline(h=12, lwd=2,lty=2) # horizontal line at 12
  
  #scatter plot Using color; same as above except region
  #X-Axis:latitude; Y-Axis:pm25:Region is divided into black and Red
  with(pollution, plot(latitude,pm25, col=region)) 
  abline(h=12, lwd=2,lty=2)
  
  #Multiple sacatter plots
  par(mfrow=c(2,1), mar = c(5,4,2,1))
  with(subset(pollution,region=="west"), plot(latitude,pm25, main="west"))
  with(subset(pollution,region=="east"), plot(latitude,pm25, main="east")) 
  
  #Base Plotting system
  library(datasets)
  data("cars")
  with(cars,plot(speed,dist))
  
  library(lattice)
  state <- data.frame(state.x77, region = state.region)
  xyplot(Life.Exp ~ Income | region, data = state,layout=c(4,1))
  
  
  library(ggplot2)
  data(mpg)
  qplot(displ, hwy, data = mpg)
  hist(airquality$Ozone)
  with(airquality,plot(wind,Ozone))
  
  #BasePlotting
  with(airquality,plot(Wind,Ozone))
  airquality <- transform(airquality, Month = factor(Month))
  boxplot(Ozone ~ Month, airquality,xlab = "Month", ylab="Ozone(ppb)")
  
  with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City"))
  #After Plotting add extra features
  with(subset(airquality, Month ==5), points(Wind,Ozone, col="blue")) 
  with(subset(airquality, Month !=5), points(Wind,Ozone, col="red"))
  legend("topright",pch = 1,col = c("blue","red"), legend = c("May","Other Months"))
  
  #pch is plotting symbol
  with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", pch=20))
  #lm is used for linear model
  model <- lm(Ozone ~ Wind, airquality)
  #Adding straight regression lines to a plot; lwd is width of line
  abline(model, lwd=2)
  
  #Multiple Plot
  par(mfrow = c(1,2)) # setting the parameter for one row two columns
  #Everytime plot is called,new plot is created
  with(airquality, {
    + plot(Wind, Ozone, main = "Ozone and Wind") 
    + plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
    + })
  
  #Adding three plot; oma is outer marging, mar is margin
  par(mfrow = c(1,3), mar = c(4,4,2,1), oma=c(0,0,2,0))
  with(airquality, {
    + plot(Wind, Ozone, main = "Ozone and Wind")
    + plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
    + plot(Temp, Ozone, main = "Ozone and Temp")
    + mtext("Ozone and Weather in New York City", outer = TRUE)
    + })
  
  x <- rnorm(100)
  > hist(x)
  > y <-rnorm(100)
  > plot(x,y)
  
  plot(x,y, xlab = "Weight", ylab = "Height", main ="Scatter Plot", pch=20)
  > legend("topleft", legend = "Data",pch=19)
  > fit <-lm(x ~ y)
  > abline(fit, lwd=3,col="red")
  > fit <-lm(y ~ x)
  > abline(fit, lwd=3,col="red")
  > abline(fit, lwd=3,col="red")
  > plot(x,y, xlab = "Weight", ylab = "Height", main ="Scatter Plot", pch=20)
  > legend("topleft", legend = "Data",pch=19)
  > fit <-lm(y ~ x)
  > abline(fit, lwd=1,col="purple")
  
  z <- rpois(100,2)
  > par(mfrow = c(2,1))
  > plot(x,y)
  > plot(x,z)
  
  > par(mar = c(4,4,1,1))
  > plot(x,y)
  > plot(x,z)
  
  > par(mar = c(2,2,1,1))
  > plot(x,y, pch=20)
  > plot(x,z, pch = 24)
  
  > par(mar = c(4,4,1,1))
  > plot(x,y, pch=20)
  > plot(x,z, pch = 24)
  
  > par(mar = c(4,4,2,2))
  > plot(x,y, pch=20)
  > plot(x,z, pch = 24)
  #Adding two rows and two columns
  > par(mfrow = c(2,2))
  > plot(x,y, pch=20)
  > plot(x,z, pch = 24)
  > plot(y,z, pch = 24)
  > plot(y,x, pch = 24)
  
  #Plotting by grouping
  > x <- rnorm(100)
  > y <- x + rnorm(100)
  > g <- gl(2,50)
  > g <- gl(2,50, labels = c("Male", "Female")) #Generate Factor levels
  #Plotting only region without data
  > plot(x,y, type ="n")
  #points function used to put data
  > points(x[g =="Male"], y[g=="Male"], col ="blue")
  > points(x[g =="Female"], y[g=="Female"], col ="pink", pch=25)
  
  #creating a file for plot
  > library(datasets)
  > pdf(file = "myplot.pdf")
  > with(faithful, plot(eruptions, waiting))
  > title(main = "Old Faithful Geyser Data")
  > dev.off()
  
  #================Lattice Plotting System===============
  library(lattice)
  > library(datasets)
  > xyplot(Ozone ~ Wind, data = airquality) #Relationship between Ozone and Wind
  # Same as above but factored against Month
  xyplot(Ozone ~ Wind | factor(Month), data = airquality, layout=c(5,1))
  
  p <- xyplot(Ozone ~ Wind , data = airquality)
  > print(p)
  
  
  #multi panel
  > set.seed(10)
  > x <- rnorm(100)
  > y <- rnorm(100)
  > f <- rep(0:1, each = 50)
  > y <- x + f - f * x + rnorm(100, sd = 0.5)
  > f <- factor(f, labels = c("Group 1", "Group 2"))
  > xyplot(y ~ x | f, layout = c(2,1))
  
  xyplot(y ~ x | f, panel = function(x, y, ...) {
    panel.xyplot(x, y, ...) ## First call the default panel function for 'xyplot'
    panel.abline(h = median(y), lty = 2) ## Add a horizontal line at the median
  })
  
  xyplot(y ~ x | f, panel = function(x, y, ...) {
    panel.xyplot(x, y, ...) ## First call default panel function
    panel.lmline(x, y, col = 2) ## Overlay a simple linear regression line
  })

  #==============ggplot===========================
  library(ggplot2)
  > str(mpg)
  > qplot(displ,hwy, data = mpg)
  > qplot(displ,hwy, data = mpg, color = drv)
  > qplot(displ,hwy, data = mpg, geom = c("point","smooth"))
  #Histogram
  qplot(hwy, data = mpg)
  qplot(hwy, data = mpg, fill =drv) # Histogram by Group
  
  qplot(displ,hwy, data = mpg, facets=.~drv)
  qplot(displ,hwy, data = mpg, facets=drv~.)
  #---   ~. defining the columns
  #Download "maacs.Rda" Raw data file from https://github.com/TarekDib03/ExploratoryDataAnalysisCoursera
  > load("maacs.Rda")
  > str(maacs)
  
  
  #Histogram
  qplot(log(eno), data = maacs)
  # Histogram by Group
  qplot(log(eno), data = maacs, fill=mopos)
  
  
  # Histogram with smooth graph
  qplot(log(eno), data = maacs, geom="density")
  # Histogram with smooth graph by Group
  qplot(log(eno), data = maacs, geom="density", color = mopos) 

  #Scattered Plot
  qplot(log(pm25),log(eno), data = maacs)
  #scattered Plot, by seperating using shapes
  qplot(log(pm25),log(eno), data = maacs, shape=mopos)
  #scattered Plot, by seperating using color
  qplot(log(pm25),log(eno), data = maacs, color=mopos)
 
  #smoothing the relationship between groups by using std linear regression model
  qplot(log(pm25),log(eno), data = maacs, color=mopos) + geom_smooth(method="lm")
  
  #Same as above but splitting the graphs using facets
  qplot(log(pm25),log(eno), data = maacs, facets=.~mopos) + geom_smooth(method="lm")
  
}
