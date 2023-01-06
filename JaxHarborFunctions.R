########################### FILE CREATION ##################################

#runStatistics - create statistics for a dataframe.
#Created by KLA on 05/16/2018.
#Last update by KLA on 06/05/2018.
#Parameters: instanious values dataframe, daily values dataframe
#Returns: dataframe.
#Status: n tested.
runStatistics<-function(dfiv,dfdv,periodValue,period)
{
  #################
  # rm(list=ls())
  # library("dataRetrieval")
  # library("Rmisc")
  # library("xlsx")
  # library("ReporteRs")
  # source("TestUSGS.R")
  # 
  # site_id <- c("301124081395901")
  # parameterCd <- c("00095","00480","00010")
  # startDate <- "2016-01-01"
  # endDate <- "2016-12-31"
  # period <- 2
  # tz <- "EST"
  # i <- 1
  # dfiv <- readNWISuv(site_id,parameterCd,paste(startDate,"T00:00",sep=""),paste(endDate,"T23:59",sep=""),tz=getTimeZone(tz))
  # dfdv <- readNWISdv(site_id,parameterCd,startDate,endDate)
  # sYear <- as.numeric(substr(startDate, start = 1, stop = 4))
  # sMonth <- as.numeric(substr(startDate, start = 6, stop = 7))
  ####################
  
  Interval <- c()
  
  #IV Salinity from Conductance.
  ivCst <- (6.859819E-38*(dfiv$X_TOP_00095_00000)^8 + -2.027297E-32*(dfiv$X_TOP_00095_00000)^7 + 2.576767E-27*(dfiv$X_TOP_00095_00000)^6 + -1.855032E-22*(dfiv$X_TOP_00095_00000)^5 + 8.450662E-18*(dfiv$X_TOP_00095_00000)^4 + -0.0000000000002616744*(dfiv$X_TOP_00095_00000)^3 + 0.000000007096135*(dfiv$X_TOP_00095_00000)^2 + 0.0005161311*(dfiv$X_TOP_00095_00000) + -0.05028803)
  ivCsb <- (6.859819E-38*(dfiv$X_BOTTOM_00095_00000)^8 + -2.027297E-32*(dfiv$X_BOTTOM_00095_00000)^7 + 2.576767E-27*(dfiv$X_BOTTOM_00095_00000)^6 + -1.855032E-22*(dfiv$X_BOTTOM_00095_00000)^5 + 8.450662E-18*(dfiv$X_BOTTOM_00095_00000)^4 + -0.0000000000002616744*(dfiv$X_BOTTOM_00095_00000)^3 + 0.000000007096135*(dfiv$X_BOTTOM_00095_00000)^2 + 0.0005161311*(dfiv$X_BOTTOM_00095_00000) + -0.05028803)
  
  #DV Salinity from Conductance.
  dvCst <- (6.859819E-38*(dfdv$X_TOP_00095_00003)^8 + -2.027297E-32*(dfdv$X_TOP_00095_00003)^7 + 2.576767E-27*(dfdv$X_TOP_00095_00003)^6 + -1.855032E-22*(dfdv$X_TOP_00095_00003)^5 + 8.450662E-18*(dfdv$X_TOP_00095_00003)^4 + -0.0000000000002616744*(dfdv$X_TOP_00095_00003)^3 + 0.000000007096135*(dfdv$X_TOP_00095_00003)^2 + 0.0005161311*(dfdv$X_TOP_00095_00003) + -0.05028803)
  dvCsb <- (6.859819E-38*(dfdv$X_BOTTOM_00095_00003)^8 + -2.027297E-32*(dfdv$X_BOTTOM_00095_00003)^7 + 2.576767E-27*(dfdv$X_BOTTOM_00095_00003)^6 + -1.855032E-22*(dfdv$X_BOTTOM_00095_00003)^5 + 8.450662E-18*(dfdv$X_BOTTOM_00095_00003)^4 + -0.0000000000002616744*(dfdv$X_BOTTOM_00095_00003)^3 + 0.000000007096135*(dfdv$X_BOTTOM_00095_00003)^2 + 0.0005161311*(dfdv$X_BOTTOM_00095_00003) + -0.05028803)
  
  #IV Salinity from USGS.
  ivSt <- dfiv$X_TOP_00480_00000
  ivSb <- dfiv$X_BOTTOM_00480_00000
  
  #DV Salinity from USGS.
  dvSt <- dfdv$X_TOP_00480_00003
  dvSb <- dfdv$X_BOTTOM_00480_00003
  
  #IV Salinity from Conductance and temperature.
  ivCtst <- (6.859819E-38*(dfiv$X_TOP_00095_00000)^8 + -2.027297E-32*(dfiv$X_TOP_00095_00000)^7 + 2.576767E-27*(dfiv$X_TOP_00095_00000)^6 + -1.855032E-22*(dfiv$X_TOP_00095_00000)^5 + 8.450662E-18*(dfiv$X_TOP_00095_00000)^4 + -0.0000000000002616744*(dfiv$X_TOP_00095_00000)^3 + 0.000000007096135*(dfiv$X_TOP_00095_00000)^2 + 0.0005161311*(dfiv$X_TOP_00095_00000) + -0.05028803)
  ivCtsb <- (6.859819E-38*(dfiv$X_BOTTOM_00095_00000)^8 + -2.027297E-32*(dfiv$X_BOTTOM_00095_00000)^7 + 2.576767E-27*(dfiv$X_BOTTOM_00095_00000)^6 + -1.855032E-22*(dfiv$X_BOTTOM_00095_00000)^5 + 8.450662E-18*(dfiv$X_BOTTOM_00095_00000)^4 + -0.0000000000002616744*(dfiv$X_BOTTOM_00095_00000)^3 + 0.000000007096135*(dfiv$X_BOTTOM_00095_00000)^2 + 0.0005161311*(dfiv$X_BOTTOM_00095_00000) + -0.05028803)
  
  #DV Salinity from Conductance and temperature.
  dvCtst <- (6.859819E-38*(dfdv$X_TOP_00095_00003)^8 + -2.027297E-32*(dfdv$X_TOP_00095_00003)^7 + 2.576767E-27*(dfdv$X_TOP_00095_00003)^6 + -1.855032E-22*(dfdv$X_TOP_00095_00003)^5 + 8.450662E-18*(dfdv$X_TOP_00095_00003)^4 + -0.0000000000002616744*(dfdv$X_TOP_00095_00003)^3 + 0.000000007096135*(dfdv$X_TOP_00095_00003)^2 + 0.0005161311*(dfdv$X_TOP_00095_00003) + -0.05028803)
  dvCtsb <- (6.859819E-38*(dfdv$X_BOTTOM_00095_00003)^8 + -2.027297E-32*(dfdv$X_BOTTOM_00095_00003)^7 + 2.576767E-27*(dfdv$X_BOTTOM_00095_00003)^6 + -1.855032E-22*(dfdv$X_BOTTOM_00095_00003)^5 + 8.450662E-18*(dfdv$X_BOTTOM_00095_00003)^4 + -0.0000000000002616744*(dfdv$X_BOTTOM_00095_00003)^3 + 0.000000007096135*(dfdv$X_BOTTOM_00095_00003)^2 + 0.0005161311*(dfdv$X_BOTTOM_00095_00003) + -0.05028803)
  
  #List of tables for expecific period.
  dfList <- list(dvCst,ivCst,dvCsb,ivCsb,dvSt,ivSt,dvSb,ivSb,dvCtst,ivCtst,dvCtsb,ivCtsb)
  
  #return list of calculations done for a expecific period (dfList).
  rList <- list()
  
  #Return format
  #Period Interval	Records	Stdev	Mean	Median	Min	Max	Q.05	Q.10	Q.25	Q.50	Q.75	Q.90	Q.95	Ci.90	Ci.95
  
  for(i in 1:(length(dfList)))
  {
    options(warn=-1)#remove warnings when period has all values NA's
    
    Stdev <- sd(dfList[[i]],na.rm = TRUE)
    Mean <- mean(dfList[[i]],na.rm = TRUE)
    Min <- min(dfList[[i]],na.rm = TRUE)
    Max <- max(dfList[[i]],na.rm = TRUE)
    Median <- median(dfList[[i]],na.rm = TRUE)
    qv <- quantile(dfList[[i]],probs=c(0.05,0.10,0.25,0.50,0.75,0.90,0.95),na.rm = TRUE)
    Q.05 <- qv["5%"]
    Q.10 <- qv["10%"]
    Q.25 <- qv["25%"]
    Q.50 <- qv["50%"]
    Q.75 <- qv["75%"]
    Q.90 <- qv["90%"]
    Q.95 <- qv["95%"]
    
    #types of ci: "upper", "mean", "lower"
    ciType <- "upper"
    ci90v <- CI(na.omit(dfList[[i]]),ci=0.90)[ciType]
    ci95v <- CI(na.omit(dfList[[i]]),ci=0.95)[ciType]
    
    rNotUsed <- sum(is.na(dfList[[i]]))
    rUsed <- sum(!is.na(dfList[[i]]))
    rTotal <- rUsed + rNotUsed
    Records <- paste(rTotal,"/",rUsed,"/",rNotUsed,sep="")
    
    options(warn=0)#enable warnings
    
    if(i%%2 == 0)
    {#Even numbers
      Interval <- getIntervalNumber(length(dfList[[i]]),period)
      tempVector <- c(" ", Interval, Records,Stdev,Mean,Min,Max,Median,qv["5%"],qv["10%"],qv["25%"],qv["50%"],qv["75%"],qv["90%"],qv["95%"],ci90v,ci95v)
    }else#Odd numbers
    {
      Interval <- "Daily"
      tempVector <- c(periodValue, Interval, Records,Stdev,Mean,Median,Min,Max,qv["5%"],qv["10%"],qv["25%"],qv["50%"],qv["75%"],qv["90%"],qv["95%"],ci90v,ci95v)
    }
    names(tempVector) <- c("Period","Interval","Records(Tot/Used/N-Used)","Stdev","Mean","Median","Min","Max","Q.05","Q.10","Q.25","Q.50","Q.75","Q.90","Q.95","Ci.90","Ci.95")
    
    #List of Vectors (each vector is a calculated time period).
    rList <- list.append(rList, tempVector)
  }
  return(rList)
}
# #runStatistics - create statistics for a dataframe.
# #Created by KLA on 05/16/2018.
# #Last update by KLA on 06/01/2018.
# #Parameters: instanious values dataframe, daily values dataframe
# #Returns: dataframe.
# #Status: n tested.
# runStatistics<-function(dfiv,dfdv)
# {
#   #################
#   # rm(list=ls())
#   # library("dataRetrieval")
#   # library("Rmisc")
#   # library("xlsx")
#   # library("ReporteRs")
#   # source("TestUSGS.R")
#   # 
#   # site_id <- c("301124081395901")
#   # parameterCd <- c("00095","00480","00010")
#   # startDate <- "2016-01-01"
#   # endDate <- "2016-12-31"
#   # period <- 4
#   # tz <- "EST"
#   # #i <- 1
#   # dfiv <- readNWISuv(site_id,parameterCd,paste(startDate,"T00:00",sep=""),paste(endDate,"T23:59",sep=""),tz=getTimeZone(tz))
#   # dfdv <- readNWISdv(site_id,parameterCd,startDate,endDate)
#   # sYear <- as.numeric(substr(startDate, start = 1, stop = 4))
#   # sMonth <- as.numeric(substr(startDate, start = 6, stop = 7))
#   ####################
#   
#   Interval <- c()
#   
#   #IV Salinity from Conductance.
#   ivCst <- (6.859819E-38*(dfiv$X_TOP_00095_00000)^8 + -2.027297E-32*(dfiv$X_TOP_00095_00000)^7 + 2.576767E-27*(dfiv$X_TOP_00095_00000)^6 + -1.855032E-22*(dfiv$X_TOP_00095_00000)^5 + 8.450662E-18*(dfiv$X_TOP_00095_00000)^4 + -0.0000000000002616744*(dfiv$X_TOP_00095_00000)^3 + 0.000000007096135*(dfiv$X_TOP_00095_00000)^2 + 0.0005161311*(dfiv$X_TOP_00095_00000) + -0.05028803)
#   ivCsb <- (6.859819E-38*(dfiv$X_BOTTOM_00095_00000)^8 + -2.027297E-32*(dfiv$X_BOTTOM_00095_00000)^7 + 2.576767E-27*(dfiv$X_BOTTOM_00095_00000)^6 + -1.855032E-22*(dfiv$X_BOTTOM_00095_00000)^5 + 8.450662E-18*(dfiv$X_BOTTOM_00095_00000)^4 + -0.0000000000002616744*(dfiv$X_BOTTOM_00095_00000)^3 + 0.000000007096135*(dfiv$X_BOTTOM_00095_00000)^2 + 0.0005161311*(dfiv$X_BOTTOM_00095_00000) + -0.05028803)
#   
#   #DV Salinity from Conductance.
#   dvCst <- (6.859819E-38*(dfdv$X_TOP_00095_00003)^8 + -2.027297E-32*(dfdv$X_TOP_00095_00003)^7 + 2.576767E-27*(dfdv$X_TOP_00095_00003)^6 + -1.855032E-22*(dfdv$X_TOP_00095_00003)^5 + 8.450662E-18*(dfdv$X_TOP_00095_00003)^4 + -0.0000000000002616744*(dfdv$X_TOP_00095_00003)^3 + 0.000000007096135*(dfdv$X_TOP_00095_00003)^2 + 0.0005161311*(dfdv$X_TOP_00095_00003) + -0.05028803)
#   dvCsb <- (6.859819E-38*(dfdv$X_BOTTOM_00095_00003)^8 + -2.027297E-32*(dfdv$X_BOTTOM_00095_00003)^7 + 2.576767E-27*(dfdv$X_BOTTOM_00095_00003)^6 + -1.855032E-22*(dfdv$X_BOTTOM_00095_00003)^5 + 8.450662E-18*(dfdv$X_BOTTOM_00095_00003)^4 + -0.0000000000002616744*(dfdv$X_BOTTOM_00095_00003)^3 + 0.000000007096135*(dfdv$X_BOTTOM_00095_00003)^2 + 0.0005161311*(dfdv$X_BOTTOM_00095_00003) + -0.05028803)
#   
#   #IV Salinity from USGS.
#   ivSt <- dfiv$X_TOP_00480_00000
#   ivSb <- dfiv$X_BOTTOM_00480_00000
#   
#   #DV Salinity from USGS.
#   dvSt <- dfdv$X_TOP_00480_00003
#   dvSb <- dfdv$X_BOTTOM_00480_00003
#   
#   #IV Salinity from Conductance and temperature.
#   ivCtst <- (6.859819E-38*(dfiv$X_TOP_00095_00000)^8 + -2.027297E-32*(dfiv$X_TOP_00095_00000)^7 + 2.576767E-27*(dfiv$X_TOP_00095_00000)^6 + -1.855032E-22*(dfiv$X_TOP_00095_00000)^5 + 8.450662E-18*(dfiv$X_TOP_00095_00000)^4 + -0.0000000000002616744*(dfiv$X_TOP_00095_00000)^3 + 0.000000007096135*(dfiv$X_TOP_00095_00000)^2 + 0.0005161311*(dfiv$X_TOP_00095_00000) + -0.05028803)
#   ivCtsb <- (6.859819E-38*(dfiv$X_BOTTOM_00095_00000)^8 + -2.027297E-32*(dfiv$X_BOTTOM_00095_00000)^7 + 2.576767E-27*(dfiv$X_BOTTOM_00095_00000)^6 + -1.855032E-22*(dfiv$X_BOTTOM_00095_00000)^5 + 8.450662E-18*(dfiv$X_BOTTOM_00095_00000)^4 + -0.0000000000002616744*(dfiv$X_BOTTOM_00095_00000)^3 + 0.000000007096135*(dfiv$X_BOTTOM_00095_00000)^2 + 0.0005161311*(dfiv$X_BOTTOM_00095_00000) + -0.05028803)
#   
#   #DV Salinity from Conductance and temperature.
#   dvCtst <- (6.859819E-38*(dfdv$X_TOP_00095_00003)^8 + -2.027297E-32*(dfdv$X_TOP_00095_00003)^7 + 2.576767E-27*(dfdv$X_TOP_00095_00003)^6 + -1.855032E-22*(dfdv$X_TOP_00095_00003)^5 + 8.450662E-18*(dfdv$X_TOP_00095_00003)^4 + -0.0000000000002616744*(dfdv$X_TOP_00095_00003)^3 + 0.000000007096135*(dfdv$X_TOP_00095_00003)^2 + 0.0005161311*(dfdv$X_TOP_00095_00003) + -0.05028803)
#   dvCtsb <- (6.859819E-38*(dfdv$X_BOTTOM_00095_00003)^8 + -2.027297E-32*(dfdv$X_BOTTOM_00095_00003)^7 + 2.576767E-27*(dfdv$X_BOTTOM_00095_00003)^6 + -1.855032E-22*(dfdv$X_BOTTOM_00095_00003)^5 + 8.450662E-18*(dfdv$X_BOTTOM_00095_00003)^4 + -0.0000000000002616744*(dfdv$X_BOTTOM_00095_00003)^3 + 0.000000007096135*(dfdv$X_BOTTOM_00095_00003)^2 + 0.0005161311*(dfdv$X_BOTTOM_00095_00003) + -0.05028803)
#   
#   
#   dfList <- list(dvCst,ivCst,dvCsb,ivCsb,dvSt,ivSt,dvSb,ivSb,dvCtst,ivCtst,dvCtsb,ivCtsb)
#   
#   #return list of calculations done for a expecific period.
#   
#   #Return format
#   #Interval	Records	Stdev	Mean	Median	Min	Max	Q.05	Q.10	Q.25	Q.50	Q.75	Q.90	Q.95	Ci.90	Ci.95
#   
#   for(i in 1:(length(dfList)))
#   {
#     sdv <- sd(dfList[[i]],na.rm = TRUE)
#     meanv <- mean(dfList[[i]],na.rm = TRUE)
#     minv <- min(dfList[[i]],na.rm = TRUE)
#     maxv <- max(dfList[[i]],na.rm = TRUE)
#     medianv <- median(dfList[[i]],na.rm = TRUE)
#     qv <- quantile(dfList[[i]],probs=c(0.05,0.10,0.25,0.50,0.75,0.90,0.95),na.rm = TRUE)
#     ci90v <- CI(na.omit(dfList[[i]]),ci=0.90)
#     ci95v <- CI(na.omit(dfList[[i]]),ci=0.95)
#     
#     rNotUsed <- sum(is.na(dfList[[i]]))
#     rUsed <- sum(!is.na(dfList[[i]]))
#     rTotal <- rUsed + rNotUsed
#     sRecords <- paste(rTotal,"/",rUsed,"/",rNotUsed,sep="")
#     
#     if(i == 1)
#     {
#       Records <- c(sRecords)
#       Stdev <- c(sdv)
#       Mean <- c(meanv)
#       Median <- c(medianv)
#       Min <- c(minv)
#       Max <- c(maxv)
#       Q.05 <- c(qv["5%"])
#       Q.10 <- c(qv["10%"])
#       Q.25 <- c(qv["25%"])
#       Q.50 <- c(qv["50%"])
#       Q.75 <- c(qv["75%"])
#       Q.90 <- c(qv["90%"])
#       Q.95 <- c(qv["95%"])
#       Ci.90 <- c(ci90v["upper"])
#       Ci.95 <- c(ci95v["upper"])
#     }else
#     {
#       Records <- c(Records,sRecords)
#       Stdev <- c(Stdev,sdv)
#       Mean <- c(Mean,meanv)
#       Median <- c(Median,medianv)
#       Min <- c(Min,minv)
#       Max <- c(Max,maxv)
#       Q.05 <- c(Q.05,qv["5%"])
#       Q.10 <- c(Q.10,qv["10%"])
#       Q.25 <- c(Q.25,qv["25%"])
#       Q.50 <- c(Q.50,qv["50%"])
#       Q.75 <- c(Q.75,qv["75%"])
#       Q.90 <- c(Q.90,qv["90%"])
#       Q.95 <- c(Q.95,qv["95%"])
#       Ci.90 <- c(Ci.90,ci90v["upper"])
#       Ci.95 <- c(Ci.95,ci95v["upper"])
#     }
#     
#     if(i%%2 == 0)
#     {#Even numbers
#       Interval <- c(Interval,paste("IV ",getIntervalNumber(length(dfList[[i]])),sep=""))
#     }else#Odd numbers
#     {
#       Interval <- c(Interval,"Daily")
#     }
#   }
#   #List of Vectors.
#   rList <- list(Interval,Records,Stdev,Mean,Median,Min,Max,Q.05,Q.10,Q.25,Q.50,Q.75,Q.90,Q.95,Ci.90,Ci.95)
#   
#   return(rList)
# } 

#GenerateYearStatisticsDocx - create a docx file with statistics information.
#Created by KLA on 05/16/2018.
#Last update by KLA on 05/31/2018.
#Parameters: filename(character),site_id(character or vector),
#            parameterCd(character or vector),startDate(character),
#            tz(character),period(numeric)
#Returns: nothing.
#Status: tested.
GenerateYearStatisticsDocx<-function(filename,site_id,parameterCd,startDate,tz,period)
{
  #setwd("D:/R/Jax Harbor/R")
  # rm(list=ls())
  # source("JaxHarborChart.R")
  # library("ReporteRs")
  # name <- "Jax Harbor Deepening"
  # year <- 2018
  # period <- 2
  # varname <- "Calculated Salinity"
  # site <- "301124081395901"
  # sitename <- "Buckman Bridge"
  # dbname <- "USGS"
  
  startYear <- as.numeric(substr(startDate, start = 1, stop = 4))
  startMonth <- as.numeric(substr(startDate, start = 6, stop = 7))
  startDate <- paste(startYear,"-",dateFormat(startMonth,0),"-01",sep="")
  
  if(monthAddition(startMonth,11) < startMonth)#sets endDate to a year from the start date.
  {
    endMonth <- monthAddition(startMonth,11)
    endYear <- startYear + 1
    endDate <- paste(endYear,"-",dateFormat(endMonth,0),"-",getmonthEndDay(endMonth,endYear),sep="")
  }else
  {
    endMonth <- monthAddition(startMonth,11)
    endYear <- startYear
    endDate <- paste(endYear,"-",dateFormat(endMonth,0),"-",getmonthEndDay(endMonth,endYear),sep="")
  }
  
  dfiv <- readNWISuv(site_id,parameterCd,paste(startDate,"T00:00",sep=""),paste(endDate,"T23:59",sep=""),tz=getTimeZone(tz))
  dfdv <- readNWISdv(site_id,parameterCd,startDate,endDate,statCd = )
  
  listofSix <- createStatistics(dfiv,dfdv,startDate,endDate,period,startMonth,startYear)
  
  
  # Create a Word document.
  extension <- ".docx"
  doc <- docx(title="Generated Statistics")
  
  # Add titles for first page.
  options( "ReporteRs-fontsize" = 24 )
  options( "ReporteRs-fontweigh" = "bold" )
  doc <- addParagraph(doc,paste(filename," ",getPeriod(period)," Statistics ",sep=""),par.properties = chprop( parProperties(), text.align = "center" ))
  doc <- addParagraph(doc,paste("for ",getSiteName(site_id),sep=""),par.properties = chprop( parProperties(), text.align = "center" ))
  doc <- addParagraph(doc,paste("from ",getMonthCharacter(startMonth)," 1, ",startYear," - ",getMonthCharacter(endMonth)," ",getmonthEndDay(endMonth,endYear),", ",endYear,sep=""),par.properties = chprop( parProperties(), text.align = "center" ))
  
  doc <- addParagraph(doc, paste("Generated on: ",Sys.Date(),sep=""), par.properties = parProperties(text.align = "center"))
  doc <- addPageBreak(doc)
  options( "ReporteRs-fontsize" = 12 )
  
  # Add  a table
  doc <- addSection(doc, landscape = TRUE)
  doc <- addTitle(doc, paste(getPeriod(period)," Statistics for ","varname",sep=""), level=2)
  
  for(i in 1:(length(listofSix)))
  {
    doc <- addParagraph(doc, paste("Database: ","USGS","   Site: ",site_id,"   ",names(listofSix)[i],sep=""))
    options( "ReporteRs-fontsize" = 8.5 )
    data <- light.table(listofSix[[i]])
    data <- setFlexTableWidths(data, c(.6,0.7,1.5,.5,.5,.6,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5))
    doc <- addFlexTable(doc,data,par.properties=parCenter())
    options( "ReporteRs-fontsize" = 12 )
    
  }
  
  #options( "ReporteRs-fontsize" = 12 )  
  doc <- addSection(doc, landscape = FALSE)
  #options( "ReporteRs-fontsize" = 12 )
  
  # # Add a plot into the Word document
  # doc <- addTitle(doc, "Grafic")
  # doc <- addPlot(doc, function() barplot(1:5, col=1:5))
  # 
  # # Add a Image into the Word document
  # doc <- addTitle(doc, "Image Test", level=2)
  # doc <- addImage(doc,"testimage.jpg",width = 5, height = 3.5)
  
  # Write the Word document to a file
  writeDoc(doc, file = getFileName(paste(filename," ",getPeriod(period)," Statistics for ",getSiteName(site_id)," from ",startYear,"-",endYear,sep=""),extension))
  print("DONE.")
}

#generateXlsxFile - create an Excel document file with USGS information.
#Created by KLA on 05/30/2018.
#Last update by KLA on 05/30/2018.
#Parameters: filename(character),site_id(character or vector),
#            parameterCd(character or vector),startDate(character),endDate(character),
#            tz(character)
#Returns: nothing.
#Status: tested.
generateXlsxFile<-function(filename,site_id,parameterCd,startDate,endDate,tz)
{
  setwd("D:/R/Jax Harbor/R")
  extension = ".xlsx"
  
  dfiv <- readNWISuv(site_id,parameterCd,paste(startDate,"T00:00",sep=""),paste(endDate,"T23:59",sep=""),tz=getTimeZone(tz))
  dfdv <- readNWISdv(site_id,parameterCd,startDate,endDate,statCd = )
  
  filename <- getFileName(filename,extension)
  wb = createWorkbook()
  
  sheet1 = xlsx::createSheet(wb, "IV")
  xlsx::addDataFrame(dfiv, sheet=sheet1, startColumn=1, row.names=FALSE)
  
  sheet2 = xlsx::createSheet(wb, "DV")
  xlsx::addDataFrame(dfdv, sheet=sheet2, startColumn=1, row.names=FALSE)
  
  xlsx::saveWorkbook(wb, filename)
  print((paste("DONE.")))
}

#createStatistics - Generate a statistics data frame.
#Created by KLA on 05/30/2018.
#Last update by KLA on 06/01/2018.
#Parameters: instantanius/daily value dataframe(character).
#Returns: dataframe.
#Status: n tested.
createStatistics<-function(dfiv,dfdv,startDate,endDate,period,sMonth,sYear)
{
  #################
  # rm(list=ls())
  # setwd("D:/R/Jax Harbor/R")
  # library("dataRetrieval")
  # library("Rmisc")
  # library("xlsx")
  # library("ReporteRs")
  # library("rlist")
  # source("TestUSGS.R")
  # 
  # site_id <- c("301124081395901")
  # parameterCd <- c("00095","00480","00010")
  # startDate <- "2016-01-01"
  # endDate <- "2016-12-31"
  # period <- 4
  # tz <- "EST"
  # dfiv <- readNWISuv(site_id,parameterCd,paste(startDate,"T00:00",sep=""),paste(endDate,"T23:59",sep=""),tz=getTimeZone(tz))
  # dfdv <- readNWISdv(site_id,parameterCd,startDate,endDate)
  # sYear <- as.numeric(substr(startDate, start = 1, stop = 4))
  # sMonth <- as.numeric(substr(startDate, start = 6, stop = 7))
  ####################
  
  #Period <- getPeriodData(period,sMonth)#returns a vector for all the months/weeks/seasons or annual.
  sList <- list()
  
  if(period == 1)#weekly - tested.
  {
    startDate <- as.Date(startDate)
    
    for (i in 1:(getIntervalData(period)))#create data frame for period.
    {
      eDate1 <- startDate+(7*(i-1))
      
      if(i == getIntervalData(period))
      {
        eDate2 <- endDate
      }else
      {
        eDate2 <- startDate+(7*i-1)
      }
      
      mdfiv <- dfiv[dfiv$dateTime >= paste(eDate1," 00:00",sep="") &
                      dfiv$dateTime <= paste(eDate2," 23:59",sep=""),]
      mdfdv <- dfdv[dfdv$Date >= eDate1 &
                      dfdv$Date <= eDate2,]
      
      print(paste(eDate1," 00:00 TO ",eDate2," 23:59",sep=""))
      
      sList <- list.append(sList, runStatistics(mdfiv,mdfdv,paste("Week #",i,sep=""),period))
    }
  }else if(period == 2)#Monthly - tested.
  {
    for (i in 1:(getIntervalData(period)))#create data frame for period.
    {
      mdfiv <- dfiv[dfiv$dateTime >= paste(sYear,"-",dateFormat(sMonth,0),"-01 00:00",sep="") &
                      dfiv$dateTime <= paste(sYear,"-",dateFormat(sMonth,0),"-",getmonthEndDay(sMonth,sYear)," 23:59",sep=""),]
      mdfdv <- dfdv[dfdv$Date >= paste(sYear,"-",dateFormat(sMonth,0),"-01 00:00",sep="") &
                      dfdv$Date <= paste(sYear,"-",dateFormat(sMonth,0),"-",getmonthEndDay(sMonth,sYear)," 23:59",sep=""),]
      
      print(paste(sYear,"-",dateFormat(sMonth,0),"-01 00:00 TO ",sYear,"-",dateFormat(sMonth,0),"-",getmonthEndDay(sMonth,sYear)," 23:59",sep=""))
      
      #if month reaches december (12); next month will be january(1) and year offset is +1.
      if(monthAddition(sMonth,1) < sMonth)
      {
        sYear <- sYear + 1
      }
      sMonth <- monthAddition(sMonth,1)
      
      sList <- list.append(sList, runStatistics(mdfiv,mdfdv,getMonthCharacter(i),period))
    }
    
  }else if (period == 3)#seasonal - tested.
  {
    sYear1 <- NULL
    for (i in 1:(getIntervalData(period)))#create data frame for period.
    {
      sMonth1 <- monthAddition(sMonth,3)
      #if month reaches december (12); next month will be january(1) and year offset is +1.
      if(monthAddition(sMonth,3) < sMonth)
      {
        sYear1 <- sYear + 1
      }else
      {
        if(is.null(sYear1))#this is to prevent using previous year when sYear1 gets updated.
        {
          sYear1 <- sYear
        }else
        {
          sYear <- sYear1
        }
      }
      mdfiv <- dfiv[dfiv$dateTime >= paste(sYear,"-",dateFormat(sMonth,0),"-01 00:00",sep="") &
                      dfiv$dateTime <= paste(sYear1,"-",dateFormat(sMonth1,0),"-",getmonthEndDay(sMonth1,sYear1)," 23:59",sep=""),]
      mdfdv <- dfdv[dfdv$Date >= paste(sYear,"-",dateFormat(sMonth,0),"-01 00:00",sep="") &
                      dfdv$Date <= paste(sYear1,"-",dateFormat(sMonth1,0),"-",getmonthEndDay(sMonth1,sYear1)," 23:59",sep=""),]
      
      print(paste(sYear,"-",dateFormat(sMonth,0),"-01 00:00 TO ",
                  sYear1,"-",dateFormat(sMonth1,0),"-",getmonthEndDay(sMonth1,sYear1)," 23:59",sep=""))
      
      m1 <- substr(getMonthCharacter(sMonth), start = 1, stop = 3)
      m2 <- substr(getMonthCharacter(sMonth1), start = 1, stop = 3)
      
      sList <- list.append(sList, runStatistics(mdfiv,mdfdv,paste(m1,"-",m2,sep=""),period))
      
      sMonth <- monthAddition(sMonth1,1)
      # 
      
      # 
      # rList <- list()
      # n<-0
      # m<-0
      # for(i in 1:(length(sList[[1]])/2))
      # {
      #   tempDf <- data.frame(Period)
      #   for(j in 1:(length(sList)))
      #   {
      #     tempDf <- data.frame(tempDf,sList[[j]][(1+n):(2+m)])
      #     j<-j+1
      #   }
      #   #round decimals to four spaces.
      #   is.num <- sapply(tempDf, is.numeric)
      #   tempDf[is.num] <- lapply(tempDf[is.num], round, 3)
      #   
      #   rList <- list.append(rList, tempDf)
      #   n<-n+2
      #   m<-m+2
      # }
    }
    
  }else#annually - tested.
  {
    if(monthAddition(sMonth,11) < sMonth)
    {
      sYear1 <- sYear + 1
    }else
    {
      sYear1 <- sYear
    }
    sMonth1 <- monthAddition(sMonth,11)
    
    mdfiv <- dfiv[dfiv$dateTime >= paste(sYear,"-",dateFormat(sMonth,0),"-01 00:00",sep="") &
                    dfiv$dateTime <= paste(sYear1,"-",dateFormat(sMonth1,0),"-",getmonthEndDay(sMonth1,sYear1)," 23:59",sep=""),]
    mdfdv <- dfdv[dfdv$Date >= paste(sYear,"-",dateFormat(sMonth,0),"-01 00:00",sep="") &
                    dfdv$Date <= paste(sYear1,"-",dateFormat(sMonth1,0),"-",getmonthEndDay(sMonth1,sYear1)," 23:59",sep=""),]
    
    print(paste(sYear,"-",dateFormat(sMonth,0),"-01 00:00 TO ",sYear1,"-",dateFormat(sMonth1,0),"-",getmonthEndDay(sMonth1,sYear1)," 23:59",sep=""))
    
    sList <- list.append(sList, runStatistics(mdfiv,mdfdv,"Annual",period))
  }
  
  
  rList <- list()
  tempDf <- data.frame()
  n<-1
  m<-2
  i<-1
  #j<-1
  print("HERE")
  for(i in 1:((length(sList[[1]]))/2))
  {
    #Period Interval	Records	Stdev	Mean	Median	Min	Max	Q.05	Q.10	Q.25	Q.50	Q.75	Q.90	Q.95	Ci.90	Ci.95
    
    for(j in 1:(length(sList)))
    {
      tempDf1 <- data.frame(Period=sList[[j]][[n]][1],Interval=sList[[j]][[n]][2],Records=sList[[j]][[n]][3],Stdev=sList[[j]][[n]][4],Mean=sList[[j]][[n]][5],Median=sList[[j]][[n]][6],Min=sList[[j]][[n]][7],Max=sList[[j]][[n]][8],Q.05=sList[[j]][[n]][9],Q.10=sList[[j]][[n]][10],Q.25=sList[[j]][[n]][11],Q.50=sList[[j]][[n]][12],Q.75=sList[[j]][[n]][13],Q.90=sList[[j]][[n]][14],Q.95=sList[[j]][[n]][15],Ci.90=sList[[j]][[n]][16],Ci.95=sList[[j]][[n]][17])
      tempDf2 <- data.frame(Period=sList[[j]][[m]][1],Interval=sList[[j]][[m]][2],Records=sList[[j]][[m]][3],Stdev=sList[[j]][[m]][4],Mean=sList[[j]][[m]][5],Median=sList[[j]][[m]][6],Min=sList[[j]][[m]][7],Max=sList[[j]][[m]][8],Q.05=sList[[j]][[m]][9],Q.10=sList[[j]][[m]][10],Q.25=sList[[j]][[m]][11],Q.50=sList[[j]][[m]][12],Q.75=sList[[j]][[m]][13],Q.90=sList[[j]][[m]][14],Q.95=sList[[j]][[m]][15],Ci.90=sList[[j]][[m]][16],Ci.95=sList[[j]][[m]][17])
      
      print(levels(factor(tempDf1$Period)))
      levels(factor(tempDf2$Period))
      
      if(j == 1)
      {
        tempDf3 <- rbind(tempDf1,tempDf2)
      }else
      {
        tempDf3 <- rbind(tempDf3,tempDf1,tempDf2)
      }
      j<-j+1
    }
    n<-n+2
    m<-m+2
    
    #round decimals to four spaces.
    is.num <- sapply(tempDf3, is.numeric)
    tempDf3[is.num] <- lapply(tempDf3[is.num], round, 3)
    
    rList <- list.append(rList, tempDf3)
  }
  print("here too")
  return(rList)
  
  # rList <- list()
  # n<-0
  # m<-0
  # for(i in 1:(length(sList[[1]][[1]])/2))
  # {
  #   tempDf <- data.frame(Period)
  #   for(j in 1:(length(sList)))
  #   {
  #     tempDf <- data.frame(tempDf,sList[[j]][(1+n):(2+m)])
  #     j<-j+1
  #   }
  #   #round decimals to four spaces.
  #   is.num <- sapply(tempDf, is.numeric)
  #   tempDf[is.num] <- lapply(tempDf[is.num], round, 3)
  #   
  #   rList <- list.append(rList, tempDf)
  #   n<-n+2
  #   m<-m+2
  # }
  # names(rList) <- c("Salinity (Conductance) TOP","Salinity (Conductance) BOTTOM","Salinity USGS TOP","Salinity USGS BOTTOM","Salinity (Cond,Temp) TOP","Salinity (Cond,Temp) BOTTOM")
  # 
  # for(i in 1:(length(rList)))
  # {
  #   names(rList[[i]]) <- c("Period","Interval","Records(Tot/Used/N-Used)","Stdev","Mean","Median","Min","Max","Q.05","Q.10","Q.25","Q.50","Q.75","Q.90","Q.95","Ci.90","Ci.95")
  # }
  # 
  # for (i in 1:(getIntervalData(period)))#create data frame for period.
  # {
  #   
  #   if(salinityType == 1) #Calculated Salinity based on Conductance.
  #   {
  #     dfiv$CalcSalinity<-(6.859819E-38*(rows$value_00010)^8 + -2.027297E-32*(rows$value_00010)^7 + 2.576767E-27*(rows$value_00010)^6 + -1.855032E-22*(rows$value_00010)^5 + 8.450662E-18*(rows$value_00010)^4 + -0.0000000000002616744*(rows$value_00010)^3 + 0.000000007096135*(rows$value_00010)^2 + 0.0005161311*(rows$value_00010) + -0.05028803)
  #     dfdv$CalcSalinity<-(6.859819E-38*(rows$value_00010)^8 + -2.027297E-32*(rows$value_00010)^7 + 2.576767E-27*(rows$value_00010)^6 + -1.855032E-22*(rows$value_00010)^5 + 8.450662E-18*(rows$value_00010)^4 + -0.0000000000002616744*(rows$value_00010)^3 + 0.000000007096135*(rows$value_00010)^2 + 0.0005161311*(rows$value_00010) + -0.05028803)
  #     d <- dvRows$CalcSalinity
  #     #newDf <- createStatistics(rList, ivRows$CalcSalinity, dvRows$CalcSalinity,period)
  #     
  #   }else if(salinityType == 2) #Calculated Salinity based on Temperarue and Conductance.
  #   {
  #     ivRows$CalcSalinity<-(6.859819E-38*(rows$value_00010)^8 + -2.027297E-32*(rows$value_00010)^7 + 2.576767E-27*(rows$value_00010)^6 + -1.855032E-22*(rows$value_00010)^5 + 8.450662E-18*(rows$value_00010)^4 + -0.0000000000002616744*(rows$value_00010)^3 + 0.000000007096135*(rows$value_00010)^2 + 0.0005161311*(rows$value_00010) + -0.05028803)
  #     dvRows$CalcSalinity<-(6.859819E-38*(rows$value_00010)^8 + -2.027297E-32*(rows$value_00010)^7 + 2.576767E-27*(rows$value_00010)^6 + -1.855032E-22*(rows$value_00010)^5 + 8.450662E-18*(rows$value_00010)^4 + -0.0000000000002616744*(rows$value_00010)^3 + 0.000000007096135*(rows$value_00010)^2 + 0.0005161311*(rows$value_00010) + -0.05028803)
  #     newDf <- createStatistics(rList, ivRows$CalcSalinity, dvRows$CalcSalinity,period)
  #   }else #Salinity value from database
  #   {
  #     newDf <- createStatistics(rList, ivRows$value_00480, dvRows$value_00480,period)
  #   }
  #   sMonth <- sMonth + 1
  #   
  #   #if month reaches december (12); next month will be january(1) and year offset is +1.
  #   if(sMonth > 12)
  #   {
  #     sMonth = sMonth - 12
  #     yn <- yn + 1
  #   }
  #   
  #   if(length(rList) == 0)
  #   {
  #     rList <- newDf
  #     #rList[[i]]
  #   }else
  #   {
  #     rList <- list.append(rList,newDf)
  #   }
  # }
}




#cloneFile - clone a file.
#Created by KLA on 05/17/2018.
#Last update by KLA on 05/17/2018.
#Parameters: filename(character),extension(character)
#Returns: nothing.
#Status: n tested.
# cloneFile<-function(filename,extension,data,dbname)
# {
#   itemLocation <- getwd()
#   itemDestination <- paste(getwd(),"/Generated Files",sep="")
#   print(paste(getwd(),"/Generated Files",sep=""))
#   #file.exists(file.path="D:/R/Jax Harbor/R/Generated Files","JaxStatTemplate.xlsx")
#   list.of.files <- data.frame(list.files(itemDestination))
# 
#   
#   file.rename(from = file.path(itemLocation, "JaxStatTemplate.xlsx"), to = file.path(itemDestination, getFileName(list.of.files,".xlsx")))
# 
#   }
# 
# 
# 
# #generateFile - create a file.
# #Created by KLA on 05/16/2018.
# #Last update by KLA on 05/16/2018.
# #Parameters: filename(character),extension(character)
# #Returns: nothing.
# #Status: tested.
# generateFile<-function(filename,extension,site_id,parameterCd,startDate,endDate,tz)
# {
#   setwd("D:/R/Jax Harbor/R")
#   
#   df <- readNWISuv(site_id,parameterCd,startDate,endDate,tz=getTimeZone(tz))
#   
#   if(extension == ".xlsx")
#   {
#     wbcopy <- loadWorkbook("JaxStatTemplate.xlsx")
#     xldf = readWorksheet(wbcopy, sheet = getSheets(wbcopy)[1]) 
#     wb <- loadWorkbook(getFileName(filename,extension), create = TRUE)
#     createSheet(wb,"Sheet1")
#     writeWorksheet(wb,xldf,sheet=1,startRow = 2,startCol = 2,header = FALSE)
#     setColumnWidth(wb,sheet=1, column=2, width=4800)
#     setColumnWidth(wb,sheet=1, column=3:(ncol(xldf)+1), width=3800)
#     
#     if(!is.null(data))
#     {
#       writeWorksheet(wb,dbname,sheet=1,startRow = 4,startCol = 3,header = FALSE)
#       writeWorksheet(wb,data,sheet=1,startRow = 2,startCol = 2,header = FALSE)
#     }
#     
#     saveWorkbook(wb)
#   }else if(extension == ".docx")
#   {
#     
#   }else if(extension == ".csv")
#   {
#     write.csv(df,file=getFileName(filename,extension))
#   }
# }
#######################################################################################


# library(ggplot2)
# library(scales)
# sql<-"SELECT site,variable,variable_code,service_type, min(CAST(strftime('%Y',observation_date) as int)) as min_year, max(CAST(strftime('%Y',observation_date) as int)) as max_year FROM usgs_time_series where variable_code='00065' and service_type='iv' group by site,variable,variable_code,service_type"
# runDs(con,sql);
# jhRunAnnualStats(con,"00480")
# //run seasonal,monthly,weekly
# //Salinity/Stage: Dames point, acosta, bickman, shands,clapbdoard creek,timaquan,trout,pottsburg, ortega, julington
# //Flow: astor, buffalo bluff
# jhHistIv(con,'02246804','00480','S(psu)',2016,100)
# //run seasonal,monthly,weekly for above
# //Boxplots
# 
# //cumlative frequency and quartile.
#   
# 
# sql<-"select site as ff,value from usgs_time_series where variable_code='00065' and service_type='dv' and observation_date between '2000-01-01' and '2017-12-31'"
# runHistF(con,sql)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# sql<-"SELECT * FROM usgs_time_series where variable_code='00480' and service_type='dv' and observation_date between '2016-01-01' and '2017-12-31'"
# res<-dbSendQuery(con,sql)
# rows<-dbFetch(res)
# rows$dateVal=as.Date(rows$observation_date)
# variable<-rows[1,]$variable
# png(filename="e:/projects/programming/node/usgs/R/charts/out.png", width=11, height=17, units='in',res=600)
# plot<-ggplot(rows,aes(x=dateVal,y=value,group=site))
# (plot <- plot + geom_line() + facet_wrap(~site,ncol=1)) 
#    plot+scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y"))
#    plot+xlab("Observation Date")
# graphics.off()
# 
# png(filename="./R/out.png", width=5000, height=5000)
# plot<-ggplot(rows,aes(x=dateVal,y=value,group=site))
# (plot <- plot + geom_line() + facet_wrap(~site,ncol=1)) + scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y"))
# graphics.off()
# 
# png(filename="e:/projects/programming/node/usgs/R/charts/out.png", width=8.5, height=11, units='in',res=1200)
# plot<-ggplot(rows,aes(x=dateVal,y=value,group=site))
# (plot <- plot + geom_line() + facet_wrap(~site,ncol=1)) + scale_x_date(breaks = date_breaks("months"), labels = date_format("%m/%y"))
# graphics.off()
# 
# plot + geom_line(aes(color=site))
# 
# 
# 
# 
# sql<-"SELECT * FROM usgs_time_series where variable_code='00065' and service_type='dv' and site='02236125' and observation_date between '2016-01-01' and '2017-12-31'"
# res<-dbSendQuery(con,sql)
# rows<-dbFetch(res)
# stat.desc(rows$value)
# 
# 



# #getUsgsStatistics - build a table based of parameter configurations.
# #Created by KLA on 05/07/2018.
# #Last update by KLA on 05/17/2018.
# #Parameters: con (sql),site,varcode,varname,service(character),period,month,year,week(numeric)
# #Returns: 
# #Status: in development.
# getUsgsStatistics<-function(con,site,varcode,varname,service,period,month,year,week,season){
#   sql<-paste("SELECT site, variable, variable_code,units,service_type,strftime('%Y-%m-%d %H:%M', observation_date, 'localtime') as obs_date, strftime('%W',observation_date, 'localtime') as week_number, qualifier,value FROM usgs_time_series where site='",site,"' and variable_code='",varcode,"' and service_type=",service," and ",getObservationDateQuery(period,month,year,week,season)," order by obs_date",sep="")
#   
#   res<-dbSendQuery(con,sql)
#   rows<-dbFetch(res)
#   
#   if(varcode == "00095")
#   {
#     rows$CalcSalinity<-(6.859819E-38*(rows$value)^8 + -2.027297E-32*(rows$value)^7 + 2.576767E-27*(rows$value)^6 + -1.855032E-22*(rows$value)^5 + 8.450662E-18*(rows$value)^4 + -0.0000000000002616744*(rows$value)^3 + 0.000000007096135*(rows$value)^2 + 0.0005161311*(rows$value) + -0.05028803)
#     print(paste(rows$CalcSalinity, sep=""))
#   }
#   rm(list=ls())
#   df <- as.data.frame(read.csv("TableFormat.csv"))
#   
#   df
#   
#   while(TRUE)
#   {
#     
#   }
#   
#   d<-rows$value
#   
#   ci90<-ci(0.9,d)
#   ci95<-ci(0.95,d)
#   
#   out<-paste("SQL DB : usgs_time_series",'\n',
#              " Period: ",getPeriod(period)," (week=",week,",month=",month,")\t","      SD: ", sd(d),'\n',
#              "   Year: ",as.character(year),'\t\t\t\t',"    MEAN: ",mean(d),'\n',
#              "    Min: ", min(d),'\t\t\t\t',"     Max: ", max(d), '\n',
#              " Length: ", length(d),'\t\t\t\t'," Service: ",service,'\n',
#              "Varcode: ", varcode,'\t\t\t\t',"    Site: ", site, '\n', 
#              "ci90[1]: ", ci90[1],'\t\t'," ci95[1]: ", ci95[1], '\n',
#              "ci90[2]: ", ci90[2],'\t\t'," ci95[2]: ", ci95[2], '\n',
#              "  Q .10: ", quantile(d,0.10),'\t\t\t',"    Quantile .25: ", quantile(d,0.25), '\n',
#              "  Q .50: ", quantile(d,0.50),'\t\t\t',"    Quantile .75: ", quantile(d,0.75), '\n',
#              "  Q .90: ", quantile(d,0.90),'\t\t\t',"    Quantile .95: ", quantile(d,0.95), '\n',
#              "  Q .05: ", quantile(d,0.05),'\n',
#              "Varname: ", varname, '\n',sep="")
#   
#   out<-(paste("\n\nSELECT site, variable, variable_code,units,service_type,strftime('%Y-%m-%d %H:%M', observation_date, 'localtime') as obs_date, strftime('%W',observation_date, 'localtime') as week_number, qualifier,value FROM usgs_time_series where site='",site,"' and variable_code='",varcode,"' and service_type=",service," and ",getObservationDateQuery(period,month,year,week,season)," order by obs_date\n",sep=""))
#   
#   cat(out)
#   #return(paste("SELECT site, variable, variable_code, option_code,method_id,units,service_type,strftime('%Y-%m-%d %H:%M', observation_date, 'localtime') as obs_date, strftime('%W',observation_date, 'localtime') as week_number, qualifier,value FROM usgs_time_series where site='","2236125","' and variable_code='","480","' and service_type=","'iv'"," and ",getObservationDateQuery(period,month,year,week,season),sep=""))
# }




######################################################### TESTED FUNCTIONS ################
#getFileName - Get the file name for a .docx document.
#Created by KLA on 05/16/2018.
#Last update by KLA on 05/21/2018.
#Parameters: filename with no extension(character).
#Returns: filename with extension.
#Status: tested.
getFileName<-function(filename, extension)
{
  options(warn=-1)
  if(file.exists(paste(filename,extension,sep=""),getwd()))
  {
    i <- 1
    while(file.exists(paste(filename," (",as.character(i),')',extension,sep="")))
    {
      i <- i + 1
    }
    options(warn=0)
    return(paste(filename , " (" , as.character(i) ,')',extension,sep="")) 
  }
  options(warn=0)
  return(paste(filename,extension,sep=""))
}

#getTimeZone - Get the time zone.
#Created by KLA on 05/30/2018.
#Last update by KLA on 05/30/2018.
#Parameters: timezone(character).
#Returns: timezone in a different format.
#Status: tested.
getTimeZone<-function(tz)
{
  #default is EST(Easter Time Zone)
  if(tz == "UTC")
  {
    return("UTC")
  }else if(tz == "HDT")
  {
    return ("America/Honolulu")    
  }else if(tz == "AKDT")
  {
    return ("America/Anchorage")   
  }else if(tz == "PDT")
  {
    return ("America/Los_Angeles")    
  }else if(tz == "MDT")
  {
    return ("America/Denver")    
  }else if(tz == "CDT")
  {
    return ("America/Chicago")    
  }
  return ("America/New_York")
}

#getMonthCharacter - Get month in a character format.
#Created by KLA on 05/29/2018.
#Last update by KLA on 05/31/2018.
#Parameters: num(numeric)
#Returns: month(character).
#Status: tested.
getMonthCharacter<-function(num)
{
  #The default return is December if number is not in range from 1-12.
  if(num == 1)
  {
    return("January")
  }else if (num == 2)
  {
    return("February")
  }else if (num == 3)
  {
    return("March")
  }else if (num == 4)
  {
    return("April")
  }else if (num == 5)
  {
    return("May")
  }else if (num == 6)
  {
    return("June")
  }else if (num == 7)
  {
    return("July")
  }else if (num == 8)
  {
    return("August")
  }else if (num == 9)
  {
    return("September")
  }else if (num == 10)
  {
    return("October")
  }else if (num == 11)
  {
    return("November")
  }else
  {
    return("December")
  }
}

#monthAddition - Returns an addition of months to a expecific month.
#                Ex: if month if 4 (april) and you want to add 11 months.
#                The result will return 3 (march) instead of 15.
#Created by KLA on 05/30/2018.
#Last update by KLA on 05/31/2018.
#Parameters: month(numeric),num(numeric)
#Returns: month(character).
#Status: tested.
monthAddition<-function(month,num)
{
  if(month+num > 12)
  {
    return(month+num-12)
  }else
  {
    return(month+num)
  }
}

#getPeriodData - Get the Period Column Data.
#Created by KLA on 05/21/2018.
#Last update by KLA on 05/31/2018.
#Parameters: period(character)
#Returns: vector period cloumn.
#Status: tested.
getPeriodData<-function(period,month)
{
  if(period == 1)#Weekly
  {
    week <- c()
    for (i in 1:53)
    {
      week <- c(week, paste("Week ",i,sep=""),"")
    }
    return(week)
  }else if (period == 2)#Monthly
  {
    rVector <- c()
    for(i in 1:12)
    {
      if(length(rVector) == 0)
      {
        rVector <- c(getMonthCharacter(month),"")
      }else
      {
        rVector <- c(rVector,getMonthCharacter(monthAddition(month,1)),"")
        month <- monthAddition(month,1)
      }
    }
    return(rVector)
  }else if (period == 3)#Seasonal
  {
    rVector <- c()
    for(i in 1:3)
    {
      month1 <- monthAddition(month,3)
      
      if(length(rVector) == 0)
      {
        rVector <- c(paste(getMonthCharacter(month),"-",getMonthCharacter(month1),sep=""),"")
      }else
      {
        rVector <- c(rVector,paste(getMonthCharacter(month),"-",getMonthCharacter(month1),sep=""),"")
      }
      month <- monthAddition(month1,1)
    }
    return(rVector)
  }else#Annual
  {
    return(c("Annual",""))
  }
}

#getInterval - Get the interval number of tables.
#Created by KLA on 05/21/2018.
#Last update by KLA on 05/29/2018.
#Parameters: period(character)
#Returns: interval number.
#Status: tested.
getIntervalData<-function(period)
{
  if(period == 1)#Weekly
  {
    return(53)
  }else if(period == 2)#Monthly
  {
    return(12)
  }else if(period == 3)#Seasonal
  {
    return(3)
  }else#Annual
  {
    return(1)
  }
}


#getIntervalNumber - Get the interval number from the length of a row.
#Created by KLA on 06/01/2018.
#Last update by KLA on 06/01/2018.
#Parameters: nRows(num)
#Returns: interval (15min,30min,45min or 1hr)[character].
#Status: tested.
getIntervalNumber<-function(rows,period)
{
  #convert everything to year values
  if(period == 1)#week
  {
    x <- 52.14
  }else if (period == 2)#month
  {
    x <- 12
  }else if (period == 3)#season
  {
    x <- 3
  }else if (period == 4)#annual
  {
    x <- 1
  }
  rows <- rows * x
  
  #this in a approximation.
  if(rows <= 10219)
  {
    return("1hr")
  }else if(rows >= 10220 && rows <= 14599)
  {
    return("45min")
  }else if(rows >= 14600 && rows <= 26279)
  {
    return("30min")
  }else if(rows >= 26280)
  {
    return("15min")
  }
}
# getIntervalNumber<-function(nRows)
# {
#   #this in a approximation.
#   if(nRows <= 10219)
#   {
#     return("1hr")
#   }else if(nRows >= 10220 && nRows <= 14599)
#   {
#     return("45min")
#   }else if(nRows >= 14600 && nRows <= 26279)
#   {
#     return("30min")
#   }else if(nRows >= 26280)
#   {
#     return("15min")
#   }
# }

#getPeriod - Gets the period in a character format.
#Created by KLA on 05/07/2018.
#Last update by KLA on 05/08/2018.
#Parameters: period (character)
#Returns: character.
#Status: tested.
getPeriod<-function(period)
{
  if(period == 1)
    return ("Weekly")
  else if (period == 2)
    return ("Monthly")
  else if (period == 3)
    return ("Seasonal")
  return ("Annual")
}

#getConfiguration - Gets the period in a character format.
#Created by KLA on 05/09/2018.
#Last update by KLA on 05/09/2018.
#Parameters: period (character)
#Returns: character.
#Status: tested.
getConfiguration<-function(period,month,year,week,season)
{
  if(period == 1)
    return (week)
  else if (period == 2)
    return (month)
  else if (period == 3)
    return (season)
  return (year)
}


#dateFormat - subtract or add a specific number to a numeric in character format.
#Created by KLA on 05/07/2018.
#Last update by KLA on 05/08/2018.
#Parameters: date (character),num(numeric)
#Returns: character.
#Status: tested.
dateFormat<-function(date,num)
{
  if(!is.numeric(date))
    date <- as.numeric(date)
  date <- date + num
  
  formatDate <- as.character(date)
  
  if(nchar(formatDate) < 2)
  {
    formatDate <- paste("0",formatDate,sep="")
  }
  return(formatDate)
}

#isLeapYear - Checks if the year is a leap year or not.
#Created by KLA on 05/04/2018.
#Last update by KLA on 05/08/2018.
#Parameters: year (numeric or character).
#Returns: boolean.
#Status: tested.
isLeapYear<-function(year)
{
  if((year %% 4) == 0) {
    if((year %% 100) == 0) {
      if((year %% 400) == 0) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else {
      return(TRUE)
    }
  } else {
    return(FALSE)
  }
}

#getmonthEndDay - Checks the length of the month in days.
#Created by KLA on 05/04/2018.
#Last update by KLA on 05/04/2018.
#Parameters: month, year (character)
#Returns: character.
#Status: tested.
getmonthEndDay<-function(month,year)
{
  if(is.element(month, c(1,3,5,7,8,10,12)))
  {
    return("31")
  }else
  {
    if(month == 2)
    {
      if(isLeapYear(year))
        return("29") 
      else
        return("28")
    }else
    {
      return("30")      
    }
  }
}


#getSiteName - Get the site name.
#Created by KLA on 06/04/2018.
#Last update by KLA on 06/04/2018.
#Parameters: site_id (character)
#Returns: character.
#Status: n tested.
getSiteName<-function(site_id)
{
  if(site_id == "2246825")
    return("CLAPBOARD CREEK NR JACKSONVILLE, FL (below Bogey Branch)")
  if (site_id == "302657081312400")
    return("CLAPBOARD CREEK AB BUCKHORN BLUFF NR JACKSONVILLE, FL (at Bogey Branch)")
  if (site_id == "2246804")
    return("DUNN CREEK AT DUNN CREEK RD NR EASTPORT, FL")
  if (site_id == "2246621")
    return("TROUT RIVER NR JACKSONVILLE, FL (at Lem Turner Rd. )")
  if (site_id == "302609081453300")
    return("TROUT RIVER BL US1 AT DINSMORE, FL (~750' southeast of Boat Ramp Pier)")
  if (site_id == "2246751")
    return("BROWARD RIVER BL BISCAYNE BLVD NR JACKSONVILLE, FL (.75 Mi NW of Harts Rd)")
  if (site_id == "2246459")
    return("CEDAR RIVER AT SAN JUAN AVENUE AT JACKSONVILLE, FL")
  if (site_id == "2246318")
    return("ORTEGA RIVER AT KIRWIN ROAD NEAR JACKSONVILLE,FL (at Argyle Forest Blvd.)")
  if (site_id == "301204081434900")
    return("ORTEGA RIVER AT COLLINS RD. (East of Blanding Blvd.)")
  if (site_id == "2246518")
    return("POTTSBURG CREEK AT US90 NR SOUTH JACKSONVILLE, FL (at Beach Blvd BR)")
  if (site_id == "2246515")
    return("POTTSBURG CREEK NR SOUTH JACKSONVILLE, FL (at Bowden Rd.)")
  if (site_id == "2246160")
    return("JULINGTON CREEK AT OLD ST AUGUSTINE RD NR BAYARD, FL")
  if (site_id == "300803081354500")
    return("JULINGTON CREEK AT HOOD LANDING NR BAYARD, FL")
  if (site_id == "2246200")
    return("DURBIN CREEK NR DURBIN, FL")
  if (site_id == "2246500")
    return("ST JOHNS RIVER AT JACKSONVILLE  (at Acosta Bridge)")
  if (site_id == "301921081394701-Historic")
    return("ST JOHNS RIVER AT JACKSONVILLE  (at Acosta Bridge)")
  if (site_id == "301817081393600")
    return("ST JOHNS RIVER BL MARCO LK AT JACKSONVILLE, FL (SAV)")
  if (site_id == "301510081383500")
    return("ST JOHNS RIVER AT CHRISTOPHER PT NR JACKSONVILLE, FL (SAV)")
  if (site_id == "8720218")
    return("ST JOHNS RIVER AT BAR PILOT DOCK  (FDEP)")
  if (site_id == "302309081333001")
    return("ST JOHNS RIVER AT DAMES POINT BRIDGE AT JACKSONVILLE, FL")
  if (site_id == "302309081333001-Historic")
    return("ST JOHNS RIVER AT DAMES POINT BRIDGE AT JACKSONVILLE, FL")
  if (site_id == "301124081395901")
    return("ST JOHNS RIVER BUCKMAN BRIDGE AT JACKSONVILLE, FL")
  if (site_id == "301124081395901-Historic")
    return("ST JOHNS RIVER BUCKMAN BRIDGE AT JACKSONVILLE, FL")
  if (site_id == "295856081372301-Historic")
    return("St Johns Shands Bridge (offline)")
  if (site_id == "294213081345300")
    return("ST JOHNS RIVER AT DANCY PT NR SPUDS, FL")
  if (site_id == "2244040")
    return("ST JOHNS RIVER AT BUFFALO BLUFF, FL")
  if (site_id == "2236125")
    return("ST JOHNS RIVER AT ASTOR, FL")
  if (site_id == "2246000")
    return("North Fork Black Creek Near Middelburg,FL")
  if (site_id == "02244440")
    return("Dunns Creek")
  if (site_id == "2245260")
    return("Deep Creek")
  if (site_id == "291830081362200")
    return("Lake George")
  if (site_id == "8720218")
    return("MAYPORT (BAR PILOTS DOCK), FL (Permanent NOAA/NOS gage)")
  if (site_id == "8720218-Historic")
    return("ST JOHNS RIVER AT BAR PILOT DOCK  (FDEP)")
  if (site_id == "8720129")
    return("DAMES POINT, FL")
  if (site_id == "8720226")
    return("Southbank Riverwalk, St Johns River")
  if (site_id == "8720245")
    return("JACKSONVILLE UNIVERSITY, FL")
  if (site_id == "8720357")
    return("I-295 BRIDGE, ST. JOHNS RIVER, FL (ST. JOHNS RIVER AT THE RUDDER CLUB)")
  if (site_id == "8720503")
    return("RED BAY POINT,  ST. JOHNS RIVER, FL (ST. JOHNS R @ SHANDS PIER NR SR 16)")
  if (site_id == "8720625")
    return("RACY POINT, ST. JOHNS RIVER. FL (ST. JOHNS RIVER AT RIVERDALE 1408 SR 13)")
  
  return("site_not_found")
}

# #createDataFrame - Creates a data frame for a expecific length.
# #Created by KLA on 05/04/2018.
# #Last update by KLA on 05/04/2018.
# #Parameters: month, year (character)
# #Returns: character.
# #Status: tested.
# createDataFrame<-function(dfiv,dfdv,period,startDate,sMonth,sYear)
# {
#   if(period == 1)#weekly - tested.
#   {
#     startDate <- as.Date(startDate)
#     
#     for (i in 1:(getIntervalData(period)))#create data frame for period.
#     {
#       eDate1 <- startDate+(7*(i-1))
#       
#       if(i == getIntervalData(period))
#       {
#         eDate2 <- endDate
#       }else
#       {
#         eDate2 <- startDate+(7*i-1)
#       }
#       
#       mdfiv <- dfiv[dfiv$dateTime >= paste(eDate1," 00:00",sep="") &
#                       dfiv$dateTime <= paste(eDate2," 23:59",sep=""),]
#       mdfdv <- dfdv[dfdv$Date >= eDate1 &
#                       dfdv$Date <= eDate2,]
#       
#       print(paste(eDate1," 00:00 TO ",eDate2," 23:59",sep=""))
#       i <- i + 1
#       #runStatistics()
#       
#     }else if(period == 2)#Monthly - tested.
#     {
#       for (i in 1:(getIntervalData(period)))#create data frame for period.
#       {
#         mdfiv <- dfiv[dfiv$dateTime >= paste(sYear,"-",dateFormat(sMonth,0),"-01 00:00",sep="") &
#                         dfiv$dateTime <= paste(sYear,"-",dateFormat(sMonth,0),"-",getmonthEndDay(sMonth,sYear)," 23:59",sep=""),]
#         mdfdv <- dfdv[dfdv$Date >= paste(sYear,"-",dateFormat(sMonth,0),"-01 00:00",sep="") &
#                         dfdv$Date <= paste(sYear,"-",dateFormat(sMonth,0),"-",getmonthEndDay(sMonth,sYear)," 23:59",sep=""),]
#         
#         print(paste(sYear,"-",dateFormat(sMonth,0),"-01 00:00 TO ",sYear,"-",dateFormat(sMonth,0),"-",getmonthEndDay(sMonth,sYear)," 23:59",sep=""))
#         
#         #if month reaches december (12); next month will be january(1) and year offset is +1.
#         if(monthAddition(sMonth,1) < sMonth)
#         {
#           sYear <- sYear + 1
#         }
#         sMonth <- monthAddition(sMonth,1)
#       }
#       #runStatistics()
#     }else if (period == 3)#seasonal - tested.
#     {
#       sYear1 <- NULL
#       for (i in 1:(getIntervalData(period)))#create data frame for period.
#       {
#         sMonth1 <- monthAddition(sMonth,3)
#         #if month reaches december (12); next month will be january(1) and year offset is +1.
#         if(monthAddition(sMonth,3) < sMonth)
#         {
#           sYear1 <- sYear + 1
#         }else
#         {
#           if(is.null(sYear1))#this is to prevent using previous year when sYear1 gets updated.
#           {
#             sYear1 <- sYear
#           }else
#           {
#             sYear <- sYear1
#           }
#         }
#         mdfiv <- dfiv[dfiv$dateTime >= paste(sYear,"-",dateFormat(sMonth,0),"-01 00:00",sep="") &
#                         dfiv$dateTime <= paste(sYear1,"-",dateFormat(sMonth1,0),"-",getmonthEndDay(sMonth1,sYear1)," 23:59",sep=""),]
#         mdfdv <- dfdv[dfdv$Date >= paste(sYear,"-",dateFormat(sMonth,0),"-01 00:00",sep="") &
#                         dfdv$Date <= paste(sYear1,"-",dateFormat(sMonth1,0),"-",getmonthEndDay(sMonth1,sYear1)," 23:59",sep=""),]
#         
#         print(paste(sYear,"-",dateFormat(sMonth,0),"-01 00:00 TO ",
#                     sYear1,"-",dateFormat(sMonth1,0),"-",getmonthEndDay(sMonth1,sYear1)," 23:59",sep=""))
#         
#         sMonth <- monthAddition(sMonth1,1)
#         #runStatistics()
#       }
#     }else#annually - tested.
#     {
#       if(monthAddition(sMonth,11) < sMonth)
#       {
#         sYear1 <- sYear + 1
#       }else
#       {
#         sYear1 <- sYear
#       }
#       sMonth1 <- monthAddition(sMonth,11)
#       
#       mdfiv <- dfiv[dfiv$dateTime >= paste(sYear,"-",dateFormat(sMonth,0),"-01 00:00",sep="") &
#                       dfiv$dateTime <= paste(sYear1,"-",dateFormat(sMonth1,0),"-",getmonthEndDay(sMonth1,sYear1)," 23:59",sep=""),]
#       mdfdv <- dfdv[dfdv$Date >= paste(sYear,"-",dateFormat(sMonth,0),"-01 00:00",sep="") &
#                       dfdv$Date <= paste(sYear1,"-",dateFormat(sMonth1,0),"-",getmonthEndDay(sMonth1,sYear1)," 23:59",sep=""),]
#       
#       print(paste(sYear,"-",dateFormat(sMonth,0),"-01 00:00 TO ",sYear1,"-",dateFormat(sMonth1,0),"-",getmonthEndDay(sMonth1,sYear1)," 23:59",sep=""))
#       #runStatistics()
#     }
#   }
# }


################Commented code / UNUSED
#write.xlsx(dfiv, file=filename,sheetName = , row.names = FALSE)
#write.xlsx(dfdv, file=filename,sheetName = , row.names = FALSE,append = TRUE)
# createSheet(wb,)
# writeWorksheet(wb,df,sheet=1,startRow = 1,startCol = 1,header = TRUE)
# # setColumnWidth(wb,sheet=1, column=2, width=4800)
# # setColumnWidth(wb,sheet=1, column=3:(ncol(xldf)+1), width=3800)
# 
# # if(!is.null(data))
# # {
# #   writeWorksheet(wb,dbname,sheet=1,startRow = 4,startCol = 3,header = FALSE)
# #   writeWorksheet(wb,data,sheet=1,startRow = 2,startCol = 2,header = FALSE)
# # }
# 
# saveWorkbook(wb)
# else if(extension == ".csv")
# {
#   filename <- getFileName(filename,extension)
#   write.csv(df,file=filename,row.names = FALSE)
# 
# }

# mdfiv <- dfiv[dfiv$dateTime >= paste(startDate+(7*(i-1))," 00:00",sep="") &
#                 dfiv$dateTime <= paste(startDate+(7*i-1)," 23:59",sep=""),]
# mdfdv <- dfdv[dfdv$Date >= startDate+(7*(i-1)) &
#                 dfdv$Date <= startDate+(7*i-1),]


boxcolor<-function(con, site, code, year,type) {
  
  sql3= paste("SELECT * FROM USGS_TIME_SERIES WHERE  site= '",site,"' AND variable_code= '",code,"' AND DATE(OBSERVATION_DATE) BETWEEN  '",year,"-01-01' AND '",year,"-12-30' AND service_type=",type, sep="")
  
  print(sql3)
  
  result3=dbSendQuery(con, sql3)
  
  data3 = dbFetch(result3, n = -1)
  
  datamax = max(data3$value)
  
  datamin = min(data3$value)
  
  
  boxplot3 = ggplot(data3, aes(x=method_id, y=value, fill=method_id)) + stat_boxplot(geom = "errorbar", width = 0.3, lwd=.9) + geom_boxplot(outlier.size=1.5, lwd=.9, varwidth = TRUE) + labs(title=paste("Variable Code",code,"at Site", site, "for",year, sep=" "), x="Position In River", y=paste("Value,",data3[1,]$units)) +  stat_summary(fun.y=mean, geom="point", shape=23, size=4.5, color="red", fill="red") + theme(plot.title = element_text(size = rel(1.5), face="bold"), axis.title = element_text(face="bold"), axis.line = element_line(colour = "black", size = .7, linetype = "solid"), legend.position="none", panel.background = element_rect(fill="NA"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_fill_brewer(palette="Paired")
  
  
  print(boxplot3  +   scale_y_continuous(breaks=seq(datamin,datamax,(datamax-datamin)/10)))
  
  #ggsave(paste(site,"_",code,"_",year,".pdf", sep=""), width=8.5, height=11)
}

boxcolor2<-function(con, site, code, year,type) {
  
  sql3= paste("SELECT * FROM USGS_TIME_SERIES WHERE  site= '",site,"' AND variable_code= '",code,"' AND DATE(OBSERVATION_DATE) BETWEEN  '",year,"-01-01' AND '",year,"-12-30' AND service_type=",type, sep="")
  
  print(sql3)
  
  result3=dbSendQuery(con, sql3)
  
  data3 = dbFetch(result3, n = -1)
  
  datamax = max(data3$value)
  
  datamin = min(data3$value)
  
  
  boxplot3 = ggplot(data3, aes(x=method_id, y=value, fill=method_id)) + stat_boxplot(geom = "errorbar", width = 0.3, lwd=.9) + geom_boxplot(outlier.size=1.5, lwd=.9, varwidth = TRUE) + labs(title=paste("Variable Code",code,"at Site", site, "for",year, sep=" "), x="Position In River", y=paste("Value,",data3[1,]$units)) +  stat_summary(fun.y=mean, geom="point", shape=23, size=4.5, color="red", fill="red") + theme(plot.title = element_text(size = rel(1.5), face="bold"), axis.title = element_text(face="bold"), axis.line = element_line(colour = "black", size = .7, linetype = "solid"), legend.position="none", panel.background = element_rect(fill="NA"), panel.grid.major = element_line(colour = "grey68"), panel.grid.minor = element_blank()) + scale_fill_brewer(palette="Paired")
  
  
  boxplot3  +   scale_y_continuous(breaks=seq(datamin,datamax,(datamax-datamin)/10))
  
  #ggsave(paste(site,"_",code,"_",year,".pdf", sep=""), width=8.5, height=11)
}

boxplain<-function(con, site, code, year,type) {
  
  sql3= paste("SELECT * FROM USGS_TIME_SERIES WHERE  site= '",site,"' AND variable_code= '",code,"' AND DATE(OBSERVATION_DATE) BETWEEN  '",year,"-01-01' AND '",year,"-12-30' AND service_type=",type, sep="")
  
  print(sql3)
  
  result3=dbSendQuery(con, sql3)
  
  boxplotdata3 = dbFetch(result3, n = -1)
  
  datamax = max(boxplotdata3$value)
  
  datamin = min(boxplotdata3$value)
  
  
  boxplot3 = ggplot(boxplotdata3, aes(x=method_id, y=value, fill=method_id)) + stat_boxplot(geom = "errorbar", width = 0.3, lwd=.9) + geom_boxplot(outlier.size=1.5, lwd=.9, fill="white", varwidth = TRUE) + labs(title=paste("Variable Code",code,"at Site", site, "for",year, sep=" "), x="Position In River", y=paste("Value,",boxplotdata3[1,]$units)) + theme(plot.title = element_text(size = rel(1.5), face="bold"), axis.title = element_text(face="bold"), axis.line=element_line(colour = "black", size= .8, linetype= "solid"), legend.position="none", panel.background = element_rect(fill="NA")) 
  
  
  boxplot3  +   scale_y_continuous(breaks=seq(datamin,datamax,(datamax-datamin)/10))
  
  #ggsave(paste(site,"_",code,"_",year,".pdf", sep=""), width=8.5, height=11)
}

cumfreq<-function(con, site, code, year,type) {
  
  sql3= paste("SELECT * FROM USGS_TIME_SERIES WHERE  site= '",site,"' AND variable_code= '",code,"' AND DATE(OBSERVATION_DATE) BETWEEN  '",year,"-01-01' AND '",year,"-12-30' AND service_type=",type, sep="")
  
  print(sql3)
  
  result3=dbSendQuery(con, sql3)
  
  mydata = dbFetch(result3, n = -1)
  
  percentiles = c(0, 0.25,0.5,0.75, 1)
  
  data = data.frame(q=quantile(mydata$value), percentiles)
  
  q=quantile(mydata$value)
  
  freqplot = ggplot(mydata %>% group_by(value) %>% summarise(n=n()), aes(x=value, colour="plum", y=n)) + geom_point(aes(y = cumsum(n))) + labs(title=paste("Cumulative Frequency Graph\n","Variable Code",code,"at Site", site, "for",year, sep=" "), y="Cumulative Frequency", x=paste("Value,",mydata[1,]$units)) + geom_vline(data = data.frame(q=quantile(mydata$value), percentiles), aes(xintercept=q),linetype = "solid") + geom_text(data = data, mapping= aes(label= paste(q,"\nQ",percentiles), x=q, y= -Inf), hjust= .15, vjust= -.5, inherit.aes=F) + theme(plot.title = element_text(size = rel(1.5), face="bold"), axis.title = element_text(face="bold"), axis.line = element_line(colour = "black", size = .7, linetype = "solid"), legend.position="none", panel.background = element_rect(fill="NA"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_fill_brewer(palette="Paired") + scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
  
  
  
  freqplot 
  
  
  
  #ggsave(paste(site,"_",code,"_",year,".pdf", sep=""), width=8.5, height=11)
}

freq<-function(con, site, code, year,type) {
  
  sql3= paste("SELECT * FROM USGS_TIME_SERIES WHERE  site= '",site,"' AND variable_code= '",code,"' AND DATE(OBSERVATION_DATE) BETWEEN  '",year,"-01-01' AND '",year,"-12-30' AND service_type=",type, sep="")
  
  print(sql3)
  
  result3=dbSendQuery(con, sql3)
  
  data3 = dbFetch(result3, n = -1)
  
  percentiles = c(0,0.25,0.5,0.75,1)
  
  
  
  freqplot = ggplot(data3, aes(x=value, colour="plum")) + stat_ecdf(data = NULL, geom="point") + labs(title=paste("Variable Code",code,"at Site", site, "for",year, sep=" "), y="Cumulative Frequency", x=paste("Value,",data3[1,]$units))  + geom_vline(data = data.frame(x=quantile(data3$value), percentiles), aes(xintercept=x),linetype = "solid") + theme(plot.title = element_text(size = rel(1.5), face="bold"), axis.title = element_text(face="bold"), axis.line = element_line(colour = "black", size = .7, linetype = "solid"), legend.position="none", panel.background = element_rect(fill="NA"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_fill_brewer(palette="Paired")
  
  
  freqplot 
  
  #ggsave(paste(site,"_",code,"_",year,".pdf", sep=""), width=8.5, height=11)
}

test13<-function(con, year, site, code,type) {
  
  sql3= paste("SELECT * FROM USGS_TIME_SERIES WHERE DATE(OBSERVATION_DATE) BETWEEN  '",year,"-01-01' AND '",year,"-12-30' AND 
              site= '",site,"' AND variable_code= '",code,"' AND service_type=",type, sep="")
  
  print(sql3)
  
  result3=dbSendQuery(con, sql3)
  
  boxplotdata3 = dbFetch(result3, n = -1)
  
  
  
  
  boxplot3 = ggplot(boxplotdata3, aes(x=method_id, y=value, color=method_id))  
  geom_boxplot(outlier.size=2) 
  labs(title=paste("Variable Code",code,"at site", site, "for",year, sep=" "), 
       x="Position In River", y="Value, uS/cm @25C") 
  theme(legend.position="none") 
  
  boxplot3  + 
    scale_y_continuous(breaks=seq(0,70000,5000))
  
  #ggsave(paste(site,"_",code,"_",year,".pdf", sep=""), width=8.5, height=11)
}
