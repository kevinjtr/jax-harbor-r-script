# 
rm(list=ls())
setwd("D:/R/Jax Harbor/R")

require("dataRetrieval")
require("Rmisc")
require("xlsx")
require("ReporteRs")
require("ggplot2")
require("DBI")
require("rlist")
require("RSQLite")
require("dplyr")
require("rnoaa")
require("data.table")


# library("dataRetrieval")
# library("Rmisc")
# library("xlsx")
# library("ReporteRs")
# library("ggplot2")
# library("DBI")
# library("rlist")
# library("RSQLite")
# library("dplyr")
# #library("RColorBrewer")
# library("rnoaa")
# library("data.table")
#source("JaxHarborChart.R")

filename="1_Jax Harbor"
site_id="301124081395901"
startDate="2016-12-01"
period=4
db="USGS"
bl=c("2007-12-01","2015-11-30")
mid=c("2015-12-01","2016-11-30")
parameterCd = c("00010","00095")


dfiv <- readNWISuv(site_id,parameterCd,paste("2016-12-01","T00:00",sep=""),paste("2017-11-30","T23:59",sep=""),tz=Sys.timezone())

df <- dfiv[c(2,3,4,6,8,10)]

df$X_TOP_00480_00000 <- (6.859819E-38*(df[[5]])^8 + -2.027297E-32*(df[[5]])^7 + 2.576767E-27*(df[[5]])^6 + -1.855032E-22*(df[[5]])^5 + 8.450662E-18*(df[[5]])^4 + -0.0000000000002616744*(df[[5]])^3 + 0.000000007096135*(df[[5]])^2 + 0.0005161311*(df[[5]]) + -0.05028803)
df$X_BOTTOM_00480_00000 <- (6.859819E-38*(df[[6]])^8 + -2.027297E-32*(df[[6]])^7 + 2.576767E-27*(df[[6]])^6 + -1.855032E-22*(df[[6]])^5 + 8.450662E-18*(df[[6]])^4 + -0.0000000000002616744*(df[[6]])^3 + 0.000000007096135*(df[[6]])^2 + 0.0005161311*(df[[6]]) + -0.05028803)
names(df) <- c("site_no","Date","WATER_TEMPERATURE_TOP","WATER_TEMPERATURE_BOTTOM","CONDUCTANCE_TOP","CONDUCTANCE_BOTTOM","SALINITY_TOP","SALINITY_BOTTOM")


df$Date <- as.character(df$Date)

con<-dbConnect(SQLite(), dbname="D:/R/Jax Harbor/R/database/JacksonvilleHarbor.db")
dbWriteTable(conn=con, name="USGS", df, append=TRUE, row.names=FALSE)
dbDisconnect(con)

sql<-paste("SELECT site,variable,variable_code,service_type, min(CAST(strftime('%Y',observation_date) as int)) as min_year, max(CAST(strftime('%Y',observation_date) as int)) as max_year FROM usgs_time_series where variable_code='",varcode,"' and service_type='dv' group by site,variable,variable_code,service_type",sep="")
res<-dbSendQuery(con,sql)
rows<-dbFetch(res)

dbWriteTable(DF, "mtcars", mtcars)


con<-dbConnect(SQLite(), dbname="D:/R/Jax Harbor/R/database/TestDB.db")
sql<-"SELECT *  FROM USGS
where strftime('%Y-%m-%d %H:%M:%S',DATE) < strftime('%Y-%m-%d %H:%M:%S', '2008-02-01 00:00:00') AND
site_no = '302309081333001' AND Interval = 'IV'"
res<-dbSendQuery(con,sql)
rows<-dbFetch(res)




if(db == "USGS")
{
  dfiv <- readNWISuv(site_id,parameterCd,paste(startDate,"T00:00",sep=""),paste(endDate,"T23:59",sep=""),tz=Sys.timezone())
  #dfdv <- readNWISdv(site_id,parameterCd,startDate,endDate,statCd = )
  colnames(dfiv)[colnames(dfiv)=="dateTime"] <- "Date"
  
}else if(db == "NOAA")
{
  product <- c("water_temperature","conductivity","salinity")
  
  df <- data.frame()
  dfiv <- data.frame()
  
  for(i in 1:(length(product)))
  {
    noaaStartDate <- as.Date(startDate)
    noaaEndDate <- as.Date(startDate) + 31
    
    while (noaaStartDate < endDate)
    {
      tryCatch({
        if(length(df) != 0)
        {
          d <- coops_search(station_name = as.numeric(site_id), time_zone = "gmt", begin_date = format.Date(noaaStartDate,"%Y%m%d"),
                            end_date = format.Date(noaaEndDate,"%Y%m%d"), product = product[i])[[2]]
          df <- rbind(df,d)
        }else
        {
          df <- coops_search(station_name = as.numeric(site_id), time_zone = "gmt", begin_date = format.Date(noaaStartDate,"%Y%m%d"),
                             end_date = format.Date(noaaEndDate,"%Y%m%d"), product = product[i])[[2]]
        }
      }, warning = function(war) {
        # do nothing.
      }, error = function(err) {
        # do nothing.
      }) # END tryCatch
      
      noaaStartDate <- noaaEndDate + 1
      
      if(noaaEndDate+31 < endDate)
      {
        noaaEndDate <- noaaEndDate + 31
      }else
      {
        noaaEndDate <- endDate
      }
    }
    df <- df[,-c(3)]
    names(df) <- c("Date",toupper(product[i]))
    
    if(length(dfiv) == 0)
    {
      dfiv <- df
    }else
    {
      setDT(dfiv)
      setDT(df)
      dfiv <- dfiv[df, on = 'Date']
    }
    rm(d)
    df <- data.frame()
  }
  
  #dfdv <- DfToDailyMean(dfiv)
  
  # dfiv$fDATE <- factor(substr(as.character(dfiv$Date),1,10))
  # wt <- tapply(dfiv[[2]], list(dfiv$fDATE), mean,na.rm=TRUE)#WATER_TEMPERATURE
  # cond <- tapply(dfiv[[3]], list(dfiv$fDATE), mean,na.rm=TRUE)#CONDUCTIVITY
  # sal <- tapply(dfiv[[4]], list(dfiv$fDATE), mean,na.rm=TRUE)#SALINITY
  # Date <- tapply(dfiv[[1]], dfiv$fDATE, mean)#Date
  # 
  # dfdv <- as.data.frame(cbind(Date,wt,cond,sal))
  # dfdv$Date <- with(dfdv, as.POSIXct("1970-01-01", "%Y-%m-%d", tz=Sys.timezone()) + Date)
  # dfdv$Date <- format.Date(dfdv$Date,"%Y-%m-%d")
  # rownames(dfdv) <- NULL
  # dfiv[[5]] <- NULL
  
}else if (db == "SJR")
{
  #setwd("D:/R/Jax Harbor/R/sjr files") #-- does not work during run time.
  #startDate <- as.Date("2000-01-01 00:00:00")
  #endDate <- as.Date("2000-12-31 23:59:00")
  #temp = list.files(pattern="*.csv")
  #for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))
  
  #sjrfiles = lapply(temp, read.delim)
  #rList <- list()
  dfiv <- data.frame()
  temp = list.files(path=paste("./sjr files/",sep=""),pattern="*.csv")
  
  for(i in 1:(length(temp)))
  {
    df<-read.csv(paste("./sjr files/",temp[i],sep=""),header=TRUE)
    df[[1]]<-as.character(df[[1]])
    df$Date <- as.POSIXct(strptime(df$Date,format='%Y-%m-%d %H:%M',tz=Sys.timezone()))
    df[df==-999999]<-NA
    df[df<0]<-NA
    if(length(dfiv)==0)
    {
      dfiv <- df
    }else
    {
      dfiv <- rbind(dfiv,df)
    }
    rm(df)
  }
  
  dfiv <- dfiv[dfiv$Site == site_id & !is.na(dfiv$Site),]
  dfiv <- dfiv[dfiv$Date >= as.POSIXct(startDate) & dfiv$Date <= as.POSIXct(endDate) & !is.na(dfiv$Date),]
  
  
  dfiv <- dfiv[ -c(1,3:4) ]
  names(dfiv) <- c("Date","WATER_TEMPERATURE","CONDUCTIVITY","SALINITY")
  
  #dfdv <- DfToDailyMean(dfiv)
  
  # dfiv$fDATE <- factor(substr(as.character(dfiv$Date),1,10))
  # WATER_TEMPERATURE <- tapply(dfiv[[2]], list(dfiv$fDATE), mean,na.rm=TRUE)#WATER_TEMPERATURE
  # CONDUCTIVITY <- tapply(dfiv[[3]], list(dfiv$fDATE), mean,na.rm=TRUE)#CONDUCTIVITY
  # SALINITY <- tapply(dfiv[[4]], list(dfiv$fDATE), mean,na.rm=TRUE)#SALINITY
  # Date <- tapply(dfiv[[1]], dfiv$fDATE, mean)#Date
  # 
  # 
  # dfdv <- as.data.frame(cbind(Date,WATER_TEMPERATURE,CONDUCTIVITY,SALINITY))
  # dfdv$Date <- with(dfdv, as.POSIXct("1970-01-01", "%Y-%m-%d", tz=Sys.timezone()) + Date)
  # dfdv$Date <- format.Date(dfdv$Date,"%Y-%m-%d")
  # rownames(dfdv) <- NULL
  # dfiv[[5]] <- NULL
  
  #setwd("D:/R/Jax Harbor/R")
}