rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
require("RJDBC")
require("lubridate")
source("JaxHarborChart.R",keep.source = TRUE)


DatabaseReadCleanDataFrame<-function(src){
  if(toupper(src) == "USGS"){
    
    table["X_00010_00000"] = rowMeans(table[,c("TEMP_TOP","TEMP_MID","TEMP_BOT")],na.rm = TRUE)
    table["X_00095_00000"] = rowMeans(table[,c("SPEC_COND_TOP","SPEC_COND_MID","SPEC_COND_BOT")],na.rm = TRUE)
    table["agency_cd"] = toupper("usgs")
    table["tz_cd"] = "America/New_York"
    table = table[,c("agency_cd","ID","READ_DATE_TIME","X_00010_00000","QUALIFIER","X_00095_00000","QUALIFIER","tz_cd")]
    t_names = c("agency_cd","ID","READ_DATE_TIME","X_00010_00000","X_00010_00000_cd","X_00095_00000","X_00095_00000_cd","tz_cd")
    names(table) <- t_names
    
  }else if (toupper(src) == "NOAA"){
    
  }
  
  return(table)
}

OracleDbRead<-function(src,stn,startDate,endDate,interval="IV"){
  
  drv <- RJDBC::JDBC("oracle.jdbc.OracleDriver",classPath= paste(getwd(),"/ojdbc6.jar",sep=""))
  
  fileName <- paste(getwd(),'/database/dboracle.txt',sep="")
  pw <- readChar(fileName, file.info(fileName)$size)
  
  con <- RJDBC::dbConnect(drv, "jdbc:oracle:thin:@saj-ora-db05:1521:K3CMAPP1", "JAX_HARBOR", pw)
  sql <- paste("SELECT * FROM ",toupper(src),"_DATA
               WHERE READ_DATE_TIME >= TO_DATE('",startDate,"','YYYY-MM-DD HH24:MI:SS') AND
               READ_DATE_TIME <= TO_DATE('",endDate,"','YYYY-MM-DD HH24:MI:SS') AND GAUGE_ID = ",stn,"",sep="")
               #AND SERVICE_TYPE = '"+interval+"';",sep="")
  
  rows <- RJDBC::dbGetQuery(con,sql)
  rows$READ_DATE_TIME <- as.POSIXct(rows$READ_DATE_TIME)
  RJDBC::dbDisconnect(con)
  #dbClearResult(res)
  
  
  return(rows)
}

SqliteDbRead<-function(src,stn,startDate,endDate,interval="IV"){
  
  con <- RSQLite::dbConnect(SQLite(), dbname = paste(getwd(),"/database/TestDB2.db",sep=""))
  
  sql<- paste("SELECT * FROM ",db,"
                  WHERE strftime('%Y-%m-%d %H:%M:%S',READ_DATE_TIME) >= strftime('%Y-%m-%d %H:%M:%S', '",startDate," 00:00:00') AND
                  strftime('%Y-%m-%d %H:%M:%S',READ_DATE_TIME) <= strftime('%Y-%m-%d %H:%M:%S', '",endDate," 23:59:59') AND
                  ID = '",stn,"' AND SERVICE_TYPE = '",interval,"'",sep="")
  
  res<-RSQLite::dbSendQuery(con,sql)
  rows<- rbind.data.frame(dbFetch(res)) 
  rows$READ_DATE_TIME <- as.POSIXct(rows$READ_DATE_TIME)
  RSQLite::dbClearResult(res)
  RSQLite::dbDisconnect(con)
  
  return(table)
}

table <- OracleDbRead("usgs","301124081395901","2020-09-01 00:00:00","2020-09-01 23:59:59")
table <- DatabaseReadCleanDataFrame("usgs")

#table["site_no_usedin"] = "301124081395901"





#table["Year_Used"] <- as.integer(format(ymd(as.Date(table[,"READ_DATE_TIME"])) %m+% months(1),"%Y")) - 2017
#table["Year_Used"] <- format(as.Date(table["READ_DATE_TIME"]) ,"%Y")

#table[,c("QUALIFIER","ID","READ_DATE_TIME","X_00010_00000","X_00095_00000")]
##dbCommit(con)


