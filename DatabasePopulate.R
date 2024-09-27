

DownloadAndPopulateDatabase<-function(site_id,assessment,baseline){
  site_id <- unique(site_id)
  assessment <- unique(assessment)
  dbNumRows<- 0
  ReadDB = FALSE
  
  if(ReadDB==TRUE)
  {
    saveDB <- FALSE
  }else
  {
    saveDB <- TRUE
  }
  
  tempTableList <- list()
  tempTableNames <- c()
  start_time <- Sys.time()
  pastSiteID <- ""
  y_past <- 0
  
  for(f in 1:(length(site_id)))
  {
    for(y in 1:(length(assessment)))
    {
      print(paste("Dowloading Data for: ",getSiteName(site_id[f],FALSE)," [",f," of ",length(site_id),"].",sep=""))
      g <- 1
      if(y == 1)
      {
        bl_fetch <- TRUE
      }else
      {
        bl_fetch <- FALSE
      }
      asy_fetch <- TRUE
      
      tb <- 1
      fg <- 1
      
      if(y_past != y)
      {
        if(y == 1)
        {
          bl <- baseline
        }else
        {
          bl <- NULL
        }
        asse <- assessment[y]
        
        parameterCd = c("00010","00095")
        
        startDate <- as.Date(assessment[y])
        startYear <- format.Date(startDate,"%Y")
        startMonth <- format.Date(startDate,"%m")
        
        endDate <- as.POSIXlt(startDate)
        endDate$year <- endDate$year+1
        endDate <- as.Date(endDate)-1
        endYear <- format.Date(endDate,"%Y")
        endMonth <- format.Date(endDate,"%m")
        
        tryCatch({
          b_list <- list()
          
          if(bl_fetch)
          {
            b_dfiv <- NULL
            b_dfdv <- NULL
            hb_dfiv <- NULL
            hb_dfdv <- NULL
            
            print(paste("Downloading Baseline Year ",baseline[1]," - ",baseline[2],"...",sep=""))
            b_dfiv<- ReadDataBase(ReadDB,saveDB,"Baseline",baseline[1],baseline[2],site_id[f],interval="IV","USGS")
            
            tryCatch({
              b_list <- convertDataFrame(b_dfiv,"USGS")
              browser
              if(site_id[f] %in% c("02246500","02245340","301204081434900","02246518",
                                   #Tier 2 Statistics
                                   "02245290",
                                   "02246459",
                                   "02246751",
                                   "02246804",
                                   "022462002",
                                   "294213081345300",
                                   "300803081354500",
                                   "301510081383500",
                                   "301817081393600",
                                   "302609081453300",
                                   "302657081312400"))
              {
                if(!ReadDB)
                {
                  if (site_id[f] == "02246500")
                  {
                    v <- "acosta/"
                  }else if(site_id[f] == "301204081434900")
                  {
                    v <- "ortega/"
                  }else if(site_id[f] == "02246518")
                  {
                    v <- "pottsburg/"
                  }else if(site_id[f] == "02245340")
                  {
                    v <- "shans/"
                    
                    #b_dfiv <- ReadDataBase(ReadDB,saveDB,"Baseline","2016-01-01","2017-11-30","02245340","IV","USGS")
                    b_dfiv <- rbind.data.frame(ReadDataBase(ReadDB,saveDB,"Baseline","2007-01-01","2009-12-31",c("295856081372301","02245340"),"IV","USGS"),b_dfiv)
                    
                    if(length(b_list) == 0)
                    {
                      b_list <- DFtoPlotFormat(convertDataFrame(b_dfiv,"USGS"))
                    }else
                    {
                      b_list <- list(DFtoPlotFormat(rbind.data.frame(b_list[[1]],convertDataFrame(b_dfiv,"USGS")[[1]])))
                    }
                  }else if (site_id[f] == "02245290")
                  {
                    v <- "2245290_SJR_Racy_Point_Salinity_BL_AY6/"
                  }else if(site_id[f] == "02246459")
                  {
                    v <- "2246459_Cedar_River_Salinity_BL_AY6/"
                  }else if(site_id[f] == "02246751")
                  {
                    v <- "2246751_Broward_River_Salinity_BL_AY6/"
                  }else if(site_id[f] == "02246804")
                  {
                    v <- "2246804_Dunn_Creek_Salinity_AY6_BL/"
                  }else if (site_id[f] == "022462002")
                  {
                    v <- "22462002_DurbinCk_Salinity_BL_AY6/"
                  }else if(site_id[f] == "294213081345300")
                  {
                    v <- "294213081345300_SJC_DancyPt_Salinity_BL_AY6/"
                  }else if(site_id[f] == "300803081354500")
                  {
                    v <- "300803081354500_JulingtonCrkHL_Salinity_BL_AY6/"
                  }else if (site_id[f] == "301510081383500")
                  {
                    v <- "301510081383500_SJR_ChrisPt_Salinity_BL_AY6/"
                  }else if(site_id[f] == "301817081393600")
                  {
                    v <- "301817081393600_MarkoLk_Salinity_BL_AY6/"
                  }else if(site_id[f] == "302609081453300")
                  {
                    v <- "302609081453300_TroutRvrUS1_Salinity_BL_AY6/"
                  }else if(site_id[f] == "302657081312400")
                  {
                    v <- "302657081312400_ClapboardBB_Salinity_BL_AY6/"
                  }
                  
                  temp = list.files(path=paste("./sjr files/",v,sep=""),pattern="*.csv")
                  hb_dfiv <- ReadDataBase(ReadDB,saveDB,"Baseline","","",site_id[f],interval="IV",db="NoaaSJR",listFiles=temp,fileExt=v)
                  b_list <- list(rbind.data.frame(DFtoPlotFormat(hb_dfiv),b_list[[1]]))
                  
                }else
                {
                  hb_dfiv <- ReadDataBase(ReadDB,saveDB,"Baseline","","",site_id[f],interval="IV",db="NoaaSJR")
                  b_list <- list(rbind.data.frame(DFtoPlotFormat(hb_dfiv),b_list[[1]]))
                }
              }
              
            },error = function(err) {
              print("WARNING: Some Baseline Year data was not loaded.")
            }) # END tryCatch
            
            if(nrow(b_dfiv) == 0)
            {
              if(nrow(hb_dfiv) != 0)
              {
                b_dfiv <- hb_dfiv
                b_dfdv <- DfMean(b_dfiv,1)
              }else
              {
                print("Note: No Baseline Year data was found...")
                bl = NULL
              }
            }else
            {
              if(!is.null(hb_dfiv))
              {
                b_dfdv <- DfMean(b_dfiv,1)
                hb_dfdv <- DfMean(hb_dfiv,1)
                
                b_dfiv <- convertDataFrame(b_dfiv,db)[[1]]
                b_dfdv <- convertDataFrame(b_dfdv,db)[[1]]
                
                b_dfiv <- rbind.data.frame(hb_dfiv,b_dfiv)
                b_dfdv <- rbind.data.frame(hb_dfdv,b_dfdv)
              }else
              {
                b_dfdv <- DfMean(b_dfiv,1)
              }
              print("Done: Downloading Baseline Year...")
            }
            bl_fetch <- FALSE
            
            # if(saveDB){
            #   dbNumRows <- dbNumRows + nrow(b_dfiv) + nrow(b_dfdv)
            #   SaveDataBase(b_dfiv,b_dfdv,"Baseline",site_id[f])
            # }
          }
        },error = function(err) {
          stop("WARNING: Baseline Year could not be loaded.")
          bl = NULL
        }) # END tryCatch
        
        tryCatch({
          if(asy_fetch)
          {
            print(paste("Downloading Assessment Year '",assessment[y], "' [",y," of ",length(assessment),"]",sep=""))
            dfiv <- ReadDataBase(ReadDB,saveDB,paste("AY",y,sep=""),startDate,endDate,site_id[f],"IV","USGS")
            #dfiv <- readNWISuv(site_id[f],parameterCd,paste(startDate,"T00:00",sep=""),paste(endDate,"T23:59",sep=""),tz=Sys.timezone())
            
            cn <- gsub('dateTime','Date',gsub('...Bottom.','',gsub('...Top.','',colnames(dfiv))))
            #colnames(dfiv)[colnames(dfiv)=="dateTime"] <- "Date"
            colnames(dfiv) <- cn
            
            if(length(dfiv) == 12)
            {
              dfiv[4] <- rowMeans(dfiv[,c(4,6)],na.rm = TRUE)
              dfiv[8] <- rowMeans(dfiv[,c(8,10)],na.rm = TRUE)
              dfiv <- dfiv[-c(6,7,10,11)]
              colnames(dfiv) <- gsub('_TOP','',gsub('_BOTTOM','',colnames(dfiv)))
            }
            
            #dfdv <- readNWISdv(site_id[f],parameterCd,startDate,endDate,statCd = )
            
            if(nrow(dfiv) == 0)
            {
              print("Note: No Assessment Year data was found...")
              asse <- NULL
            }else
            {
              dfdv <- DfMean(dfiv,1)
              print("Done: Downloading Assessment Year...")
            }
            if(y == length(assessment))
            {
              asy_fetch <- FALSE
            }
            
            # if(saveDB){
            #   dbNumRows <- dbNumRows + nrow(b_dfiv) + nrow(b_dfdv)
            #   SaveDataBase(dfiv,dfdv,paste("Assessment Year ",y,sep=""),site_id[f])
            # }
          }
        },error = function(err){
          stop("WARNING: Assessment Year could not be loaded.")
          asse = NULL
        })
      }
    }
  }
}

GetDataFromDatabase<-function(db,startDate,endDate,site_usedin,interval){
  
  con <- dbConnect(SQLite(), dbname = paste(getwd(),"/database/jax_harbor_database.db",sep=""))
  rDF <- data.frame()
  
  for(i in 1:(length(db)))
  {
    sql<- paste("SELECT * FROM ",db[i],"
                WHERE strftime('%Y-%m-%d %H:%M:%S',DATE) >= strftime('%Y-%m-%d %H:%M:%S', '",startDate," 00:00:00') AND
                strftime('%Y-%m-%d %H:%M:%S',DATE) <= strftime('%Y-%m-%d %H:%M:%S', '",endDate," 23:59:59') AND
                site_no_usedin = '",site_usedin,"' AND Interval = '",interval,"'",sep="")
    
    res<-dbSendQuery(con,sql)
    rows<- rbind.data.frame(dbFetch(res)) 
    rows$Date <- as.POSIXct(rows$Date)
    rows <- rows[-c(match(c("Interval","Year Used","site_no_usedin","downloaded_date"),names(rows)))]
    
    if(db[i] == "USGS")
    {
      rows <- convertDataFrame(rows,db[i])
      
      if(class(rows) == "list"){
        rows <- rows[[1]]
      }
    }
    
    if(length(rDF) == 0){
      rDF <- rbind.data.frame(rDF,rows)
    }else{
      rDF <- rbind.data.frame(rDF,rows[c(names(rDF))])
    }
  }
  
  dbClearResult(res)
  dbDisconnect(con)
  return(rDF)
}

ReadDataBase<-function(ReadDB=FALSE,saveDB,year_used,startDate,endDate,site,interval="IV",db="USGS",parameterCd=c("00010","00095"),listFiles=NULL,fileExt=""){
  con <- dbConnect(SQLite(), dbname = paste(getwd(),"/database/jax_harbor_database.db",sep=""))
  rows <- data.frame()
  #parameterCd <- c("00010","00095","00480","00060","00065","00300","63160","63680","72137","32319","65231","99988")

  if(length(site) > 1)
  {
    site_usedin <- site[2]
    site <- site[1]
  }else
  {
    site_usedin <- site
  }
  
  tryCatch({
    #grepl('cd',names(rows)[5],fixed = TRUE)

    if(ReadDB)
    {
      if(db == "USGS")
      {
        sql<- paste("SELECT * FROM ",db,"
                  WHERE strftime('%Y-%m-%d %H:%M:%S',DATE) >= strftime('%Y-%m-%d %H:%M:%S', '",startDate," 00:00:00') AND
                  strftime('%Y-%m-%d %H:%M:%S',DATE) <= strftime('%Y-%m-%d %H:%M:%S', '",endDate," 23:59:59') AND
                  site_no_usedin = '",site_usedin,"' AND Interval = '",interval,"'",sep="")
      }else
      {
        sql<- paste("SELECT * FROM ",db,"
                  WHERE [Year Used] = 'Baseline' AND
                  site_no_usedin = '",site_usedin,"' AND Interval = '",interval,"'",sep="")
      }
      
      res<-dbSendQuery(con,sql)
      rows<- rbind.data.frame(dbFetch(res)) 
      rows$Date <- as.POSIXct(rows$Date)
      rows <- rows[-c(match(c("Interval","Year Used","site_no_usedin","downloaded_date"),names(rows)))]
      
      dbClearResult(res)
    }else
    {
      if(db == "USGS")
      {
        tryCatch({
          rows <- readNWISuv(site,parameterCd,paste(startDate,"T00:00",sep=""),paste(endDate,"T23:59",sep=""),tz=Sys.timezone())
          r_cn <- gsub('dateTime','Date',gsub('...Bottom.','',gsub('...Top.','',colnames(rows))))
          names(rows) <- r_cn
          
          if(length(rows) == 12)
          {
            rows[4] <- rowMeans(rows[,c(4,6)],na.rm = TRUE)
            rows[8] <- rowMeans(rows[,c(8,10)],na.rm = TRUE)
            rows <- rows[-c(6,7,10,11)]
            colnames(rows) <- gsub('_TOP','',gsub('_BOTTOM','',colnames(rows)))
          }
          
          rows$Date <- as.POSIXct(rows$Date)
          
          r <- rows[,!grepl('_cd',names(rows),fixed=TRUE)]
          r <- r[-c(match("site_no",names(r)),match("Date",names(r)))]
          rows <- rows[rowSums(is.na(r)) != ncol(r),]
          rows <- rows[!is.na(rows$Date),]
          
          if(rows[["site_no"]][1] == "02246500")
          {
            rows <- rows[rows[["Date"]] > as.POSIXct("2015-11-30 23:59:59"),]
          }
          
        },error = function(err){
          print(paste("No Data found from: Site '",site,"' data for '",startDate," - ",endDate,"'.",sep=""))
        })
      }else
      {
        tryCatch({
          
          #Grab dates from USGS Baseline. This will remove duplicate baseline data.
          sql<- paste("SELECT * FROM ","USGS","
                  WHERE [Year Used] = 'Baseline' AND
                  site_no_usedin = '",site_usedin,"' AND Interval = '",interval,"'",sep="")
          
          res<-dbSendQuery(con,sql)
          usgsRows<- rbind.data.frame(dbFetch(res)) 
          usgsRows$Date <- as.POSIXct(usgsRows$Date)
          usgsRows <- usgsRows[-c(match(c("Interval","Year Used","site_no_usedin","downloaded_date"),names(usgsRows)))]
          dbClearResult(res)
          
          r <- usgsRows[,!grepl('_cd',names(usgsRows),fixed=TRUE)]
          cond <- names(r)[grepl("00095",names(r),fixed = TRUE)]
          r <- r[,c("site_no","Date",cond)]
          dates <- r[!is.na(r[cond]),][["Date"]]
          
        },error = function(err){
          paste("Baseline does not contain USGS data.")
        })
        
        for(i in 1:(length(listFiles)))
        {
          rows<-read.csv(paste("./sjr files/",fileExt,listFiles[i],sep=""),header=TRUE)
          rows[[1]]<-as.character(rows[[1]])
          rows[[2]]<-as.character(rows[[2]])
          rows[[3]]<-as.numeric(rows[[3]])
          rows[[4]]<-as.numeric(rows[[4]])
          rows[[5]]<-as.numeric(rows[[5]])
          
          # Add a default time "00:00" if no time is present in the date
          rows$Date <- ifelse(grepl(" ", rows$Date), 
                              rows$Date, 
                              paste0(rows$Date, " 00:00"))
          
          # Convert the entire column to POSIXct
          rows$Date <- as.POSIXct(strptime(rows$Date, format='%m/%d/%Y %H:%M', tz=Sys.timezone()))
          names(rows) <- c("site_no","Date","WATER_TEMPERATURE","CONDUCTIVITY","SALINITY")
        }
        
        rows <- rows[rowSums(is.na(rows[c(3,4,5)])) != ncol(rows[c(3,4,5)]),]
        rows <- rows[!is.na(rows$Date),]
        site <- rows[[1]][1]
        
        #remove duplicated data in baseline.
        if(length(dates) != 0)
        {
          rows <- rows[!rows$Date %in% dates,]
        }
      }
      
      if(saveDB && nrow(rows) != 0){
        SaveDataBase(con,rows,"IV",year_used,c(site_usedin,site),db)
        SaveDataBase(con,DfMean(rows,1),"DV",year_used,c(site_usedin,site),db)
      }
    }
    
    dbDisconnect(con)
    
    return(rows)
  },error = function(err){
    print(err)
    print(paste("WARNING: ReadDataBase(). Site '",site,"' data for '",startDate," - ",endDate,"' was not saved.",sep=""))
  })
}

#saveDBtoFIle
SaveDataBase<-function(con,rows,interval,year_used,site=NULL,db){
  con <- dbConnect(SQLite(), dbname = paste(getwd(),"/database/jax_harbor_database.db",sep=""))
  #site[1] - site_usedin
  #site[2] - site
  
  tryCatch({
    
    if("site_no" %in% names(rows))
    {
      col_classes <- c("character","character",sapply(rows,class),"character","character")
      r <- cbind.data.frame(interval,year_used,rows,site[1],as.character(Sys.time()))
    }else
    {
      col_classes <- c("character","character","character",sapply(rows,class),"character","character")
      r <- cbind.data.frame(interval,year_used,site[2],rows,site[1],as.character(Sys.time()))
    }
    
    for(i in 1:(ncol(r)))
    {
     r[[i]] <- ToClass(r[[i]],col_classes[i])
    }

    #r$Date <- as.character(r$Date)
    
    #DB SAVE.
    if(db == "USGS")
    {
      names(r) <- c("Interval","Year Used",names(rows),"site_no_usedin","downloaded_date")
      dbWriteTable(conn=con, name="USGS", r, append=TRUE, row.names=FALSE)
      #print(paste(unlist(col_classes),"\n'",site[1],"' '",interval,"' Saved to DB..."))
      #print(r[1,])
    }else
    {
      names(r) <- c("Interval","Year Used","site_no","Date","WATER_TEMPERATURE","CONDUCTIVITY","SALINITY","site_no_usedin","downloaded_date")
      dbWriteTable(conn=con, name="NoaaSJR", r, append=TRUE, row.names=FALSE)
      #print(paste(unlist(col_classes),"\n'",site[1],"' '",interval,"' Saved to DB..."))
      #print(r[1,])
    }
  },error = function(err){
    stop(paste("WARNING: SaveDataBase(). Site ",site," data for ",year_used," was not saved.",sep=""))
  })
}

ToClass<-function(x,class){
  
  if(class == "numeric")
  {
    return (as.numeric(x))
  }
  if(class == "double")
  {
    return (as.double(x))
  }
  
  return (as.character(x))
}

bind_columns<-function(x,vectorY){
  for(i in 1:(length(vector)))
  bind_cols( r , Interval = sprintf(interval,seq(1:(nrow(r)))) )
}