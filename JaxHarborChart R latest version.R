#Version 1.1.0 last update 01/16/2020 by KLA CESAJ-EN-DG.
#Made for USACE FOUO.
#Remarks:
#Date format stored in dataframes: MM-DD-YYYY HH:MM.
#This a year based srtatistics script, everything gets analyzed in years (12months, 52 weeks, 3 quarters).


###################### WORD DOCUMENT FUNCTIONS #############################

# add a title.
addParapraph=function(doc, my.title, font_size=14, bold=FALSE, text_align="left"){
  my.prop=fp_text(font.size = font_size, bold = bold, font.family = "Times", )
  the.title=fpar(ftext(my.title, prop=my.prop), text.align = text_align)
  doc = body_add_fpar(doc, the.title)
  return(doc)
}

# add an image, such as a jpg or png file
addImage=function(doc, image, h=5, w=5){
  doc = body_add_img(doc, src=image,
                     height=h, width=w,
                     style="centered")
  return(doc)
}

# start landscape
startLandscape=function(doc){
  doc=body_end_section_continuous(doc)
  return(doc)
}
# end landscape
endLandscape=function(doc){
  doc=body_end_section_landscape(doc)
  return(doc)
}

# add an empty line
add.empty.line=function(doc){
  doc = body_add_par(doc, " ")
  return(doc)
}
#add page break

addPageBreak=function(doc){
  doc = body_add_break(doc, pos="after")
  return(doc)
}

# add a data frame as a table - R 3.6
addTable3.6=function(doc, tbl, col.keys=NULL, col.digits=NULL){
  # create basic flextable
  # f.table=qflextable(tbl)
  # # set numbers of decimals for numeric variables, if specified
  # if(!is.null(col.keys)){
  #   for(j in 1:length(col.keys)){
  #     f.table=colformat_num(x=f.table,
  #                           col_keys=col.keys[j],
  #                           digits=col.digits[j])
  #   }
  # }
  # 
  # # set table borders
  # f.table=border_outer(f.table, part="all",
  #                      border=fp_border(color="black", width = 1))
  # f.table=border_inner_h(f.table, part="all",
  #                        border=fp_border(color="black", width = 1))
  # f.table=border_inner_v(f.table, part="all",
  #                        border=fp_border(color="black", width = 1))
  # 
  # # set fonts
  # f.table=font(f.table,  fontname = "Times", part = "all")
  # # also set the table's header font as bold
  # f.table=bold(f.table, part = "header")
  # 
  # # add the table to the document
  # flextable::
  
  doc = body_add_table(doc, value = tbl)
  # value = f.table, 
  #align = "left" )
  return(doc)
}

# add a data frame as a table
addTable=function(doc, tbl, col.keys=NULL, col.digits=NULL){
  # create basic flextable
  f.table=qflextable(tbl)
  # set numbers of decimals for numeric variables, if specified
  if(!is.null(col.keys)){
    for(j in 1:length(col.keys)){
      f.table=colformat_num(x=f.table,
                            col_keys=col.keys[j],
                            digits=col.digits[j])
    }
  }
  
  # set table borders
  f.table=border_outer(f.table, part="all",
                       border=fp_border(color="black", width = 1))
  f.table=border_inner_h(f.table, part="all",
                         border=fp_border(color="black", width = 1))
  f.table=border_inner_v(f.table, part="all",
                         border=fp_border(color="black", width = 1))
  
  # set fonts
  f.table=font(f.table,  fontname = "Times", part = "all")
  # also set the table's header font as bold
  f.table=bold(f.table, part = "header")
  
  # add the table to the document
  doc <- flextable::body_add_flextable(doc, 
                                       value = f.table, 
                                       align = "left" )
  return(doc)
}

########################### FILE CREATION ##################################

CreateWordDocWithGeneratedTitlePage<-function(title,bl,asse,p,filename,site_id,b,a,startYear,endYear){
  
  extension <- ".docx"
  
  #ooc <- docx(title=title)
  doc = new.word.doc()
  
  # Add titles for first page.
  #options( "ReporteRs-fontsize" = 24 )
  #options( "ReporteRs-fontweigh" = "bold" )
  #options('ReporteRs-default-font' = "Times New Roman")
  #ooc <- addParagraph(doc,paste(getPeriod(p)," Statistics ",filename,sep=""),par.properties = chprop( parProperties(), text.align = "center" ))
  #ooc <- addParagraph(doc,paste("for ",length(site_id)," site/s",sep=""),par.properties = chprop( parProperties(), text.align = "center" ))
  
  doc <- addParagraph(doc, paste(getPeriod(p)," Statistics ",filename,sep=""), 24, TRUE, "center")
  doc <- addParagraph(doc, paste("for ",length(site_id)," site/s",sep=""), 24, TRUE, "center")
  
  if(!is.null(bl))
  {
    #ooc <- addParagraph(doc,paste("Base Period:\nfrom ",months(b[1])," ",as.numeric(format.Date(b[1],"%d")),", ",format.Date(b[1],"%Y")," - ",months(b[2])," ",as.numeric(format.Date(b[2],"%d")),", ",format.Date(b[2],"%Y"),sep=""), par.properties = chprop( parProperties(), text.align = "center" ))
    doc <- addParagraph(doc, paste("Base Period:\nfrom ",months(b[1])," ",as.numeric(format.Date(b[1],"%d")),", ",format.Date(b[1],"%Y")," - ",months(b[2])," ",as.numeric(format.Date(b[2],"%d")),", ",format.Date(b[2],"%Y"),sep=""), 24, TRUE, "center")
  }
  
  if(!is.null(asse))
  {
    #ooc <- addParagraph(doc, paste("Assessment Period:\nfrom ",getMonthCharacter(a[1])," 1, ",a[2]," - ",getMonthCharacter(a[3])," ",getmonthEndDay(a[3],a[4]),", ",a[4],sep=""),par.properties = chprop( parProperties(), text.align = "center" ))
    doc <- addParagraph(doc, paste("Assessment Period:\nfrom ",getMonthCharacter(a[1])," 1, ",a[2]," - ",getMonthCharacter(a[3])," ",getmonthEndDay(a[3],a[4]),", ",a[4],sep=""), 24, TRUE, "center")
  }
  
  printYear <- "YEAR"
  
  if(startYear == endYear)
  {
    printYear <- startYear
  }else
  {
    printYear <- paste(startYear,"-",endYear,sep="")
  }
  
  #ooc <- addParagraph(doc, paste("\n\nGenerated on: ",months(Sys.Date()),format.Date(Sys.Date()," %d, %Y "),format(Sys.time(),"%H:%M:%S"),sep=""), par.properties = parProperties(text.align = "center"))
  #ooc <- addPageBreak(doc)
  
  doc <- addParagraph(doc, paste("\n\nGenerated on: ",months(Sys.Date()),format.Date(Sys.Date()," %d, %Y "),format(Sys.time(),"%H:%M:%S"),sep=""), 24, TRUE, "center")
  doc <- addPageBreak(doc)
  
  #options( "ReporteRs-fontsize" = 12 )
  
  # Add  a table
  #doc <- addSection(doc, landscape = TRUE)
  doc <- startLandscape(doc)
  
  return(doc)
}

isDFValuesOverN<-function(df,n){
  c <- 0
  
  df <- as.data.frame(df)
  for(j in 1:(ncol(df)))
  {
    if(c != 0)
    {
      return(TRUE)
    }
    
    for(i in 1:(nrow(df)))
    {
      if(!is.na(as.double(df[i,j])))
      {
        c <- c + sum(as.double(df[i,j]) >= n)
      }else if(grepl("±",df[i,j]))
      {
        c <- c + sum(as.double(unlist(regmatches(df[i,j],gregexpr("[[:digit:]]+\\.*[[:digit:]]*",df[i,j])))) >= n)
      }else if(grepl("/",df[i,j]))
      {
        c <- c + sum(as.double(unlist(regmatches(df[i,j],gregexpr("[[:digit:]]+\\.*[[:digit:]]*",df[i,j])))) >= n)
      }else if (grepl("-",df[i,j]))
      {
        c <- c + sum(as.double(unlist(regmatches(df[i,j],gregexpr("[[:digit:]]+\\.*[[:digit:]]*",df[i,j])))) >= n)
      }
    }
  }
  
  return(c != 0)
}

DataFrameRoundToUSGS<-function(df){
  df <- as.data.frame(df)
  df[] <- lapply(df, as.character)
  
  if(isDFValuesOverN(df,11))
  {
    r <- 1
  }else
  {
    r <- 2
  }
  
  for(j in 1:(ncol(df)))
  {
    for(i in 1:(nrow(df)))
    {
      if(grepl("±",df[i,j]))
      {
        x <- as.double(unlist(regmatches(df[i,j],gregexpr("[[:digit:]]+\\.*[[:digit:]]*",df[i,j]))))
        x <- format(round(x,r),nsmall = r)
        df[i,j] <- paste(x[1],"  (±",x[2],")",sep="")
        
      }else if(grepl("/",df[i,j]))
      {
        x <- as.double(unlist(regmatches(df[i,j],gregexpr("[[:digit:]]+\\.*[[:digit:]]*",df[i,j]))))
        x <- format(round(x,r),nsmall = r)
        df[i,j] <- paste(x[1]," / ",x[2],sep="")
        
      }else if (grepl("-",df[i,j]))
      {
        x <- as.double(unlist(regmatches(df[i,j],gregexpr("[[:digit:]]+\\.*[[:digit:]]*",df[i,j]))))
        x <- format(round(x,r),nsmall = r)
        df[i,j] <- paste(x[1]," - ",x[2],sep="")
      }else if(!is.na(as.double(df[i,j])) && df[i,j] != "" && !grepl("[a-z]",df[2,1]))
      {
        df[i,j] <- format(round(as.double(df[i,j]),r),nsmall = r)
      }
    }
  }
  
  return(df)
}

VectorRoundToUSGS<-function(v){
  for(i in 1:length(v))
  {
    if(as.double(v[i]) >= 11)
    {
      v[i] <- round(v[i],1)
    }else
    {
      v[i] <- round(v[i],2)
    }
  }
  return(v)
}

GetWeekByNumber<-function(n){
  if(is.finite(n))
  {
    return(paste("Week #",n,sep=""))
  }else if (grepl("L",n, fixed = TRUE))
  {
    x <- as.integer(unlist(regmatches(n,gregexpr("[[:digit:]]+\\.*",n))))[1]
    return (paste ("Week #",x," [Log]",sep=""))
  }
  
  return("")
}

GetMonthByNumber<-function(n){
  
  if(is.finite(n))
  {
    m <- monthAddition(n,11)
    return (month.abb[m])
  }else if (grepl("L",n, fixed = TRUE))
  {
    x <- as.integer(unlist(regmatches(n,gregexpr("[[:digit:]]+\\.*",n))))[1]
    m <- monthAddition(x,11)
    return (paste (month.abb[m]," [Log]",sep=""))
  }
  
  return("")
}

GetSeasonByNumber<-function(n){
  
  if(is.finite(n))
  {
    if(n == 1)
    {
      return (getSeason(12))
    }else if (n == 2)
    {
      return (getSeason(4))
    }else if (n == 3)
    {
      return (getSeason(8))
    }
  }else if (grepl("L",n, fixed = TRUE))
  {
    x <- as.integer(unlist(regmatches(n,gregexpr("[[:digit:]]+\\.*",n))))[1]
    
    if(x == 1)
    {
      return (paste (getSeason(12)," [Log]",sep=""))
    }else if (x == 2)
    {
      return (paste (getSeason(4)," [Log]",sep=""))
    }else if (x == 3)
    {
      return (paste (getSeason(8)," [Log]",sep=""))
    }
  }
  
  return("")
}

GetPlotTitleName<-function(n,period,plotName,site,type="Report"){
  
  period <- floor(period)
  pn <- GrabPlotNumber(n)
  
  if(grepl("L",pn, fixed = TRUE))
  {
    pt <- " [Log]"
  }else
  {
    pt <- ""
  }
  
  if(type == "Report")
  {
    if(grepl("bplot",n, fixed = TRUE) != TRUE && grepl("mplot",n, fixed = TRUE) != TRUE)
    {
      if(period == 1)
      {
        return (paste(getPeriod(period)," - ",GetWeekByNumber(pn)," - ",plotName," - ",site," - F",sep=""))
      }else if (period == 2)
      {
        return (paste(getPeriod(period)," - ",GetMonthByNumber(pn)," - ",plotName," - ",site," - F",sep=""))
      }else if (period == 3)
      {
        return (paste(getPeriod(period)," - ",GetSeasonByNumber(pn)," - ",plotName," - ",site," - F",sep=""))
      }
    }
    
    return(paste(getPeriod(period),pt," - ",plotName," - ",site," - F",sep=""))
    
  }else
  {
    if(grepl("bplot",n, fixed = TRUE) != TRUE && grepl("mplot",n, fixed = TRUE) != TRUE)
    {
      if(period == 1)
      {
        return (paste(plotName," - ",GetWeekByNumber(pn)," - ",site," - F",sep=""))
      }else if (period == 2)
      {
        return (paste(plotName," - ",getPeriod(period)," - ",GetMonthByNumber(pn)," - ",site," - F",sep=""))
      }else if (period == 3)
      {
        return (paste(plotName," - ",getPeriod(period)," - ",GetSeasonByNumber(pn)," - ",site," - F",sep=""))
      }
    }
    
    return(paste(plotName," - ",getPeriod(period),pt," - ",site," - F",sep=""))
  }
}

GrabPlotNumber<-function(x){
  y <- x
  
  if(grepl( "/", x, fixed = TRUE)){
    splt_string <- strsplit(y, split = "/")[[1]]
    y <- splt_string[[length(splt_string)]]
  }
  
  if(grepl( "-", y, fixed = TRUE)){
    splt_string <- strsplit(y, split = "-")[[1]]
    y_length <- length(splt_string)
    
    if(y_length < 4){##single plot
      return(1)
    }
  }
  
  #cplot-(01)-555555-P1.jpg  -- returns whats inside ( ).
  pattern="-(.*?)-"
  result <- regmatches(y,regexec(pattern,y))
  result <- result[[1]][2]
  
  if(!is.na(as.integer(result)))
  {
    return(as.integer(result))
  }
  
  return(substr(result,1,3))
}

GetIncrementInteger<-function(n){
  
  if(n > 30)
  {
    return(4)
  }else if (n <= 30 && n > 20)
  {
    return(3)
  }else if (n <= 20 && n > 15)
  {
    return(2)
  }else if (n <= 15 && n > 10)
  {
    return(1.5)
  }else if (n <= 10 && n > 5)
  {
    return(1)
  }else if (n <= 5 && n > 3)
  {
    return(.5)
  }else if (n <= 3 && n > 1)
  {
    return(.25)
  }else if (n <= 1 && n > 0)
  {
    return(.1)
  }
  
  return(1)
}

RoundSalinityValues<-function(data){
  if(is.data.frame(data) == TRUE)
  {
    #d <- data[["SALINITY"]]
    data <- data[data["SALINITY"] > 0.01,]#Remove negative values.
    
    df1 <- data[data["SALINITY"] < 11,]
    df2 <- data[data["SALINITY"] >= 11,]
    
    df1 <- df1[is.finite(df1[["SALINITY"]]),]
    df2 <- df2[is.finite(df2[["SALINITY"]]),]
    
    if(nrow(df1) > 0){
      df1["SALINITY"] <- round(df1["SALINITY"],2)
    }
    
    if(nrow(df2) > 0){
      df2["SALINITY"] <- round(df2["SALINITY"],1)
    }
    
    data <- rbind.data.frame(df1,df2)
    #na_removed <- nrow(data) - nrow(new_data)
    
    # if(na_removed > 0){
    #   print(paste(na_removed," NA's removed from plotting data frame.",sep=""))
    # }
    
    data <- data[do.call(order, data),] 
  }else
  {
    ####
    #### CHANGE TABLES TO MATCH PREVIOUS ROUND - KLA CHANGE.
    ####
    
    data <- data[data >= 0.01] #Remove negative values.
    
    data1 <- round(data[data < 11],2)
    data2 <- round(data[data >= 11],1)
    data <- c(data1,data2)
  }
  
  return(data)
}

BinomConfInterval<-function(successes,total){
  
  tryCatch({
    ci <- BinomCI(x=successes,n=total, conf.level = 0.95, method = "clopper-pearson")
    return (ci[1,])
  },error = function(err) {
    ci <- c(NA,NA,NA)
    names(ci) <- c("est","lwr.ci","upr.ci")
    return(ci)
  })
}

ConfInterval<-function(data,cl,old=FALSE){
  
  tryCatch({
    if(!old)
    {
      if(unique(data) == 1)
      {
        ci <- rep(unique(d),3)
        names(ci) <- c("mean","lwr.ci","upr.ci")
      }else
      {
        sink(tempfile())#disable warning messages when all data are the same.
        on.exit(sink()) 
        invisible(ci <- MeanCI(na.omit(data), method="boot", conf.level = cl, type="basic", R=1000))
      }
    }else
    {
      ci <- CI(na.omit(data),cl)
      names(ci) <- c("upr.ci","est","lwr.ci")
    }
    
    return (ci)
  },error = function(err) {
    ci <- c(NA,NA,NA)
    names(ci) <- c("mean","lwr.ci","upr.ci")
    return(ci)
  })
}

#GenerateYearStatisticsDocx - create a docx file with statistics information.
#Created by KLA on 05/16/2018.
#Last update by KLA on 01/16/2020.
#Parameters: filename(character),site_id(character or vector),
#            parameterCd(character or vector),startDate(character),
#            tz(character),period(numeric)
#Returns: nothing.
#Status: tested.
GenerateYearStatisticsDocx<-function(filename,site_id,assessment,baseline){
  ReportWrite <- TRUE
  AppendixWrite <- TRUE
  db <- "USGS"
  middle <- NULL
  m_list <- NULL
  period <- c(1,1.5,2,3,4)
  dbNumRows <- 0
  ReadDB <- TRUE
  saveDB <- FALSE
  DISABLE_GRAPH_GENERATE <- FALSE
  
  ### Plots ###
  stressTableList <- list()
  stressTableNames <- c()
  cumfreq_tables <- list()
  
  tempTableList <- list()
  tempTableNames <- c()
  start_time <- Sys.time()
  pastSiteID <- ""
  runOncePerSite <- TRUE
  
  
  ### Clear Temp Folder ###
  if(IsTempFolderNotEmpty()){
    #ClearTempFolder()
  }
  
  for(f in 1:(length(site_id)))
  {
    b_list <- list()
    y_past <- 0#reset y_past due to change of station.
    
    ##### All Assessment Year Data #####
    aListAllYears <- data.frame()
    aVectorAllYears <- c()
    #############+
    
    for(y in 1:(length(assessment)))
    {
      print(paste("Generating Statistics for: ",getSiteName(site_id[f],FALSE)," [",f," of ",length(site_id),"].",sep=""))
      g <- 1
      if(y == 1)
      {
        bl_fetch <- TRUE
      }else
      {
        bl_fetch <- FALSE
      }
      
      asy_fetch <- TRUE
      
      for(g in 1:(length(period)))
      {
        print(paste("Generating ",getPeriod(period[g])," Report",sep=""))
        
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
          mid <- middle
          
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
            #b_list2 <- list()
            #Fetch Baseline Year Data.
            print(paste('FETCH BL DATA? ',bl_fetch,sep=""))
            if(bl_fetch)
            {
              b_dfiv <- NULL
              b_dfdv <- NULL
              hb_dfiv <- NULL
              hb_dfdv <- NULL
              b_list <- NULL
              print("Downloading Baseline Year...")
              
              #b_dfiv2 <- read.csv("./baselineIV.csv",header=TRUE)
              b_dfiv<- ReadDataBase(ReadDB,saveDB,"Baseline",baseline[1],baseline[2],site_id[f],interval="IV","USGS")
              #b_cn <- gsub('dateTime','Date',gsub('...Bottom.','',gsub('...Top.','',colnames(b_dfiv))))
              #colnames(b_dfiv)[colnames(b_dfiv)=="dateTime"] <- "Date"
              # colnames(b_dfiv) <- b_cn
              # 
              # if(length(b_dfiv) == 12)
              # {
              #   b_dfiv[4] <- rowMeans(b_dfiv[,c(4,6)],na.rm = TRUE)
              #   b_dfiv[8] <- rowMeans(b_dfiv[,c(8,10)],na.rm = TRUE)
              #   b_dfiv <- b_dfiv[-c(6,7,10,11)]
              #   colnames(b_dfiv) <- gsub('_TOP','',gsub('_BOTTOM','',colnames(b_dfiv)))
              # }
              #nyears <- length(unique(format.Date(b_dfiv[["Date"]],format='%Y',tz=Sys.timezone())))
              
              #stime <- as.integer(substr(getIntervalNumber(b_dfiv["Date"]),start=1,stop=2))
              
              #nyears * stime * (365 * 24) * .5
              tryCatch({
                b_list <- DFtoPlotFormat(convertDataFrame(b_dfiv,"USGS"))
                
                if(site_id[f] %in% c("02246500","02245340","301204081434900","02246518"))
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
                
                
                #b_list <- list.append(b_list,convertDataFrame(b_dfiv,"USGS","bottom"))
              },error = function(err) {
                print("WARNING: Some Baseline Year data was not loaded.")
              }) # END tryCatch
              
              #b_list2 <- list.append(b_list2,convertDataFrame(b_dfiv2,"USGS","top"))
              #b_list2 <- list.append(b_list2,convertDataFrame(b_dfiv2,"USGS","bottom"))
              
              #colnames(b_dfiv)[colnames(b_dfiv)=="dateTime"] <- "Date"
              #for(i in 1:(length(b_list2)))
              #{
              #  b_list2[[i]]$Date <- as.POSIXct(strptime(b_list2[[i]]$Date,format='%m/%d/%Y %H:%M',tz=Sys.timezone()))
              #}
              
              #t_types <- c("character","numeric","Date","numeric","character","numeric","character","numeric","character","numeric","character","character")
              #b_dfiv <- convertDfColumn(b_dfiv,t_types,"%m/%d/%Y %H:%M")
              #b_dfiv2 <- convertDfColumn(b_dfiv2,t_types,"%m/%d/%Y %H:%M")
              
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
                  
                  if(!is.na(match("site_no",names(hb_dfiv))))
                  {
                    hb_dfiv <- hb_dfiv[-c(match("site_no",names(hb_dfiv)))]
                    hb_dfdv <- hb_dfdv[-c(match("site_no",names(hb_dfdv)))]
                  }
                  
                  if(!is.na(match("site_no",names(hb_dfiv))))
                  {
                    b_dfiv <- b_dfiv[-c(match("site_no",names(b_dfiv)))]
                    b_dfdv <- b_dfdv[-c(match("site_no",names(b_dfdv)))]
                  }
                  
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
          
          #Fetch Assessment Year Data.
          tryCatch({
            
            print(paste('FETCH ASY DATA? ',asy_fetch,sep=""))
            if(asy_fetch)
            {
              print(paste("Downloading Assessment Year '",assessment[y], "' [",y," of ",length(assessment),"]",sep=""))
              dfiv <- ReadDataBase(ReadDB,saveDB,paste("AY",y,sep=""),startDate,endDate,site_id[f],"IV","USGS")
              #dfiv <- readNWISuv(site_id[f],parameterCd,paste(startDate,"T00:00",sep=""),paste(endDate,"T23:59",sep=""),tz=Sys.timezone())
              # cn <- gsub('dateTime','Date',gsub('...Bottom.','',gsub('...Top.','',colnames(dfiv))))
              # #colnames(dfiv)[colnames(dfiv)=="dateTime"] <- "Date"
              # colnames(dfiv) <- cn
              # 
              # if(length(dfiv) == 12)
              # {
              #   dfiv[4] <- rowMeans(dfiv[,c(4,6)],na.rm = TRUE)
              #   dfiv[8] <- rowMeans(dfiv[,c(8,10)],na.rm = TRUE)
              #   dfiv <- dfiv[-c(6,7,10,11)]
              #   colnames(dfiv) <- gsub('_TOP','',gsub('_BOTTOM','',colnames(dfiv)))
              # }
              
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
          
          # tryCatch({
          #   m_list <- list()
          #   if(!is.null(mid))
          #   {
          #     print("Downloading Middle Year...")
          #     if(db == "USGS")
          #     {
          #       m_dfiv <- readNWISuv(site_id[f],parameterCd,paste(mid[1],"T00:00",sep=""),paste(mid[2],"T23:59",sep=""),tz=Sys.timezone())
          #       m_cn <- gsub('dateTime','Date',gsub('...Bottom.','',gsub('...Top.','',colnames(m_dfiv))))
          #       #colnames(m_dfiv)[colnames(m_dfiv)=="dateTime"] <- "Date"
          #       colnames(m_dfiv) <- m_cn
          #       
          #       if(length(m_dfiv) == 12)
          #       {
          #         m_dfiv[4] <- rowMeans(m_dfiv[,c(4,6)],na.rm = TRUE)
          #         m_dfiv[8] <- rowMeans(m_dfiv[,c(8,10)],na.rm = TRUE)
          #         b <- m_dfiv[-c(6,7,10,11)]
          #         colnames(m_dfiv) <- gsub('_TOP','',gsub('_BOTTOM','',colnames(m_dfiv)))
          #       }
          #       
          #       #m_dfiv <- read.csv("./middleIV.csv",header=TRUE)
          #       
          #       tryCatch({
          #         m_list <- convertDataFrame(m_dfiv,"USGS")
          #         #m_list <- list.append(m_list,convertDataFrame(m_dfiv,"USGS","bottom"))
          #       },error = function(err) {
          #         #do nothing.
          #       }) # END tryCatch
          #       
          #       #colnames(m_dfiv)[colnames(m_dfiv)=="dateTime"] <- "Date"
          #       #for(i in 1:(length(m_list)))
          #       #{
          #       #  m_list[[i]]$Date <- as.POSIXct(strptime(m_list[[i]]$Date,format='%m/%d/%Y %H:%M',tz=Sys.timezone()))
          #       #}
          #     }
          #     #t_types <- c("character","numeric","Date","numeric","character","numeric","character","numeric","character","numeric","character","character")
          #     #m_dfiv <- convertDfColumn(m_dfiv,t_types,"%m/%d/%Y %H:%M")
          #     
          #     if(nrow(m_dfiv) == 0)
          #     {
          #       print("Note: No Middle Year data was found...")
          #       mid = NULL
          #     }else
          #     {
          #       m_dfdv <- DfMean(m_dfiv,1)
          #       print("Done: Downloading Middle Year...")
          #     }
          #   }
          #   
          # },error = function(err) {
          #   print("WARNING: Middle Year could not be loaded.")
          #   mid = NULL
          # }) # END tryCatch
          
          # if(db == "USGS")
          # {
          # }else if(db == "NOAA")
          # {
          #   # product <- c("water_temperature","conductivity","salinity")
          #   # 
          #   # df <- data.frame()
          #   # dfiv <- data.frame()
          #   # 
          #   # for(i in 1:(length(product)))
          #   # {
          #   #   noaaStartDate <- as.Date(startDate)
          #   #   noaaEndDate <- as.Date(startDate) + 31
          #   #   
          #   #   while (noaaStartDate < endDate)
          #   #   {
          #   #     tryCatch({
          #   #       if(length(df) != 0)
          #   #       {
          #   #         d <- coops_search(station_name = as.numeric(site_id[f]), time_zone = "gmt", begin_date = format.Date(noaaStartDate,"%Y%m%d"),
          #   #                           end_date = format.Date(noaaEndDate,"%Y%m%d"), product = product[i])[[2]]
          #   #         df <- rbind(df,d)
          #   #       }else
          #   #       {
          #   #         df <- coops_search(station_name = as.numeric(site_id[f]), time_zone = "gmt", begin_date = format.Date(noaaStartDate,"%Y%m%d"),
          #   #                            end_date = format.Date(noaaEndDate,"%Y%m%d"), product = product[i])[[2]]
          #   #       }
          #   #     }, warning = function(war) {
          #   #       # do nothing.
          #   #     }, error = function(err) {
          #   #       # do nothing.
          #   #     }) # END tryCatch
          #   #     
          #   #     noaaStartDate <- noaaEndDate + 1
          #   #     
          #   #     if(noaaEndDate+31 < endDate)
          #   #     {
          #   #       noaaEndDate <- noaaEndDate + 31
          #   #     }else
          #   #     {
          #   #       noaaEndDate <- endDate
          #   #     }
          #   #   }
          #   #   df <- df[,-c(3)]
          #   #   names(df) <- c("Date",toupper(product[i]))
          #   #   
          #   #   if(length(dfiv) == 0)
          #   #   {
          #   #     dfiv <- df
          #   #   }else
          #   #   {
          #   #     setDT(dfiv)
          #   #     setDT(df)
          #   #     dfiv <- dfiv[df, on = 'Date']
          #   #   }
          #   #   rm(d)
          #   #   df <- data.frame()
          #   # }
          #   # 
          #   # dfdv <- DfMean(dfiv,1)
          #   
          #   # dfiv$fDATE <- factor(substr(as.character(dfiv$Date),1,10))
          #   # wt <- tapply(dfiv[[2]], list(dfiv$fDATE), mean,na.rm=TRUE)#WATER_TEMPERATURE
          #   # cond <- tapply(dfiv[[3]], list(dfiv$fDATE), mean,na.rm=TRUE)#CONDUCTIVITY
          #   # sal <- tapply(dfiv[[4]], list(dfiv$fDATE), mean,na.rm=TRUE)#SALINITY
          #   # Date <- tapply(dfiv[[1]], dfiv$fDATE, mean)#Date
          #   #
          #   # dfdv <- as.data.frame(cbind(Date,wt,cond,sal))
          #   # dfdv$Date <- with(dfdv, as.POSIXct("1970-01-01", "%Y-%m-%d", tz=Sys.timezone()) + Date)
          #   # dfdv$Date <- format.Date(dfdv$Date,"%Y-%m-%d")
          #   # rownames(dfdv) <- NULL
          #   # dfiv[[5]] <- NULL
          #   
          # }else if (db == "SJR")
          # {
          #   #setwd("D:/R/Jax Harbor/R/sjr files") #-- does not work during run time.
          #   #startDate <- as.Date("2000-01-01 00:00:00")
          #   #endDate <- as.Date("2000-12-31 23:59:00")
          #   #temp = list.files(pattern="*.csv")
          #   #for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))
          #   
          #   #sjrfiles = lapply(temp, read.delim)
          #   #rList <- list()
          #   dfiv <- data.frame()
          #   temp = list.files(path=paste("./sjr files/",sep=""),pattern="*.csv")
          #   
          #   for(i in 1:(length(temp)))
          #   {
          #     df<-read.csv(paste("./sjr files/",temp[i],sep=""),header=TRUE)
          #     df[[1]]<-as.character(df[[1]])
          #     df$Date <- as.POSIXct(strptime(df$Date,format='%Y-%m-%d %H:%M',tz=Sys.timezone()))
          #     
          #     df[df==-999999]<-NA
          #     df[df<0]<-NA
          #     
          #     if(length(dfiv)==0)
          #     {
          #       dfiv <- df
          #     }else
          #     {
          #       dfiv <- rbind.data.frame(dfiv,df)
          #     }
          #     rm(df)
          #     dfiv <- dfiv[dfiv$Site == site_id[f] & !is.na(dfiv$Site),]
          #     dfiv <- dfiv[dfiv$Date >= as.POSIXct(startDate) & dfiv$Date <= as.POSIXct(endDate) & !is.na(dfiv$Date),]
          #     
          #     
          #     dfiv <- dfiv[ -c(1,3:4) ]
          #     names(dfiv) <- c("Date","WATER_TEMPERATURE","CONDUCTIVITY","SALINITY")
          #     
          #     dfdv <- DfMean(dfiv,1)
          #     
          #     # dfiv$fDATE <- factor(substr(as.character(dfiv$Date),1,10))
          #     # WATER_TEMPERATURE <- tapply(dfiv[[2]], list(dfiv$fDATE), mean,na.rm=TRUE)#WATER_TEMPERATURE
          #     # CONDUCTIVITY <- tapply(dfiv[[3]], list(dfiv$fDATE), mean,na.rm=TRUE)#CONDUCTIVITY
          #     # SALINITY <- tapply(dfiv[[4]], list(dfiv$fDATE), mean,na.rm=TRUE)#SALINITY
          #     # Date <- tapply(dfiv[[1]], dfiv$fDATE, mean)#Date
          #     #
          #     #
          #     # dfdv <- as.data.frame(cbind(Date,WATER_TEMPERATURE,CONDUCTIVITY,SALINITY))
          #     # dfdv$Date <- with(dfdv, as.POSIXct("1970-01-01", "%Y-%m-%d", tz=Sys.timezone()) + Date)
          #     # dfdv$Date <- format.Date(dfdv$Date,"%Y-%m-%d")
          #     # rownames(dfdv) <- NULL
          #     # dfiv[[5]] <- NULL
          #     
          #     #setwd("D:/R/Jax Harbor/R")
          #   }
          # }
        }
        
        if(!is.null(asse) || !is.null(mid) || !is.null(bl))
        {
          if(!is.null(asse))
          {
            if(g == 1){
              print(paste("AY-",y," Dates: [",startDate," to ",endDate,"] Length: [dfiv: ",length(dfiv[[1]]),", dfdv: ",length(dfdv[[1]]),"]",sep=""))
            }
            
            listofATables <- createStatistics(dfiv,dfdv,startDate,endDate,period[g],startMonth,startYear,db)
          }
          
          # if(!is.null(mid))
          # {
          #   m_startDate<- range(m_dfiv$Date,na.rm = TRUE)[1]
          #   m_endDate <- range(m_dfiv$Date,na.rm = TRUE)[2]
          #   m_startMonth <- months(startDate)
          #   m_startYear <- format.Date(m_startDate,"%Y")
          #   m_endYear <- format.Date(m_endDate,"%Y")
          #   
          #   listofMTables <- createStatistics(m_dfiv,m_dfdv,m_startDate,m_endDate,period[g],startMonth,m_startYear,db)
          # }
          
          if(!is.null(bl))
          {
            b_startDate<- range(b_dfiv$Date,na.rm = TRUE)[1]
            b_endDate <- range(b_dfiv$Date,na.rm = TRUE)[2]
            b_startMonth <- months(startDate)
            b_startYear <- format.Date(b_startDate,"%Y")
            b_endYear <- format.Date(b_endDate,"%Y")
            
            if(g == 1){
              print(paste("BL Dates: [",b_startDate," to ",b_endDate,"] Length: [dfiv: ",length(b_dfiv[[1]]),", dfdv: ",length(b_dfdv[[1]]),"]",sep=""))
            }
            
            listofBTables <- createStatistics(b_dfiv,b_dfdv,b_startDate,b_endDate,period[g],startMonth,b_startYear,db)
          }
          
          tableNames <- c()
          
          if(!is.null(asse))
          {
            for(i in 1:(length(listofATables)))
            {
              tableNames <- c(tableNames,names(listofATables)[i])
            }
          }
          # 
          # if(!is.null(mid))
          # {
          #   if(m_startYear == m_endYear)
          #   {
          #     m_printYear <- m_startYear
          #   }else
          #   {
          #     m_printYear <- paste(m_startYear,"-",m_endYear,sep="")
          #   }
          #   
          #   m_tableNames <- c()
          #   for(i in 1:(length(listofMTables)))
          #   {
          #     m_tableNames <- c(m_tableNames,names(listofMTables)[i])
          #     my_text <- pot(paste(filename,".",f,".",period[g],".t",tb," - ",getPeriod(period[g])," Statistics for ",getSiteName(site_id[f],""),sep=""),textProperties(font.weight='bold', font.size = 12))
          #     doc <- addParagraph(doc, my_text)
          #     tb <- tb + 1
          #     doc <- addParagraph(doc, paste("Database: ",db,"      Site: ",site_id[f],"      Observation: ",names(listofMTables)[i],"      Year: ",m_printYear," [Middle]",sep=""))
          #     options( "ReporteRs-fontsize" = 9 )
          #     data <- light.table(listofMTables[[i]])
          #     data <- setZebraStyle(data,odd = '#eeeeee', even = 'white')
          #     data <- setFlexTableWidths(data, c(.9,0.7,1.5,.5,.5,.6,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5))
          #     doc <- addFlexTable(doc,data,par.properties=parCenter())
          #     options( "ReporteRs-fontsize" = 12 )
          #     doc<-addPageBreak(doc)
          #   }
          # }
          if(!is.null(bl))
          {
            #   if(b_startYear == b_endYear)
            #   {
            #     b_printYear <- b_startYear
            #   }else
            #   {
            #     b_printYear <- paste(b_startYear,"-",b_endYear,sep="")
            #   }
            #   
            b_tableNames <- c()
            for(i in 1:(length(listofBTables)))
            {
              b_tableNames <- c(b_tableNames,names(listofBTables)[i])
              #     my_text <- pot(paste(filename,".",f,".",period[g],".t",tb," - ",getPeriod(period[g])," Statistics for ",getSiteName(site_id[f],""),sep=""),textProperties(font.weight='bold', font.size = 12))
              #     doc <- addParagraph(doc, my_text)
              #     tb <- tb + 1
              #     doc <- addParagraph(doc, paste("Database: ",db,"      Site: ",site_id[f],"      Observation: ",names(listofBTables)[i],"      Year: ",b_printYear," [Baseline]",sep=""))
              #     options( "ReporteRs-fontsize" = 9 )
              #     data <- light.table(listofBTables[[i]])
              #     data <- setZebraStyle(data,odd = '#eeeeee', even = 'white')
              #     data <- setFlexTableWidths(data, c(.9,0.7,1.5,.5,.5,.6,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5))
              #     doc <- addFlexTable(doc,data,par.properties=parCenter())
              #     options( "ReporteRs-fontsize" = 12 )
              #     doc<-addPageBreak(doc)
            }
          }
          
          if(exists("listofATables"))
          {
            for(i in 1:(length(listofATables)))
            {
              salLevel <- gsub('Sal(Cond) ','',names(listofATables)[i],fixed = TRUE)
              if(salLevel == "Sal(Cond)")
              {
                salLevel = 'NA'
              }
              tempTableList <- list.append(tempTableList,listofATables[[i]])
              tempTableNames <- c(tempTableNames,paste("Y",y,"-A-P",period[g],"-",salLevel,"-",site_id[f],sep=""))
            }
          }
          if(exists("listofBTables") && y < 2)
          {
            for(i in 1:(length(listofBTables)))
            {
              salLevel <- gsub('Sal(Cond) ','',names(listofATables)[i],fixed = TRUE)
              if(salLevel == "Sal(Cond)")
              {
                salLevel = 'NA'
              }
              tempTableList <- list.append(tempTableList,listofBTables[[i]])
              tempTableNames <- c(tempTableNames,paste("Y0-B-P",period[g],"-",salLevel,"-",site_id[f],sep=""))
            }
          }
          if(exists("listofMTables"))
          {
            for(i in 1:(length(listofMTables)))
            {
              salLevel <- gsub('Sal(Cond) ','',names(listofATables)[i],fixed = TRUE)
              if(salLevel == "Sal(Cond)")
              {
                salLevel = 'NA'
              }
              tempTableList <- list.append(tempTableList,listofMTables[[i]])
              tempTableNames <- c(tempTableNames,paste("Y",y,"-M-P",period[g],"-",salLevel,"-",site_id[f],sep=""))
            }
          }
          
          plotDataList <- list()
          
          #Daily Values First.
          if(db == "USGS")
          {
            #Get calculated Salinity from Conductance.
            plotDataList <- DFtoPlotFormat(convertDataFrame(dfiv,db))
            #plotDataList <- list.append(plotDataList,convertDataFrame(dfiv,db,"top"))
            #plotDataList <- list.append(plotDataList,convertDataFrame(dfiv,db,"bottom"))
            #plotDataList <- list.append(plotDataList,convertDataFrame(dfiv,db,"bottom"))
            #names(plotDataList) <- c("DV Salinity(Conductance) TOP","IV Salinity(Conductance) TOP","DV Salinity(Conductance) BOTTOM","IV Salinity(Conductance) BOTTOM")
          }else if(db == "NOAA")
          {
            plotDataList <- list(dfiv)
          }else if (db == "SJR")
          {
            plotDataList <- list(dfiv)
          }
          #names(plotDataList) <- tableNames
          
          for(i in 1:5)#Boxplot | Cum Freq | Histograms [3 total graphs].
          {
            for(j in 1:(length(plotDataList)))#Cum Freq.
            {
              if(length(m_list) == 0)
              {
                mList <- NULL
              }else
              {
                mList <- m_list[[j]]
                mList <- RoundSalinityValues(mList)
              }
              
              if(length(b_list) == 0)
              {
                bList <- NULL
              }else
              {
                bList <- b_list[[j]]
                bList <- RoundSalinityValues(bList)
              }
              
              if(length(plotDataList) == 0)
              {
                aList <- NULL
              }else
              {
                aList <- plotDataList[[j]]
                total_rows <- nrow(aList)
                aList <- RoundSalinityValues(aList)
                new_total_rows <- nrow(aList)
                
                if(i == 1 && !(y %in% aVectorAllYears)){
                  aVectorAllYears <- c(aVectorAllYears, y)
                  print(paste("aListAllYears [",nrow(aListAllYears),"] aList NA's removed [",total_rows - new_total_rows,"]",sep=""))
                  aListAllYears <- rbind.data.frame(aListAllYears,aList)
                  print(paste("Boxplot: aListAllYears [",nrow(aListAllYears),"] was updated...",sep=""))
                }
              }
              
              ### Box Plot ###
              if(i == 1 && length(b_list) != 0 && y == length(assessment) && period[g] == floor(period[g]) && !DISABLE_GRAPH_GENERATE)#Boxplots
              {  print("GENERATING BOXPLOT.")
                #b_boxplotToDocx(doc,aList,bList,mList,period[g],tableNames[j],site_id[f],FALSE,startDate,c(filename,f,period[g],fg))
                b_boxplotToDocx("doc",aList,bList,mList,aListAllYears,period[g],tableNames[j],site_id[f],FALSE,startDate,c(filename,f,period[g],fg))
                fg <- fg + 1
                #b_boxplotToDocx(doc,aList,bList,mList,period[g],tableNames[j],site_id[f],TRUE,startDate,c(filename,f,period[g],fg))
                #fg <- fg + 1
              }
              
              ### Cum Freq ###
              if(i == 2 && length(b_list) != 0 && y == length(assessment) && period[g] == floor(period[g]) && !DISABLE_GRAPH_GENERATE)#Cumulative Frequency
              {
                print("GENERATING CUM FREQ.")
                cumfreq_tables <- b_cumFreqOrHistogramToDocx("doc",aList,bList,mList,period[g],tableNames[j],site_id[f],"cumfreq",startDate,c(filename,f,period[g],fg),cumfreq_tables)
                fg <- FigureCounter(fg,period[g])
                # if(period[g] == period[length(period)])
                # {
                #   save(cumfreq_tables, file="ctables.RData")
                # }
              }
              
              # ## Histogram ###
              if(i == 3 && length(b_list) != 0 && y == length(assessment) && period[g] == floor(period[g]) && !DISABLE_GRAPH_GENERATE)#Histograms
              {  print("GENERATING HISTOGRAM.")
                #b_cumFreqOrHistogramToDocx(doc,aList,bList,mList,period[g],tableNames[j],"histogram1",startDate)
                b_cumFreqOrHistogramToDocx("doc",aList,bList,mList,period[g],tableNames[j],site_id[f],"histogram2",startDate,c(filename,f,period[g],fg))
                fg <- FigureCounter(fg,period[g])
                fg <- FigureCounter(fg,period[g])
              }
              
              ### Salinity Stress Table ###
              if(i == 4 && length(b_list) != 0 && y == length(assessment) && period[g] ==  period[length(period)])#Stress Table for last year.
              {
                print("GENERATING STRESS TABLE.")
                
                a_dv <- DfMean(aList,1)
                b_dv <- DfMean(bList,1)
                m_dv <- DfMean(mList,1)
                
                #salinityStressTable(doc,a_dv,b_dv,m_dv,paste(tableNames[j]," [Daily Value]",sep=""),site_id[f],db,c(filename,f,period[g],tb))
                stressTableList <- list.append(stressTableList,salinityStressTable(f,a_dv,b_dv,m_dv,paste(tableNames[j]," [Daily Value]",sep=""),site_id[f],db,c(filename,f,period[g],tb)))
                stressTableNames <- c(stressTableNames,paste("Station ",site_id[f],sep=""))
              }
              
              ## Moving Avg Plot ###
              if(i == 5 && length(b_list) != 0 && y == length(assessment) && period[g] ==  period[length(period)] && !DISABLE_GRAPH_GENERATE)#sevenDayMovingAveragePlot
              {
                print("GENERATING 7-DAY MOV AVG.")
                sevenDayMovingAveragePlot("doc",aList,bList,mList,period[g],tableNames[j],site_id[f],startDate,c(filename,f,period[g],fg))
              }
            }
          }
          # Write the Word document to a file
          #writeDoc(doc, file = getFileName(paste(filename,".",f,".",period[g]," ",getPeriod(period[g])," Statistics for ",getSiteName(site_id[f],"")," from ",printYear,sep=""),extension))
          
        }else
        {
          print("No Report Was Generated.")
        }
        
        pastSiteID <- site_id[f]
        y_past <- y
      }
    }
    
    #y_past <- 0#reset y_past due to change of station.
  }
  #years done.
  
  # if(length(cumfreq_tables) == 0)
  # {
  #   load("ctables.RData")
  # }
  
  names(tempTableList) <- tempTableNames
  
  for(i in 1:(length(tempTableList)))
  {
    tempTableList[[i]]["Ci.90 (Lo-Up)"] <- paste(tempTableList[[i]][["Ci.90.Lo"]]," - ",tempTableList[[i]][["Ci.90.Up"]],sep = '')
    tempTableList[[i]]["Ci.95 (Lo-Up)"] <- paste(tempTableList[[i]][["Ci.95.Lo"]]," - ",tempTableList[[i]][["Ci.95.Up"]],sep = '')
    tempTableList[[i]]["Mean_Stdev"] <- paste(tempTableList[[i]][["Mean"]]," (±",tempTableList[[i]][["Stdev"]],")",sep = '')
    tempTableList[[i]]["Q90 / Q95"] <- paste(tempTableList[[i]][["Q.90"]]," / ",tempTableList[[i]][["Q.95"]],sep = '')
    tempTableList[[i]]["Min_Max"] <- paste(tempTableList[[i]][["Min"]]," - ",tempTableList[[i]][["Max"]],sep = '')
  }
  
  #x <- FilterListByStringInName(tempTableList,c("-A-", "Y1-", site_id[1]),paste("P",1,sep=""))
  nmL <- names(tempTableList)
  bl <- NULL
  if(length(FilterListByStringInName(tempTableList,c("-B-"))) > 0)
  {
    bl <- "Baseline"
  }
  col_names <-  c(bl, paste0("AY",1:(length(assessment))))
  station_locations <- c("TOP","MIDDLE","BOTTOM","NA")[c(length(nmL[grepl("TOP", nmL,fixed = TRUE)]) > 0, length(nmL[grepl("MIDDLE", nmL,fixed = TRUE)]) > 0, length(nmL[grepl("BOTTOM", nmL,fixed = TRUE)]) > 0, length(nmL[grepl("NA", nmL,fixed = TRUE)]) > 0)]
  
  #save.image(file='myEnvironment.RData')
  #save(list = c("tempTableList","site_id","period","assessment","bl","col_names","station_locations"),file='data.RData')
  
  
  #load(file='myEnvironment.RData')
  #load(file='data.RData')
  
  
  aa <- Tier1_AnnualALLStations(tempTableList,site_id,period,assessment,bl,col_names,station_locations)
  
  bb <- Tier1_Annual_ByStation(tempTableList,site_id,period,assessment,bl,col_names,station_locations)
  
  cc <- Tier1_Seasonal_ByStation(tempTableList,site_id,period,assessment,bl,col_names,station_locations)
  
  dd <- Tier1_Monthly_ByStation(tempTableList,site_id,period,assessment,bl,col_names)
  
  ee <- Tier1Weekly_FreqWksOverBaseline(tempTableList,site_id,period,assessment,bl,col_names)
  
  ff <- Tier1_Weekly_ByWeek_Appendix(tempTableList,site_id,period,assessment,bl,col_names)
  
  tablesList <- list(AnnualALLStations=aa,Annual_ByStation=bb,Seasonal_ByStation=cc,
                     Monthly_ByStation=dd,Weekly_FreqWksOverBaseline=ee,Weekly_ByWeek_Appendix=ff)
  
  #Create a Word document.
  extension <- ".docx"
  
  doc <- CreateWordDocWithGeneratedTitlePage("Generated Report Statistics",bl,asse,period[g],filename,site_id,c(b_startDate,b_endDate),c(startMonth,startYear,endMonth,endYear),startYear,endYear)
  
  tableOfContents <- 
    "Outline of Results			Report Table #

  Tier 1
  Section 1:	Annual time-scale: All stations		\t\t4,5 & 6

  Section 2: 	Annual, seasonal, monthly, and weekly time-scales: by station
  \tStation 1 - annual    \t\t\t\t\t\t7
  \tStation 1 - seasonal    \t\t\t\t\t\t8
  \tStation 1 - monthly
  \t\tMean(+/- STD)	\t\t\t\t9
  \t\t90/95  Percentile	\t\t\t\t10
  \t\tRange (min- Max)	\t\t\t\t11
  \tStation 1 - weekly	  \t\t\t\t\t12
  \tStation 2 - annual  	\t\t\t\t\t13
  \tStation 2 - seasonal  	\t\t\t\t\t14
  \tStation 2 - monthly
  \t\tMean(+/- STD)	\t\t\t\t15
  \t\t90/95  Percentile	\t\t\t\t16
  \t\tRange (min- Max)	\t\t\t\t17
  \tStation 2 - weekly	  \t\t\t\t\t18
  \tStation 3 - annual  	\t\t\t\t\t19
  \tStation 3- seasonal	  \t\t\t\t\t20
  \tStation 3 - monthly
  \t\tMean(+/- STD)	\t\t\t\t21
  \t\t90/95  Percentile	\t\t\t\t22
  \t\tRange (min- Max)	\t\t\t\t23
  \tStation 3 - weekly  	\t\t\t\t\t24
  \tStation 4.	\t\t\t\t\t\t\t25,26,27,28,29,30
  \tStation 5.	\t\t\t\t\t\t\t30,31,32,33,34,35
  \tStation 6.	\t\t\t\t\t\t\t36,37,38,39,40,41

  Tier 2
  \tStation 1	  	\t\t\t\t\t42
  \tStation 2	  	\t\t\t\t\t43
  \tStation 3	  	\t\t\t\t\t44	45
  \tStation 4.	  	\t\t\t\t\t46
  \tStation 5.	  	\t\t\t\t\t47	48
  \tStation 6.	  	\t\t\t\t\t49	50
  All stations Annual, Seasonal, Monthly 90/95 Percentile Exceedance Summary	51"
  
  #options( "ReporteRs-fontsize" = 8 )
  #ooc <- addParagraph(doc,tableOfContents)
  #ooc <- addPageBreak(doc)
  
  doc <- addParapraph(doc, tableOfContents, font_size=8)
  doc <- addPageBreak(doc)
  #options( "ReporteRs-fontsize" = 12 )
  
  #Tier 1
  section1 <- c(1,5.1) #once.
  section2 <- c(2,3,4,5.2)#per station.
  p_order <- c(4,3,2,1)
  #section2 <- list(P4=c(2),P3=c(3),P2=c(4),P1=c(5.2))
  
  #Tier 2
  #SAV Frequency of salinity Stress
  
  tryCatch({
    if(!is.null(assessment))
    {
      tableLngth <- length(tablesList)
      
      
      ### Report Populate. ###
      if(ReportWrite)
      {
        t_counter <- 4
        f_counter <- 6
        
        #Section 1.
        print("Creating Docx Section 1.")
        #ooc <- addParagraph(doc,pot("Tier 1",textProperties(font.weight='bold', font.size = 18)))
        #ooc <- addParagraph(doc,pot("Section 1: Summary Statistics of All stations",textProperties(font.weight='bold', font.size = 16)))
        
        doc <- addParapraph(doc, "Tier 1", 18, TRUE)
        doc <- addParapraph(doc, "Section 1: Summary Statistics of All stations", 16, TRUE)
        
        for(i in 1:(length(section1)))
        {
          if(section1[i] <= tableLngth)
          {
            print(paste("Creating Docx Table ",i,".",sep=""))
            doc_counter_list <- ReturnDocxTable(doc,tablesList,section1[i],assessment,"Report","",t_counter)
            doc <- doc_counter_list[[1]]
            t_counter <- doc_counter_list[[2]]
          }
        }
        
        #Section 2.
        print("Creating Docx Section 2.")
        #ooc <- addParagraph(doc,pot("Section 2: Annual, seasonal, monthly, and weekly time-scales by station",textProperties(font.weight='bold', font.size = 16)))
        
        doc <- addParapraph(doc, "Section 2: Annual, seasonal, monthly, and weekly time-scales by station", 16, TRUE)
        
        for(h in 1:(length(site_id)))# num of sites
        {
          for(i in 1:(length(section2)))#periods
          {
            #ooc <- addParagraph(doc,pot(paste("Station ",h," - ",getPeriod(p_order[i])),textProperties(font.weight='bold', font.size = 14)))
            
            doc <- addParapraph(doc, paste("Station ",h," - ",getPeriod(p_order[i])), 14, TRUE)
            
            #Add Boxplot
            b_names <- GetPlotImageNames('bplot',site_id[h],paste("P",p_order[i],sep=""))
            if(length(b_names) != 0)
            {
              doc <- AddPlotImagesToWordDoc(doc,b_names,8,5,p_order[i],"Boxplot",getSiteName(site_id[h]),f_counter)
              f_counter <- f_counter + length(b_names)
            }
            
            #Add CumFreqs.
            if(p_order[i] != 1)
            {
              c_names <- GetPlotImageNames('cplot',site_id[h],paste("P",p_order[i],sep=""))
              if(length(c_names) != 0)
              {
                doc <- AddPlotImagesToWordDoc(doc,c_names,6,4,p_order[i],"Cum Freq",getSiteName(site_id[h]),f_counter)
                f_counter <- f_counter + length(c_names)
              }
            }
            
            #Add 7DayMovAvg Plots.
            if(p_order[i] == 1)
            {
              ma_names <- GetPlotImageNames('mplot',site_id[h],paste("P",p_order[i],sep=""))
              if(length(ma_names) != 0)
              {
                doc <- AddPlotImagesToWordDoc(doc,ma_names,9,5,p_order[i],"Moving Average Assessment Year Salinity",getSiteName(site_id[h]),f_counter)
                f_counter <- f_counter + length(ma_names)
              }
            }
            
            #AddTable.
            if(section2[i] <= tableLngth)
            {
              paste(getPeriod(i)," - T",t_counter,sep="")
              doc_counter_list <- ReturnDocxTable(doc,tablesList,section2[i],assessment,"Report",paste("Station ",h,sep=""),t_counter)
              doc <- doc_counter_list[[1]]
              t_counter <- doc_counter_list[[2]]
            }
          }
        }
        
        print("Tier 2.")
        #ooc <- addParagraph(doc,pot("Tier 2",textProperties(font.weight='bold', font.size = 18)))
        #ooc <- addParagraph(doc,pot("SAV Frequency of salinity Stress",textProperties(font.weight='bold', font.size = 16)))
        
        doc <- addParagraph(doc,"Tier 2", 18, TRUE)
        doc <- addParagraph(doc,"SAV Frequency of salinity Stress", 16, TRUE)
        
        for(i in 1:(length(stressTableList)))
        {
          my_text <- paste("Stress Table for Station ",i," - ",getSiteName(site_id[i])," - T",t_counter,sep="")
          t_counter <- t_counter + 1
          #salinityStressTable(a_dv, b_dv, m_dv, paste(tableNames[j]," [Daily Value]",sep=""),site_id[f],db, c(filename,f,period[g],tb))
          #salinityStressTable(  df, df_b, df_m, graphName                                   ,site_id   ,db, fLabel){
          #options( "ReporteRs-fontsize" = 12 )
          #my_text <- pot(paste(fLabel[1],".",fLabel[2],".",fLabel[3],".t",as.numeric(fLabel[4])," - ",getPeriod(fLabel[3])," Statistics for ",getSiteName(site_id[i],""),sep=""),textProperties(font.weight='bold', font.size = 12))
          #fLabel[4] <- as.numeric(fLabel[4])+1
          # ooc <- addParagraph(doc, my_text)
          # ooc <- addParagraph(doc,paste("Extreme Stress Table for: ", "Sal(Cond) [Daily Value]"),par.properties = chprop( parProperties(), text.align = "center" ))
          
          # ooc <- addParagraph(doc,stressTableList[[i]][[2]][1],par.properties = chprop( parProperties() ))
          # ooc <- addParagraph(doc,stressTableList[[i]][[2]][2],par.properties = chprop( parProperties() ))
          # ooc <- addFlexTable(doc,stressTableList[[i]][[1]],par.properties=parCenter())
          # ooc<-addPageBreak(doc)
          
          doc <- addParagraph(doc, my_text)
          doc <- addParagraph(doc, paste("Extreme Stress Table for: ", "Sal(Cond) [Daily Value]"), text_align = "center" )
          
          doc <- addParagraph(doc,stressTableList[[i]][[2]][1])
          doc <- addParagraph(doc,stressTableList[[i]][[2]][2])
          doc <- addTable(doc,stressTableList[[i]][[1]])
          doc <- addPageBreak(doc)
        }
        
        # Write the Word document to a file
        #ooc <- addSection(doc, landscape = FALSE)
        doc <- endLandscape(doc)
        wd <- getwd()
        setwd(paste(getwd(),"/Reports",sep=""))
        #writeDoc(doc, file = getFileName("Report",extension))
        print(doc, target=getFileName("Report",extension))
        setwd(wd)
      }
      
      ### Appendix Populate. ###
      if(AppendixWrite)
      {
        print("Creating Docx Appendices.")
        
        t_counter <- 1
        f_counter <- 1
        
        appendix <- list(P4=c(),P3=c(3),P2=c(),P1=c(6))#per station.
        p_order <- c(4,3,2,1)
        
        for(h in 1:(length(site_id)))# num of sites
        {
          a_doc <- CreateWordDocWithGeneratedTitlePage(paste("Generated Appendix (",LETTERS[h],".1) Statistics",sep=""),bl,asse,period[g],paste("Appendix (",LETTERS[h],".1)",sep=""),site_id,c(b_startDate,b_endDate),c(startMonth,startYear,endMonth,endYear),startYear,endYear)
          a_docH <- CreateWordDocWithGeneratedTitlePage(paste("Generated Appendix (",LETTERS[h],".2) Statistics",sep=""),bl,asse,period[g],paste("Appendix (",LETTERS[h],".2)",sep=""),site_id,c(b_startDate,b_endDate),c(startMonth,startYear,endMonth,endYear),startYear,endYear)
          
          #a_ooc <- addParagraph(a_doc,pot(paste("Appendix ",LETTERS[h],sep=""),textProperties(font.weight='bold', font.size = 18)))
          a_doc <- addParagraph(a_doc, paste("Appendix ",LETTERS[h],sep=""), 18, TRUE)
          #a_doc <- addParagraph(a_doc,pot("Tier 1",textProperties(font.weight='bold', font.size = 16)))
          
          #a_docH <- addParagraph(a_docH,pot(paste("Appendix ",LETTERS[h],sep=""),textProperties(font.weight='bold', font.size = 18)))
          a_docH <- addParagraph(a_docH, paste("Appendix ",LETTERS[h],sep=""), 18, TRUE)
          #a_docH <- addParagraph(a_docH,pot("Tier 1",textProperties(font.weight='bold', font.size = 16)))
          
          #a_doc <- addSection(a_doc, landscape = TRUE)
          
          for(i in 1:(length(p_order)))#periods
          {
            #a_doc <- addParagraph(a_doc,pot(paste("Station ",h," - ",getPeriod(p_order[i])),textProperties(font.weight='bold', font.size = 14)))
            
            #Add CumFreqs.
            if(p_order[i] == 1)
            {
              c_names <- GetPlotImageNames('cplot',site_id[h],paste("P",p_order[i],sep=""))
              if(length(c_names) != 0)
              {
                a_doc <- AddPlotImagesToWordDoc(a_doc,c_names,6,4,p_order[i],paste(LETTERS[h]," . ",i," . Cum Freq",sep=""),getSiteName(site_id[h]),f_counter,"Appendix")
                #a_doc <- AddPlotImagesToAppendixWordDoc(a_doc,c_names,6,4,paste(LETTERS[h],".",i,".F - ",getSiteName(site_id[h]),sep=""),f_counter)
                f_counter <- f_counter + length(c_names)
              }
            }
            
            #Add Histograms.
            h_names <- GetPlotImageNames('hplot',site_id[h],paste("P",p_order[i],sep=""))
            if(length(h_names) != 0)
            {
              #a_docH <- addParagraph(a_docH,pot(paste("Station ",h," - ",getPeriod(p_order[i])),textProperties(font.weight='bold', font.size = 14)))
              a_docH <- AddPlotImagesToWordDoc(a_docH,h_names,8,5,p_order[i],paste(LETTERS[h]," . ",i," . Histogram",sep=""),getSiteName(site_id[h]),f_counter,"Appendix")
              #a_docH <- AddPlotImagesToAppendixWordDoc(a_docH,h_names,8,5,paste(LETTERS[h],".",i,".F - ",getSiteName(site_id[h]),sep=""),f_counter)
              f_counter <- f_counter + length(h_names)
            }
            
            #AddTable.
            if(section2[i] <= tableLngth && !is.null(appendix[[i]]))
            {
              for(j in 1:(length(appendix[[i]])))
              {
                #paste(getPeriod(i)," - T",t_counter,sep="")
                a_doc_counter_list <- ReturnDocxTable(a_doc,tablesList,appendix[[i]][j],assessment,"APPX",paste("Station ",h,sep=""),t_counter,paste(LETTERS[h]," . ",i," . ",sep=""))
                a_doc <- a_doc_counter_list[[1]]
                t_counter <- a_doc_counter_list[[2]]
              }
            }
          }
          
          # Write the Word document to a file
          #a_ooc <- addSection(a_doc, landscape = FALSE)
          #a_oocH <- addSection(a_docH, landscape = FALSE)
          
          a_doc <- endLandscape(a_doc)
          a_docH <- endLandscape(a_docH)
          
          wd <- getwd()
          setwd(paste(getwd(),"/Reports",sep=""))
          #writeDoc(a_doc, file = getFileName(paste("Appendix ",LETTERS[h],".1",sep=""),extension))
          #writeDoc(a_docH, file = getFileName(paste("Appendix ",LETTERS[h],".2",sep=""),extension))
          
          print(doc, target=getFileName(paste("Appendix ",LETTERS[h],".1",sep=""),extension))
          print(doc, target=getFileName(paste("Appendix ",LETTERS[h],".2",sep=""),extension))
          setwd(wd)
        }
      }
      
      #clear stored images from hdd.
      if(IsTempFolderNotEmpty()){
        #ClearTempFolder()
      }
    }
  },error = function(err)
  {
    stop("UH OH, AN ERROR OCCURED GENERATING THE DOCX DOCUMENT.")
    #do nothing.
  })
  
  #list.search(x, identical(., 'A-'))
  #FilterListByArrayOfConditions(tempTableList,c(site_id[1],"Y1-","A-"))
  
  #list.filter(tempTableList, grepl(site_id[f],type))
  #str(list.filter(tempTableList, TRUE))
  #Filter(tempTableList,function(x) grepl(site_id[f],x))
  tryCatch({
    #print(paste("Total Rows Used: ",dbNumRows,sep=""))
    print(Sys.time() - start_time)
    #print(paste("Completed. Runtime: ",total_time,"",sep=""))
  },error = function(err)
  {
    
    #do nothing.
  })
}

AddPlotImagesToWordDoc<-function(doc,img,w=9,h=6,period,plotName,site_name,cnt,type="Report"){
  
  if(length(img) != 0)
  {
    for(i in 1:(length(img)))
    {
      #name <- gsub(paste(getwd(),"/temp/",sep=""),'',img[i],fixed=TRUE)
      #name <- gsub('.jpg','',name,fixed=TRUE)
      #ooc <- addParagraph(doc,paste(GetPlotTitleName(as.character(img[i]),period,plotName,site_name,type),cnt+i-1,sep=""))
      #ooc <- addImage(doc, img[i],width=w,height=h)
      #ooc <- addPageBreak(doc)
      
      doc <- addParapraph(doc, paste(GetPlotTitleName(as.character(img[i]),period,plotName,site_name,type),cnt+i-1,sep=""), font_size=12)
      doc <- addImage(doc, img[i],h=h,w=w)
      doc <- addPageBreak(doc)
    }
  }
  
  return(doc)
}

GetPlotImageNames<-function(name=NULL,station=NULL,period=NULL,num=NULL){
  x <- c(name,station,period,num)
  
  if(length(x) != 0)
  {
    rVector <- GetListOfTempFiles()
    
    for(i in 1:(length(x)))
    {
      rVector <- rVector[grepl(x[i],rVector,fixed = TRUE)]
    }
    
  }else
  {
    return(NULL)
  }
  
  return(rVector)
}

roundToUSGS<-function(x){
  
  if(!is.finite(x))
  {
    if(grepl("±",x))
    {
      x <- unlist(regmatches(x,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",x)))
      return(paste(x[1]))
    }else if(grepl("/",x))
    {
      x <- unlist(regmatches(x,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",x)))
    }else if (grepl("-",x))
    {
      x <- unlist(regmatches(x,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",x)))
    }
    
  }else
  {
    if(length(x[x >= 11]) != 0)
    {
      return (format(round(x,1), nsmall = 1))
    }else
    {
      return (format(round(x,2), nsmall = 2))
    }
  }
  
  return(x)
}

#Created by KLA 01/15/2020.
Tier1_Weekly_ByWeek_Appendix<-function(tList,sites,periods,asy,bl,col_names){
  print("Tier1_Weekly_ByWeek_Appendix")
  removeRows <- seq(1,52*2,by=2)
  weeksVector <- c(paste0("Week ",seq(1:52)),"AVERAGE of Averages","Weeks Exceeding Average")
  stnVector = c()
  rdf = NULL
  tableVector <- c()
  listofStns <- list()
  listofTables <- list()
  
  tables <- c("Mean_Stdev","Q90 / Q95","Min_Max")
  
  for(i in 1:(length(sites)))
  {
    for(k in 1:(length(tables)))
    {
      if(!is.null(bl))
      {
        df <- FilterListByStringInName(tList,c("Y0-B-P1-",sites[i]))[[1]]
        BL_MEAN_SD <- df[tables[k]][-removeRows,]
        
        if(tables[k] %in% c("Mean_Stdev","Q90 / Q95","Min_Max"))
        {
          if(tables[k] == "Mean_Stdev")
          {
            nums1 <- (df["Mean"][-removeRows,])
            nums2 <- (df["Stdev"][-removeRows,])
            avg_nums1 <- (mean(nums1[is.finite(nums1)]))
            avg_nums2 <- (mean(nums2[is.finite(nums2)]))
            
            avgOfAvg <- paste(avg_nums1," (±",avg_nums2,")",sep="")
            
            v1 <- nums1 > avg_nums1
            v2 <- nums2 > avg_nums2
            v1[is.na(v1)] <- FALSE
            v2[is.na(v2)] <- FALSE
            
            numExceedAvg <- sum(v1 & v2)
            
          }else if (tables[k] == "Q90 / Q95")
          {
            nums1 <- (df["Q.90"][-removeRows,])
            nums2 <- (df["Q.95"][-removeRows,])
            avg_nums1 <- (mean(nums1[is.finite(nums1)]))
            avg_nums2 <- (mean(nums2[is.finite(nums2)]))
            
            avgOfAvg <- paste(avg_nums1," / ",avg_nums2,sep="")
            
            v1 <- nums1 > avg_nums1
            v2 <- nums2 > avg_nums2
            v1[is.na(v1)] <- FALSE
            v2[is.na(v2)] <- FALSE
            
            numExceedAvg <- sum(v1 & v2)
            
          }else if (tables[k] == "Min_Max")
          {
            nums1 <- (df["Min"][-removeRows,])
            nums2 <- (df["Max"][-removeRows,])
            avg_nums1 <- (mean(nums1[is.finite(nums1)]))
            avg_nums2 <- (mean(nums2[is.finite(nums2)]))
            
            avgOfAvg <- paste(avg_nums1," - ",avg_nums2,sep="")
            
            v1 <- nums1 > avg_nums1
            v2 <- nums2 > avg_nums2
            v1[is.na(v1)] <- FALSE
            v2[is.na(v2)] <- FALSE
            
            numExceedAvg <- sum(v1 & v2)
          }
        }else
        {
          avgOfAvg <- mean(BL_MEAN_SD)
          numExceedAvg <- sum(BL_MEAN_SD > avgOfAvg)
        }
        
        BL_MEAN_SD <- c(BL_MEAN_SD,avgOfAvg,numExceedAvg)
        rdf <- data.frame(BL_MEAN_SD)
      }
      
      if(length(FilterListByStringInName(tList,c("-A-"))) > 0)
      {
        for(j in 1:(length(asy)))
        {
          df <- FilterListByStringInName(tList,c(paste("Y",j,"-A-P1-",sep=""),sites[i]))[[1]]
          ASY_MEAN_SD <- df[tables[k]][-removeRows,]
          
          if(tables[k] %in% c("Mean_Stdev","Q90 / Q95","Min_Max"))
          {
            if(tables[k] == "Mean_Stdev")
            {
              nums1 <- (df["Mean"][-removeRows,])
              nums2 <- (df["Stdev"][-removeRows,])
              avg_nums1 <- (mean(nums1[is.finite(nums1)]))
              avg_nums2 <- (mean(nums2[is.finite(nums2)]))
              
              avgOfAvg <- paste(avg_nums1," (±",avg_nums2,")",sep="")
              
              v1 <- nums1 > avg_nums1
              v2 <- nums2 > avg_nums2
              v1[is.na(v1)] <- FALSE
              v2[is.na(v2)] <- FALSE
              
              numExceedAvg <- sum(v1 & v2)
              
            }else if (tables[k] == "Q90 / Q95")
            {
              nums1 <- (df["Q.90"][-removeRows,])
              nums2 <- (df["Q.95"][-removeRows,])
              avg_nums1 <- (mean(nums1[is.finite(nums1)]))
              avg_nums2 <- (mean(nums2[is.finite(nums2)]))
              
              avgOfAvg <- paste(avg_nums1," / ",avg_nums2,sep="")
              
              v1 <- nums1 > avg_nums1
              v2 <- nums2 > avg_nums2
              v1[is.na(v1)] <- FALSE
              v2[is.na(v2)] <- FALSE
              
              numExceedAvg <- sum(v1 & v2)
              
            }else if (tables[k] == "Min_Max")
            {
              nums1 <- (df["Min"][-removeRows,])
              nums2 <- (df["Max"][-removeRows,])
              avg_nums1 <- (mean(nums1[is.finite(nums1)]))
              avg_nums2 <- (mean(nums2[is.finite(nums2)]))
              
              avgOfAvg <- paste(avg_nums1," - ",avg_nums2,sep="")
              
              v1 <- nums1 > avg_nums1
              v2 <- nums2 > avg_nums2
              v1[is.na(v1)] <- FALSE
              v2[is.na(v2)] <- FALSE
              
              numExceedAvg <- sum(v1 & v2)
            }
            
          }else
          {
            avgOfAvg <- mean(ASY_MEAN_SD)
            numExceedAvg <- sum(ASY_MEAN_SD > avgOfAvg)
          }
          
          ASY_MEAN_SD <- c(ASY_MEAN_SD,avgOfAvg,numExceedAvg)
          
          if(is.null(rdf))
          {
            rdf <- data.frame(ASY_MEAN_SD)
          }else
          {
            rdf[paste("Year ",j,sep="")] <- ASY_MEAN_SD
          }
        }
      }
      
      rownames(rdf) <- weeksVector
      names(rdf) <- col_names
      
      rdf <- rbind.data.frame(DataFrameRoundToUSGS(rdf[-c(54),]),rdf[c(54),])
      
      if(length(listofTables) == 0)
      {
        listofTables <- list(rdf)
        #tableVector <- c(tableVector,tables[k])
      }else
      {
        listofTables <- list.append(listofTables,rdf)
        #tableVector <- c(tableVector,tables[k])
      }
    }
    
    names(listofTables) <- tables
    listofStns <- list.append(listofStns,listofTables)
    listofTables <- list()
    stnVector <- c(stnVector,paste("Station ",i,sep=""))
  }
  names(listofStns) <- stnVector
  
  return(listofStns)
}

#Created by KLA 01/15/2020.
Tier1Weekly_FreqWksOverBaseline<-function(tList,sites,periods,asy,bl,row_names){
  print("Tier1Weekly_FreqWksOverBaseline")
  table_names <- c("MEAN (±SD)","90th / 95th Percentiles","Range (Min - Max")
  namesTable <- c()
  removeRows <- seq(1,52*2,by=2)
  weeksVector <- paste0("Week #",seq(1:52))
  movingAvgVector <- c()
  stnVector = c()
  stnNameVector = c()
  listOfVectors = list()
  listofMovingAverages = list()
  listofStns <- list()
  Mean_DF = NULL
  
  for(i in 1:(length(sites)))
  {
    if(!is.null(bl))
    {
      df <- FilterListByStringInName(tList,c("Y0-B-P1-",sites[i]))[[1]]
      BL_MEAN_ROWS <- df["Mean"][-removeRows,]
      BL_MEAN_ROWS <- BL_MEAN_ROWS[is.finite(BL_MEAN_ROWS)]
      
      BL_MEAN_SD <- (mean(BL_MEAN_ROWS))
      stnVector <- c(sum(BL_MEAN_ROWS > BL_MEAN_SD))
    }
    
    if(length(FilterListByStringInName(tList,c("-A-"))) > 0 && !is.null(bl))
    {
      for(j in 1:(length(asy)))
      {
        df <- FilterListByStringInName(tList,c(paste("Y",j,"-A-P1-",sep=""),sites[i]))[[1]]
        ASY_MEAN_SD <- df["Mean"][-removeRows,]#Select IV Data.
        ASY_MEAN_SD <- ASY_MEAN_SD[is.finite(ASY_MEAN_SD)]
        
        x <- sum(ASY_MEAN_SD > BL_MEAN_SD)
        x[is.na(x)] <- 0
        stnVector <- c(stnVector,x)
        df2 <- FilterListByStringInName(tList,c(paste("Y",j,"-A-P1.5-",sep=""),sites[i]))[[1]]
        asyMovingAvgDF <- subset(df2, select=c(5,4,7,8,20:21))[-(removeRows+1),]#Select DV Data.
        rownames(asyMovingAvgDF) <- weeksVector
        asyMovingAvgDF <- DataFrameRoundToUSGS(asyMovingAvgDF)
        listofMovingAverages <- list.append(listofMovingAverages,asyMovingAvgDF)
        
        movingAvgVector <- c(movingAvgVector,paste("Y",j,"-A-P1.5-MovAvg-",sites[i],sep=""))
      }
      names(listofMovingAverages) <- movingAvgVector
    }else
    {
      return(NULL)
    }
    listOfVectors <- list.append(listOfVectors,stnVector)
    names(listofMovingAverages) <- movingAvgVector
    listofStns <- list.append(listofStns,listofMovingAverages)
    stnNameVector <- c(stnNameVector,paste("Station ",i,sep=""))
    listofMovingAverages <- list()
    movingAvgVector <- c()
  }
  
  for(i in 1:(length(listOfVectors)))
  {
    #as.data.frame(do.call(rbind, FilterListByStringInName(listOfVectors,c("MEAN",sites[i]))))
    if(is.null(Mean_DF))
    {
      Mean_DF <- data.frame((listOfVectors[[i]]))
      names(Mean_DF) <- paste("Station ",i,sep="")
    }else
    {
      Mean_DF[paste("Station ",i,sep="")] <- (listOfVectors[[i]])
    }
    
  }
  
  names(listofMovingAverages) <- movingAvgVector
  names(listofStns) <- stnNameVector
  rownames(Mean_DF) <- row_names
  return(list(Mean_DF=Mean_DF,MovingAvgs=listofStns))
}

#Created by KLA 01/14/2020.
Tier1_Monthly_ByStation<-function(tList,sites,periods,asy,bl,row_names){
  print("Tier1_Monthly_ByStation")
  table_names <- c("MEAN (±SD)","90th / 95th Percentiles","Range (Min - Max")
  namesTable <- c()
  rListNames <- c()
  listOfVectors = list()
  removeRows = c(1,3,5,7,9,11,13,15,17,19,21,23)
  monthsVector <- substring(FilterListByStringInName(tList,c("P2-",sites[1]))[[1]][-(removeRows+1),][[1]], first = 1, last = 3)
  
  rList <- list()
  
  for(i in 1:(length(sites)))
  {
    if(!is.null(bl))
    {
      df <- FilterListByStringInName(tList,c("Y0-B-P2-",sites[i]))[[1]]
      
      VECTOR_MEAN_SD <- df["Mean_Stdev"][-removeRows,]
      TABLE_90_95   <-  df["Q90 / Q95"][-removeRows,]
      TABLE_MIN_MAX <-  df["Min_Max"][-removeRows,]
      
      if(!is.null(VECTOR_MEAN_SD) && !is.null(TABLE_90_95) && !is.null(TABLE_MIN_MAX))
      {
        listOfVectors <- list.append(listOfVectors,VECTOR_MEAN_SD,TABLE_90_95,TABLE_MIN_MAX)
        namesTable <- c(namesTable,paste("Y0-B-MEAN-","-",sites[i],sep=""),
                        paste("Y0-B-Q90Q95-","-",sites[i],sep=""),paste("Y0-B-MinMax-","-",sites[i],sep="")) 
      }
    }
    
    if(length(FilterListByStringInName(tList,c("-A-"))) > 0)
    {
      for(j in 1:(length(asy)))
      {
        df <- FilterListByStringInName(tList,c(paste("Y",j,"-A-P2-",sep=""),sites[i]))[[1]]
        VECTOR_MEAN_SD <- df["Mean_Stdev"][-removeRows,]
        TABLE_90_95   <-  df["Q90 / Q95"][-removeRows,]
        TABLE_MIN_MAX <-  df["Min_Max"][-removeRows,]
        
        if(!is.null(VECTOR_MEAN_SD) && !is.null(TABLE_90_95) && !is.null(TABLE_MIN_MAX))
        {
          listOfVectors <- list.append(listOfVectors,VECTOR_MEAN_SD,TABLE_90_95,TABLE_MIN_MAX)
          namesTable <- c(namesTable,paste("Y",j,"-A-MEAN-","-",sites[i],sep=""),
                          paste("Y",j,"-A-Q90Q95-","-",sites[i],sep=""),paste("Y",j,"-A-MinMax-","-",sites[i],sep="")) 
        }
      }
    }
  }
  names(listOfVectors) <- namesTable
  
  for(i in 1:(length(sites)))
  {
    Mean_DF <- as.data.frame(do.call(rbind.data.frame, FilterListByStringInName(listOfVectors,c("MEAN",sites[i]))))
    q90q95_DF <- as.data.frame(do.call(rbind.data.frame, FilterListByStringInName(listOfVectors,c("Q90Q95",sites[i]))))
    MinMax_DF <- as.data.frame(do.call(rbind.data.frame, FilterListByStringInName(listOfVectors,c("MinMax",sites[i]))))
    
    Mean_DF <- DataFrameRoundToUSGS(Mean_DF)
    q90q95_DF <- DataFrameRoundToUSGS(q90q95_DF)
    MinMax_DF <- DataFrameRoundToUSGS(MinMax_DF)
    
    colnames(Mean_DF) <- monthsVector
    colnames(q90q95_DF) <- monthsVector
    colnames(MinMax_DF) <- monthsVector
    rownames(Mean_DF) <- row_names
    rownames(q90q95_DF) <- row_names
    rownames(MinMax_DF) <- row_names
    
    rList <- list.append(rList,list("P2-Mean"=Mean_DF,"P2-q90q95"=q90q95_DF,"P2-MinMax"=MinMax_DF))
    rListNames <- c(rListNames,paste("Station ",i,sep=""))
  }
  names(rList) <- rListNames
  return(rList)
}

#Created by KLA 01/14/2020.
Tier1_Seasonal_ByStation<-function(tList,sites,periods,asy,bl,row_names,stn_loc){
  print("Tier1_Seasonal_ByStation")
  listofStns <- list()
  listofDFs <- list()
  df_names <- c()
  stn_names <- c()
  b <- as.integer(substr(asy[1],6,7)) 
  b <- c(getSeason(b),getSeason(b+4),getSeason(b+8))
  seasons <- c(paste(extendMonthName(substr(b[1],1,3))," - ",
                     extendMonthName(substr(b[1],5,7))),
               paste(extendMonthName(substr(b[2],1,3))," - ",
                     extendMonthName(substr(b[2],5,7))),
               paste(extendMonthName(substr(b[3],1,3))," - ",
                     extendMonthName(substr(b[3],5,7))))
  
  for(i in 1:(length(sites)))
  {
    listofDFs <- list(data.frame(),data.frame(),data.frame(),data.frame(),data.frame(),data.frame())
    
    names(listofDFs) <- c(paste(seasons[1],"-P3-DF1-",sites[i],sep=""),paste(seasons[1],"-P3-APPX-DF1-",sites[i],sep=""),
                          paste(seasons[2],"-P3-DF2-",sites[i],sep=""),paste(seasons[2],"-P3-APPX-DF2-",sites[i],sep=""),
                          paste(seasons[3],"-P3-DF3-",sites[i],sep=""),paste(seasons[3],"-P3-APPX-DF3-",sites[i],sep=""))
    
    if(!is.null(bl))
    {
      df <- FilterListByStringInName(tList,c("Y0-B-P3-",sites[i]))[[1]]
      df <- cbind.data.frame(df[,c(1,2,3)],DataFrameRoundToUSGS(df[,-c(1,2,3)])) 
      
      for(l in 1:(length(listofDFs)/2))
      {
        #Select Mean	StDev	Minimum	Maximum	Q-90	Q-95
        #Select Mean	StDev	Minimum	Maximum	Q-05	Q-10	Q-25	Q-50	Q-75	Q-90	Q-95	CI-90	CI-95
        listofDFs[[l*2-1]] <- subset(df, select=c(5,4,7,8,23))[2*l,]
        listofDFs[[l*2]] <- subset(df, select=c(5,4,7:15,20:21))[2*l,]
      }
    }
    
    if(length(FilterListByStringInName(tList,c("-A-"))) > 0)
    {
      for(j in 1:(length(asy)))
      {
        df <- FilterListByStringInName(tList,c(paste("Y",j,"-A-P3-",sep=""),sites[i]))[[1]]
        df <- cbind.data.frame(df[,c(1,2,3)],DataFrameRoundToUSGS(df[,-c(1,2,3)]))
        
        for(l in 1:(length(listofDFs)/2))
        {
          #Select Mean	StDev	Minimum	Maximum	Q-90	Q-95
          #Select Mean	StDev	Minimum	Maximum	Q-05	Q-10	Q-25	Q-50	Q-75	Q-90	Q-95	CI-90	CI-95
          listofDFs[[l*2-1]] <- rbind.data.frame(listofDFs[[l*2-1]],subset(df, select=c(5,4,7,8,23))[2*l,])
          listofDFs[[l*2]] <- rbind.data.frame(listofDFs[[l*2]],subset(df, select=c(5,4,7:15,20:21))[2*l,])
        }
      }
    }
    
    stn_names <- c(stn_names, paste("Station ",i,sep=""))
    
    for(m in 1:(length(listofDFs)))
    {
      row.names(listofDFs[[m]]) <- row_names
    }
    listofStns <-  list.append(listofStns,listofDFs)
  }
  
  names(listofStns) <- stn_names
  #return(listofStns)
  
  for(i in 1:(length(seasons)))
  {
    if(i == 1)
    {
      rdf_rownames <- c(seasons[i],"",row_names)
      apx_rdf_rownames <- c(seasons[i],row_names)
    }else
    {
      rdf_rownames <- c(rdf_rownames,c("",seasons[i],"",row_names))
      apx_rdf_rownames <- c(apx_rdf_rownames,c(seasons[i],row_names))
    }
  }
  
  rListOfStns <- list()
  rStnNames <- c()
  for(i in 1:(length(listofStns)))#Per Station
  {
    rListOfDFs <- list()
    rdfV <- c()
    for(j in 1:2)#Per Table [1-Report , 2-Appendix]
    {
      if(j%%2 != 0) #Odd numbers - Report.
      {
        season1r <- as.matrix(FilterListByStringInName(listofStns[[i]],c(seasons[1],sites[i]))[[1]])
        season2r <- as.matrix(FilterListByStringInName(listofStns[[i]],c(seasons[2],sites[i]))[[1]])
        season3r <- as.matrix(FilterListByStringInName(listofStns[[i]],c(seasons[3],sites[i]))[[1]])
        
        col_names <- colnames(season1r)
        
        report_df <- rbind(rep("",ncol(season1r)),
                           col_names,          
                           season1r,
                           rep("",ncol(season2r)),
                           rep("",ncol(season2r)),
                           col_names, 
                           season2r,
                           rep("",ncol(season3r)),
                           rep("",ncol(season3r)),
                           col_names, 
                           season3r)
        report_df <- cbind(rdf_rownames,report_df)
        rListOfDFs <- list.append(rListOfDFs,report_df)
        rdfV <- c(rdfV,paste("Report-P3-",sites[i],sep=""))
        
      }else #Even numbers - Appendix.
      {
        season1a <- as.matrix(FilterListByStringInName(listofStns[[i]],c(seasons[1],sites[i]))[[2]],"")
        season2a <- as.matrix(FilterListByStringInName(listofStns[[i]],c(seasons[2],sites[i]))[[2]],"")
        season3a <- as.matrix(FilterListByStringInName(listofStns[[i]],c(seasons[3],sites[i]))[[2]])
        
        col_names <- colnames(season1a)
        
        appendix_df <- rbind(##rep("",ncol(season1a)),
          col_names,  
          season1a,
          ##rep("",ncol(season2a)),
          ##rep("",ncol(season2a)),
          col_names,  
          season2a,
          ##rep("",ncol(season3a)),
          ##rep("",ncol(season3a)),
          col_names,  
          season3a)
        appendix_df <- cbind(apx_rdf_rownames,appendix_df)
        rListOfDFs <- list.append(rListOfDFs,appendix_df)
        rdfV <- c(rdfV,paste("APPX-P3-",sites[i],sep=""))
        
      }
    }
    names(rListOfDFs) <- rdfV
    rListOfStns <- list.append(rListOfStns,rListOfDFs)
    rStnNames <- c(rStnNames,paste("Station ",i,sep=""))
  }
  
  names(rListOfStns) <- rStnNames
  return(rListOfStns)
}

Tier1_Annual_ByStation<-function(tList,sites,periods,asy,bl,row_names,stn_loc){
  print("Tier1_Annual_ByStation")
  rdf <- NULL
  listofDFs = list()
  df_names <- c()
  
  for(i in 1:(length(sites)))
  {
    if(!is.null(bl))
    {
      for(k in 1:(length(stn_loc)))
      {
        df <- FilterListByStringInName(tList,c("Y0-B-P4-",stn_loc[k],sites[i]))[[1]]
        df <- subset(df, select=c(3,5,4,7:15,20:21))[-c(1),]
        
        if(!is.null(df))
        {
          if(is.null(rdf))
          {
            rdf <- data.frame(Years=row_names)
            rdf <- cbind.data.frame(df)
          }
        }
      }
    }
    
    if(length(FilterListByStringInName(tList,c("-A-"))) > 0)
    {
      for(j in 1:(length(asy)))
      {
        for(k in 1:(length(stn_loc)))
        {
          df <- FilterListByStringInName(tList,c(paste("Y",j,"-A-P4-",sep=""),stn_loc[k],sites[i]))[[1]]
          df <- subset(df, select=c(3,5,4,7:15,20:21))[-c(1),]
          
          if(!is.null(df))
          {
            if(is.null(rdf))
            {
              rdf <- data.frame(Years=row_names)
              rdf <- cbind.data.frame(df)
            }else
            {
              rdf <- rbind.data.frame(rdf,df)
            }
          }
        }
      }
    }
    df_names <- c(df_names,paste("Station ",i,sep=""))
    rdf <- cbind.data.frame(Years=row_names,rdf)
    rdf <- cbind.data.frame(rdf[,c(1,2)],DataFrameRoundToUSGS(rdf[,-c(1,2)]))
    listofDFs <- list.append(listofDFs,rdf)
    rdf <- NULL
  }
  names(listofDFs) <- df_names
  return(listofDFs)
}

#Created by KLA on 01/03/2020.
Tier1_AnnualALLStations<-function(tList,sites,periods,asy,bl,row_names,stn_loc){
  print("Tier1_AnnualALLStations")
  table_names <- c("MEAN (±SD)","90th / 95th Percentiles","Range (Min - Max)")
  namesTable <- c()
  listOfVectors = list()
  
  for(i in 1:(length(sites)))
  {
    if(!is.null(bl))
    {
      for(k in 1:(length(stn_loc)))
      {
        VECTOR_MEAN_SD <- FilterListByStringInName(tList,c("Y0-B-P4-",stn_loc[k],sites[i]))[[1]][["Mean_Stdev"]][[2]]
        TABLE_90_95   <- FilterListByStringInName(tList,c("Y0-B-P4-",stn_loc[k],sites[i]))[[1]][["Q90 / Q95"]][[2]]
        TABLE_MIN_MAX <- FilterListByStringInName(tList,c("Y0-B-P4-",stn_loc[k],sites[i]))[[1]][["Min_Max"]][[2]]
        
        if(!is.null(VECTOR_MEAN_SD) && !is.null(TABLE_90_95) && !is.null(TABLE_MIN_MAX))
        {
          listOfVectors <- list.append(listOfVectors,VECTOR_MEAN_SD,TABLE_90_95,TABLE_MIN_MAX)
          namesTable <- c(namesTable,paste("Y0-B-MEAN-",stn_loc[k],"-",sites[i],sep=""),
                          paste("Y0-B-Q90Q95-",stn_loc[k],"-",sites[i],sep=""),paste("Y0-B-MinMax-",stn_loc[k],"-",sites[i],sep="")) 
        }
      }
    }
    
    if(length(FilterListByStringInName(tList,c("-A-"))) > 0)
    {
      for(j in 1:(length(asy)))
      {
        for(k in 1:(length(stn_loc)))
        {
          VECTOR_MEAN_SD <- FilterListByStringInName(tList,c(paste("Y",j,"-A-P4-",sep=""),stn_loc[k],sites[i]))[[1]][["Mean_Stdev"]][[2]]
          TABLE_90_95   <-  FilterListByStringInName(tList,c(paste("Y",j,"-A-P4-",sep=""),stn_loc[k],sites[i]))[[1]][["Q90 / Q95"]][[2]]
          TABLE_MIN_MAX <-  FilterListByStringInName(tList,c(paste("Y",j,"-A-P4-",sep=""),stn_loc[k],sites[i]))[[1]][["Min_Max"]][[2]]
          
          if(!is.null(VECTOR_MEAN_SD) && !is.null(TABLE_90_95) && !is.null(TABLE_MIN_MAX))
          {
            listOfVectors <- list.append(listOfVectors,VECTOR_MEAN_SD,TABLE_90_95,TABLE_MIN_MAX)
            namesTable <- c(namesTable,paste("Y",j,"-A-MEAN-",stn_loc[k],"-",sites[i],sep=""),
                            paste("Y",j,"-A-Q90Q95-",stn_loc[k],"-",sites[i],sep=""),paste("Y",j,"-A-MinMax-",stn_loc[k],"-",sites[i],sep="")) 
          }
        }
      }
    }
  }
  
  names(listOfVectors) <- namesTable
  
  for(i in 1:(length(table_names)))
  {
    if(i == 1)
    {
      rdf_rownames <- c(table_names[i],"",row_names)
    }else
    {
      rdf_rownames <- c(rdf_rownames,c("",table_names[i],"",row_names))
    }
  }
  
  for(i in 1:(length(sites)))
  {
    if(i == 1)
    {
      Mean_ <- matrix(c("",paste("Station ",i,sep=""),unlist(FilterListByStringInName(listOfVectors,c("MEAN",sites[i]))),""))
      q90q95_ <- matrix(c("",paste("Station ",i,sep=""),unlist(FilterListByStringInName(listOfVectors,c("Q90Q95",sites[i]))),""))
      MinMax_ <- matrix(c("",paste("Station ",i,sep=""),unlist(FilterListByStringInName(listOfVectors,c("MinMax",sites[i])))))
    }else
    {
      Mean_ <- cbind(Mean_,matrix(c("",paste("Station ",i,sep=""),unlist(FilterListByStringInName(listOfVectors,c("MEAN",sites[i]))),"")))
      q90q95_ <- cbind(q90q95_,matrix(c("",paste("Station ",i,sep=""),unlist(FilterListByStringInName(listOfVectors,c("Q90Q95",sites[i]))),"")))
      MinMax_ <- cbind(MinMax_,matrix(c("",paste("Station ",i,sep=""),unlist(FilterListByStringInName(listOfVectors,c("MinMax",sites[i]))))))
    }
  }
  
  rdf <- rbind.data.frame(Mean_,q90q95_,MinMax_)
  rdf <- cbind(rdf_rownames,rdf)
  rdf <- cbind.data.frame(rdf[,c(1)],DataFrameRoundToUSGS(rdf[,-c(1)]))
  
  return (list(AnnualAllStations=rdf))
  #return(list(Mean_DF=Mean_DF,q90q95_DF=q90q95_DF,MinMax_DF=MinMax_DF))
  #x <- FilterListByStringInName(tempTableList,c("-A-", paste("Y",,"-",sep=""), site_id[1]),paste("P",1,sep=""))
}

#Created by KLA on 01/03/2020.
FilterListByStringInName<- function(tList,conditions,rmvConditions=FALSE){
  rList <- list()
  nms <- c()
  
  x = conditions
  #x = c("-A-", "Y1", site_id[1])
  y = names(tList)
  
  for(i in 1:(length(x)))
  {
    if(!rmvConditions){
      y = y[grepl(x[i], y,fixed = TRUE)]
    }else
    {
      y = y[!grepl(x[i], y,fixed = TRUE)]
    }
  }
  
  if(length(y) != 0)
  {
    for(j in 1:(length(y)))
    {
      if(length(tList) > 0 && !is.null(tList[[y[j]]]))
      {
        rList <- list.append(rList,tList[[y[j]]]) 
      }
    }
    
  }else
  {
    return(NULL)
  }
  names(rList) <- y
  return (rList)
}

#createStatistics - Generate a statistics data frame.
#Created by KLA on 05/30/2018.
#Last update by KLA on 01/16/2020.
#Parameters: instantanius/daily value dataframe(character).
#Returns: dataframe.
#Status: tested.
createStatistics<-function(dfiv,dfdv,startDate,endDate,period,sMonth,sYear,db,site="")
{
  tryCatch({
    #Period <- getPeriodData(period,sMonth)#returns a vector for all the months/weeks/seasons or annual.
    sList <- list()
    rdfiv <- data.frame()
    rdfdv <- data.frame()
    
    if(floor(period) == 1)#weekly - tested.
    {
      if(period==1.5)
      {
        dfdv <- GetDataFrameMovingAverage(dfdv,7,site,TRUE,NULL)
      }
      
      startDate <- as.Date(startDate)
      
      for (i in 1:(getIntervalData(period)))#create data frame for period.
      {
        tryCatch({
          eDate1 <- startDate+(7*(i-1))
          week <- week(eDate1)
          mdfiv <- dfiv[week(dfiv$Date) == week,]
          
          mdfdv <- dfdv[week(dfdv$Date) == week,]
          mdfdv <- mdfdv[!is.na(mdfdv$Date),]
          #print(paste(head(mdfdv$Date,n=1)," TO ",tail(mdfdv$Date,n=1),sep=""))
          
          sList <- list.append(sList, runStatistics(mdfiv,mdfdv,paste("Week #",week,sep=""),period,db))
        },error = function(err){
          stop("ERROR: CreateStatistics(). Calculating Weekly Stadistics.")
        })
      }
    }else if(floor(period) == 2)#Monthly - tested.
    {
      for (i in 1:(getIntervalData(period)))#create data frame for period.
      {
        tryCatch({
          p <- getMonthCharacter(monthAddition(sMonth,i-1))
          
          mdfiv <- dfiv[months(dfiv$Date) == p,]
          
          mdfdv <- dfdv[months(dfdv$Date) == p,]
          mdfdv <- mdfdv[!is.na(mdfdv$Date),]
          #print(paste(head(mdfdv$Date,n=1)," TO ",tail(mdfdv$Date,n=1),sep=""))
          
          sList <- list.append(sList, runStatistics(mdfiv,mdfdv,p,period,db))
        },error = function(err){
          stop("ERROR: CreateStatistics(). Monthly Stadistics.")
        })
      }
    }else if (floor(period) == 3)#seasonal - tested.
    {
      for (i in 1:(getIntervalData(period)))#create data frame for period.
      {
        tryCatch({
          seasonMonths <- getMonthsFromSeason(i,sMonth)
          mdfiv <- dfiv[months(dfiv$Date) %in% seasonMonths,]
          
          mdfdv <- dfdv[months(dfdv$Date) %in% seasonMonths,]
          mdfdv <- mdfdv[!is.na(mdfdv$Date),]
          seasonMonths <- substr(seasonMonths,start=1,stop=3)
          #print(paste(head(mdfdv$Date,n=1)," TO ",tail(mdfdv$Date,n=1),sep=""))
          
          sList <- list.append(sList, runStatistics(mdfiv,mdfdv,paste(seasonMonths[1],"-",seasonMonths[4],sep=""),period,db))
        },error = function(err){
          stop("ERROR: CreateStatistics(). Seasonal Stadistics.")
        })
      }
    }else#annually - tested.
    {
      tryCatch({
        #print(paste(head(dfiv$Date,n=1)," TO ",tail(dfdv$Date,n=1),sep=""))
        
        sList <- list.append(sList, runStatistics(dfiv,dfdv,"Annual",period,db))
      },error = function(err){
        stop("ERROR: CreateStatistics(). Annual Stadistics.")
      })
    }
    
    #This is where the tables that are printed on the word document get arranged.
    rList <- list()
    tempDf <- data.frame()
    n<-1
    m<-2
    
    for(i in 1:((length(sList[[1]]))/2))
    {
      #Period Interval	Records	Stdev	Mean	Median	Min	Max	Q.05	Q.10	Q.25	Q.50	Q.75	Q.90	Q.95	Ci.90	Ci.95
      
      for(j in 1:(length(sList)))
      {
        tempDf1 <- data.frame(Period=as.character(sList[[j]][[n]][1]),Interval=as.character(sList[[j]][[n]][2]),Records=as.character(sList[[j]][[n]][3]),Stdev=as.numeric(sList[[j]][[n]][4]),Mean=as.numeric(sList[[j]][[n]][5]),Median=as.numeric(sList[[j]][[n]][6]),Min=as.numeric(sList[[j]][[n]][7]),Max=as.numeric(sList[[j]][[n]][8]),Q.05=as.numeric(sList[[j]][[n]][9]),Q.10=as.numeric(sList[[j]][[n]][10]),Q.25=as.numeric(sList[[j]][[n]][11]),Q.50=as.numeric(sList[[j]][[n]][12]),Q.75=as.numeric(sList[[j]][[n]][13]),Q.90=as.numeric(sList[[j]][[n]][14]),Q.95=as.numeric(sList[[j]][[n]][15]),Ci.90.Lo=as.numeric(sList[[j]][[n]][16]),Ci.90.Up=as.numeric(sList[[j]][[n]][17]),Ci.95.Lo=as.numeric(sList[[j]][[n]][18]),Ci.95.Up=as.numeric(sList[[j]][[n]][19]))
        tempDf2 <- data.frame(Period=as.character(sList[[j]][[m]][1]),Interval=as.character(sList[[j]][[m]][2]),Records=as.character(sList[[j]][[m]][3]),Stdev=as.numeric(sList[[j]][[m]][4]),Mean=as.numeric(sList[[j]][[m]][5]),Median=as.numeric(sList[[j]][[m]][6]),Min=as.numeric(sList[[j]][[m]][7]),Max=as.numeric(sList[[j]][[m]][8]),Q.05=as.numeric(sList[[j]][[m]][9]),Q.10=as.numeric(sList[[j]][[m]][10]),Q.25=as.numeric(sList[[j]][[m]][11]),Q.50=as.numeric(sList[[j]][[m]][12]),Q.75=as.numeric(sList[[j]][[m]][13]),Q.90=as.numeric(sList[[j]][[m]][14]),Q.95=as.numeric(sList[[j]][[m]][15]),Ci.90.Lo=as.numeric(sList[[j]][[m]][16]),Ci.90.Up=as.numeric(sList[[j]][[m]][17]),Ci.95.Lo=as.numeric(sList[[j]][[n]][18]),Ci.95.Up=as.numeric(sList[[j]][[n]][19]))
        
        #print(levels(factor(tempDf1$Period)))
        colnames(tempDf1)[3] <- "Records(Total/%Used)"
        colnames(tempDf2)[3] <- "Records(Total/%Used)"
        if(j == 1)
        {
          tempDf3 <- rbind.data.frame(tempDf1,tempDf2)
        }else
        {
          tempDf3 <- rbind.data.frame(tempDf3,tempDf1,tempDf2)
        }
        #j<-j+1
      }
      n<-n+2
      m<-m+2
      
      #dfList <- list(dvCst,ivCst,dvCsb,ivCsb,dvSt,ivSt,dvSb,ivSb,dvCtst,ivCtst,dvCtsb,ivCtsb)
      #   
      
      #round decimals to two spaces.
      
      #is.num <- sapply(tempDf3, is.numeric)
      #tempDf3[is.num] <- lapply(tempDf3[is.num], round, 2)
      
      rList <- list.append(rList, tempDf3)
    }
    if(db == "USGS")
    {
      rchar <- c()
      dataAvail <- usgsDataAvailable(dfiv,db)
      
      if("T" %in% dataAvail)
      {
        rchar <- c(rchar,"Sal(Cond) TOP")
      }
      if("B" %in% dataAvail)
      {
        rchar <- c(rchar,"Sal(Cond) BOTTOM")
      }
      if("M" %in% dataAvail)
      {
        rchar <- c(rchar,"Sal(Cond) MIDDLE")
      }
      if("NA" %in% dataAvail)
      {
        rchar <- c(rchar,"Sal(Cond)")
      }
      names(rList) <- rchar
      
      #,"Salinity (USGS) TOP","Salinity (USGS) BOTTOM",
      #"Salinity (Temp,Cond) TOP","Salinity (Temp,Cond) BOTTOM")
    }
    if(db == "NOAA" || db== "SJR")
    {
      names(rList) <- c("Salinity")
    }
    
    return(rList)
    
  },error = function(err) {
    stop("ERROR: createStatistics().")
  }) # END tryCatch
}

names_<-function(x){
  if(!is.null(names(x)))
  {
    return(names(x))
  }else
  {
    return("")
  }
}

#runStatistics - create statistics for a dataframe.
#Created by KLA on 05/16/2018.
#Last update by KLA on 12/27/2018.
#Parameters: instanious values dataframe, daily values dataframe
#Returns: dataframe.
#Status: tested.
runStatistics<-function(dfiv,dfdv,periodValue,period,db)
{
  #print(n)
  tryCatch({
    dfList <- list()
    intervalNums <- c()
    nameList <- c()
    lNames <- c()
    
    #Daily Values First.
    if(db == "USGS")
    {
      if(ncol(dfiv) %in% c(4,5) & ncol(dfdv) %in% c(4,5))
      {
        dfList <- list.append(dfList,dfdv[, grepl("SALINITY", toupper(names(dfdv)), fixed=TRUE)])
        dfList <- list.append(dfList,dfiv[, grepl("SALINITY", toupper(names(dfiv)), fixed=TRUE)])
        intervalNums <- c(getIntervalNumber(dfiv))
        nameList <- c(nameList,"","")
      }else
      {
        #Get calculated Salinity from Conductance.
        dvList <- convertDataFrame(dfdv,db)
        ivList <- convertDataFrame(dfiv,db)
        
        for(i in 1:(max(length(ivList),length(dvList))))
        {
          if(!is.null(dvList))
          {
            dfList <- list.append(dfList,dvList[[i]][[4]])
          }else
          {
            dfList <- list.append(dfList,NULL)
          }
          
          if(!is.null(ivList))
          {
            dfList <- list.append(dfList,ivList[[i]][[4]])
          }else
          {
            dfList <- list.append(dfList,NULL)
          }
          
          #dfList <- list.append(dfList,dvList[[i]][[4]])
          #dfList <- list.append(dfList,ivList[[i]][[4]])
          
          intervalNums <- c(intervalNums,getIntervalNumber(ivList[[i]]))
          nameList <- c(nameList,names_(dvList[i]),names_(ivList[i]))
        }
      }
      
      # for(i in 1:(length(ivList)))
      # {
      #   
      # 
      # }
      
      #dfList <- list.append(dfList,convertDataFrame(dfdv,db,"top")[[4]]) 
      #dfList <- list.append(dfList,convertDataFrame(dfiv,db,"top")[[4]])
      
      #dfList <- list.append(dfList,convertDataFrame(dfdv,db,"bottom")[[4]])
      
      #nameList <- c(names(dvList),names(ivList))
      
      for(i in 1:(length(dfList)))
      {
        if(i%%2 != 0)
        {
          lNames <- c(lNames, paste("DV Sal(Cond) ",nameList[i],sep=""))
        }else
        {
          lNames <- c(lNames, paste("IV Sal(Cond) ",nameList[i],sep=""))
        }
      }
      names(dfList) <- lNames
      #print(names(dfList))
      
      #intervalNums <- c(getIntervalNumber(convertDataFrame(dfiv,db,"top")),getIntervalNumber(convertDataFrame(dfiv,db,"bottom")))
    }
    
    if(db == "NOAA" || db == "SJR")
    {
      dfList <- list.append(dfList,dfdv[[4]],dfiv[[4]])
      names(dfList) <- c("DV Salinity","IV Salinity")
      intervalNums <- getIntervalNumber(dfiv)
    }
    
    Interval <- c()
    
    # #IV Salinity from Conductance.
    # ivCst <- (6.859819E-38*(dfiv$X_TOP_00095_00000)^8 + -2.027297E-32*(dfiv$X_TOP_00095_00000)^7 + 2.576767E-27*(dfiv$X_TOP_00095_00000)^6 + -1.855032E-22*(dfiv$X_TOP_00095_00000)^5 + 8.450662E-18*(dfiv$X_TOP_00095_00000)^4 + -0.0000000000002616744*(dfiv$X_TOP_00095_00000)^3 + 0.000000007096135*(dfiv$X_TOP_00095_00000)^2 + 0.0005161311*(dfiv$X_TOP_00095_00000) + -0.05028803)
    # ivCsb <- (6.859819E-38*(dfiv$X_BOTTOM_00095_00000)^8 + -2.027297E-32*(dfiv$X_BOTTOM_00095_00000)^7 + 2.576767E-27*(dfiv$X_BOTTOM_00095_00000)^6 + -1.855032E-22*(dfiv$X_BOTTOM_00095_00000)^5 + 8.450662E-18*(dfiv$X_BOTTOM_00095_00000)^4 + -0.0000000000002616744*(dfiv$X_BOTTOM_00095_00000)^3 + 0.000000007096135*(dfiv$X_BOTTOM_00095_00000)^2 + 0.0005161311*(dfiv$X_BOTTOM_00095_00000) + -0.05028803)
    # 
    # #DV Salinity from Conductance.
    # dvCst <- (6.859819E-38*(dfdv$X_TOP_00095_00003)^8 + -2.027297E-32*(dfdv$X_TOP_00095_00003)^7 + 2.576767E-27*(dfdv$X_TOP_00095_00003)^6 + -1.855032E-22*(dfdv$X_TOP_00095_00003)^5 + 8.450662E-18*(dfdv$X_TOP_00095_00003)^4 + -0.0000000000002616744*(dfdv$X_TOP_00095_00003)^3 + 0.000000007096135*(dfdv$X_TOP_00095_00003)^2 + 0.0005161311*(dfdv$X_TOP_00095_00003) + -0.05028803)
    # dvCsb <- (6.859819E-38*(dfdv$X_BOTTOM_00095_00003)^8 + -2.027297E-32*(dfdv$X_BOTTOM_00095_00003)^7 + 2.576767E-27*(dfdv$X_BOTTOM_00095_00003)^6 + -1.855032E-22*(dfdv$X_BOTTOM_00095_00003)^5 + 8.450662E-18*(dfdv$X_BOTTOM_00095_00003)^4 + -0.0000000000002616744*(dfdv$X_BOTTOM_00095_00003)^3 + 0.000000007096135*(dfdv$X_BOTTOM_00095_00003)^2 + 0.0005161311*(dfdv$X_BOTTOM_00095_00003) + -0.05028803)
    # 
    # #IV Salinity from Source.
    # ivSt <- dfiv$X_TOP_00480_00000
    # ivSb <- dfiv$X_BOTTOM_00480_00000
    # 
    # #DV Salinity from Source.
    # dvSt <- dfdv$X_TOP_00480_00003
    # dvSb <- dfdv$X_BOTTOM_00480_00003
    # 
    # #IV Salinity from Conductance and temperature.
    # #ivCtst <- (6.859819E-38*(dfiv$X_TOP_00095_00000)^8 + -2.027297E-32*(dfiv$X_TOP_00095_00000)^7 + 2.576767E-27*(dfiv$X_TOP_00095_00000)^6 + -1.855032E-22*(dfiv$X_TOP_00095_00000)^5 + 8.450662E-18*(dfiv$X_TOP_00095_00000)^4 + -0.0000000000002616744*(dfiv$X_TOP_00095_00000)^3 + 0.000000007096135*(dfiv$X_TOP_00095_00000)^2 + 0.0005161311*(dfiv$X_TOP_00095_00000) + -0.05028803)
    # #ivCtsb <- (6.859819E-38*(dfiv$X_BOTTOM_00095_00000)^8 + -2.027297E-32*(dfiv$X_BOTTOM_00095_00000)^7 + 2.576767E-27*(dfiv$X_BOTTOM_00095_00000)^6 + -1.855032E-22*(dfiv$X_BOTTOM_00095_00000)^5 + 8.450662E-18*(dfiv$X_BOTTOM_00095_00000)^4 + -0.0000000000002616744*(dfiv$X_BOTTOM_00095_00000)^3 + 0.000000007096135*(dfiv$X_BOTTOM_00095_00000)^2 + 0.0005161311*(dfiv$X_BOTTOM_00095_00000) + -0.05028803)
    # 
    # #DV Salinity from Conductance and temperature.
    # #dvCtst <- (6.859819E-38*(dfdv$X_TOP_00095_00003)^8 + -2.027297E-32*(dfdv$X_TOP_00095_00003)^7 + 2.576767E-27*(dfdv$X_TOP_00095_00003)^6 + -1.855032E-22*(dfdv$X_TOP_00095_00003)^5 + 8.450662E-18*(dfdv$X_TOP_00095_00003)^4 + -0.0000000000002616744*(dfdv$X_TOP_00095_00003)^3 + 0.000000007096135*(dfdv$X_TOP_00095_00003)^2 + 0.0005161311*(dfdv$X_TOP_00095_00003) + -0.05028803)
    # #dvCtsb <- (6.859819E-38*(dfdv$X_BOTTOM_00095_00003)^8 + -2.027297E-32*(dfdv$X_BOTTOM_00095_00003)^7 + 2.576767E-27*(dfdv$X_BOTTOM_00095_00003)^6 + -1.855032E-22*(dfdv$X_BOTTOM_00095_00003)^5 + 8.450662E-18*(dfdv$X_BOTTOM_00095_00003)^4 + -0.0000000000002616744*(dfdv$X_BOTTOM_00095_00003)^3 + 0.000000007096135*(dfdv$X_BOTTOM_00095_00003)^2 + 0.0005161311*(dfdv$X_BOTTOM_00095_00003) + -0.05028803)
    # 
    # #List of tables for expecific period.
    # #dfList <- list(dvCst,ivCst,dvCsb,ivCsb,dvSt,ivSt,dvSb,ivSb,dvCtst,ivCtst,dvCtsb,ivCtsb)
    # 
    # dfList <- list(dvCst,ivCst,dvCsb,ivCsb,dvSt,ivSt,dvSb,ivSb)
    
    #return list of calculations done for a expecific period (dfList).
    rList <- list()
    
    #Return format
    #Period Interval	Records	Stdev	Mean	Median	Min	Max	Q.05	Q.10	Q.25	Q.50	Q.75	Q.90	Q.95	Ci.90	Ci.95
    
    for(i in 1:(length(dfList)))
    {
      if(!is.null(dfList[[i]]))
      {
        options(warn=-1)#remove warnings when period has all values NA's
        
        data <- dfList[[i]][is.finite(dfList[[i]])]
        totalData <- data
        
        data <- RoundSalinityValues(data)
        
        Stdev <- sd(data,na.rm = TRUE)
        Stdev[!is.finite(Stdev)] <- NA
        
        Mean <- mean(data,na.rm = TRUE)
        Mean[!is.finite(Mean)] <- NA
        
        Min <- min(data,na.rm = TRUE)
        Min[!is.finite(Min)] <- NA
        
        Max <- max(data,na.rm = TRUE)
        Max[!is.finite(Max)] <- NA
        
        Median <- median(data,na.rm = TRUE)
        Median[!is.finite(Median)] <- NA
        
        qv <- quantile(data,probs=c(0.05,0.10,0.25,0.50,0.75,0.90,0.95),na.rm = TRUE)
        qv[!is.finite(qv)] <- NA
        
        Q.05 <- qv["5%"]
        Q.10 <- qv["10%"]
        Q.25 <- qv["25%"]
        Q.50 <- qv["50%"]
        Q.75 <- qv["75%"]
        Q.90 <- qv["90%"]
        Q.95 <- qv["95%"]
        
        #types of ci: "mean", "upr.ci", "lwr.ci"
        ciUpper <- "upr.ci"
        ciLower <- "lwr.ci"
        
        tryCatch({
          
        })
        
        ci90 <- ConfInterval(data,0.90)
        ci95 <- ConfInterval(data,0.95)
        
        ci90Up <- ci90[ciUpper]
        ci90Up[!is.finite(ci90Up)] <- NA
        
        ci90Lo <- ci90[ciLower]
        ci90Lo[!is.finite(ci90Lo)] <- NA
        
        
        ci95Up <- ci95[ciUpper]
        ci95Up[!is.finite(ci95Up)] <- NA
        
        ci95Lo <- ci95[ciLower]
        ci95Lo[!is.finite(ci95Lo)] <- NA
        
        rUsed <- length(data)
        nRows <- length(totalData)
        
        Records <- paste(nRows," / ",round((rUsed/nRows)*100,2),"%",sep="")
        
        options(warn=0)#enable warnings
        
        if(i%%2 == 0)
        {#Even numbers
          
          Interval <- intervalNums[i/2]
          
          tempVector <- c(" ", Interval, Records)
          
        }else#Odd numbers
        {
          Interval <- "Daily"
          tempVector <- c(periodValue, Interval, Records)
        }
        
        roundVector <- c(Stdev,Mean,Median,Min,Max,qv["5%"],qv["10%"],qv["25%"],qv["50%"],qv["75%"],qv["90%"],qv["95%"],ci90Lo,ci90Up,ci95Lo,ci95Up)
        
        tempVector <- c(tempVector,(roundVector))
      }else
      {
        if(i%%2 == 0)
        {#Even numbers
          
          tempVector <- c("","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")
        }else#Odd numbers
        {
          tempVector <- c(periodValue,"Daily","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")
        }
      }
      
      names(tempVector) <- c("Period","Interval","Records(Total/%Used)","Stdev","Mean","Median","Min","Max","Q.05","Q.10","Q.25","Q.50","Q.75","Q.90","Q.95","Ci.90.Lo","Ci.90.Up","Ci.95.Lo","Ci.95.Up")
      #List of Vectors (each vector is a calculated time period).
      rList <- list.append(rList, tempVector)
    }
    
    return(rList)
  },error = function(err) {
    stop("ERROR: runStatistics().")
  }) # END tryCatch
}

#generateXlsxFile - create an Excel document file with USGS information.
#Created by KLA on 05/30/2018.
#Last update by KLA on 12/10/2018.
#Parameters: filename(character),site_id(character or vector),
#            parameterCd(character or vector),startDate(character),endDate(character),
#            tz(character)
#Returns: nothing.
#Status: tested.
generateXlsxFile<-function(filename,site_id,parameterCd,startDate,endDate)
{
  tryCatch({
    print("Generating Excel file...")
    setwd(dirname(rstudioapi::getSourceEditorContext()$path))
    extension = ".xlsx"
    
    dfiv <- ReadDataBase(TRUE,FALSE,"",startDate,endDate,site_id,"IV",db="USGS")
    #dfiv <- readNWISuv(site_id,parameterCd,paste(startDate,"T00:00",sep=""),paste(endDate,"T23:59",sep=""),tz=Sys.timezone())
    dfdv <- ReadDataBase(TRUE,FALSE,"",startDate,endDate,site_id,"DV",db="USGS")
    
    filename <- getFileName(filename,extension)
    wb = createWorkbook()
    
    sheet1 = xlsx::createSheet(wb, "IV")
    xlsx::addDataFrame(dfiv, sheet=sheet1, startColumn=1, row.names=FALSE)
    
    sheet2 = xlsx::createSheet(wb, "DV")
    xlsx::addDataFrame(dfdv, sheet=sheet2, startColumn=1, row.names=FALSE)
    
    xlsx::saveWorkbook(wb, filename)
    print((paste("DONE.")))
    
  },error = function(err) {
    stop("ERROR: generateXlsxFile().")
  }) # END tryCatch
}

######################################################### TESTED FUNCTIONS ################
#getFileName - Get the file name for a .docx document.
#Created by KLA on 05/16/2018.
#Last update by KLA on 01/30/2020.
#Parameters: filename with no extension(character).
#Returns: filename with extension.
#Status: tested.
getFileName<-function(filename, extension)
{
  tryCatch({
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
    
  },error = function(err) {
    stop("ERROR: getFileName().")
  }) # END tryCatch
}

#getMonthCharacter - Get month in a character format.
#Created by KLA on 05/29/2018.
#Last update by KLA on 05/31/2018.
#Parameters: num(numeric)
#Returns: month(character).
#Status: tested.
getMonthCharacter<-function(num)
{
  tryCatch({
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
    
  },error = function(err) {
    stop("ERROR: getMonthCharacter().")
  }) # END tryCatch
}

#extendMonthName - convert 3 character month format to normal length.
#Created by KLA on 07/30/2018.
#Last update by KLA on 07/30/2018.
#Parameters: month(character)[abbreviated 3 character format].
#Returns: month(character).
#Status: tested.
extendMonthName<-function(month)
{
  tryCatch({
    if(month == "Jan")
    {
      return("January")
    }else if (month == "Feb")
    {
      return("February")
    }else if (month == "Mar")
    {
      return("March")
    }else if (month == "Apr")
    {
      return("April")
    }else if (month == "May")
    {
      return("May")
    }else if (month == "Jun")
    {
      return("June")
    }else if (month == "Jul")
    {
      return("July")
    }else if (month == "Aug")
    {
      return("August")
    }else if (month == "Sep")
    {
      return("September")
    }else if (month == "Oct")
    {
      return("October")
    }else if (month == "Nov")
    {
      return("November")
    }else if (month == "Dec")
    {
      return("December")
    }
    return(month)
    
  },error = function(err) {
    stop("ERROR: extendMonthName().")
  }) # END tryCatch
}

#monthAddition - Returns an addition of months to a expecific month.
#                Ex: if month if 4 (april) and you want to add 11 months.
#                The result will return 3 (march) instead of 15.
#Created by KLA on 05/30/2018.
#Last update by KLA on 08/02/2018.
#Parameters: month(numeric),num(numeric)
#Returns: month(character).
#Status: tested.
monthAddition<-function(month,num)
{
  tryCatch({
    month <- monthToInteger(month)
    
    if(month+num > 12)
    {
      month <- month + num
      
      while(month > 12)
      {
        month <- month - 12
      }
      return(month)
    }else
    {
      return(month+num)
    }
    
  },error = function(err) {
    stop("ERROR: monthAddition().")
  }) # END tryCatch
}

#getInterval - Get the interval number of tables.
#Created by KLA on 05/21/2018.
#Last update by KLA on 01/16/2020.
#Parameters: period(character)
#Returns: interval number.
#Status: tested.
getIntervalData<-function(period)
{
  period <- floor(period)
  
  tryCatch({
    if(period == 1)#Weekly
    {
      return(52)
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
    
  },error = function(err) {
    stop("ERROR: getIntervalData().")
  }) # END tryCatch
}

#monthToInteger - Converts a month character(both abbreviated or non) to an integer.
#Created by KLA on 08/02/2018.
#Last update by KLA on 08/02/2018.
#Parameters: month(numeric).
#Returns: month (integer).
#Status: tested.
monthToInteger<-function(month)
{
  tryCatch({
    if(!is.numeric(month))#if month is not numeric check if its abbreviated or not.
    {
      if(nchar(as.character(month)) > 3)
      {
        month <- match(month,month.name)
      }else
      {
        if(nchar(as.character(month)) == 3)
        {
          month <- match(month,month.abb)
        }else
        {
          month <- as.numeric(month)
        }
      }
    }
    while(month > 12)
    {
      month <- month - 12
    }
    return(month)
    
  },error = function(err) {
    stop("ERROR: monthToInteger().")
  }) # END tryCatch
}

#getMonthsFromSeason - Converts a numeric season and starting month character to
#                      a vector of months inside the season.
#Created by KLA on 08/16/2018.
#Last update by KLA on 08/16/2018.
#Parameters: season(numeric),starting month(character or numeric).
#Returns: vector of months in a specific season.
#Status: tested.
getMonthsFromSeason<-function(season,sMonth)
{
  tryCatch({
    season <-  as.integer(season)
    sMonth <- monthToInteger(sMonth)
    
    if(season == 1)
    {
      return(c(getMonthCharacter(sMonth),getMonthCharacter(monthAddition(sMonth,1)),getMonthCharacter(monthAddition(sMonth,2)),getMonthCharacter(monthAddition(sMonth,3))))
    }
    
    if(season == 2)
    {
      return(c(getMonthCharacter(monthAddition(sMonth,4)),getMonthCharacter(monthAddition(sMonth,5)),getMonthCharacter(monthAddition(sMonth,6)),getMonthCharacter(monthAddition(sMonth,7))))
    }
    
    if(season == 3)
    {
      return(c(getMonthCharacter(monthAddition(sMonth,8)),getMonthCharacter(monthAddition(sMonth,9)),getMonthCharacter(monthAddition(sMonth,10)),getMonthCharacter(monthAddition(sMonth,11))))
    }
    
  },error = function(err) {
    stop("ERROR: getMonthsFromSeason().")
  }) # END tryCatch
}

#monthToSeason - Converts a month character to its season.
#Created by KLA on 08/02/2018.
#Last update by KLA on 08/02/2018.
#Parameters: month(character),month(character)
#Returns: season. [abbreviated months]
#Status: tested.
monthToSeason<-function(month,sMonth)
{
  tryCatch({
    month <-  monthToInteger(month)
    sMonth <- monthToInteger(sMonth)
    
    season1 <- c(sMonth,monthAddition(sMonth,1),monthAddition(sMonth,2),monthAddition(sMonth,3))
    season2 <- c(monthAddition(sMonth,4),monthAddition(sMonth,5),monthAddition(sMonth,6),monthAddition(sMonth,7))
    season3 <- c(monthAddition(sMonth,8),monthAddition(sMonth,9),monthAddition(sMonth,10),monthAddition(sMonth,11))
    
    if(month %in% season1)
    {#season 1
      return(getSeason(sMonth))
    }
    
    if(month %in% season2)
    {#season 2
      return(getSeason(monthAddition(sMonth,4)))
    }
    
    if(month %in% season3)
    {#season 3
      return(getSeason(monthAddition(sMonth,8)))
    }
    
  },error = function(err) {
    stop("ERROR: monthToSeason().")
  }) # END tryCatch
}

#getSeason - Get the season.
#Created by KLA on 07/05/2018.
#Last update by KLA on 08/02/2018.
#Parameters: month(character)
#Returns: season. [abbreviated months]
#Status: tested.
getSeason<-function(month)
{
  tryCatch({
    month <- monthToInteger(month)
    return(paste(month.abb[month],"-",month.abb[monthAddition(month,3)],sep=""))
  },error = function(err) {
    stop("ERROR: getSeason().")
  }) # END tryCatch
}

#getSeasonExtPrint - Get the season ready to print.
#Last update by KLA on 02/18/2020.
#Parameters: month(character)
#Returns: season. [abbreviated months]
#Status: tested.
getSeasonExtPrint<-function(month)
{
  tryCatch({
    month <- monthToInteger(month)
    return(paste(month.name[month]," - ",month.name[monthAddition(month,3)],sep=""))
  },error = function(err) {
    stop("ERROR: getSeason().")
  }) # END tryCatch
}


#getIntervalNumber - Get the interval number from the length of a row.
#Created by KLA on 06/01/2018.
#Last update by KLA on 02/22/2019.
#Parameters: rows (data frame).
#Returns: interval (15min,30min,45min or 1hr)[character].
#Status: tested.
getIntervalNumber<-function(rows)
{
  tryCatch({
    if(ncol(rows) >= 4)
    {
      rows <- rows[!is.na(rows[[4]]), ]
    }
    
    #Aprox Interval calculator
    r1 <- rows$Date
    
    if(length(r1) > 3)
    {
      r2 <- r1[-c(1)]
      r1 <- r1[-c(length(r1))]
      r3 <- r2-r1
      units(r3) <- "mins"
      interval <- as.numeric(names(sort(table(r3),decreasing=TRUE)[1]))
      if(interval < 60)
      {
        return(paste(interval,"min",sep=""))
      }else
      {
        return(paste(interval/60,"hr",sep=""))
      }
    }
    return("NA")
    
  },error = function(err) {
    return("NA")
  }) # END tryCatch
}

#periodPrint - Gets the period in a character format.
#Created by KLA on 08/09/2018.
#Last update by KLA on 01/16/2020.
#Parameters: period (character)
#Returns: character.
#Status: tested.
periodPrint<-function(period)
{
  period <- floor(period)
  tryCatch({
    if(period == 1)
      return ("Weeks")
    else if (period == 2)
      return ("Months")
    else if (period == 3)
      return ("Seasons")
    return ("Years")
  },error = function(err) {
    stop("ERROR: periodPrint().")
  }) # END tryCatch
}

#getPeriod - Gets the period in a character format.
#Created by KLA on 05/07/2018.
#Last update by KLA on 05/08/2018.
#Parameters: period (character)
#Returns: character.
#Status: tested.
getPeriod<-function(period)
{
  period <- floor(as.integer(period))
  tryCatch({
    if(period == 1)
      return ("Weekly")
    else if (period == 2)
      return ("Monthly")
    else if (period == 3)
      return ("Seasonal")
    return ("Annual")
  },error = function(err) {
    stop("ERROR: getPeriod().")
  }) # END tryCatch
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

#getAbbSiteName - Get the abbreviated site name.
#Created by KLA on 08/09/2018.
#Last update by KLA on 08/09/2018.
#Parameters: site_id (character)
#Returns: character.
#Status: tested.
getAbbSiteName<-function(site_id)
{
  site_id <- as.character(site_id)
  
  #USGS
  if(as.integer64(site_id) == 2246825)
    return("C CREEK NR")
  if (as.integer64(site_id) == 302657081312400)
    return("C CREEK AB")
  if (as.integer64(site_id) == 2246804)
    return("D CREEK")
  if (as.integer64(site_id) == 2246621)
    return("T RIVER NR")
  if (as.integer64(site_id) == 302609081453300)
    return("T RIVER BL")
  if (as.integer64(site_id) == 2246751)
    return("B RIVER BL")
  if (as.integer64(site_id) == 2246459)
    return("C RIVER")
  if (as.integer64(site_id) == 2246318)
    return("O RIVER AT KIRWIN ROAD")
  if (as.integer64(site_id) == 301204081434900)
    return("ORTEGA RIVER")
  if (as.integer64(site_id) == 2246518)
    return("POTTSBURG CREEK")
  if (as.integer64(site_id) == 2246515)
    return("P CREEK NR SOUTH")
  if (as.integer64(site_id) == 2246160)
    return("J CREEK AT OLD ST AUGUSTINE")
  if (as.integer64(site_id) == 300803081354500)
    return("J CREEK AT H LANDING")
  if (as.integer64(site_id) == 2246200)
    return("D CREEK NR DURBIN")
  if (as.integer64(site_id) == 2246500)
    return("Acosta Bridge")
  if (site_id == "301921081394701-Historic")
    return("A Bridge")
  if (as.integer64(site_id) == 301817081393600)
    return("SJR BL MARCO")
  if (as.integer64(site_id) == 301510081383500)
    return("SJR AT CHRISTOPHER")
  if (as.integer64(site_id) == 8720218)
    return("SJR AT BAR PILOT DOCK")
  if (as.integer64(site_id) == 302309081333001)
    return("DAMES POINT BRIDGE")
  if (site_id == "302309081333001-Historic")
    return("DAMES POINT BRIDGE")
  if (as.integer64(site_id) == 301124081395901)
    return("SJR BUCKMAN BRIDGE")
  if (site_id == "301124081395901-Historic")
    return("SJR BUCKMAN BRIDGE")
  if (site_id == "295856081372301-Historic")
    return("S Bridge")
  if (as.integer64(site_id) == 294213081345300)
    return("SJR AT DANCY")
  if (as.integer64(site_id) == 2244040)
    return("SJR AT BUFFALO BLUFF")
  if (as.integer64(site_id) == 2236125)
    return("SJR AT ASTOR")
  if (as.integer64(site_id) == 2246000)
    return("B Creek")
  if (as.integer64(site_id) == 02244440)
    return("D Creek")
  if (as.integer64(site_id) == 2245260)
    return("D Creek")
  if (as.integer64(site_id) == 291830081362200)
    return("L George")
  if (as.integer64(site_id) == 8720218)
    return("MAYPORT")
  if (site_id == "8720218-Historic")
    return("SJR AT B P DOCK")
  if (as.integer64(site_id) == 8720129)
    return("DAMES POINT")
  if (as.integer64(site_id) == 8720226)
    return("S Riverwalk")
  if (as.integer64(site_id) == 8720245)
    return("JACKSONVILLE UNIVERSITY")
  if (as.integer64(site_id) == 8720357)
    return("I-295 BRIDGE")
  if (as.integer64(site_id) == 8720503)
    return("RED BAY POINT")
  if (as.integer64(site_id) == 8720625)
    return("RACY POINT")
  
  #NOAA
  if (as.integer64(site_id) == 8720218)
    return("Mayport")
  if (as.integer64(site_id) == 8720228)
    return("L Jetties")
  if (as.integer64(site_id) == 8720233)
    return("B Island Command")
  if (as.integer64(site_id) == 8720219)
    return("Dames Point")
  if (as.integer64(site_id) == 8720215)
    return("Navy Fuel Depot")
  if (as.integer64(site_id) == 8720245)
    return("Jacksonville University")
  if (as.integer64(site_id) == 8720625)
    return("Racy Point")
  
  #SJR
  if (site_id == "301921081394701-1")
    return("ACOSTA BRIDGE")
  
  sitesDf <- getAllSites()
  sitesDf <- sitesDf$station_nm[sitesDf$site_no == site_id]
  
  if(!is.null(sitesDf))
  {
    return(substr(sitesDf,start=0,stop=24))
  }
  
  return("site_name_not_found")
}

#getSiteName - Get the site name.
#Created by KLA on 06/04/2018.
#Last update by KLA on 01/04/2019.
#Parameters: site_id (character), abbreviated? (character)
#Returns: character.
#Status: tested.
getSiteName<-function(site_id,abbv=TRUE)
{
  if(abbv == TRUE)
  {
    return (getAbbSiteName(site_id))
  }
  
  site_id <- as.character(site_id)
  
  # tryCatch({
  #   site_id = as.integer(site_id)
  #   
  #   if(is.finite(site_id)){
  #     site_id = as.character(site_id)
  #   }else{
  #     site_id = ""
  #   }
  #   
  # },error = function(err){
  #   print("site name was not found.")
  #   site_id = ""
  # }) # END tryCatch
  
  # if(is.null(abbr))
  # {
  #   abbr <- ""
  # }
  # 
  # site_id <- as.character(site_id)
  # 
  # sitesDf <- getAllSites()
  # site_name <- sitesDf$station_nm[sitesDf$site_no == site_id]
  # 
  # if(!is.null(site_name))
  # {
  #   if(substr(tolower(abbr),start=1,stop=2) == "ab" || abbr == TRUE)
  #   {
  #     return(substr(site_name,start=0,stop=24))
  #   }
  #   return(site_name)
  # }
  # 
  # return("site_name_not_found")
  
  # return(site_name)
  # 
  #USGS
  if(as.integer64(site_id) == 2246825)
    return("CLAPBOARD CREEK NR JACKSONVILLE, FL (below Bogey Branch)")
  if (as.integer64(site_id) == 302657081312400)
    return("CLAPBOARD CREEK AB BUCKHORN BLUFF NR JACKSONVILLE, FL (at Bogey Branch)")
  if (as.integer64(site_id) == 2246804)
    return("DUNN CREEK AT DUNN CREEK RD NR EASTPORT, FL")
  if (as.integer64(site_id) == 2246621)
    return("TROUT RIVER NR JACKSONVILLE, FL (at Lem Turner Rd. )")
  if (as.integer64(site_id) == 302609081453300)
    return("TROUT RIVER BL US1 AT DINSMORE, FL (~750' southeast of Boat Ramp Pier)")
  if (as.integer64(site_id) == 2246751)
    return("BROWARD RIVER BL BISCAYNE BLVD NR JACKSONVILLE, FL (.75 Mi NW of Harts Rd)")
  if (as.integer64(site_id) == 2246459)
    return("CEDAR RIVER AT SAN JUAN AVENUE AT JACKSONVILLE, FL")
  if (as.integer64(site_id) == 2246318)
    return("ORTEGA RIVER AT KIRWIN ROAD NEAR JACKSONVILLE,FL (at Argyle Forest Blvd.)")
  if (as.integer64(site_id) == 301204081434900)
    return("ORTEGA RIVER AT COLLINS RD. (East of Blanding Blvd.)")
  if (as.integer64(site_id) == 2246518)
    return("POTTSBURG CREEK AT US90 NR SOUTH JACKSONVILLE, FL (at Beach Blvd BR)")
  if (as.integer64(site_id) == 2245340)
    return("POTTSBURG CREEK AT US90 NR SOUTH JACKSONVILLE, FL (at Beach Blvd BR)")
  if (as.integer64(site_id) == 2246515)
    return("POTTSBURG CREEK NR SOUTH JACKSONVILLE, FL (at Bowden Rd.)")
  if (as.integer64(site_id) == 2246160)
    return("JULINGTON CREEK AT OLD ST AUGUSTINE RD NR BAYARD, FL")
  if (as.integer64(site_id) == 300803081354500)
    return("JULINGTON CREEK AT HOOD LANDING NR BAYARD, FL")
  if (as.integer64(site_id) == 2246200)
    return("DURBIN CREEK NR DURBIN, FL")
  if (as.integer64(site_id) == 2246500)
    return("ST JOHNS RIVER AT JACKSONVILLE  (at Acosta Bridge)")
  if (site_id == "301921081394701-Historic")
    return("ST JOHNS RIVER AT JACKSONVILLE  (at Acosta Bridge)")
  if (site_id == 301817081393600)
    return("ST JOHNS RIVER BL MARCO LK AT JACKSONVILLE, FL (SAV)")
  if (as.integer64(site_id) == 301510081383500)
    return("ST JOHNS RIVER AT CHRISTOPHER PT NR JACKSONVILLE, FL (SAV)")
  if (as.integer64(site_id) == 8720218)
    return("ST JOHNS RIVER AT BAR PILOT DOCK  (FDEP)")
  if (as.integer64(site_id) == 302309081333001)
    return("ST JOHNS RIVER AT DAMES POINT BRIDGE AT JACKSONVILLE, FL")
  if (site_id == "302309081333001-Historic")
    return("ST JOHNS RIVER AT DAMES POINT BRIDGE AT JACKSONVILLE, FL")
  if (as.integer64(site_id) == 301124081395901)
    return("ST JOHNS RIVER BUCKMAN BRIDGE AT JACKSONVILLE, FL")
  if (site_id == "301124081395901-Historic")
    return("ST JOHNS RIVER BUCKMAN BRIDGE AT JACKSONVILLE, FL")
  if (site_id == "295856081372301-Historic")
    return("St Johns Shands Bridge (offline)")
  if (as.integer64(site_id) == 294213081345300)
    return("ST JOHNS RIVER AT DANCY PT NR SPUDS, FL")
  if (as.integer64(site_id) == 2244040)
    return("ST JOHNS RIVER AT BUFFALO BLUFF, FL")
  if (as.integer64(site_id) == 2236125)
    return("ST JOHNS RIVER AT ASTOR, FL")
  if (as.integer64(site_id) == 2246000)
    return("North Fork Black Creek Near Middelburg,FL")
  if (as.integer64(site_id) == 02244440)
    return("Dunns Creek")
  if (as.integer64(site_id) == 2245260)
    return("Deep Creek")
  if (as.integer64(site_id) == 291830081362200)
    return("Lake George")
  if (as.integer64(site_id) == 8720218)
    return("MAYPORT (BAR PILOTS DOCK), FL (Permanent NOAA/NOS gage)")
  if (site_id == "8720218-Historic")
    return("ST JOHNS RIVER AT BAR PILOT DOCK  (FDEP)")
  if (as.integer64(site_id) == 8720129)
    return("DAMES POINT, FL")
  if (as.integer64(site_id) == 8720226)
    return("Southbank Riverwalk, St Johns River")
  if (as.integer64(site_id) == 8720245)
    return("JACKSONVILLE UNIVERSITY, FL")
  if (as.integer64(site_id) == 8720357)
    return("I-295 BRIDGE, ST. JOHNS RIVER, FL (ST. JOHNS RIVER AT THE RUDDER CLUB)")
  if (as.integer64(site_id) == 8720503)
    return("RED BAY POINT,  ST. JOHNS RIVER, FL (ST. JOHNS R @ SHANDS PIER NR SR 16)")
  if (as.integer64(site_id) == 02245340)
    return("RED BAY POINT")
  if (as.integer64(site_id) == 8720625)
    return("RACY POINT, ST. JOHNS RIVER. FL (ST. JOHNS RIVER AT RIVERDALE 1408 SR 13)")
  
  #NOAA
  if (as.integer64(site_id) == 8720218)
    return("Mayport (Bar Pilots Dock), FL")
  if (as.integer64(site_id) == 8720228)
    return("Little Jetties, FL")
  if (as.integer64(site_id) == 8720233)
    return("Blount Island Command, FL")
  if (as.integer64(site_id) == 8720219)
    return("Dames Point, FL")
  if (as.integer64(site_id) == 8720215)
    return("Navy Fuel Depot, FL")
  if (as.integer64(site_id) == 8720245)
    return("Jacksonville University, FL")
  if (as.integer64(site_id) == 8720625)
    return("Racy Point, St Johns River, FL")
  
  #SJR
  if (site_id == "301921081394701-1")
    return("SJR AT ACOSTA BRIDGE")
  
  # sitesDf <- getAllSites()
  # sitesDf <- sitesDf$station_nm[sitesDf$site_no == site_id]
  # 
  # if(!is.null(sitesDf))
  # {
  #   return(sitesDf)
  # }
  
  return("")
}

################################### PLOTTING #################################

#setPlottingLevels - Set ploting levels for boxplots.
#Created by KLA on N/A.
#Last update by KLA on N/A.
#Parameters: df (dataframe), period (numeric), append (character), startDate (Date)
#Returns: dataframe.
#Status: tested.
setPlottingLevels<-function(df,period,append,startDate,abbv=FALSE)
{
  df <- df[!is.na(df$Date),]
  
  tryCatch({
    
    if(abbv){
      start <- 3
    }else{
      start <- 1
    }
    
    if(period == 1)#convert to weekly data.
    {
      #df$Period <- format.Date(df$Date,"%V")
      df$Period <- paste(week(as.Date(df$Date)),append,sep="")
      
    }else
    {
      if(period == 2)#convert to monthly data.
      {
        df$Period <- paste(substr(months(as.Date(df$Date)), start = 1, stop = 3),append,sep="")
        #period_data <- unique(df$Period)
      }else
      {
        df$Period <- paste(substr(months(as.Date(df$Date)),start=1,stop=3),append,sep="")
        #df$Period <- paste(months(as.Date(df$Date))," ",format.Date(df$Date,"%y"),sep="")
        #period_data <- unique(df$Period)
      }
      if(period == 3)#convert to seasonal data.
      {
        period_data <- unique(df$Period)
        sMonthChar <- months(startDate)
        
        for(i in 1:(length(period_data)))
        {
          df$Period[df$Period == period_data[i]] <- paste(monthToSeason(substr(period_data[i],start=1,stop=3),sMonthChar),append,sep="")
          #df$Period[df$Period == period_data[i]] <- paste(getSeason(substr(period_data[i], start = 1, stop = nchar(period_data[i])-3)),
          #substr(period_data[i], start = nchar(period_data[i])-2, stop = nchar(period_data[i])),sep="")
        }
      }
      
      if(period == 4)#convert to Annual data.
      {
        
        if(!is.null(startDate))
        {
          if(strftime(startDate, "%m%d") == "0101")
          {
            df$Period <- paste(format.Date(df$Date, "%Y"),append,sep="")
          }else
          {
            startDateDayNumberVector <- rep(as.integer(strftime(startDate, "%j")), length(df$Date))
            
            df$offset <- as.integer( (startDateDayNumberVector + as.integer(IsLeapYear(df$Date) & !IsLeapYear(startDate))) > (as.integer(strftime(df$Date, "%j")) + as.integer(!IsLeapYear(df$Date) & IsLeapYear(startDate))) )
            df$Period <- as.numeric(strftime(df$Date, "%Y"))
            #df$Period <- paste(df$Period-df$offset,"-",df$Period+as.integer(!df$offset),append,sep="")
            df$Period <- paste(substr(as.character(df$Period-df$offset), start = start, stop = 4),
                               "-",
                               substr(as.character(df$Period+as.integer(!df$offset)), start = 3, stop = 4),append,sep="")
            
            #}else{
            #df$Period <- paste(df$Period-df$offset,"-",substr(as.character(df$Period+as.integer(!df$offset)), start = 3, stop = 4),append,sep="")
            #}
            
            df$offset <- NULL
            #df$Period <- within(df, Period[Name == 'John Smith' & State == 'WI'] <- 'John Smith1')
            #df$Period <- paste(format.Date(df$Date,"%Y"),append,sep="")
            #print(as.numeric(strftime(df$Date, "%j")))
          }
        }else
        {
          df$Period <- paste(format.Date(df$Date, "%Y"),append,sep="")
        }
      }
      
      # if(period == 4)#convert to Annual data.
      # {
      #   df$Period <- paste(format.Date(df$Date,"%Y"),append,sep="")
      # }
      
      # period_data <- unique(df$Period)
      # 
      # if(length(period_data) < 2)
      # {
      #   period_data <- ""
      # }
    }
    
    return(df)
    
  },error = function(err) {
    stop("ERROR: settPlottingLevels().")
  }) # END tryCatch
}

#DfMean - Transform a Data Frame to N-Days Mean Values.
#Created by KLA on N/A.
#Last update by KLA on 01/22/2019.
#Parameters: df (dataframe)
#Returns: dataframe.
#Status: tested.
DfMean<-function(df,n)
{
  if(!is.null(df))
  {
    tryCatch({
      columnNames <- c(names(df),"MeanDate")
      nColumns <- ncol(df)
      
      col_idx <- 1
      
      if(366 >= n & n > 1)
      {
        df$fDATE <- substr(as.character(df$Date),1,10)
        startD <- 1
        endD <- n
        c <- 1 #counter
        
        while(c <= ceiling(366/n))
        {
          tryCatch({
            #df[startD <= as.numeric(strftime(df$Date, format = "%j")) 
            #   & endD >= as.numeric(strftime(df$Date, format = "%j")),]$day <- startD
            
            df[startD <= as.numeric(strftime(df$Date, format = "%j")) 
               & endD >= as.numeric(strftime(df$Date, format = "%j")),]$fDATE <- df[startD <= as.numeric(strftime(df$Date, format = "%j")) 
                                                                                    & endD >= as.numeric(strftime(df$Date, format = "%j")),]$fDATE[1]
          },error = function(err) {
            #no data found for expecific date range.
          }) # END tryCatch.
          #df[df$Date < df$day & endD >= df$day,]$day <- c
          
          startD <- endD + 1
          endD <- endD + n
          c <- c + 1
        }
        
        df$Date <- as.POSIXct(df$fDATE, "%Y-%m-%d", tz=Sys.timezone())
        df$fDATE <- factor(df$fDATE)
      }else
      {
        df$Date <- as.POSIXct(df$Date, "%Y-%m-%d", tz=Sys.timezone())
        df$fDATE <- factor(substr(as.character(df$Date),1,10))
      }
      
      Date <- tapply(df[["Date"]], df$fDATE, mean)#Date
      
      for(i in 1:(nColumns))
      {
        class <- class(df[[i]])
        if(class == "Date" | class == "POSIXct") {col_idx = i}
        
        if(i==1)
        {
          if(class != "numeric" & class != "POSIXct" & class != "Date")
          {
            cMean <- as.character(names(sort(table(df[[i]]),decreasing=TRUE)[1]))
            dfdv <- data.frame(tapply(df[[i]], list(df$fDATE), mean,na.rm=TRUE))
            dfdv[[i]] <- cMean
          }else
          {#column is a numeric.
            dfdv <- data.frame(tapply(df[[i]], list(df$fDATE), mean,na.rm=TRUE))
            dfdv[[i]][dfdv[[i]] == "NaN" | dfdv[[i]] == "-Inf"] = "NA"
            dfdv[[i]] <- as.numeric(dfdv[[i]])
          }
        }else
        {
          if(class != "numeric" & class != "POSIXct" & class != "Date")
          {
            r <- df[[i]]
            cMean <- as.character(names(sort(table(r),decreasing=TRUE)[1]))
            dfdv[[i]] <- cMean
          }else
          {#column is a numeric.
            
            dfdv <- cbind.data.frame(dfdv,tapply(df[[i]], list(df$fDATE), mean,na.rm=TRUE))
            dfdv[[i]][dfdv[[i]] == "NaN" | dfdv[[i]] == "-Inf"] = "NA"
            dfdv[[i]] <- as.numeric(dfdv[[i]])
          }
        }
      }
      
      dfdv$Date <- with(dfdv, as.POSIXct("1970-01-01", "%Y-%m-%d", tz=Sys.timezone()) + Date)
      dfdv$Date <- as.Date(dfdv$Date,"%Y-%m-%d",tz=Sys.timezone())
      rownames(dfdv) <- NULL
      names(dfdv) <- columnNames
      dfdv$fDATE <- NULL
      dfdv$Date <- dfdv$MeanDate
      dfdv$MeanDate <- NULL
      
      dfdv$MeanDate
      
      return(dfdv)
      
    },error = function(err) {
      stop("ERROR: DfMean().")
    }) # END tryCatch
  }
  return(NULL)
}

#DfToDailyMean2 - Transform a Data Frame to Daily Mean Values.
#Created by KLA on N/A.
#Last update by KLA on N/A.
#Parameters: df (dataframe)
#Returns: dataframe.
#Status: tested.
# DfToDailyMean2<-function(df)
# {
#   tryCatch({
#     columnNames <- c(names(df),"MeanDate")
#     nColumns <- ncol(df)
#     
#     col_idx <- 1
#     df$fDATE <- factor(substr(as.character(df$Date),1,10))
#     Date <- tapply(df[["Date"]], df$fDATE, mean)#Date
#     
#     for(i in 1:(nColumns))
#     {
#       class <- class(df[[i]])
#       if(class == "Date" | class == "POSIXct") {col_idx = i}
#       
#       if(i==1)
#       {
#         if(class != "numeric" & class != "POSIXct" & class != "Date")
#         {
#           cMean <- as.character(names(sort(table(df[[i]]),decreasing=TRUE)[1]))
#           dfdv <- data.frame(tapply(df[[i]], list(df$fDATE), mean,na.rm=TRUE))
#           dfdv[[i]] <- cMean
#         }else
#         {#column is a numeric.
#           dfdv <- data.frame(tapply(df[[i]], list(df$fDATE), mean,na.rm=TRUE))
#           dfdv[[i]][dfdv[[i]] == "NaN" | dfdv[[i]] == "-Inf"] = "NA"
#           dfdv[[i]] <- as.numeric(dfdv[[i]])
#         }
#       }else
#       {
#         if(class != "numeric" & class != "POSIXct" & class != "Date")
#         {
#           r <- df[[i]]
#           cMean <- as.character(names(sort(table(r),decreasing=TRUE)[1]))
#           dfdv[[i]] <- cMean
#         }else
#         {#column is a numeric.
#           
#           dfdv <- cbind(dfdv,tapply(df[[i]], list(df$fDATE), mean,na.rm=TRUE))
#           dfdv[[i]][dfdv[[i]] == "NaN" | dfdv[[i]] == "-Inf"] = "NA"
#           dfdv[[i]] <- as.numeric(dfdv[[i]])
#         }
#       }
#     }
#     
#     dfdv$Date <- with(dfdv, as.POSIXct("1970-01-01", "%Y-%m-%d", tz=Sys.timezone()) + Date)
#     dfdv$Date <- as.Date(dfdv$Date,"%Y-%m-%d",tz=Sys.timezone())
#     rownames(dfdv) <- NULL
#     names(dfdv) <- columnNames
#     dfdv$fDATE <- NULL
#     dfdv$Date <- dfdv$MeanDate
#     dfdv$MeanDate <- NULL
#     
#     dfdv$MeanDate
#     
#     return(dfdv)
#     
#   },error = function(err) {
#     stop("ERROR: DfToDailyMean().")
#   }) # END tryCatch
# }



# convertDfColumn<-function(df,typeVector,dFormat)
# {
#   tryCatch({
#     if(ncol(df) == length(typeVector))
#     {
#       for(i in 1:(ncol(df)))
#       {
#         if(typeVector[i] == "character")
#         {
#           df[[i]] <- as.character(df[[i]])
#           
#         }else if(typeVector[i] == "numeric")
#         {
#           df[[i]] <- as.numeric(df[[i]])
#           
#         }else if(typeVector[i] == "Date" | typeVector[i] != "POSIXct")
#         {
#           df[[i]] <- as.POSIXct(df[[i]],dFormat,tz=Sys.timezone())
#         }
#       }
#     }else
#     {
#       print("WARNING: Vector Length does not match the number of columns in Data Frame.")
#     }
#     
#     return(df)
#     
#   },error = function(err) {
#     stop("ERROR: convertDfColumn().")
#   }) # END tryCatch
# }

#usgsDataAvailable - Check a USGS dataframe to see the location were the data was collected (TOP,MIDDLE,BOTTOM and NA).
#Created by KLA on N/A.
#Last update by KLA on 12/31/2018.
#Parameters: df (dataframe), db (character)
#Returns: character (vector)
#Status: tested.
usgsDataAvailable<-function(df,db)
{
  if(db == "USGS")
  {
    a_data <- c()
    rch <- c ()
    
    for(i in 1:(ncol(df)))
    {
      if(i+3 < ncol(df))
      {
        a_data <- c(a_data,substr(names(df)[i+3],start=3,stop=5))
      }
    }
    
    a_data <- unique(a_data)
    a_data <- tolower(a_data)
    
    if("top" %in% a_data)#contains top
    {
      rch <- c(rch,"T")
    }
    if("bot" %in% a_data)#contains bottom
    {
      rch <- c(rch,"B")
    }
    
    if("mid" %in% a_data)#contains middle
    {
      rch <- c(rch,"M")
    }
    if("000" %in% a_data || "004" %in% a_data)#not specficied.
    {
      rch <- c(rch,"NA")
    }
    return (rch)
  }
}

#convertDataFrame - Transform a dataframe into a simplified form (Date, Water_temp, Conductivity, Salinity).
#Created by KLA on N/A.
#Last update by KLA on 12/31/2018.
#Parameters: df (dataframe), db (character)
#Returns: list (dataframe)
#Status: tested.
convertDataFrame<-function(df,db)
{
  tryCatch({
    if(db=="USGS")
    {
      rList <- list()
      a_data <- c()
      nameList <- c()
      
      avalData <- usgsDataAvailable(df,db)
      
      if("T" %in% avalData)
      {
        df_r <- df[,c("Date","X_TOP_00010_00000","X_TOP_00095_00000")]
        df_r$newrow <- (6.859819E-38*(df_r[[3]])^8 + -2.027297E-32*(df_r[[3]])^7 + 2.576767E-27*(df_r[[3]])^6 + -1.855032E-22*(df_r[[3]])^5 + 8.450662E-18*(df_r[[3]])^4 + -0.0000000000002616744*(df_r[[3]])^3 + 0.000000007096135*(df_r[[3]])^2 + 0.0005161311*(df_r[[3]]) + -0.05028803)
        names(df_r) <-c("Date","WATER_TEMPERATURE_TOP","CONDUCTIVITY_TOP","SALINITY_TOP")
        df_r[df_r < 0.01] <- NA
        rList <- list.append(rList,df_r)
        nameList <- c(nameList,"TOP")
      }
      if ("B" %in% avalData)
      {
        df_r <- df[,c("Date","X_BOTTOM_00010_00000","X_BOTTOM_00095_00000")]
        df_r$newrow <- (6.859819E-38*(df_r[[3]])^8 + -2.027297E-32*(df_r[[3]])^7 + 2.576767E-27*(df_r[[3]])^6 + -1.855032E-22*(df_r[[3]])^5 + 8.450662E-18*(df_r[[3]])^4 + -0.0000000000002616744*(df_r[[3]])^3 + 0.000000007096135*(df_r[[3]])^2 + 0.0005161311*(df_r[[3]]) + -0.05028803)
        names(df_r) <-c("Date","WATER_TEMPERATURE_BOTTOM","CONDUCTIVITY_BOTTOM","SALINITY_BOTTOM")
        df_r[df_r < 0.01] <- NA
        rList <- list.append(rList,df_r)
        nameList <- c(nameList,"BOTTOM")
        
      }
      if("M" %in% avalData)
      {
        df_r <- df[,c("Date","X_MIDDLE_00010_00000","X_MIDDLE_00095_00000")]
        df_r$newrow <- (6.859819E-38*(df_r[[3]])^8 + -2.027297E-32*(df_r[[3]])^7 + 2.576767E-27*(df_r[[3]])^6 + -1.855032E-22*(df_r[[3]])^5 + 8.450662E-18*(df_r[[3]])^4 + -0.0000000000002616744*(df_r[[3]])^3 + 0.000000007096135*(df_r[[3]])^2 + 0.0005161311*(df_r[[3]]) + -0.05028803)
        names(df_r) <-c("Date","WATER_TEMPERATURE_MIDDLE","CONDUCTIVITY_MIDDLE","SALINITY_MIDDLE")
        df_r[df_r < 0.01] <- NA
        rList <- list.append(rList,df_r)
        nameList <- c(nameList,"MIDDLE")
      }
      if("NA" %in% avalData)
      {
        df_r <- df[,c("Date","X_00010_00000","X_00095_00000")]
        df_r$newrow <- (6.859819E-38*(df_r[[3]])^8 + -2.027297E-32*(df_r[[3]])^7 + 2.576767E-27*(df_r[[3]])^6 + -1.855032E-22*(df_r[[3]])^5 + 8.450662E-18*(df_r[[3]])^4 + -0.0000000000002616744*(df_r[[3]])^3 + 0.000000007096135*(df_r[[3]])^2 + 0.0005161311*(df_r[[3]]) + -0.05028803)
        names(df_r) <-c("Date","WATER_TEMPERATURE","CONDUCTIVITY","SALINITY")
        df_r[df_r < 0.01] <- NA
        rList <- list.append(rList,df_r)
        nameList <- c(nameList,"")
      }
      names(rList) <- nameList
      
      return(rList)
    }
    
    return(NULL)
  },error = function(err) {
    return (NULL);
    #do nothing.
    #stop("ERROR: convertDataFrame().")
  }) # END tryCatch
}


#####################################including baseline



#getDfPeriods - Returns the available period data in order to create the timeline in certains ggplots (ex: boxplot).
#Created by KLA on N/A.
#Last update by KLA on N/A.
#Parameters: period (numeric), startDate (Date), df (dataframe).
#Returns: character (vector)
#Status: tested.
getDfPeriods<-function(period,startDate,df,abbv=FALSE)
{
  tryCatch({
    
    if(abbv){
      start <- 3
    }else{
      start <- 1
    }
    
    rVector <- c()
    sMonth <- as.numeric(format.Date(startDate,"%m"))
    sYear <- as.numeric(range(format.Date(df$Date, "%Y"),na.rm = TRUE)[1])
    eYear <- as.numeric(range(format.Date(df$Date, "%Y"),na.rm = TRUE)[2])
    
    if(period == 1)#convert to weekly data.
    {
      for(i in 1:52)
      {
        weeks <- unique(substr(week(df$Date),start=1,stop=3))
        rVector <- c(rVector,paste(weeks[i],"_b",sep=""),paste(weeks[i],"_m",sep=""),weeks[i])
        #rVector <- c(rVector,paste("Week #",i,"_b",sep=""),paste("Week #",i,sep=""))
      }
    }
    
    if(period == 2)#convert to monthly data.
    {
      for (i in 1:12)#create data frame for period.
      {
        rVector <- c(rVector,paste(month.abb[sMonth],"_b",sep=""),paste(month.abb[sMonth],"_m",sep=""),month.abb[sMonth])
        sMonth <- monthAddition(sMonth,1)
      }
    }
    
    if(period == 3)#convert to seasonal data.
    {
      for (i in 1:3)#create data frame for period.
      {
        season <- getSeason(sMonth)
        rVector <- c(rVector,paste(season,"_b",sep=""),paste(season,"_m",sep=""),paste(season,sep=""))
        sMonth <- monthAddition(sMonth,4)
      }
    }
    
    if(period == 4)#convert to Annual data.
    {
      year <- sYear
      
      if(!is.null(startDate))
      {
        if(strftime(startDate, "%m%d") == "0101")
        {
          while(year <= eYear)
          {
            rVector <- c(rVector,paste(year,"_b",sep=""),paste(year,"_m",sep=""),paste(year,sep=""))
            year <- year + 1
          }
        }else
        {
          while(year <= eYear)
          {
            rVector <- c(rVector,paste(substr(as.character(year-1), start = start, stop = 4),"-",substr(as.character(year), start = 3, stop = 4),"_b",sep=""),
                         paste(substr(as.character(year-1), start = start, stop = 4),"-",substr(as.character(year), start = 3, stop = 4),"_m",sep=""),
                         paste(substr(as.character(year-1), start = start, stop = 4),"-",substr(as.character(year), start = 3, stop = 4),sep=""))
            
            year <- year + 1
          }
        }
      }
      
      # year <- sYear
      # while(year <= eYear)
      # {
      #   rVector <- c(rVector,paste(year,"_b",sep=""),paste(year,"_m",sep=""),paste(year,sep=""))
      #   year <- year + 1
      # }
    }
    
    return(rVector)
    
  },error = function(err) {
    stop("ERROR: getDfPeriods().")
  }) # END tryCatch
}

#b_boxplotFunc - Create a boxplot.
#Created by KLA on N/A.
#Last update by KLA on 02/26/2019.
#Parameters: df (dataframe assesment), df_b (dataframe baseline), df_m (dataframe middle), period (numeric),
#graphName (character), site_id (character), zoom (boolean), sDate (Date).
#Returns: boxplot.
#Status: tested.
b_boxplotFunc<- function(df,df_b,df_m,df_allYears,period,graphName,site_id,zoom,sDate)
{
  tryCatch({
    notitle <- TRUE
    df_all <- data.frame()
    #startDate <- df$Date[1]
    
    if(period == 4){
      abbv_year <- TRUE
      start <- 3
    }else{
      abbv_year <- FALSE
      start <- 1
    }
    
    if(!is.null(df_b))
    {
      df2 <- setPlottingLevels(df_b,period,"_b",sDate, abbv_year)
      
      b_yearRange <- range(format.Date(df2$Date, "%Y"),na.rm = TRUE)
      b_yearRange <- paste(substr(as.character(b_yearRange[1]), start = start, stop = 4),
                           "-",
                           substr(as.character(b_yearRange[2]), start = start, stop = 4)," (b)",sep="")
      df2$Year <- b_yearRange
      df_all <- rbind.data.frame(df_all,df2)
    }
    
    if(!is.null(df_m))
    {
      df3 <- setPlottingLevels(df_m,period,"_m",sDate, abbv_year)
      m_yearRange <- range(format.Date(df3$Date, "%Y"),na.rm = TRUE)
      m_yearRange <- paste(substr(as.character(m_yearRange[1]), start = start, stop = 4),
                           "-",
                           substr(as.character(m_yearRange[2]), start = start, stop = 4)," (m)",sep="")
      df3$Year <- m_yearRange
      df_all <- rbind.data.frame(df_all,df3)
    }
    
    if(period == 4){
      df1 <- setPlottingLevels(df_allYears,period,"",sDate, abbv_year)
    }else{
      df1 <- setPlottingLevels(df,period,"",sDate, abbv_year)
    }
    
    yearRange <- range(format.Date(df1$Date, "%Y"),na.rm = TRUE)
    yearRange <- paste(substr(as.character(yearRange[1]), start = start, stop = 4),
                       "-",
                       substr(as.character(yearRange[2]), start = start, stop = 4)," (a)",sep="")
    df1$Year <- yearRange
    df_all <- rbind.data.frame(df_all,df1)
    df_all <- df_all[!is.na(df_all$Date),]
    
    lv <- unique(df_all$Year)
    
    df_all$Year <- factor(df_all$Year, levels = lv,ordered = TRUE)
    
    order <- getDfPeriods(period,sDate,df_all,abbv_year)
    #df_all[match(order, df_all$Period),]
    
    df_all$Period <- factor(df_all$Period, levels = order)
    
    # df_all$Period2 <- substr(df_all$Period,start=1,stop=3)
    # orderP2 <- unique(df_all$Period2)
    # df_all$Period2 <- factor(df_all$Period2, levels = orderP2)
    
    datamax = max(na.omit(df_all[[4]]))
    datamin = min(na.omit(df_all[[4]]))
    
    periods <- as.character(unique(df_all$Year))
    
    colorVector <- c()
    
    for(i in 1:(length(periods)))
    {
      if(substr(periods[i],start=nchar(periods[i])-1,stop = nchar(periods[i])-1) == "b")
      {
        colorVector <- c(colorVector,"#85C1E9")#Blue
      }else
      {
        if(substr(periods[i],start=nchar(periods[i])-1,stop = nchar(periods[i])-1) == "m")
        {
          colorVector <- c(colorVector,"#90EE90")#Green
        }else
        {
          colorVector <- c(colorVector,"#F1948A")#Red
        }
      }
    }
    colnames(df_all)[4] <- "VALUE"
    #colnames(d_b)[4] <- "VALUE"
    
    nFrames = as.numeric(!is.null(df_b)) + as.numeric(!is.null(df_m))
    if( (nFrames > 1 & period == 2) | period == 1)
    {
      angle<-90
    }else
    {
      angle<-0
    }
    
    bshape <- 1
    line_width <- .1
    
    if(notitle)
    {
      plotTitle <- ""
    }else
    {
      plotTitle <- paste(getSiteName(site_id,"abb"),"\nBoxplot for ",graphName," [ZOOM 1:5]","\nPeriod: ",
                         months(sDate)," ",as.numeric(format.Date(sDate,"%d"))," - ",months(sDate+years(1)-days(1))," ",as.numeric(format.Date(sDate+years(1)-days(1),"%d")),
                         sep="")
    }
    
    if(zoom == TRUE)
    {
      x1 <- median(c(datamin,datamax))
      
      if(x1 > 5)
      {
        x1 <- 5
      }
      breaks <- c(round(seq(datamin,x1,(x1-datamin)/10),2),round(seq(x1,datamax,(datamax-x1)/6),2))
      
      #boxplot = ggplot(df_all, aes(x=Period, y=VALUE,fill=Year))  + geom_boxplot(outlier.size=1.5, lwd=.9 ,varwidth = FALSE,colour="#000000")+ stat_boxplot(geom = "errorbar", width = 0.5, lwd=.9) + labs(title=paste(getAbbSiteName(site_id),"\nBoxplot for ",graphName," [ZOOM 1:5]",sep=""), x=periodPrint(period), y="Salinity (ppt)")+  stat_summary(fun.y=mean, geom="point", shape=23, size=3.5, color="black", fill="grey")   + scale_y_continuous(breaks=breaks)+theme_light()+theme(plot.title = element_text(hjust=0.5),legend.position="bottom",axis.text.x = element_text(angle=angle))+coord_cartesian(ylim=c(datamin,x1))+ scale_fill_manual(values=colorVector)
      
      boxplot = ggplot(df_all, aes(x=Period, y=VALUE,fill=Year))  + geom_boxplot(outlier.shape = bshape, outlier.size=1.5, lwd=line_width ,varwidth = FALSE,colour="#000000")+ stat_boxplot(geom = "errorbar", width = 0.5, lwd=.9) + labs(fill="Period",title=plotTitle, x=periodPrint(period), y="Salinity (ppt)")+  stat_summary(fun.y=mean, geom="point", shape=23, size=3.5, color="black", fill="grey")   + scale_y_continuous(breaks=breaks)+theme_light()+theme(plot.title = element_text(hjust=0.5),legend.position="bottom",axis.text.x = element_text(angle=angle))+coord_cartesian(ylim=c(datamin,x1))+ scale_fill_manual(values=colorVector)
      
    }else
    {
      #breaks <- c(round(seq(datamin,datamax,(datamax-datamin)/10),2))
      increment <- GetIncrementInteger(datamax-datamin)
      breaks <- seq(0,datamax,increment)
      #boxplot = ggplot(df_all, aes(x=Period, y=VALUE,fill=Year))  + geom_boxplot(outlier.size=1.5, lwd=line_width ,varwidth = FALSE,colour="#000000")+ stat_boxplot(geom = "errorbar", width = 0.5, lwd=.9) + labs(title=paste(getAbbSiteName(site_id),"\nBoxplot for ",graphName,"\n",sep=""), x=periodPrint(period), y="Salinity (ppt)")+  stat_summary(fun.y=mean, geom="point", shape=23, size=3.5, color="black", fill="grey")   + scale_y_continuous(breaks=breaks)+theme_light()+theme(plot.title = element_text(hjust=0.5),legend.position="bottom",axis.text.x = element_text(angle=angle))+ scale_fill_manual(values=colorVector)
      
      boxplot = ggplot(df_all, aes(x=Period, y=VALUE,fill=Year))  + geom_boxplot(outlier.shape = bshape, outlier.size=1.5, lwd=line_width ,varwidth = FALSE,colour="#000000")+
        stat_boxplot(geom = "errorbar", width = 0.5, lwd=.5) +
        labs(fill="Period",title=plotTitle, x=periodPrint(period), y="Salinity (ppt)")+
        stat_summary(fun.y=mean, geom="point", shape=23, size=3.5, color="black",fill="grey")   +
        scale_y_continuous(breaks=breaks)+theme_light()+
        theme(plot.title = element_text(hjust=0.5),legend.position="bottom",axis.text.x = element_text(angle=angle))+
        scale_fill_manual(values=colorVector)
    }
    
    #+ scale_fill_manual(values=colorVector), 
    #cumfreqplot = ggplot(nDf, aes(x=VALUE,y=n,color=Year)) + geom_point() + scale_color_manual(values = c("red", "blue"))  + geom_hline(data = data, aes(yintercept=percentiles),colour="black",linetype = "solid")+ labs(title=paste(pasteInfo,"\n Cumulative Frequency for ",graphName,sep=""), y="Cumulative Frequency", x="Salinity (ppt)") + scale_x_continuous(breaks=round(seq(datamin,datamax,(datamax-datamin)/5),2)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + geom_point(data=data,aes(x=q,y = percentiles),colour="black",size=7,shape=1)+geom_point(data=data_b,aes(x=q_b,y = percentiles),colour="black",size=7,shape=1)+theme_light()+theme(plot.title = element_text(hjust=0.5))
    
    #+ theme(plot.title = element_text(size = rel(1.5), face="bold"), axis.title = element_text(face="bold"), axis.line = element_line(colour = "black", size = .7, linetype = "solid"), legend.position="none", panel.background = element_rect(fill="NA"), panel.grid.major = element_line(colour = "grey68"), panel.grid.minor = element_blank())
    #+ scale_fill_manual(values=colorVector) 
    
    #print(boxplot)
    
    #boxplot = ggplot(df_all, aes(x=Period, y=VALUE, fill=Period,color=Period)) + stat_boxplot(geom = "errorbar", width = 0.3, lwd=.9) + geom_boxplot(outlier.size=1.5, lwd=.9, varwidth = TRUE) + labs(title=paste("Boxplot for ",graphName,sep=""), x=paste(getPeriod(period)," ",yearRange,sep=""), y="Salinity") +  stat_summary(fun.y=mean, geom="point", shape=23, size=4.5, color="red", fill="red") + theme(plot.title = element_text(size = rel(1.5), face="bold"), axis.title = element_text(face="bold"), axis.line = element_line(colour = "black", size = .7, linetype = "solid"), legend.position="none", panel.background = element_rect(fill="NA"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_fill_brewer(palette="Paired")
    
    #boxplot = ggplot(df_all,mapping= aes_q(x=as.name(names(df_all)[5]), y=as.name(names(df_all)[4]), fill=as.name(names(df_all)[5])))                                                 + geom_boxplot()                                                      + labs(title=paste("Boxplot for ",graphName,sep=""), x=paste(getPeriod(period)," ",yearRange,sep=""), y="Salinity")                                                                                                                                                                                                                                                                                                                                                                                                                                                                               +   scale_y_continuous(breaks=seq(datamin,datamax,(datamax-datamin)/10))
    #print(boxplot)
    
    #boxplot = ggplot(df_all, aes(x=Period, y=VALUE, fill=Period,color=Period)) + stat_boxplot(geom = "errorbar", width = 0.3, lwd=.9)                                                                    + labs(title=paste("Boxplot for ",graphName,sep=""), x=paste(getPeriod(period)," ",yearRange,sep=""), y="Salinity") +  stat_summary(fun.y=mean, geom="point", shape=23, size=4.5, color="red", fill="red") + theme(plot.title = element_text(size = rel(1.5), face="bold"), axis.title = element_text(face="bold"), axis.line = element_line(colour = "black", size = .7, linetype = "solid"), legend.position="none", panel.background = element_rect(fill="NA"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())                   + scale_fill_brewer(palette="Paired")+   scale_y_continuous(breaks=seq(datamin,datamax,(datamax-datamin)/10))
    #print(boxplot
    
    return(boxplot)
    
  },error = function(err) {
    return(paste("No Boxplot data available for: ",graphName,sep=""))
  }) # END tryCatch
}


#####DO NOT CHANGE THIS CODE######################
#####THIS CODE DELETES FILES PERMANENTLY##########
##################################################

#If p is False files will me moved to recycle bin upon deleting.
ClearTempFolder<-function(p=TRUE){
  if(p)
  {
    unlink(paste(getwd(),"/temp/*",sep=""))
  }else
  {
    do.call(file.remove, GetListOfTempFiles())
  }
}

IsTempFolderNotEmpty<-function(){
  return (length(GetListOfTempFiles()) != 0)
}

GetListOfTempFiles<-function(){
  return (list.files(paste(getwd(),"/temp",sep=""), full.names = TRUE))
}
##################################################
##################################################

#b_boxplotToDocx - Writes a ggplot Boxplot Graph to a word document.
#Created by KLA on N/A.
#Last update by KLA on N/A.
#Parameters: doc (Word doc), df (dataframe assesment), df_b (dataframe baseline), df_m (dataframe middle), period (numeric),
#graphName (character), site_id (character), zoom (boolean), sDate (Date).
#Returns: Word doc.
#Status: tested.
b_boxplotToDocx<-function(doc,df,df_b,df_m,df_allYears,period,graphName,site_id,zoom,sDate,fLabel,id)
{
  tryCatch({
    b <- b_boxplotFunc(df,df_b,df_m,df_allYears,period,graphName,site_id,zoom,sDate)
    if(is.character(b))
    {
      
    }else
    {
      imgName <- paste("bplot-",site_id,"-P",fLabel[3],".jpg",sep="")
      ggsave(path="temp",filename=imgName,width=9,height=6,units="in",dpi=150)
    }
    
  },error = function(err) {
    stop("ERROR: b_boxplotToDocx().")
  }) # END tryCatch
}

shiftVector<- function(x,y,apy){
  
  while(!is.null(x) & !is.null(y) & paste(y[1],apy,sep="") != x[1])
  {
    temp <- x[c(1)]
    x <- x[-c(1)]
    x <- c(x,temp)
  }
  return(x);
}


#Data Must in Daily Values.
DFMovingAverage<-function(df,n=7){
  
  if(nrow(df) > 0 && ncol(df) > 0)
  {
    for(i in 1:(ncol(df)))
    {
      if(sum(is.finite(df[[i]])) > 0 && sum(!is.POSIXct(df[[i]]) > 0))
      {
        df[[i]] <- rollmean(df[[i]],n,fill = NA)
      }
    }
    df <- df[!is.na(df["Date"]), ]
  }
  return(df)
}

#sevenDayMovingAveragePlot - Writes a ggplot to a word document.
#Created by KLA on 02/20/2020.
#Parameters: doc (Word doc), df (dataframe assesment), df_b (dataframe baseline), df_m (dataframe middle), period (numeric),
#graphName (character), type (character), sDate (Date).
#Returns: Word doc.
#Status: not tested.
sevenDayMovingAveragePlot<- function(doc,df,df_b,df_m,period,graphName,site_id,sDate,fLabel){
  
  tryCatch({
    notitle=TRUE
    #percentiles = c(0, 0.05,0.10,0.25,0.5,0.75,0.90,0.95,1)
    percentiles = c(0.5,0.90,0.95)
    x_intercepts <- c()
    #assessment year
    df <- DfMean(df,1)
    df <- GetDataFrameMovingAverage(df,7,site_id,TRUE,NULL)
    
    df$n <- df$Date
    month(df$n) = month(df$n) + 1
    
    df$n <- as.numeric(strftime(df$n, format = "%j"))
    df <- setPlottingLevels(df,period,"",sDate)
    tempVector <- unique(df$Period)
    yearRange <- range(format.Date(df$Date,"%Y"),na.rm = TRUE)
    yearRange <- paste(yearRange[1],"-",yearRange[2]," (a)",sep="")
    df <- df[order(df[[4]]),]
    
    if(!is.null(df_b))#baseline year.
    {
      byr <- range(year(df_b$Date),na.rm = TRUE)
      year(df_b$Date) = 0
      df_b <- DfMean(df_b,1)
      df_b <- GetDataFrameMovingAverage(df_b,7,site_id,FALSE,NULL)
      
      df_b$n <- df_b$Date
      month(df_b$n) = month(df_b$n) + 1
      
      df_b$n <- as.numeric(strftime(df_b$n, format = "%j"))
      df_b <- setPlottingLevels(df_b,period,"_b",sDate)
      #tempVector_b <- shiftVector(unique(df_b$Period),tempVector,"_b")
      #b_yearRange <- range(format.Date(df_b$Date,"%Y"),na.rm = TRUE)
      b_yearRange <- paste(byr[1],"-",byr[2]," (b)",sep="")
      df_b <- df_b[order(df_b[[4]]),]
    }
    
    if(!is.null(df_m))#middle year.
    {
      df_m$n <- as.numeric(strftime(df_m$Date, format = "%j"))
      df_m <- setPlottingLevels(df_m,period,"_m",sDate)
      #tempVector_m <- shiftVector(unique(df_b$Period),tempVector,"_m")
      m_yearRange <- range(format.Date(df_m$Date,"%Y"),na.rm = TRUE)
      m_yearRange <- paste(m_yearRange[1],"-",m_yearRange[2]," (m)",sep="")
      df_m <- df_m[order(df_m[[4]]),]
    }
    
    #set the year range from startYear-endYear
    if(period == 4)
    {
      numPlots <- 1
    }else
    {
      numPlots <- length(tempVector)
    }
    
  },error = function(err) {
    stop("ERROR: sevenDayMovingAveragePlot().")
  }) # END tryCatch
  
  for(i in 1:numPlots)
  {
    tryCatch({
      if(period == 1)
      {
        pasteInfo <- paste("Week #",tempVector[i],sep="")#weekly
      }else if (period == 2)
      {
        pasteInfo <- extendMonthName(tempVector[i])# months
      }else if (period == 3)
      {
        pasteInfo <- tempVector[i]#seasons
      }else
      {
        pasteInfo <- "Annual"
      }
      
      df_all <- data.frame()
      colorVector <- c()
      data_b <- NULL
      data_m <- NULL
      data <- NULL
      
      if(!is.null(df_b))
      {
        tryCatch({
          if(period != 4 )#baseline.
          {
            d_b <- df_b[df_b$Period == paste(tempVector[i],"_b",sep=""),]
          }else
          {
            d_b <- df_b
          }
          q_b=quantile(d_b[[4]],percentiles,na.rm = TRUE)
          data_b = data.frame(q_b, percentiles)
          #is.num <- sapply(data_b, is.numeric)
          #data_b[is.num] <- lapply(data_b[is.num], round, 2)
          d_b <- d_b[!is.na(d_b[[4]]),]
          
          colnames(d_b)[4] <- "VALUE"
          
          d_b$Year <- b_yearRange
          df_all <- rbind.data.frame(df_all,d_b)
          colorVector <- c(colorVector,"blue")
        },error = function(err) {
          #no data found for period.
          d_b <- NULL
        }) # END tryCatch
      }
      
      if(!is.null(df_m))
      {
        tryCatch({
          if(period != 4 )#middle.
          {
            d_m <- df_m[df_m$Period == paste(tempVector[i],"_m",sep=""),]
          }else
          {
            d_m <- df_m
          }
          
          if(period != 4 ){d_m <- df_m[df_m$Period == tempVector_m[i],]}#middle.
          q_m=quantile(d_m[[4]],percentiles,na.rm = TRUE)
          data_m = data.frame(q_m, percentiles)
          #is.num <- sapply(data_m, is.numeric)
          #data_m[is.num] <- lapply(data_m[is.num], round, 2)
          d_m <- d_m[!is.na(d_m[[4]]),]
          
          colnames(d_m)[4] <- "VALUE"
          d_m$Year <- m_yearRange
          df_all <- rbind.data.frame(df_all,d_m)
          colorVector <- c(colorVector,"green")
          
        },error = function(err) {
          #no data found for period.
          d_m <- NULL
        }) # END tryCatch
      }
      
      tryCatch({
        if(period != 4 )#assessment.
        {
          d <- df[df$Period == tempVector[i],]
        }else
        {
          d <- df
        }
        
        q=quantile(d[[4]],percentiles,na.rm = TRUE)
        data = data.frame(q, percentiles)
        #is.num <- sapply(data, is.numeric)
        #data[is.num] <- lapply(data[is.num], round, 2)
        d<-d[!is.na(d[[4]]),]
        
        colnames(d)[4] <- "VALUE"
        d$Year <- yearRange
        df_all <- rbind.data.frame(df_all,d)
        colorVector <- c(colorVector,"red")
      },error = function(err) {
        #no data found for period.
        d <- NULL 
      }) # END tryCatch
      
      lv <- unique(df_all$Year)
      
      df_all$Year <- factor(df_all$Year, levels = lv,ordered = TRUE)
      
      datamax = max(df_all["VALUE"],na.rm = TRUE)
      datamin = min(df_all["VALUE"],na.rm = TRUE)
      
      daymax = max(df_all["n"],na.rm = TRUE)
      daymin = min(df_all["n"],na.rm = TRUE)
      
      #movAvgPlot = ggplot(df_all, aes(x=n,y=VALUE,color=Year)) + geom_point() + scale_color_manual(values = colorVector)  + geom_hline(data = data, aes(yintercept=percentiles),colour="black",linetype = "solid")+ labs(title=paste(pasteInfo,"\n Cumulative Frequency for ",graphName,sep=""), y="Cumulative Frequency", x="Salinity (ppt)") + scale_x_continuous(breaks=round(seq(datamin,datamax,(datamax-datamin)/9),2)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+theme_light()+theme(plot.title = element_text(hjust=0.5))
      
      #movAvgPlot = ggplot(df_all, aes(x=n,y=VALUE,color=Year)) + geom_point() + scale_color_manual(values = colorVector)  + geom_vline(data = data, aes(xintercept=percentiles),colour="black",linetype = "solid") + labs(title="Moving Average Plot", y="7-Day Moving Avg Salinity (ppt)", x="Day of the Year") + scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+theme_light()+theme(plot.title = element_text(hjust=0.5))
      
      mn <- ddply(df_all, "Year", summarise, grp.mean=mean(VALUE))
      q05 <- ddply(df_all, "Year", summarise, grp.mean=quantile(VALUE,0.05))
      q90 <- ddply(df_all, "Year", summarise, grp.mean=quantile(VALUE,0.90))
      q95 <- ddply(df_all, "Year", summarise, grp.mean=quantile(VALUE,0.95))
      
      
      #geom_vline(data=q05, aes(xintercept=grp.mean, color=sex) +
      #geom_vline(data=q90, aes(xintercept=grp.mean, color=sex) +
      #geom_vline(data=q95, aes(xintercept=grp.mean, color=sex) +
      
      if(notitle)
      {
        plotTitle <- ""
      }else
      {
        plotTitle <- "7-Day Moving Average"
      }
      
      #round(seq(daymin,daymax,(daymax-daymin)/5),2)
      x_breaks <- c(1,50,100,150,200,250,300,366)
      
      #round(seq(datamin,datamax,(datamax-datamin)/9),2)
      increment <- GetIncrementInteger(datamax-datamin)
      y_breaks <- seq(0,datamax,increment)
      
      #save.image(file='myEnvironment.RData')
      #save(list = c("df_all", "plotTitle", "mn", "q05", "q90", "q95","x_breaks","y_breaks","colorVector","datamax","datamin","daymax","daymin","lv","pasteInfo"),file='data.RData')
      
      
      #load(file='myEnvironment.RData')
      #load(file='data.RData')
      
      
      range_days <- daymax - daymin
      x_pos_b <- c(0, ceiling(range_days*.11), 
                   ceiling(range_days*.22), ceiling(range_days*.33))
      x_pos_a <- c(ceiling(-range_days*.33), ceiling(-range_days*.22), 
                   ceiling(-range_days*.11), 0)
      
      range_data <- datamax - datamin
      padding_y <- .02 * range_data
      
      movAvgPlot = ggplot(df_all, aes(x=n,y=VALUE,color=Year)) +
        scale_x_continuous(breaks=x_breaks) +
        scale_y_continuous(breaks=y_breaks) +
        geom_line() + scale_color_manual(values = colorVector)  +
        geom_hline(data=mn, aes(yintercept=grp.mean, color=Year),linetype="dashed") +
        geom_hline(data=q05, aes(yintercept=grp.mean, color=Year),linetype="F1") +
        geom_hline(data=q90, aes(yintercept=grp.mean, color=Year),linetype="longdash") +
        geom_hline(data=q95, aes(yintercept=grp.mean, color=Year),linetype="twodash") +
        labs(color="Period",title=plotTitle, y="7-Day Moving Avg Salinity (ppt)", x="Day of the Year") +
        theme_light()+theme(plot.title = element_text(hjust=0.5),legend.position = "bottom")
      
      letter1 <- substr(mn[1,1],nchar(as.character(mn[1,1]))-2,nchar(as.character(mn[1,1])))
      letter2 <- substr(mn[2,1],nchar(as.character(mn[2,1]))-2,nchar(as.character(mn[2,1])))
      
      
      movAvgPlot <- movAvgPlot + geom_text(data=q05[1,], aes(daymin,q05[[1,"grp.mean"]],label = paste("q05",letter1,sep="")),color="black",nudge_x = x_pos_b[1],nudge_y = padding_y)#,check_overlap = TRUE)
      movAvgPlot <- movAvgPlot + geom_text(data=mn[1,], aes(daymin,mn[[1,"grp.mean"]],label = paste("Mn",letter1,sep="")),color="black",nudge_x = x_pos_b[2],nudge_y = padding_y)#,check_overlap = TRUE)
      movAvgPlot <- movAvgPlot + geom_text(data=q90[1,], aes(daymin,q90[[1,"grp.mean"]],label = paste("q90",letter1,sep="")),color="black",nudge_x = x_pos_b[3],nudge_y = padding_y)#,check_overlap = TRUE)
      movAvgPlot <- movAvgPlot + geom_text(data=q95[1,], aes(daymin,q95[[1,"grp.mean"]],label = paste("q95",letter1,sep="")),color="black",nudge_x = x_pos_b[4],nudge_y = padding_y)#,check_overlap = TRUE)
      
      movAvgPlot <- movAvgPlot + geom_text(data=q05[2,], aes(daymax,q05[[2,"grp.mean"]],label = paste("q05",letter2,sep="")),color="black",nudge_x = x_pos_a[1],nudge_y = padding_y)#,check_overlap = TRUE)
      movAvgPlot <- movAvgPlot + geom_text(data=mn[2,], aes(daymax,mn[[2,"grp.mean"]],label = paste("Mn",letter2,sep="")),color="black",nudge_x = x_pos_a[2],nudge_y = padding_y)#,check_overlap = TRUE)
      movAvgPlot <- movAvgPlot + geom_text(data=q90[2,], aes(daymax,q90[[2,"grp.mean"]],label = paste("q90",letter2,sep="")),color="black",nudge_x = x_pos_a[3],nudge_y = padding_y)#,check_overlap = TRUE)
      movAvgPlot <- movAvgPlot + geom_text(data=q95[2,], aes(daymax,q95[[2,"grp.mean"]],label = paste("q95",letter2,sep="")),color="black",nudge_x = x_pos_a[4],nudge_y = padding_y)#,check_overlap = TRUE)
      
      movAvgPlot <- movAvgPlot + coord_cartesian(clip = "off")
      
      if(i < 10)
      {
        num_i <- paste('0',i,sep="")
      }else
      {
        num_i <- i
      }
      imgName <- paste("mplot-",num_i,"-",site_id,"-P",1,".jpg",sep="")
      ggsave(path="temp",filename=imgName,width=9,height=5,units="in",dpi=150)
    },error = function(err) {
      
    }) # END tryCatch
  }
}



#b_cumFreqOrHistogramToDocx - Writes a ggplot CumFreq or Histogram Graph to a word document.
#Created by KLA on N/A.
#Last update by KLA on N/A.
#Parameters: doc (Word doc), df (dataframe assesment), df_b (dataframe baseline), df_m (dataframe middle), period (numeric),
#graphName (character), type (character), sDate (Date).
#Returns: Word doc.
#Status: tested.
b_cumFreqOrHistogramToDocx<- function(doc,df,df_b,df_m,period,graphName,site_id,type,sDate,fLabel,cTableList=list()){
  
  tryCatch({
    notitle=TRUE
    #percentiles = c(0, 0.05,0.10,0.25,0.5,0.75,0.90,0.95,1)
    percentiles = c(0.25,0.5,0.75,0.90,0.95)
    cTableListNames <- c()
    #assessment year
    df$n <- 1
    df <- setPlottingLevels(df,period,"",sDate)
    
    if(period != 1){
      tempVector <- unique(df$Period)
    }else{
      tempVector <- c(48, 49, 50, 51, 21, seq(1:47))
    }
    
    yearRange <- range(format.Date(df$Date,"%Y"),na.rm = TRUE)
    yearRange <- paste(yearRange[1],"-",yearRange[2]," (a)",sep="")
    df <- df[order(df[[4]]),]
    
    if(!is.null(df_b))#baseline year.
    {
      df_b$n <- 1
      df_b <- setPlottingLevels(df_b,period,"_b",sDate)
      #tempVector_b <- shiftVector(unique(df_b$Period),tempVector,"_b")
      b_yearRange <- range(format.Date(df_b$Date,"%Y"),na.rm = TRUE)
      b_yearRange <- paste(b_yearRange[1],"-",b_yearRange[2]," (b)",sep="")
      df_b <- df_b[order(df_b[[4]]),]
    }
    
    if(!is.null(df_m))#middle year.
    {
      df_m$n <- 1
      df_m <- setPlottingLevels(df_m,period,"_m",sDate)
      #tempVector_m <- shiftVector(unique(df_b$Period),tempVector,"_m")
      m_yearRange <- range(format.Date(df_m$Date,"%Y"),na.rm = TRUE)
      m_yearRange <- paste(m_yearRange[1],"-",m_yearRange[2]," (m)",sep="")
      df_m <- df_m[order(df_m[[4]]),]
    }
    
    #set the year range from startYear-endYear
    if(period == 4)
    {
      numPlots <- 1
    }else
    {
      numPlots <- length(tempVector)
    }
    
  },error = function(err) {
    stop("ERROR: b_cumFreqOrHistogramToDocx().")
  }) # END tryCatch
  
  for(i in 1:numPlots)
  {
    tryCatch({
      if(period == 1)
      {
        pasteInfo <- paste("Week #",tempVector[i],sep="")#weekly
      }else if (period == 2)
      {
        pasteInfo <- extendMonthName(tempVector[i])# months
      }else if (period == 3)
      {
        pasteInfo <- tempVector[i]#seasons
      }else
      {
        pasteInfo <- "Annual"
      }
      
      df_all <- data.frame()
      colorVector <- c()
      data_b <- NULL
      data_m <- NULL
      data <- NULL
      
      if(!is.null(df_b))
      {
        tryCatch({
          if(period != 4 )#baseline.
          {
            d_b <- df_b[df_b$Period == paste(tempVector[i],"_b",sep=""),]
          }else
          {
            d_b <- df_b
          }
          q_b=quantile(d_b[[4]],percentiles,na.rm = TRUE)
          data_b = data.frame(q_b, percentiles)
          #is.num <- sapply(data_b, is.numeric)
          #data_b[is.num] <- lapply(data_b[is.num], round, 2)
          d_b <- d_b[!is.na(d_b[[4]]),]
          
          db_sum <- sum(d_b$n)
          d_b$n <- cumsum(d_b$n)/db_sum
          colnames(d_b)[4] <- "VALUE"
          
          d_b$Year <- b_yearRange
          df_all <- rbind.data.frame(df_all,d_b)
          colorVector <- c(colorVector,"blue")
        },error = function(err) {
          #no data found for period.
          d_b <- NULL
        }) # END tryCatch
      }
      
      if(!is.null(df_m))
      {
        tryCatch({
          if(period != 4 )#middle.
          {
            d_m <- df_m[df_m$Period == paste(tempVector[i],"_m",sep=""),]
          }else
          {
            d_m <- df_m
          }
          
          if(period != 4 ){d_m <- df_m[df_m$Period == tempVector_m[i],]}#middle.
          q_m=quantile(d_m[[4]],percentiles,na.rm = TRUE)
          data_m = data.frame(q_m, percentiles)
          #is.num <- sapply(data_m, is.numeric)
          #data_m[is.num] <- lapply(data_m[is.num], round, 2)
          d_m <- d_m[!is.na(d_m[[4]]),]
          
          dm_sum <- sum(d_m$n)
          d_m$n <- cumsum(d_m$n)/dm_sum
          colnames(d_m)[4] <- "VALUE"
          d_m$Year <- m_yearRange
          df_all <- rbind.data.frame(df_all,d_m)
          colorVector <- c(colorVector,"green")
          
        },error = function(err) {
          #no data found for period.
          d_m <- NULL
        }) # END tryCatch
      }
      
      tryCatch({
        if(period != 4 )#assessment.
        {
          d <- df[df$Period == tempVector[i],]
        }else
        {
          d <- df
        }
        
        q=quantile(d[[4]],percentiles,na.rm = TRUE)
        data = data.frame(q, percentiles)
        #is.num <- sapply(data, is.numeric)
        #data[is.num] <- lapply(data[is.num], round, 2)
        d<-d[!is.na(d[[4]]),]
        
        d_sum <- sum(d$n)
        d$n <- cumsum(d$n)/d_sum
        colnames(d)[4] <- "VALUE"
        d$Year <- yearRange
        df_all <- rbind.data.frame(df_all,d)
        colorVector <- c(colorVector,"red")
      },error = function(err) {
        #no data found for period.
        d <- NULL 
      }) # END tryCatch
      
      #cumfreqplot = ggplot(d, aes_q(x=as.name(names(d)[4]), colour="plum", y=as.name(names(d)[6]))) + geom_point(aes(y = cumsum(n))) + labs(title=paste("Cum Freq for ",graphName,sep=""), y="Cumulative Frequency", x=paste("Salinity ",yearRange,sep="")) + geom_vline(data = data, aes(xintercept=q),linetype = "solid") + geom_text(data = data, mapping= aes(label= paste(q,"\nQ",percentiles), x=q, y= -Inf), hjust= .15, vjust= -.5, inherit.aes=F) + theme(plot.title = element_text(size = rel(1.5), face="bold"), axis.title = element_text(face="bold"), axis.line = element_line(colour = "black", size = .7, linetype = "solid"), legend.position="none", panel.background = element_rect(fill="NA"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_fill_brewer(palette="Paired") + scale_x_continuous(breaks=round(seq(datamin,datamax,(datamax-datamin)/5),2)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
      #print(cumfreqplot)
      #cumfreqplot = ggplot() + geom_point(data=d,aes(x=SALINITY_TOP,y = cumsum(n)),pch=21, size=5, colour="blue") + geom_polygon(data=d_b,mapping=aes_q(x=as.name(names(d_b)[4]), colour="plum", y=as.name(names(d_b)[5])),alpha=0) + labs(title=paste("Cum Freq for ",graphName,sep=""), y="Cumulative Frequency", x=paste("Salinity ",yearRange,sep="")) + geom_vline(data = data, aes(xintercept=q),linetype = "solid") + geom_text(data = data, mapping= aes(label= paste(q,"\nQ",percentiles), x=q, y= -Inf), hjust= .15, vjust= -.5, inherit.aes=F) + theme(plot.title = element_text(size = rel(1.5), face="bold"), axis.title = element_text(face="bold"), axis.line = element_line(colour = "black", size = .7, linetype = "solid"), legend.position="none", panel.background = element_rect(fill="NA"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_fill_brewer(palette="Paired")
      #ggplot() + geom_point(data=d_b,aes(x=SALINITY_TOP,y = n),colour="blue") + geom_point(data=d,aes(x=SALINITY_TOP,y = n),colour="red") + labs(title=paste("Cum Freq for ",graphName,sep=""), y="Cumulative Frequency", x=paste("Salinity ",yearRange,sep="")) + geom_vline(data = data, aes(xintercept=q),linetype = "solid")
      #nDf <- rbind(d_b,d)
      
      lv <- unique(df_all$Year)
      
      df_all$Year <- factor(df_all$Year, levels = lv,ordered = TRUE)
      
      #cumfreqplot = ggplot(nDf, aes(x=VALUE,y=n,color=Year)) + geom_point() + scale_color_manual(values = c("red", "blue"))  + geom_hline(data = data, aes(yintercept=percentiles),colour="black",linetype = "solid")+ labs(title=paste(pasteInfo,"\n Cumulative Frequency for ",graphName,sep=""), y="Cumulative Frequency", x="Salinity (ppt)") + scale_x_continuous(breaks=round(seq(datamin,datamax,(datamax-datamin)/5),2)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + geom_point(data=data,aes(x=q,y = percentiles),colour="black",size=7,shape=1)+geom_point(data=data_b,aes(x=q_b,y = percentiles),colour="black",size=7,shape=1)+theme(plot.title = element_text(hjust=0.5),axis.line = element_line(colour = "black", size = .7, linetype = "solid"))
      
      datamax = max(df_all["VALUE"],na.rm = TRUE)
      datamin = min(df_all["VALUE"],na.rm = TRUE)
      
      #datamax = max(c(d[[4]],d_b[[4]]),d_m[[4]],na.rm = TRUE)
      #datamin = min(c(d[[4]],d_b[[4]]),d_m[[4]],na.rm = TRUE)
      
      if(type == "cumfreq")
      {
        if(notitle)
        {
          plotTitle <- ""
        }else
        {
          plotTitle <- paste(pasteInfo,"\n Cumulative Frequency for ",graphName,sep="")
        }
        
        #breaks <- round(seq(datamin,datamax,(datamax-datamin)/9),2)
        increment <- GetIncrementInteger(datamax-datamin)
        breaks <- seq(0,datamax,increment)
        cumfreqplot = ggplot(df_all, aes(x=VALUE,y=n,color=Year)) + geom_point() + scale_color_manual(values = colorVector)  + geom_hline(data = data, aes(yintercept=percentiles),colour="black",linetype = "solid")+ labs(color="Period",title=plotTitle, y="Cumulative Frequency", x="Salinity (ppt)") + scale_x_continuous(breaks=breaks) + scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+theme_light()+theme(plot.title = element_text(hjust=0.5),legend.position = "bottom")
        
        # if(type == "img")
        # {
        my_text <- pot(paste(fLabel[1],".",fLabel[2],".",fLabel[3],".f",as.numeric(fLabel[4])," - ",getPeriod(period)," Statistics for ",getSiteName(site_id,""),sep=""),textProperties(font.weight='bold', font.size = 12))
        fLabel[4] <- as.numeric(fLabel[4])+1
        if(i < 10)
        {
          num_i <- paste('0',i,sep="")
        }else
        {
          num_i <- i
        }
        imgName <- paste("cplot-",num_i,"-",site_id,"-P",fLabel[3],".jpg",sep="")
        
        ggsave(path="temp",filename=imgName,width=6,height=4,units="in",dpi=150)
        
        d_table <- data.frame(data[2])
        tNames <- c()
        
        if(!is.null(data_b))
        {
          d_table$data_b <- data_b[1]
          tNames <- c(tNames,b_yearRange)
        }
        if(!is.null(data_m))
        {
          d_table$data_m <- data_m[1]
          tNames <- c(tNames,m_yearRange)
        }
        d_table$data <- data[1]
        
        names(d_table) <- c("percentile",tNames,yearRange)
        d_table <- light.table(d_table)
        #d_table[,2] = textProperties(color = "#FF0000")
        #d_table[,3] = textProperties(color = "#0000FF")
        
        if(!is.null(data_b)){d_table[,b_yearRange,to='header'] = textProperties(color="#0000FF")}
        if(!is.null(data_m)){d_table[,m_yearRange,to='header'] = textProperties(color="#008000")}
        d_table[,yearRange,to='header'] = textProperties(color="#FF0000")
        #data <- setFlexTableWidths(data, c(.9,0.7,1.5,.5,.5,.6,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5))
        d_table <- setZebraStyle(d_table,odd = '#eeeeee', even = 'white')
        cTableList <- list.append(cTableList,d_table)
        cTableListNames <- c(cTableListNames,paste("cplot-",num_i,"-",site_id,"-P",fLabel[3],".jpg",sep=""))
      }
      
      if(substr(type,start=1,stop=nchar(type)-1) == "histogram")
      {
        if(substr(type,start=nchar(type),stop=nchar(type)) == 1)
        {
          histType <-"identity"
          alpha=.8
          binwidth = .5
        }else
        {
          histType <-"dodge"
          alpha=.8
          binwidth = .80
        }
        #w/ normal d curve
        
        if(notitle)
        {
          plotTitle <- ""
        }else
        {
          plotTitle <- paste(pasteInfo,"\n Histogram for ",graphName,"\n w/ Normal Distribution Curve",sep="")
        }
        
        increment <- GetIncrementInteger(datamax-datamin)
        breaks <- seq(0,datamax,increment)
        #breaks <- round(seq(datamin,datamax,(datamax-datamin)/9),2)
        
        histogram <- ggplot(df_all, aes(x=VALUE,fill=Year)) +
          geom_histogram(aes(y=..ncount..), alpha=alpha,position=histType,stat=,binwidth=binwidth)+
          scale_fill_manual(values = colorVector)+
          labs(fill="Period",title=plotTitle, y="Normalized Count", x="Salinity (ppt)")+
          theme_light()+theme(plot.title = element_text(hjust=0.5),legend.position = "bottom")+
          scale_x_continuous(breaks=breaks)
        
        if(exists("d"))
        {
          if(!is.null(d))
          {
            histogram <- histogram + stat_function(fun=dnorm,size=1,args=list(mean=mean(d$VALUE),sd=sd(d$VALUE)),colour="red")
          }
        }
        if(exists("d_m"))
        {
          if(!is.null(d_m))
          {
            histogram <- histogram + stat_function(fun=dnorm,size=1,args=list(mean=mean(d_m$VALUE),sd=sd(d_m$VALUE)),colour="#006400")
          }
        }
        if(exists("d_b"))
        {
          if(!is.null(d_b))
          {
            histogram <- histogram + stat_function(fun=dnorm,size=1,args=list(mean=mean(d_b$VALUE),sd=sd(d_b$VALUE)),colour="blue")
          }
        }
        if(i < 10)
        {
          num_i <- paste('0',i,sep="")
        }else
        {
          num_i <- i
        }
        imgName <- paste("hplot-",num_i,"-",site_id,"-P",fLabel[3],".jpg",sep="")
        ggsave(path="temp",filename=imgName,width=9,height=6,units="in",dpi=150)
        my_text <- pot(paste(fLabel[1],".",fLabel[2],".",fLabel[3],".f",as.numeric(fLabel[4])," - ",getPeriod(period)," Statistics for ",getSiteName(site_id,""),sep=""),textProperties(font.weight='bold', font.size = 12))
        fLabel[4] <- as.numeric(fLabel[4])+1
        
        if(notitle)
        {
          plotTitle <- ""
        }else
        {
          plotTitle <- paste(pasteInfo,"\n Histogram for ",graphName,"\n w/ Log(Normal Distribution) Curve",sep="")
        }
        
        increment <- GetIncrementInteger(datamax-datamin)
        breaks <- seq(0,datamax,increment)
        #breaks <- round(seq(datamin,datamax,(datamax-datamin)/9),2)
        
        histogram <- ggplot(df_all, aes(x=VALUE,fill=Year)) + 
          geom_histogram(aes(y=..ncount..), alpha=alpha,position=histType,stat=,binwidth=binwidth)+
          scale_fill_manual(values = colorVector)+
          labs(fill="Period",title=plotTitle, y="Normalized Count", x="Salinity (ppt)")+
          theme_light()+theme(plot.title = element_text(hjust=0.5),legend.position = "bottom")+
          scale_x_continuous(breaks=breaks)
        
        if(exists("d"))
        {
          if(!is.null(d))
          {
            histogram <- histogram + stat_function(fun=dnorm,size=1,args=list(mean=mean(log(d$VALUE)),sd=sd(log(d$VALUE))),colour="red")
          }
        }
        if(exists("d_m"))
        {
          if(!is.null(d_m))
          {
            histogram <- histogram + stat_function(fun=dnorm,size=1,args=list(mean=mean(log(d_m$VALUE)),sd=sd(log(d_m$VALUE))),colour="#006400")
          }
        }
        if(exists("d_b"))
        {
          if(!is.null(d_b))
          {
            histogram <- histogram + stat_function(fun=dnorm,size=1,args=list(mean=mean(log(d_b$VALUE)),sd=sd(log(d_b$VALUE))),colour="blue")
          }
        }
        
        if(i >= 10)
        {
          z <- ""
        }else
        {
          z <- "0"
        }
        
        imgName <- paste("hplot-",z,i,"L-",site_id,"-P",fLabel[3],".jpg",sep="")
        ggsave(path="temp",filename=imgName,width=9,height=6,units="in",dpi=150)
        my_text <- pot(paste(fLabel[1],".",fLabel[2],".",fLabel[3],".f",as.numeric(fLabel[4])," - ",getPeriod(period)," Statistics for ",getSiteName(site_id,""),sep=""),textProperties(font.weight='bold', font.size = 12))
        fLabel[4] <- as.numeric(fLabel[4])+1
      }
      
    },error = function(err) {
      
    }) # END tryCatch
  }
  if(type == "cumfreq")
  {
    names(cTableList) <- cTableListNames
    return (cTableList)
  }
  #return(doc)
}


#createStressColumn - Writes a Salinity Stress column from DAILY VALUES to a data frame.
#Created by KLA on 02/12/2019.
#Last update by KLA on 02/19/2019.
#Parameters: v (dataframe), type (character) [assessment,baseline or middle]
#graphName (character), type (character), sDate (Date).
#Returns: dataframe.
#Status: n tested.
createStressColumn<- function(v,type){
  # 
  # if(i%%2 != 0)
  # {#top or NA
  #   if(is.null(df_top))
  #   {
  
  v <- as.numeric(v)
  
  mn <- c(mean(na.omit(subset(v,v>=25))),
          mean(na.omit(subset(v,v < 25 & v >= 15))),
          mean(na.omit(subset(v,v < 15 & v >= 10))),
          mean(na.omit(subset(v,v < 10 & v >= 5))),
          mean(na.omit(subset(v,v < 5  & v >=  0))))
  
  mn <- replace(mn, is.nan(mn), 0)
  
  vlen <- length(subset(v,v>=0))
  
  freq <- c( as.numeric(length(subset(v,v>=25))),
             as.numeric(length(subset(v,25 > v & v >= 15))),
             as.numeric(length(subset(v,15 > v & v >= 10))),
             as.numeric(length(subset(v,10 > v & v >=  5))),
             as.numeric(length(subset(v, 5 > v & v >=  0))))
  
  if(type == "assessment")
  {
    cm <- mn
    
    # freqText <- c(paste(round((freq[1]/vlen)*100,2),"",sep=""),
    #           paste(round((freq[2]/vlen)*100,2),"",sep=""),
    #           paste(round((freq[3]/vlen)*100,2),"",sep=""),
    #           paste(round((freq[4]/vlen)*100,2),"",sep=""),
    #           paste(round((freq[5]/vlen)*100,2),"",sep=""))
    
    freqText <- c(round((freq[1]/vlen)*100,2),
                  round((freq[2]/vlen)*100,2),
                  round((freq[3]/vlen)*100,2),
                  round((freq[4]/vlen)*100,2),
                  round((freq[5]/vlen)*100,2))
    
    r <- data.frame(  c(freqText[1],
                        freqText[2],
                        freqText[3],
                        freqText[4],
                        freqText[5]))
  }else
  {
    ci_ <- c(BinomConfInterval(freq[1],vlen)["upr.ci"],
             BinomConfInterval(freq[2],vlen)["upr.ci"],
             BinomConfInterval(freq[3],vlen)["upr.ci"],
             BinomConfInterval(freq[4],vlen)["upr.ci"],
             BinomConfInterval(freq[5],vlen)["upr.ci"])
    
    ci_ <- replace(ci_, is.nan(ci_), 0)
    ci_ <- VectorRoundToUSGS(ci_)
    
    nd <- nchar(strsplit(as.character(max(ci_)), "\\.")[[1]][2])
    
    if(is.na(nd))
    {
      nd <- 2
    }
    
    freq <- c(round((freq[1]/vlen)*100,nd),
              round((freq[2]/vlen)*100,nd),
              round((freq[3]/vlen)*100,nd),
              round((freq[4]/vlen)*100,nd),
              round((freq[5]/vlen)*100,nd))
    
    cm <- ci_ + freq
    
    # freqText <- c(paste(freq[1],"",sep=""),
    #           paste(freq[2],"",sep=""),
    #           paste(freq[3],"",sep=""),
    #           paste(freq[4],"",sep=""),
    #           paste(freq[5],"",sep=""))
    
    freqText <- c(freq[1],
                  freq[2],
                  freq[3],
                  freq[4],
                  freq[5])
    
    r <- data.frame(  c(freqText[1],
                        freqText[2],
                        freqText[3],
                        freqText[4],
                        freqText[5]),
                      
                      c(as.numeric(ci_[1]),
                        as.numeric(ci_[2]),
                        as.numeric(ci_[3]),
                        as.numeric(ci_[4]),
                        as.numeric(ci_[5])),
                      
                      c(as.numeric(cm[1]),
                        as.numeric(cm[2]),
                        as.numeric(cm[3]),
                        as.numeric(cm[4]),
                        as.numeric(cm[5])))
  }
  
  #is.num <- sapply(r, is.numeric)
  #r[is.num] <- lapply(r[is.num], round, 2)
  
  r <- data.frame(lapply(r, function(x) {
    gsub(NaN, 0, x)
  }))
  
  return(r)
}

#getStressCellColorCode - gets color code for salinity stress cell.
#Created by KLA on 02/20/2019.
#Last update by KLA on 02/20/2019.
#Parameters: x, y
#Returns: color code ex: "#FFFFFF"
#Status: n tested.
getStressCellColorCode<- function(r,c){
  
  c <- as.integer(c)
  r <- as.integer(r)
  
  if(r == 4)
  {
    if(c == 1)
    {#tomato
      return ("#FFD5D5")#less red
    }
    return("#FFA2A2")#more red
  }
  if(r == 5)
  {
    if(c == 1)
    {#yellow
      return ("#FFFF00")
    }
    if(c == 7)
    {#yellow
      return ("#FFFF00")
    }
    if(c >= 30 && c < 90)
    {#orange
      return ("#FFA500")
    }
    if(c >= 90)
    {#tomato
      return ("#FFD5D5")
    }
  }
  if(r == 6)
  {
    if(c == 1)
    {#yellowgreen
      return ("#9ACD32")
    }
    #yellow
    return ("#FFFF00")
    
  }
  if(r == 7)
  {
    if(c >= 60)
    {#yellow
      return ("#FFFF00")
    }else
    {#green
      return ("#32CD32")
    }
  }
  if(r == 8)
  {#green
    return ("#32CD32")#(#008000) <- green
  }
  return("FFFFFF")
}


GetDataFrameMovingAverage<-function(df,ma=7,site_id="",ReadDB=FALSE,col_name=NULL){
  
  if(class(df) != "data.frame")
  {
    df <- as.data.frame(df)
  }
  
  if(ReadDB)
  {
    r <- range(df[["Date"]])
    r <- as.Date(r)
    
    startDate1 = as.Date(df[["Date"]][1]) - days(ma+1)
    endDate1 = as.Date(df[["Date"]][1]) - days(1)
    
    startDate2 = as.Date(df[["Date"]][nrow(df)]) + days(1)
    endDate2 = as.Date(df[["Date"]][nrow(df)]) + days(ma+1)
    
    df <- rbind.data.frame(GetDataFromDatabase(c("USGS","NoaaSJR"),startDate1,endDate1,site_id,"DV"),df)
    df <- rbind.data.frame(df,GetDataFromDatabase(c("USGS","NoaaSJR"),startDate2,endDate2,site_id,"DV"))
    
    df <- DFMovingAverage(df,ma)
    df <- df[df[["Date"]] >= r[1] & df[["Date"]] <= r[2],]
    
  }else
  {
    df <- DFMovingAverage(df,ma)
  }
  
  if(!is.null(col_name))
  {
    return(df[[col_name]])
  }else
  {
    return(df)
  }
}

#salinityStressTable - Writes a Salinity Stress table from DAILY VALUES to a word document.
#Created by KLA on 01/10/2019.
#Last update by KLA on 02/21/2019.
#Parameters: doc (Word doc), df (dataframe assesment), df_b (dataframe baseline), df_m (dataframe middle), db (character)
#Returns: Word doc.
#Status: n tested.
salinityStressTable<- function(stn,df,df_b,df_m,graphName,site_id,db,fLabel){
  
  tryCatch({
    
    c <- c(1, 7, 30, 60, 90)
    df_all <- NULL
    dfallList <- list()
    coln <- c()
    avalD <- c()
    obs <- as.numeric(!is.null(df_b)) + as.numeric(!is.null(df_m)) + as.numeric(!is.null(df))
    avalP <- c()
    seqB <- NA
    seqA <- NA
    seqM <- NA
    
    if(db == "USGS")
    {
      df_top <- NULL
      df_bot <- NULL
      
      if(!is.null(df_b))
      {
        coln <- c(coln,"Freq (%)","UCi","Freq + UCi")
        avalD <- c(avalD,"Bl","Bl","Bl")
        colB <- 3
      }
      
      if(!is.null(df_m))
      {
        coln <- c(coln,"Freq (%)","UCi","Freq + UCi")
        avalD <- c(avalD,"Md","Md","Md")
        colM <- 3
      }
      
      if(!is.null(df))
      {
        #coln <- c(coln,"Freq","UCi","Mn")
        #avalD <- c(avalD,"As","As","As")
        
        coln <- c(coln,"Freq (%)")
        avalD <- c(avalD,"As")
        colA <- 1
      }
      coln <- rep(coln,length(c))
      
      #coln <- c(coln,coln,coln,coln)
      #avalD <- c(avalD,avalD,avalD,avalD)
      
      for(i in 1:(length(c)))
      {
        avalP <- c(avalP,rep(c[i],colB),rep(c[i],colA))
        
        # j <- 1
        # while(j <= obs*3)
        # {
        #   avalP <- c(avalP,c[i])
        #   j <- j + 1
        # }
      }
      
      for(j in 1:(length(c)))
      {
        if(!is.null(df_b))
        {
          v_b <- GetDataFrameMovingAverage(df_b,c[j],site_id,FALSE,4)
          
          if(is.null(df_all))
          {
            df_all <- createStressColumn(v_b,"baseline")
          }else
          {
            df_all <- cbind.data.frame(df_all,createStressColumn(v_b,"baseline"))
          }
        }
        if(!is.null(df_m))
        {
          v_m <- rollmean(df_m[[4]], c[j])
          if(is.null(df_all))
          {
            df_all <- createStressColumn(v_m,"middle")
          }else
          {
            df_all <- cbind.data.frame(df_all,createStressColumn(v_m,"middle"))
          }
        }
        if(!is.null(df))
        {
          v <- GetDataFrameMovingAverage(df,c[j],site_id,TRUE,4)
          if(is.null(df_all))
          {
            df_all <- createStressColumn(v,"assessment")
          }else
          {
            df_all <- cbind.data.frame(df_all,createStressColumn(v,"assessment"))
          }
        }
      }
      
      bl_total_samples <- sum(is.finite(df_b[[4]]))
      ay_total_samples <- sum(is.finite(df[[4]]))
      
      # total_samples_row <- avalD
      # 
      # total_samples_row <- replace(total_samples_row, total_samples_row=="Bl", bl_total_samples)
      # total_samples_row <- replace(total_samples_row, total_samples_row=="As", ay_total_samples)
      
      new.df  <- insertRow(as.matrix(df_all),1,coln)
      new.df <- insertRow(new.df,1,avalD)
      df_all <- as.data.frame(insertRow(new.df,1,avalP))
      
      rownames(df_all) <- c(" ","  ","   ",
                            "sal >= 25",
                            "25 > sal >= 15",
                            "15 > sal >= 10",
                            "10 > sal >= 5",
                            "5 >  sal >= 0")
      
      dfallList <- list.append(dfallList,df_all)
      df_all <- NULL
    }
    
    tryCatch({
      for(i in 1:(length(dfallList)))
      {
        d <- dfallList[[i]]
        #options( "ReporteRs-fontsize" = 7 )
        d_table <- FlexTable(data=d,add.rownames = TRUE,header.columns=FALSE)
        
        for(j in 1:2)
        {
          end <- 1
          start <- 2
          
          if(j == 1)
          {
            colpp <- floor(d_table[["numcol"]]/length(c))
            len <- length(c)
            
            end <- end + colpp
            for(k in 1:(len))
            {
              d_table = spanFlexTableColumns( d_table,i=j, from = start, to=end )
              start <- start + colpp
              end <- end + colpp
            }
            
          }else
          {
            colpp <- colA + colB
            len <- floor(d_table[["numcol"]]/colpp)
            
            end <- end + colB
            for(k in 1:(len))
            {
              d_table = spanFlexTableColumns( d_table,i=j, from = start, to=end)
              
              #d_table = spanFlexTableColumns( d_table,i=j, from = start, to=end )
              
              start <- start + colpp
              end <- end + colpp
            }
          }
          
          
        }
        
        n <- 3 #Baseline column ID - 1.Freq / 2.UCi / 3.Freq+UCi
        freqBl <- seq(n,ncol(d),by=colB+colA)
        
        for(j in 1:(ncol(d)))#j - column number.
        {
          for(i in 1:(nrow(d)))#i - row number.
          {
            if(!(as.character(d[[j]][[i]]) %in% c("0","0%")) & i >= 4)
            {
              d_table <- setFlexTableBackgroundColors(d_table, i = i, j = j+1, colors = getStressCellColorCode(i,as.character(d[[j]][[1]])))
              
              tryCatch({
                
                if(j %in% freqBl)
                {
                  #Assessment Year Freq > Baseline Freq
                  if(as.double(gsub('%','',d[i,j+(colB+colA-n)])) > as.double(gsub('%','',d[i,j])))
                  {
                    d_table[i,j+1+(colB+colA-n)] <- textProperties(underlined = TRUE,font.weight = "bold")
                  }
                }
                
              },error = function(err) {
                stop("ERROR: UH-OH.")
              })
              
            }
          }
        }
        
        vc <- c()
        for(i in 1:(d_table[["numcol"]]))
        {
          if(i == 1)
          {
            vc <- c(vc,3)
          }else if(i%%3 == 0)
          {
            vc <- c(vc,1.5)
          }else
          {
            vc <- c(vc,.5)
          }
        }
        
        returnList <- list()
        returnList <- list.append(returnList,d_table)
        returnList <- list.append(returnList,c(paste("Total Baseline samples: ",bl_total_samples,sep=""),paste("Total Assessment Year samples: ",ay_total_samples,sep="")))
        
        return(returnList)
      }
      
    },error = function(err) {
      stop("ERROR: salinityStressTable().")
      
    }) # END tryCatch
    
  },error = function(err) {
    stop("ERROR: salinityStressTable().")
  }) # END tryCatch
  
  #return(doc)
}

#printAllSites - View all sites in Nassau, Clay, Duval and St. Johns County in Florida.
#Created by KLA on N/A.
#Last update by KLA on 12/31/2018.
#Parameters: none.
#Returns: nothing.
#Status: tested.
printAllSites<-function()
{
  r1 <- whatNWISsites(stateCd="FL",parameterCd=c("00010","00095","00480"),countyCd="031")#duval
  r2 <- whatNWISsites(stateCd="FL",parameterCd=c("00010","00095","00480"),countyCd="109")#st johns
  r3 <- whatNWISsites(stateCd="FL",parameterCd=c("00010","00095","00480"),countyCd="019")#duval
  r4 <- whatNWISsites(stateCd="FL",parameterCd=c("00010","00095","00480"),countyCd="035")#st johns
  r1 <- r1[,c("site_no","station_nm")]
  r2 <- r2[,c("site_no","station_nm")]
  r3 <- r3[,c("site_no","station_nm")]
  r4 <- r4[,c("site_no","station_nm")]
  r1 <- rbind.data.frame(r1,r2,r3,r4)
  rm(r2,r3,r4)
  View(r1)
}

#getAllSites - Return dataframe of site id and name of all sites in Nassau, Clay, Duval and St. Johns County in Florida.
#Created by KLA on N/A.
#Last update by KLA on 12/31/2018.
#Parameters: none.
#Returns: dataframe.
#Status: tested.
getAllSites<-function()
{
  r1 <- whatNWISsites(stateCd="FL",parameterCd=c("00010","00095","00480"),countyCd="031")#duval
  r2 <- whatNWISsites(stateCd="FL",parameterCd=c("00010","00095","00480"),countyCd="109")#st johns
  r3 <- whatNWISsites(stateCd="FL",parameterCd=c("00010","00095","00480"),countyCd="019")#duval
  r4 <- whatNWISsites(stateCd="FL",parameterCd=c("00010","00095","00480"),countyCd="035")#clay
  r1 <- r1[,c("site_no","station_nm")]
  r2 <- r2[,c("site_no","station_nm")]
  r3 <- r3[,c("site_no","station_nm")]
  r4 <- r4[,c("site_no","station_nm")]
  r1 <- rbind.data.frame(r1,r2,r3,r4)
  rm(r2,r3,r4)
  return(r1)
}

#viewSiteAvailableData - View Available data of a site.
#Created by KLA on N/A.
#Last update by KLA on 12/31/2018.
#Parameters: site (character).
#Returns: none.
#Status: tested.
viewSiteAvailableData<-function(site)
{
  View(whatNWISdata(siteNumber = site))
}

#FigureCounter - Will add how many figures a period creates to the figure counter.
#Created by KLA on 03/01/19.
#Last update by KLA on 03/01/2019.
#Parameters: t (numeric),p(numeric).
#Returns: numeric.
#Status: tested.
FigureCounter<-function(t,p)
{
  p <- floor(p)
  if(p == 1)#weekly
  {
    t <- t+52
  }else if (p == 2)#monthly
  {
    t <- t+12
  }else if(p == 3)#seasonal
  {
    t <- t+3
  }else#annual
  {
    t <- t+1
  }
  return (t)
}

#Created by KLA on 02/04/2020.
ReturnDocxTable <- function(doc,tablesList,i,asse,filterBy="",site="",t_counter=NA,appx_name="",stn=""){
  
  tryCatch({
    if(i %in% c(1,2))
    {
      if(i == 2 && site != "")
      {
        tablesList[[i]] <- FilterListByStringInName(tablesList[[i]],site)
      }
      
      for(j in 1:(length(tablesList[[i]])))#Per table.
      {
        doc <- addParagraph(doc, paste("Annual - T",t_counter,sep=""))
        t_counter <- t_counter + 1
        #options( "ReporteRs-fontsize" = 9 )
        
        #tablesList[[i]][[j]] <- DataFrameRoundToUSGS(tablesList[[i]][[j]])
        
        if(i == 1)
        {
          data <- FlexTable(tablesList[[i]][[j]],header.columns = FALSE)
          data <- setZebraStyle(data,odd = 'white', even = '#eeeeee')
        }else
        {
          data <- FlexTable(tablesList[[i]][[j]])
          data <- setZebraStyle(data,odd = '#eeeeee', even = 'white')
        }
        
        #data <- setFlexTableWidths(data, c(.9,0.7,1.5,.5,.5,.6,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5))
        doc <- addTable(doc,data)
        #options( "ReporteRs-fontsize" = 12 )
      }
      doc <- addPageBreak(doc)
    }
    
    if(i %in% c(3,4))
    {
      t_names <- list(T3="Seasonal - T",T4="Monthly - T",T6="Weekly - T")
      monthly_names <- c("MEAN (±SD)","90th / 95th Percentiles","Range (Min - Max)")
      
      if(i == 3)
      {
        if(filterBy == "Report")
        {
          remove = TRUE#Remove APPX
        }else
        {
          remove = FALSE#Keep Appx
        }
        for(j in 1:(length(tablesList[[i]])))
        {
          tablesList[[i]][[j]] <- FilterListByStringInName(tablesList[[i]][[j]],"APPX",remove)
        }
      }
      
      if(site != "")
      {
        tablesList[[i]] <- FilterListByStringInName(tablesList[[i]],site)
      }
      
      for(j in 1:(length(tablesList[[i]])))#Per Station
      {
        for(k in 1:(length(tablesList[[i]][[j]])))#Per Tables
        {
          if(i == 3)
          {
            if(filterBy != "APPX")
            {
              #Report Print.
              doc <- addParagraph(doc, paste(t_names[paste("T",i,sep="")],t_counter,sep=""))
            }else
            {
              #Appendix Print.
              doc <- addParagraph(doc, paste(appx_name,t_names[paste("T",i,sep="")],t_counter,sep=""))
            }
          }else
          {
            if(filterBy != "APPX")
            {
              #Report Print.
              doc <- addParagraph(doc, paste(monthly_names[k]," - T",t_counter,sep=""))
            }else
            {
              #Appendix Print.
              doc <- addParagraph(doc, paste(appx_name,monthly_names[k]," - T",t_counter,sep=""))
            }
          }
          t_counter <- t_counter + 1
          #options("ReporteRs-fontsize" = 9)
          
          #tablesList[[i]][[j]][[k]] <- DataFrameRoundToUSGS(tablesList[[i]][[j]][[k]])
          if(i == 3)
          {
            data <- FlexTable(tablesList[[i]][[j]][[k]],add.rownames = FALSE,header.columns=FALSE)
            data <- setZebraStyle(data,odd = 'white', even = '#eeeeee')
          }else
          {
            data <- FlexTable(tablesList[[i]][[j]][[k]],add.rownames = TRUE)
            data <- setZebraStyle(data,odd = '#eeeeee', even = 'white')
          }
          
          #data <- setFlexTableWidths(data, c(.9,0.7,1.5,.5,.5,.6,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5))
          doc <- addTable(doc, data)
          #options( "ReporteRs-fontsize" = 12 )
        }
      }
      doc <- addPageBreak(doc)
    }
    
    if(floor(i) == 5)
    {
      t_names <- list(T5.1="Number of weeks per year that the weekly average of salinity measurements exceeds the baseline average salinity - T",
                      T5.2="Assessment Year - Weekly (7-day moving average) Statistics - T")
      
      n <- 5
      
      for(j in 1:(length(tablesList[[n]])))#Per Tables
      {
        if(i == 5.1 && j == 1)
        {
          doc <- addParagraph(doc, paste(t_names[paste("T",i,sep="")],t_counter,sep=""))
          t_counter <- t_counter + 1
          #options( "ReporteRs-fontsize" = 9 )
          #tablesList[[n]][[j]] <- DataFrameRoundToUSGS(tablesList[[n]][[j]])
          data <- light.table(tablesList[[n]][[j]],add.rownames = TRUE)
          data <- setZebraStyle(data,odd = '#eeeeee', even = 'white')
          #data <- setFlexTableWidths(data, c(.9,0.7,1.5,.5,.5,.6,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5))
          doc <- addTable(doc, data)
          #options( "ReporteRs-fontsize" = 12 )
        }
        
        
        GetSeasonsPrint<-function(s,sMonth=12){
          s_offset <- 4 * s
          getMonthsFromSeason()
        }
        
        if(i != 5.1 && j == 2)
        {
          if(site != "")
          {
            tablesList[[n]][[j]] <- FilterListByStringInName(tablesList[[n]][[j]],site)
          }
          
          for(k in 1:(length(tablesList[[n]][[j]])))#Per station
          {
            tablesList[[n]][[j]][[k]] <- FilterListByStringInName(tablesList[[n]][[j]][[k]],paste("Y",length(asse),"-",sep=""))
            
            for(l in 1:(length(tablesList[[n]][[j]][[k]])))#Per table
            {
              doc <- addParagraph(doc, paste(t_names[paste("T",i,sep="")],t_counter,sep=""))
              t_counter <- t_counter + 1
              #options( "ReporteRs-fontsize" = 9 )
              #tablesList[[n]][[j]][[k]][[l]] <- DataFrameRoundToUSGS(tablesList[[n]][[j]][[k]][[l]])
              data <- light.table(tablesList[[n]][[j]][[k]][[l]],add.rownames = TRUE)
              data <- setZebraStyle(data,odd = '#eeeeee', even = 'white')
              #data <- setFlexTableWidths(data, c(.9,0.7,1.5,.5,.5,.6,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5))
              doc <- addTable(doc, data)
              #options( "ReporteRs-fontsize" = 12 )
            }
          }
        }
      }
      doc <- addPageBreak(doc)
    }
    
    if(i == 6)
    {
      t_names <- list(T3="Seasonal - T",T4="Monthly - T",T6="Weekly - T")
      monthly_names <- c("MEAN (±SD)","90th / 95th Percentiles","Range (Min - Max)")
      
      for(k in 1:(length(tablesList[[i]][[site]])))#Per Tables
      {
        if(filterBy != "APPX")
        {
          #Report Print.
          doc <- addParagraph(doc, paste(monthly_names[k]," - T",t_counter,sep=""))
        }else
        {
          #Appendix Print.
          doc <- addParagraph(doc, paste(appx_name,monthly_names[k]," - T",t_counter,sep=""))
        }
        
        t_counter <- t_counter + 1
        #options("ReporteRs-fontsize" = 9)
        data <- FlexTable(tablesList[[i]][[site]][[k]],add.rownames = TRUE)
        data <- setZebraStyle(data,odd = '#eeeeee', even = 'white')
        doc <- addTable(doc, data)
        #options( "ReporteRs-fontsize" = 12 )
      }
      
      doc <- addPageBreak(doc)
    }
    return(list(doc,t_counter))
    
  },error = function(err){
    stop(paste("WARNING: ReturnDocxTable(). Site ",site,".",sep=""))
  })
}

DFtoPlotFormat<-function(df){
  # rdf <- data.frame()
  # names_df <- toupper(names(df))
  # x <- toupper(c("Date","TEMP","CONDUC","SALIN"))
  # 
  # for(i in 1:(length(x)))
  # {
  #   rdf <- names_df[grepl(x[i],names_df,fixed=TRUE)]
  # }
  
  if(!is.na(match("site_no",names(df))))
  {
    return(df[-c(match("site_no",names(df)))]) 
  }
  
  return(df)
}