#printMeanSTD - Prints the Mean and STD in a specific period defined in duration. (Periods: Weekly(1), monthly(2), seasonal(3), annual(4))
#               duration is a numeric.
#Created by KLA on 05/04/2018.
#Last update by KLA on 05/04/2018.
#Status: not tested.
#Returns: nothing.
# printMeanSTD<-function(con,site1,varcode,varname,year,duration){
#   sql<-paste("SELECT * FROM usgs_time_series where site='",site1,"' and variable_code='",varcode,"' and service_type='iv' and observation_date between '",year,"-01-01' and '",year,"-12-31'",sep="")
#   res<-dbSendQuery(con,sql)
#   rows<-dbFetch(res)
#   d<-rows$value
#   #site,varcode,year,num samples,mean,sd,min,q05,ci05,q10,ci10,q25,q50,q75,ci90,q90,ci95,q95,max
#   out<-paste("MEAN: ", mean(d) ,'\n', " SD: ", sd(d))
#   cat(out)
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

#getCOnfiguration - Gets the period in a character format.
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

#getObservationDateQuery - gets the observation date Query (MM-DD-YYYY)
#Compatible databases: USGS, SJRWMD and NOAA.
#Created by KLA on 05/04/2018.
#Last update by KLA on 05/08/2018.
#Parameters: period (numeric)
#Returns: character.
#Status: tested.
getObservationDateQuery<-function(period, month, year, week, season)
{
  # #verify month is a character format "MM"
  # if(!is.character(month))
  # {
  #   month <- paste("0",as.character(month),sep="")
  # }
  # else
  # {
  #   if(nchar(month) < 2)
  #     month <- paste("0",as.character(month),sep="")
  # }
  # #Verify year is a character format "YYYY"
  # if(!is.character(year))
  #   year <- as.character(year)
  
  endd <- getmonthEndDay(month,year)
  
  if(period == 1)     #Weekly MM-DD-YYYY [Finished].
  {
    return(paste("week_number ='",dateFormat(week,-1),"'",sep=""))
  }else
  {
    if(period == 2)   #Monthly MM-DD-YYYY [Finished.]
    {
      return(paste("obs_date between '",year,"-",dateFormat(month,0),"-","01","' AND '",
                   year,"-",dateFormat(month,0),"-",endd,"'",sep=""))
    }else
    {
      if(period == 3) #Seasonal MM-DD-YYYY [Finished.]
      {
        if(season == 1)
          return(paste("obs_date between '",dateFormat(year,-1),"-12-01","' AND '",year,"-03-31'",sep=""))
        if(season == 2)
          return(paste("obs_date between '",year,"-04-01","' AND '",year,"-07-31'",sep=""))
        
        return(paste("obs_date between '",year,"-08-01","' AND '",year,"-11-30'",sep=""))
      }
      else            #Annual MM-DD-YYYY [Finished.]
      {
        return(paste("obs_date between '",year,"-01-01' AND '",year,"-12-31'",sep=""))
      }
    }
  }
}

#getUsgsStatistics - build a table based of parameter configurations.
#Created by KLA on 05/07/2018.
#Last update by KLA on 05/09/2018.
#Parameters: con (sql),site,varcode,varname,service(character),period,month,year,week(numeric)
#Returns: 
#Status: in development.
getUsgsStatistics<-function(con,site,varcode,varname,service,period,month,year,week,season){
  
  sql<-paste("SELECT site, variable, variable_code, option_code,method_id,units,service_type,strftime('%Y-%m-%d',observation_date) as obs_date, strftime('%W',observation_date) as week_number, qualifier,value FROM usgs_time_series_limited where site='",site,"' and variable_code='",varcode,"' and service_type=",service," and ",getObservationDateQuery(period,month,year,week,season),sep="")
  paste     ("SELECT site, variable, variable_code, option_code,method_id,units,service_type,strftime('%Y-%m-%d',observation_date) as obs_date, strftime('%W',observation_date) as week_number, qualifier,value FROM usgs_time_series_limited where site='",site,"' and variable_code='",varcode,"' and service_type=",service," and ",getObservationDateQuery(period,month,year,week,season),sep="")
  
  res<-dbSendQuery(con,sql)
  rows<-dbFetch(res)
  d<-rows$value
  ci90<-ci(0.9,d)
  ci95<-ci(0.95,d)
  
  out<-paste("SQL DB : usgs_time_series",'\n',
             " Period: ",getPeriod(period)," (week=",week,",month=",month,")\t","      SD: ", sd(d),'\n',
             "   Year: ",as.character(year),'\t\t\t\t',"    MEAN: ",mean(d),'\n',
             "    Min: ", min(d),'\t\t\t\t',"     Max: ", max(d), '\n',
             " Length: ", length(d),'\t\t\t\t'," Service: ",service,'\n',
             "Varcode: ", varcode,'\t\t\t\t',"    Site: ", site, '\n', 
             "ci90[1]: ", ci90[1],'\t\t'," ci95[1]: ", ci95[1], '\n',
             "ci90[2]: ", ci90[2],'\t\t'," ci95[2]: ", ci95[2], '\n',
             "  Q .10: ", quantile(d,0.10),'\t\t\t',"    Quantile .25: ", quantile(d,0.25), '\n',
             "  Q .50: ", quantile(d,0.50),'\t\t\t',"    Quantile .75: ", quantile(d,0.75), '\n',
             "  Q .90: ", quantile(d,0.90),'\t\t\t',"    Quantile .95: ", quantile(d,0.95), '\n',
             "  Q .05: ", quantile(d,0.05),'\n',
             "Varname: ", varname, '\n',sep="")
  
  cat(out)
  #return(paste("SELECT site, variable, variable_code, option_code,method_id,units,service_type,strftime('%Y-%m-%d',observation_date) as obs_date, strftime('%W',observation_date) as week_number, qualifier,value FROM usgs_time_series where site='","2236125","' and variable_code='","480","' and service_type=","'iv'"," and ",getObservationDateQuery(period,month,year,week,season),sep=""))
}
# 
# #getSjrwmdStatistics - build a table based of parameter configurations for SJRWMD databse.
# #Created by KLA on 05/09/2018.
# #Last update by KLA on 05/09/2018.
# #Parameters: con (sql),site,varcode,varname,service(character),period,month,year,week(numeric)
# #Returns: 
# #Status: in development.
# getSjrwmdStatistics<-function(con,site,varcode,varname,service,period,month,year,week,season){
#   sql<-paste("SELECT site, variable, method_id, units, service_type, strftime('%Y-%m-%d',observation_date) as obs_date, strftime('%W',observation_date) as week_number, value FROM sjrwmd_time_series_top250 where site='",site,"' and variable='",varname,"' and service_type=",service," and ",getObservationDateQuery(period,month,year,week,season),sep="")
#   #sql<-paste("SELECT site, variable, variable_code, option_code,method_id,units,service_type,strftime('%Y-%m-%d',observation_date) as obs_date, strftime('%W',observation_date) as week_number, qualifier,value FROM usgs_time_series where site='",site,"' and variable_code='",varcode,"' and service_type=",service," and ",getObservationDateQuery(period,month,year,week,season),sep="")
#   paste     ("SELECT site, variable, method_id, units, service_type, strftime('%Y-%m-%d',observation_date) as obs_date, strftime('%W',observation_date) as week_number, value FROM sjrwmd_time_series_top250 where site='",site,"' and variable='",varname,"' and service_type=",service," and ",getObservationDateQuery(period,month,year,week,season),sep="")
#   
#   res<-dbSendQuery(con,sql)
#   rows<-dbFetch(res)
#   d<-rows$value
#   ci90<-ci(0.9,d)
#   ci95<-ci(0.95,d)
#   
#   out<-paste(" SQL DB: sjrwmd_time_series_top250",'\n',
#              " Period: ",getPeriod(period)," (week=",week,",month=",month,")\t","      SD: ", sd(d),'\n',
#              "   Year: ",as.character(year),'\t\t\t\t',"    MEAN: ",mean(d),'\n',
#              "    Min: ", min(d),'\t\t\t',"     Max: ", max(d), '\n',
#              " Length: ", length(d),'\t\t\t\t'," Service: ",service,'\n',
#              "Varcode: ", varcode,'\t\t\t\t',"    Site: ", site, '\n', 
#              "ci90[1]: ", ci90[1],'\t\t'," ci95[1]: ", ci95[1], '\n',
#              "ci90[2]: ", ci90[2],'\t\t'," ci95[2]: ", ci95[2], '\n',
#              "  Q .10: ", quantile(d,0.10),'\t\t',"    Quantile .25: ", quantile(d,0.25), '\n',
#              "  Q .50: ", quantile(d,0.50),'\t\t\t',"    Quantile .75: ", quantile(d,0.75), '\n',
#              "  Q .90: ", quantile(d,0.90),'\t\t\t',"    Quantile .95: ", quantile(d,0.95), '\n',
#              "  Q .05: ", quantile(d,0.05),'\n',
#              "Varname: ", varname, '\n',sep="")
#   
#   cat(out)
#   #return(paste("SELECT site, variable, variable_code, option_code,method_id,units,service_type,strftime('%Y-%m-%d',observation_date) as obs_date, strftime('%W',observation_date) as week_number, qualifier,value FROM usgs_time_series where site='","2236125","' and variable_code='","480","' and service_type=","'iv'"," and ",getObservationDateQuery(period,month,year,week,season),sep=""))
# }
# 
# #getNoaaTimeStatistics - build a table based of parameter configurations for Noaa_Wind databse.
# #Created by KLA on 05/09/2018.
# #Last update by KLA on 05/09/2018.
# #Parameters: con (sql),site,varcode,varname,qualifier(character),period,month,year,week(numeric)
# #Returns: 
# #Status: in development.
# getNoaaTimeStatistics<-function(con,site,varcode,varname,qualifier,period,month,year,week,season){
#   sql<-paste("SELECT site, variable, units, strftime('%Y-%m-%d',observation_date) as obs_date, strftime('%W',observation_date) as week_number, qualifier, value, sigma, f FROM noaa_time_series_top1000 where site='",site,"' and variable='",varname,"' and qualifier=",qualifier," and ",getObservationDateQuery(period,month,year,week,season),sep="")
#   #sql<-paste("SELECT site, variable, variable_code, option_code,method_id,units,service_type,strftime('%Y-%m-%d',observation_date) as obs_date, strftime('%W',observation_date) as week_number, qualifier,value FROM usgs_time_series where site='",site,"' and variable_code='",varcode,"' and service_type=",service," and ",getObservationDateQuery(period,month,year,week,season),sep="")
#   paste          ("SELECT site, variable, units, strftime('%Y-%m-%d',observation_date) as obs_date, strftime('%W',observation_date) as week_number, qualifier, value, sigma, f FROM noaa_time_series_top1000 where site='",site,"' and variable='",varname,"' and qualifier=",qualifier," and ",getObservationDateQuery(period,month,year,week,season),sep="")
#   
#   res<-dbSendQuery(con,sql)
#   rows<-dbFetch(res)
#   d<-rows$value
#   ci90<-ci(0.9,d)
#   ci95<-ci(0.95,d)
#   
#   out<-paste(" SQL DB: noaa_time_series_top1000",'\n',
#              " Period: ",getPeriod(period)," (week=",week,",month=",month,")\t","      SD: ", sd(d),'\n',
#              "   Year: ",as.character(year),'\t\t\t\t',"    MEAN: ",mean(d),'\n',
#              "    Min: ", min(d),'\t\t\t',"     Max: ", max(d), '\n',
#              " Length: ", length(d),'\t\t\t\t'," Service: ",service,'\n',
#              "Varcode: ", varcode,'\t\t\t\t',"    Site: ", site, '\n', 
#              "ci90[1]: ", ci90[1],'\t\t'," ci95[1]: ", ci95[1], '\n',
#              "ci90[2]: ", ci90[2],'\t\t'," ci95[2]: ", ci95[2], '\n',
#              "  Q .10: ", quantile(d,0.10),'\t\t',"    Quantile .25: ", quantile(d,0.25), '\n',
#              "  Q .50: ", quantile(d,0.50),'\t\t\t',"    Quantile .75: ", quantile(d,0.75), '\n',
#              "  Q .90: ", quantile(d,0.90),'\t\t\t',"    Quantile .95: ", quantile(d,0.95), '\n',
#              "  Q .05: ", quantile(d,0.05),'\n',
#              "Varname: ", varname, '\n',sep="")
#   
#   cat(out)
#   #return(paste("SELECT site, variable, variable_code, option_code,method_id,units,service_type,strftime('%Y-%m-%d',observation_date) as obs_date, strftime('%W',observation_date) as week_number, qualifier,value FROM usgs_time_series where site='","2236125","' and variable_code='","480","' and service_type=","'iv'"," and ",getObservationDateQuery(period,month,year,week,season),sep=""))
# }
# 
# #getNoaaWindStatistics - build a table based of parameter configurations for Noaa_Wind databse.
# #Created by KLA on 05/09/2018.
# #Last update by KLA on 05/09/2018.
# #Parameters: con (sql),site,varcode,varname,degree(numeric),period,month,year,week(numeric)
# #Returns: 
# #Status: in development.
# getNoaaWindStatistics<-function(con,site,varcode,varname,degree,period,month,year,week,season){
#   sql<-paste("SELECT site, variable, units, strftime('%Y-%m-%d',observation_date) as obs_date, strftime('%W',observation_date) as week_number, degree, direction, speed, gust, f FROM noaa_wind__time_series_t1000 where site='",site,"' and variable='",varname,"' and ",getObservationDateQuery(period,month,year,week,season),sep="")
#   #sql<-paste("SELECT site, variable, variable_code, option_code,method_id,units,service_type,strftime('%Y-%m-%d',observation_date) as obs_date, strftime('%W',observation_date) as week_number, qualifier,value FROM usgs_time_series where site='",site,"' and variable_code='",varcode,"' and service_type=",service," and ",getObservationDateQuery(period,month,year,week,season),sep="")
#   paste     ("SELECT site, variable, units, strftime('%Y-%m-%d',observation_date) as obs_date, strftime('%W',observation_date) as week_number, qualifier, value, sigma, f FROM noaa_wind__time_series_t1000 where site='",site,"' and variable='",varname,"' and ",getObservationDateQuery(period,month,year,week,season),sep="")
#   
#   res<-dbSendQuery(con,sql)
#   rows<-dbFetch(res)
#   d<-rows$value
#   ci90<-ci(0.9,d)
#   ci95<-ci(0.95,d)
#   
#   out<-paste(" SQL DB: noaa_wind__time_series_t1000",'\n',
#              " Period: ",getPeriod(period)," (week=",week,",month=",month,")\t","      SD: ", sd(d),'\n',
#              "   Year: ",as.character(year),'\t\t\t\t',"    MEAN: ",mean(d),'\n',
#              "    Min: ", min(d),'\t\t\t',"     Max: ", max(d), '\n',
#              " Length: ", length(d),'\t\t\t\t'," Service: ",service,'\n',
#              "Varcode: ", varcode,'\t\t\t\t',"    Site: ", site, '\n', 
#              "ci90[1]: ", ci90[1],'\t\t'," ci95[1]: ", ci95[1], '\n',
#              "ci90[2]: ", ci90[2],'\t\t'," ci95[2]: ", ci95[2], '\n',
#              "  Q .10: ", quantile(d,0.10),'\t\t',"    Quantile .25: ", quantile(d,0.25), '\n',
#              "  Q .50: ", quantile(d,0.50),'\t\t\t',"    Quantile .75: ", quantile(d,0.75), '\n',
#              "  Q .90: ", quantile(d,0.90),'\t\t\t',"    Quantile .95: ", quantile(d,0.95), '\n',
#              "  Q .05: ", quantile(d,0.05),'\n',
#              "Varname: ", varname, '\n',sep="")
#   
#   cat(out)
#   #return(paste("SELECT site, variable, variable_code, option_code,method_id,units,service_type,strftime('%Y-%m-%d',observation_date) as obs_date, strftime('%W',observation_date) as week_number, qualifier,value FROM usgs_time_series where site='","2236125","' and variable_code='","480","' and service_type=","'iv'"," and ",getObservationDateQuery(period,month,year,week,season),sep=""))
# }