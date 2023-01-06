rm(list=ls())

#Add db library and make connection to SQLite database
library("DBI")
library("RSQLite")
library(XLConnect)
source("JaxHarborChart.R")

#FileConnect
#createFile("Jax Harbor Stats",".xlsx",df,"USGS_TIME_SERIES")
cloneFile()

#con<-dbConnect(RSQLite::SQLite(),"D:/usgs_20170623.sqlite")
con<-dbConnect(SQLite(), dbname="D:/R/Jax Harbor/Original/One site only/usgs_20180430.sqlite")

# List of sites and variable codes
usgsSites <- c(
  "2236125",
  "2244040",
  "2244440",
  "2245260",
  "2246000",
  "2246160",
  "2246318",
  "2246459",
  "2246500",
  "2246515",
  "2246518",
  "2246621",
  "2246751",
  "2246804",
  "2246825",
  "291830081362200",
  "294213081345300",
  "300803081354500",
  "301124081395901",
  "301204081434900",
  "301510081383500",
  "301817081393600",
  "302309081333001",
  "302609081453300",
  "302657081312400"
)

sjrwmdSites <- c(
  "301921081394701",
  "8720218",
  "301124081395901",
  "302309081333001",
  "294213081345300",
  "295856081372301"
)

noaaSites <- c(
  "8720218",#wind
  "8720219",
  "8720226",
  "8720245",
  "8720357",#wind
  "8720503",#wind
  "8720625"
)

var_codes <- c(
  "00065",
  "00060",
  "72137",
  "00010",
  "00095",
  "00480",
  "63160",
  "00300",
  "99988",
  "32319",
  "65231",
  "63680"
)
  
  var_codes <- c(
    "65",
    "60",
    "72137",
    "10",
    "95",
    "480",
    "63160",
    "00300",
    "99988",
    "32319",
    "65231",
    "63680"
)
  #USGS DB.
  period = c(2,0)
  year = 2017
  siteNum = 19
  #varcode = "00095"
  service = "'iv'"
  #varname = "Specific conductance, water, unfiltered, microsiemens per centimeter at 25&#176;C"
  
  #getUsgsStatistics<-function(con,site,service,period,year)
 getUsgsStatistics(con,usgsSites[siteNum],service,period,year) 
  
  
  
# dbListFields(con, "usgs_time_series")
# mydata <- dbGetQuery(con,"Select site, variable, value , observation_date from usgs_time_series where variable_code = 480 and
                      #observation_date like '1999% order by observation_date' ")

# summary(mydata)
#sql<-paste("SELECT * FROM usgs_time_series where 
#site='",site1,"' and variable_code='",varcode,"' and
#service_type='iv' and observation_date between '",year,"-01-01' and
#'",year,"-12-31'",sep="") (con,site1,varcode,varname,year)
#printMeanSTD(con,"3028","480","Salinity, water, unfiltered, parts per thousand","1999","monthly")

#SJRWMD DB.
#period = 2
#month = 1
#year = 1995
#week = 0
#season = 0
#siteNum = 19
#varcode = "00095"
#service = "'dv'"
#varname = "Specific conductance, water, unfiltered, microsiemens per centimeter at 25&#176;C"


 
 
 
# period = 3
# month = 0
# year = 1999
# week = 0
# season = 2
# siteNum = 16
# varcode = 480
# service = "'iv'"
# varname = "Salinity, water, unfiltered, parts per thousand"

 
 #getUsgsStatistics<-function(con,site,service,period,month,year,week,season)

 
# #USGS TEST.
#getUsgsStatistics(con,usgsSites[siteNum],varcode,varname,service,period,month,year, week,season)
#sql<-paste     ("SELECT site, variable, variable_code,units,service_type,strftime('%Y-%m-%d %H:%M', observation_date, 'localtime') as obs_date, strftime('%W',observation_date,'localtime') as week_number, qualifier,value FROM usgs_time_series where site='",usgsSites[siteNum],"' and variable_code='",varcode,"' and service_type=",service," and ",getObservationDateQuery(period,month,year,week,season),sep="")
#paste          ("SELECT site, variable, variable_code,units,service_type,strftime('%Y-%m-%d %H:%M', observation_date, 'localtime') as obs_date, strftime('%W',observation_date,'localtime') as week_number, qualifier,value FROM usgs_time_series where site='",usgsSites[siteNum],"' and variable_code='",varcode,"' and service_type=",service," and ",getObservationDateQuery(period,month,year,week,season),sep="")

#SJRWMD TEST.
# getSjrwmdStatistics(con,sjrwmdSites[siteNum],varcode,varname,service,period,month,year, week,season)
# sql<-paste     ("SELECT site, variable, method_id, units, service_type, strftime('%Y-%m-%d %H:%M', observation_date, 'localtime') as obs_date, strftime('%W',observation_date,'localtime') as week_number, value FROM sjrwmd_time_series_top250 where site='",sjrwmdSites[siteNum],"' and variable='",varname,"' and service_type=",service," and ",getObservationDateQuery(period,month,year,week,season),sep="")
# paste          ("SELECT site, variable, method_id, units, service_type, strftime('%Y-%m-%d %H:%M', observation_date, 'localtime') as obs_date, strftime('%W',observation_date,'localtime') as week_number, value FROM sjrwmd_time_series_top250 where site='",sjrwmdSites[siteNum],"' and variable='",varname,"' and service_type=",service," and ",getObservationDateQuery(period,month,year,week,season),sep="")


#NOAATIME TEST.
# getNoaaTimeStatistics(con,site,varcode,varname,"'v'",period,month,year,week,season)
# sql<-paste     ("SELECT site, variable, units, strftime('%Y-%m-%d %H:%M', observation_date, 'localtime') as obs_date, strftime('%W',observation_date,'localtime') as week_number, qualifier, value, sigma, f FROM noaa_time_series_top1000 where site='",noaaSites[siteNum],"' and variable='",varname,"' and qualifier=","'v'"," and ",getObservationDateQuery(period,month,year,week,season),sep="")
# paste          ("SELECT site, variable, units, strftime('%Y-%m-%d %H:%M', observation_date, 'localtime') as obs_date, strftime('%W',observation_date,'localtime') as week_number, qualifier, value, sigma, f FROM noaa_time_series_top1000 where site='",noaaSites[siteNum],"' and variable='",varname,"' and qualifier=","'v'"," and ",getObservationDateQuery(period,month,year,week,season),sep="")
#   
  
# # #NOAAWIND TEST.
# getNoaaWindStatistics(con,site,varcode,varname,0,period,month,year,week,season)
# sql<-paste     ("SELECT site, variable, units, strftime('%Y-%m-%d %H:%M', observation_date, 'localtime') as obs_date, strftime('%W',observation_date,'localtime') as week_number, degree, direction, speed, gust, f FROM noaa_wind_time_series_t1000 where site='","8720218","' and variable='",varname,"' and ",getObservationDateQuery(period,month,year,week,season),sep="")
# paste          ("SELECT site, variable, units, strftime('%Y-%m-%d %H:%M', observation_date, 'localtime') as obs_date, strftime('%W',observation_date,'localtime') as week_number, qualifier, value, sigma, f FROM noaa_wind_time_series_t1000 where site='","8720218","' and variable='",varname,"' and ",getObservationDateQuery(period,month,year,week,season),sep="")
# 
# 
#res<-dbSendQuery(con,sql)
#rows<-dbFetch(res)
#rows$CalcSalinity<-(6.859819E-38*(rows$value)^8 + -2.027297E-32*(rows$value)^7 + 2.576767E-27*(rows$value)^6 + -1.855032E-22*(rows$value)^5 + 8.450662E-18*(rows$value)^4 + -0.0000000000002616744*(rows$value)^3 + 0.000000007096135*(rows$value)^2 + 0.0005161311*(rows$value) + -0.05028803)

#class(rows)


#write.csv(rows, file = "MyData.csv",row.names=FALSE)
#print(getDescriptiveStatistics(con,"2236125","480","Salinity, water, unfiltered, parts per thousand","'iv'",period,month,year, week,season))
#res<-dbSendQuery(con,sql)
#rows<-dbFetch(res)
#d<-rows$value
#rows

#getObservationDateQuery(period, month, year)




# if(period == "weekly")
# {
#   sql<-paste("SELECT * FROM usgs_time_series where site='","3028","' and variable_code='","480","' and service_type='iv' and observation_date between '","1999","-01-01' and '","1999","-12-31'",sep="")
# }else
# {
#   if(period == "monthly")
#   {
   #sql<-paste("SELECT * FROM usgs_time_series where site='","3028","' and variable_code='","480","' and service_type='iv' and observation_date between '","1999","-02-01' and '","1999","-02-28'",sep="")
  # paste("SELECT * FROM usgs_time_series where site='","2236125","' and variable_code='","480","' and service_type='iv' and observation_date between '","1999","-02-01' and '","1999","-02-28'",sep="")
   #paste("SELECT * FROM usgs_time_series where site='","3028","' and variable_code='","480","' and service_type='iv' and observation_date between '","1999-", month, "-01' and '","1999-",month, "-", getmonthEndDay(month,year),"'",sep="")
#paste("SELECT * FROM usgs_time_series WHERE site='","3028","' AND variable_code='","480","' AND service_type='iv' AND observation_date BETWEEN ",getObservationDateQuery(period,month,year),sep="")
   #sql<-paste("SELECT * FROM usgs_time_series WHERE site='","2236125","' AND variable_code='","480","' AND service_type='iv' AND observation_date BETWEEN ",getObservationDateQuery(period,month,year),sep="")
   #paste("SELECT * FROM usgs_time_series WHERE site='","2236125","' AND variable_code='","480","' AND service_type='iv' AND observation_date BETWEEN ",getObservationDateQuery(period,month,year),sep="")

   #ql<-paste("SELECT site, variable, variable_code, option_code,method_id,units,service_type,strftime('%Y-%m-%d %H:%M', observation_date, 'localtime') as obs_date,qualifier,value FROM usgs_time_series WHERE site = 2236125 and variable_code = 480 and obs_date between '1999-02-01' and '1999-02-28'")
   #paste("SELECT site, variable, variable_code, option_code,method_id,units,service_type,strftime('%Y-%m-%d %H:%M', observation_date, 'localtime') as obs_date,qualifier,value FROM usgs_time_series WHERE site = 2236125 and variable_code = 480 and obs_date between '1999-02-01' and '1999-02-28'")
   
   #jhRunAnnualStats(con,"480")
   #jhChartSite(con,"2236125","1999-02-01","1999-02-28","480","iv")

  
  #paste("SELECT site, variable, variable_code, option_code,method_id,units,service_type,strftime('%Y-%m-%d %H:%M', observation_date, 'localtime') as obs_date,qualifier,value FROM usgs_time_series where site='","2236125","' and variable_code='","480","' and service_type=","'iv'"," and obs_date between ",getObservationDateQuery(period,month,year),sep="")
   #sql<-paste("SELECT site, variable, variable_code, option_code,method_id,units,service_type,strftime('%Y-%m-%d %H:%M', observation_date, 'localtime') as obs_date,qualifier,value FROM usgs_time_series where site='","2236125","' and variable_code='","480","' and service_type=","'iv'"," and obs_date between ",getObservationDateQuery(period,month,year),sep="")
   
   #sql<-paste("SELECT site, variable, variable_code, option_code,method_id,units,service_type,strftime('%Y-%m-%d %H:%M', observation_date, 'localtime') as obs_date,qualifier,value FROM usgs_time_series where site='","2236125","' and variable_code='","480","' and service_type=","'iv'"," and obs_date between ",getObservationDateQuery(period,month,year),sep="")
   #paste("SELECT site, variable, variable_code, option_code,method_id,units,service_type,strftime('%Y-%m-%d %H:%M', observation_date, 'localtime') as obs_date,qualifier,value FROM usgs_time_series where site='","2236125","' and variable_code='","480","' and service_type=","'iv'"," and obs_date between ",getObservationDateQuery(period,month,year),sep="")
   
 #res<-dbSendQuery(con,sql)
 # 
 #    rows<-dbFetch(res)
 #    d<-rows$value
 #    rows
 #    #site,varcode,year,num samples,mean,sd,min,q05,ci05,q10,ci10,q25,q50,q75,ci90,q90,ci95,q95,max
 #    out<-paste("MEAN: ", mean(d) ,'\n', " SD: ", sd(d))
 #    cat(out)
    
 
    
   
   #      sql<-paste("SELECT * FROM usgs_time_series where site='","3028","' and variable_code='","480","' and service_type='iv' and observation_date between '","1999-", month, "-01' and '","1999-",month, getmonthEndDay(month,1999),"'",sep="")

#   }else
#   {
#     if(period == "seasonal")
#     {
#       sql<-paste("SELECT * FROM usgs_time_series where site='","3028","' and variable_code='","480","' and service_type='iv' and observation_date between '","1999","-01-01' and '","1999","-12-31'",sep="")
#     }else#annual
#     {
#       sql<-paste("SELECT * FROM usgs_time_series where site='","3028","' and variable_code='","480","' and service_type='iv' and observation_date between '","1999","-01-01' and '","1999","-12-31'",sep="")
#     }
#   }
# }

# 
# res<-dbSendQuery(con,sql)
# rows<-dbFetch(res)
# d<-rows$value
# rows
# #site,varcode,year,num samples,mean,sd,min,q05,ci05,q10,ci10,q25,q50,q75,ci90,q90,ci95,q95,max
# out<-paste("Period: ",period, '\n', " MEAN: ", mean(d) ,'\n', "   SD: ", sd(d))
# cat(out)
dbDisconnect(con)
