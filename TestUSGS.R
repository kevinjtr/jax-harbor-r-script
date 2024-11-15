rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
require("dataRetrieval")
require("bit64")
require("Rmisc")
require("xlsx")
require("officer")
require("flextable")
require("ggplot2")
require("DBI")
require("rlist")
require("RSQLite")
require("dplyr")
require("rnoaa")
require("data.table")
require("lubridate")
require("zoo")
require("miscTools")
require("DescTools")
source("JaxHarborChartNew.R", encoding="utf-8")
source("DatabasePopulate.R",keep.source = TRUE)
ClearTempFolder()
# require("officer")
# require("flextable")
# require("rvg")
# require("flextable")
# require("mschart")

siteList <- c(
              "302309081333001",#Lvl1
             # "02246500",#Lvl1
             # "301817081393600",#Lvl2
              #"301510081383500",#Lvl2
             # "301124081395901",#Lvl1 ###big baseline
              #"02245340",#Lvl1
             # "02245290",#Lvl2
             # "294213081345300",#Lvl2
             #-----------------------------------------
             # "302657081312400"#Lvl2
              #"02246804",#Lvl2
             # "02246751",#Lvl2
              #"302609081453300",#Lvl2
             # "02246518",#Lvl1
               "02246459"#Lvl2
             # "301204081434900",#Lvl1
              #"300803081354500"#Lvl2
             # "022462002"#Lvl2
            )

#Discarted:
#  "02246160"
#  "02246621"
#  "02246825"

assessment_years <- c(
  "2017-12-01","2018-12-01","2019-12-01","2020-12-01", "2021-12-01",
  "2022-12-01")
baseline_year <- c("2008-01-01","2017-11-30")

GenerateYearStatisticsDocx(filename="Report",site_id=siteList,assessment=assessment_years,baseline=baseline_year)

#download_assessment_years <- c("2017-12-01","2018-12-01","2019-12-01","2020-12-01", "2021-12-01", "2022-12-01")
#download_baseline_year <- c("2008-01-01","2017-11-30")

#DownloadAndPopulateDatabase(site_id=siteList,assessment=download_assessment_years,baseline=download_baseline_year)

#c("2008-01-01","2016-06-30")
viewSiteAvailableData('295856081372301')

printAllSites()

generateXlsxFile("Site Generated File.",site_id="301124081395901",parameterCd=c("00010","00095","00480"),
                 startDate="2017-12-01",endDate="2017-12-31")

############# Stations close to Requirement locations #############
###           Dames Point     - 302309081333001                 ###
###           Acosta Bridge   - 02246500                        ###
###           Buckman Bridge  - 301124081395901                 ###
###           Red Bay Point   - 295856081372301 (x) , 02245340  ###
###           Pottsburg Creek - 02246515 (X), 02246518          ###
###           Ortega River    - 301204081434900                 ###
###################################################################

#########################     Periods     #########################
###                   period = 4 - Year Stats.                  ###
###                   period = 3 - Seasonal Stats.              ###
###                   period = 2 - Monthly Stats.               ###
###                   period = 1 - Weekly Stats.                ###
###################################################################

##//[,-c(1,5,7,9,11,13,15,16)]
#View(readNWISuv("302309081333001",c("00010","00095","00300","00065","00480"),paste("2008-11-02","T00:00",sep=""),paste("2008-11-02","T23:59",sep=""),tz=Sys.timezone()))