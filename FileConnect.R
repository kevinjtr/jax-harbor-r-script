rm(list=ls())
# load XLConnect


#creating an Excel workbook. Both .xls and .xlsx file formats can be used.

#wb <- loadWorkbook('JaxStatTemplate.xlsx')






#createFile("Jax Harbor Stats",".xlsx")


#writing into sheets within an Excel workbook : 
#writing ChickWeight data frame into chickSheet


# xldf = readWorksheet(wb, sheet = getSheets(wb)[1])
# class(xldf)
# head(xldf)
# 
# # replace XL cells with new content
# for (id in 1:nrow(income_data)) {
#   colcount <- 1
#   for (nm in names(xldf)[1:3]){
#     xldf[id,nm] <- income_data[id,colcount]
#     colcount <- colcount + 1
#   }
# }
# 
# # save XL sheet and file
# sheet_name <- "Salaries"
# renameSheet(wb, sheet = getSheets(wb)[1], newName = sheet_name)
# writeWorksheet(wb,xldf,sheet=getSheets(wb)[1],startRow=2,header=F)
# saveWorkbook(wb,'income_data.xlsx')
# 
# # Case 2
# income_data <- data.frame('ID'=c(1,2,3), 'FirstName'=c('Joe','Mike','Liv'), 'LastName'=c('Smith','Steel','Storm'), 'Income'=c(100000,20000,80000), 'PhoneNumber'=c('888-888-1111','888-888-2222','888-888-3333'))
# income_data$FirstName <- as.character(income_data$FirstName)
# income_data$LastName <- as.character(income_data$LastName)
# income_data$PhoneNumber <- as.character(income_data$PhoneNumber)
# head(income_data)
# 
# library(XLConnect)
# wb <- loadWorkbook('sample2.xlsx')
# xldf = readWorksheet(wb, sheet = getSheets(wb)[1]) 
# for (id in 1:nrow(income_data)) {
#   colcount <- 1
#   for (nm in names(xldf)[1:5]){
#     xldf[id,nm] <- income_data[id,colcount]
#     colcount <- colcount + 1
#   }
# }
# 
# # save XL sheet and file
# sheet_name <- "Salaries"
# renameSheet(wb, sheet = getSheets(wb)[1], newName = sheet_name)
# writeWorksheet(wb,xldf,sheet=getSheets(wb)[1],startRow=2,header=F)
# saveWorkbook(wb,'income_data.xlsx')