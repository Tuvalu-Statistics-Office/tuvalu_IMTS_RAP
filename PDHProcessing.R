#Dynamic directory path mapping
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#Referencing the setup source file
source("setup.R")


#Connect to a MySQLite database which will be used for reporting
mydb <- dbConnect(RSQLite::SQLite(), "data/imts.db")

curYear <- max(impo$Year)
curMonth <- impo %>%
              select(Year, Month) %>%
              arrange(desc(Year), desc(Month))
curMonth <- head(curMonth, 1)
curMonth <- curMonth$Month

month <- data.frame(
  Month = c(1,2,3,4,5,6,7,8,9,10,11,12),
  monthCode = c("01","02","03","04","05","06","07","08","09","10","11","12"),
  monthName = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
  Quarter = c(1,1,1,2,2,2,3,3,3,4,4,4),
  quarterName = c("Quarter 1", "Quarter 1", "Quarter 1", "Quarter 2", "Quarter 2", "Quarter 2", "Quarter 3", "Quarter 3", "Quarter 3", "Quarter 4", "Quarter 4", "Quarter 4")
)

dbWriteTable(mydb, "tblmonth", month, overwrite = TRUE )

# Create Quarter dataframe
quarter <- data.frame(
  Quarter = c(1,2,3,4),
  quarterName = c("Quarter 1", "Quarter 2", "Quarter 3", "Quarter 4")
)

dbWriteTable(mydb, "tblquarter", quarter, overwrite = TRUE)

#Sourcing all the functions that are generating the tables
source("functions/table1.R")
source("functions/table2.R")
source("functions/table4.R")
source("functions/table5.R")

table1 <- table1(statFrame)
table2 <- table2(statFrame)
table4 <- table4(statFrame)
table5 <- table5(statFrame)

#Merge all the extracted tables
final_data <- rbind(table1, table2, table4, table5)

#Reordering the columns according to the .STAT column arrangement.
columnOrder <- c("FREQ", "TIME_PERIOD", "GEO_PICT", "INDICATOR", "TRADE_FLOW", "COMMODITY", "COUNTERPART", "TRANSPORT", "CURRENCY", "OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "OBS_STATUS", "DATA_SOURCE", "OBS_COMMENT")
final_data <- final_data[, columnOrder]

# Write final data to csv file
write.csv(final_data, "output/IMTS_DOT_STAT_TV.csv", row.names = FALSE)

#Close connection the SQLite database
dbDisconnect(mydb)