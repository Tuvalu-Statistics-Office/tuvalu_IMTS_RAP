#Dynamic directory path mapping
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#Referencing the setup source file
source("setup.R")


#Connect to a MySQLite database which will be used for reporting
mydb <- dbConnect(RSQLite::SQLite(), "data/imts.db")

# Load the import excel file into the R working environment
#import <- read_excel("data/import.xlsx")
#export <- read_excel("data/export.xlsx")
#country <- read.csv("other/country.csv")
#countries <- read.csv("other/countries.csv")
#hsClass <- read.csv("other/importClassification.csv")
#principalImports <- read.csv("other/principalImports.csv")


#Reformatting HS2 column into having a width of 2 digit
#width <- 2
#hsClass$hs2Code <- sprintf(paste0('%0', hs2digits, 'd'), hsClass$hs2)
#import$HS2 <- as.numeric(import$HS2)
#import$hs2Code <- sprintf(paste0('%0', hs2digits, 'd'), import$HS2)
#colnames(export)[colnames(export) == "Chapter"] <- "HS2"
#export$hs2Code <- sprintf(paste0('%0', hs2digits, 'd'), export$HS2)

#Reformatting month to include zero infront of 1 digit numbers
#import$Month <- sprintf("%02d", import$Month)

impo <- dbGetQuery(mydb, "SELECT * FROM impo")
hsClass <- dbGetQuery(mydb, "SELECT * FROM hsClass")

curYear <- max(impo$Year)
curMonth <- impo %>%
              select(Year, Month) %>%
              arrange(desc(Year), desc(Month))
curMonth <- head(curMonth, 1)
curMonth <- curMonth$Month

#merge imports and exports with hs classes
#import_class <- merge(impo, hsClass, by = "hs2")
export_class <- merge(export, hsClass, by = "hs2")

# Define TIME_PERIOD for later use
impo$yearMonth <- paste(impo$Year, impo$Month, sep = "-")

# Reformat Export date to get proper date and define Year and Month of export
export$date <- as.Date(export$`SAD Date`)
export$Year <- as.integer(format(export$date, "%Y"))
export$Month <- as.integer(format(export$date, "%m"))

#Reformatting month to include zero infront of 1 digit numbers
export$Month <- sprintf("%02d", export$Month)

export$yearMonth <- paste(export$Year, export$Month, sep = "-")

#Write Import and Export tables into the sqlite database
dbWriteTable(mydb, "impo", import, overwrite = TRUE)
dbWriteTable(mydb, "export", export, overwrite = TRUE)
dbWriteTable(mydb, "hsclass", hsClass, overwrite = TRUE)
dbWriteTable(mydb, "tblprinImports", principalImports, overwrite = TRUE)
dbWriteTable(mydb, "tblcountry", country, overwrite = TRUE)

# Create month dataframe 
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
source("functions/table3.R")
source("functions/table4.R")
source("functions/table5.R")

table1 <- table1(statFrame)
table2 <- table2(statFrame)
table3 <- table3(statFrame)
table4 <- table4(statFrame)
table5 <- table5(statFrame)

#Merge all the extracted tables
final_data <- rbind(table1, table2, table3, table4, table5)

#Reordering the columns according to the .STAT column arrangement.
columnOrder <- c("FREQ", "TIME_PERIOD", "GEO_PICT", "INDICATOR", "TRADE_FLOW", "COMMODITY", "COUNTERPART", "TRANSPORT", "CURRENCY", "OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "OBS_STATUS", "DATA_SOURCE", "OBS_COMMENT")
final_data <- final_data[, columnOrder]

# Write final data to csv file
write.csv(final_data, "output/IMTS_DOT_STAT_TV.csv", row.names = FALSE)

#Close connection the SQLite database
dbDisconnect(mydb)