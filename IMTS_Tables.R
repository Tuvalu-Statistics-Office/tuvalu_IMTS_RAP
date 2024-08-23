#------------------------------------------------------------------------------
# This script produces IMTS tables for Tuvalu.
#------------------------------------------------------------------------------

#Referencing the setup source file
source("setup.R")

#Establish connection to SQLite database
mydb <- dbConnect(RSQLite::SQLite(), "data/imts.db")

#Declare variables to pass year and month for processing
#Staff to specify year and month for tables
startYear = 2024
endYear = 2024

startMonth = 1
endMonth = 6

#Default table Styling

tableTheme <- list(
  fontName="Helvetica, arial",
  fontSize="0.75em",
  headerBackgroundColor = "#800000",
  headerColor = "#FFF8DC",
  cellBackgroundColor="#FFF8DC",
  cellColor="#800000",
  outlineCellBackgroundColor = "#800000",
  outlineCellColor = "#000000",
  totalBackgroundColor = "#FFF8DC",
  totalColor="#800000",
  totalStyle = "bold",
  borderColor = "#457c4b"
)

wb <- createWorkbook(creator = Sys.getenv("USERNAME"))

#------------------------------------------------------------------------------
# Preparing table that contains both import and export
#------------------------------------------------------------------------------
imp1 <- dbGetQuery(mydb, "SELECT Office, sadDate AS Date, sadNo AS SAD, sadModel AS Type, Chapter, cif AS mcif, 
                   coeSelected AS Country, regionSelected AS Region, Month, Year
                   FROM impo")
imp1$Type[imp1$Type=="IM 4"] <- "Import"
imp1$xcif <- 0
dbWriteTable(mydb, "imp1", imp1, overwrite = TRUE)

exp1 <- dbGetQuery(mydb, "SELECT Office, `SAD Date` AS Date, `SAD #` AS SAD, `SAD Model` AS Type, Chapter, mcif,
                   Country,regionSelected AS Region, Month, Year, cif AS xcif
                   FROM export")
dbWriteTable(mydb, "exp1", exp1, overwrite = TRUE)

#Append import and exports
trade <- dbGetQuery(mydb, "SELECT * FROM imp1 UNION ALL SELECT * FROM exp1")
dbWriteTable(mydb, "trade", trade, overwrite = TRUE)

# Important note: ASO to change the year and month
#------------------------------------------------------------------------------
# Table T1 - Balance of Trade
#------------------------------------------------------------------------------
tab1 <- dbGetQuery(mydb, "SELECT Year, Month, sum(mcif) AS Import, sum(xcif) As Export
                          FROM trade
                          GROUP BY Year, Month")

#Apply filter to get the required records
tab1 <- tab1 |>
  filter((Year >= startYear & Year <= endYear) & (Month >= startMonth & Month <= endMonth))

tab1$tradeBal <- tab1$Export - tab1$Import
pt <- PivotTable$new()
pt$addData(tab1)
pt$addRowDataGroups("Year")
pt$addRowDataGroups("Month")
pt$defineCalculation(calculationName="Exports", summariseExpression="format(round(sum(Export), 0), big.mark = ',')")
pt$defineCalculation(calculationName="Imports", summariseExpression="format(round(sum(Import), 0), big.mark = ',')")
pt$defineCalculation(calculationName="Balance", summariseExpression="format(round(sum(tradeBal), 0), big.mark = ',')")
pt$theme <- tableTheme
pt$renderPivot()

addWorksheet(wb, "T1")
pt$writeToExcelWorksheet(wb=wb, wsName="T1", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
#------------------------------------------------------------------------------
# Table T2 - Total imports
#------------------------------------------------------------------------------
tab2 <- dbGetQuery(mydb, "SELECT Year, Month, sum(cif) AS Value, hsGroup AS Chapter, hsDescription AS Desc, hsGroupNum AS Num
                   FROM impo
                   GROUP BY Year, Month, Chapter")

#Apply filter to get the required records
tab2 <- tab2 |>
  filter((Year >= startYear & Year <= endYear) & (Month >= startMonth & Month <= endMonth))

tab2$fullChpt <- paste0(tab2$Chapter,"-",tab2$Desc)

pt <- PivotTable$new()
pt$addData(tab2)
pt$addColumnDataGroups("Num")
pt$addColumnDataGroups("fullChpt", addTotal = FALSE)
pt$addRowDataGroups("Year")
pt$addRowDataGroups("Month")
pt$defineCalculation(calculationName="TotalImports", summariseExpression="format(round(sum(Value), 0), big.mark = ',')")
pt$theme <- tableTheme
pt$renderPivot()

addWorksheet(wb, "T2")
pt$writeToExcelWorksheet(wb=wb, wsName="T2", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#------------------------------------------------------------------------------
# Table T3 - Total exports
# There are 3 tables for exports:
#   T3A - Domestic exports (from Customs and Fisheries data)
#   T3B - Re-exports (from Energy data)
#   T3C - The sum of the domestic and re-exports
#   Table T3 only produces domestic export from Customs data
#------------------------------------------------------------------------------
tab3 <- dbGetQuery(mydb, "SELECT Year, Month, sum(cif) AS Value, hsGroup AS Chapter, hsDescription AS Desc, hsGroupNum AS Num
                   FROM export
                   GROUP BY Year, Month, Chapter")
tab3$fullChpt <- paste0(tab3$Chapter,"-",tab3$Desc)
#Need to uncomment the line above when there is export data

#Apply filter to get the required records
tab3 <- tab3 |>
  filter((Year >= startYear & Year <= endYear) & (Month >= startMonth & Month <= endMonth))

pt <- PivotTable$new()
pt$addData(tab3)
pt$addColumnDataGroups("Num")
pt$addColumnDataGroups("fullChpt",addTotal = FALSE) #change variable to 'fullChpt' when there is export data
pt$addRowDataGroups("Year")
pt$addRowDataGroups("Month")
pt$defineCalculation(calculationName="TotalExports", summariseExpression="format(round(sum(Value), 0), big.mark = ',')")
pt$theme <- tableTheme
pt$renderPivot()

addWorksheet(wb, "T3")
pt$writeToExcelWorksheet(wb=wb, wsName="T3", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#------------------------------------------------------------------------------
# Table T4 - Principle exports
#------------------------------------------------------------------------------
tab4 <- dbGetQuery(mydb,"SELECT princ_x_desc AS Commodity, Year, Month, sum(cif) AS Value
                   FROM export
                   GROUP BY Year, Month, princ_x_desc")

#Apply filter to get the required records
tab4 <- tab4 |>
  filter((Year >= startYear & Year <= endYear) & (Month >= startMonth & Month <= endMonth))


pt <- PivotTable$new()
pt$addData(tab4)
pt$addColumnDataGroups("Year")
pt$addColumnDataGroups("Month")
pt$addRowDataGroups("Commodity")
pt$defineCalculation(calculationName="Export", summariseExpression="format(round(sum(Value), 0), big.mark = ',')")
pt$theme <- tableTheme
pt$renderPivot()

addWorksheet(wb, "T4")
pt$writeToExcelWorksheet(wb=wb, wsName="T4", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#------------------------------------------------------------------------------
# Table T5 - Principle imports (taken directly from original script)
#------------------------------------------------------------------------------
tab5 <- dbGetQuery(mydb, "SELECT impo.Year,
                                      impo.Month,
                                      tblmonth.monthName,
                                      tblprinImports.PRINC_desc AS Commodity,
                                      sum(impo.CIF) AS Value
                               FROM impo
                               INNER JOIN tblprinImports ON impo.prinCode = tblprinImports.PRINC_IMP
                               INNER JOIN tblmonth ON impo.Month = tblmonth.Month
                               GROUP BY Year, impo.Month, monthName, Commodity
                               ORDER BY Year, impo.Month
                        ")
#Apply filter to get the required records
tab5 <- tab5 |>
  filter((Year >= startYear & Year <= endYear) & (Month >= startMonth & Month <= endMonth))


pt <- PivotTable$new()
pt$addData(tab5)
pt$addColumnDataGroups("Year")
pt$addColumnDataGroups("monthName")
pt$addRowDataGroups("Commodity")
pt$defineCalculation(calculationName="Import", summariseExpression="format(round(sum(Value), 0), big.mark = ',')")
pt$theme <- tableTheme
pt$renderPivot()

addWorksheet(wb, "T5")
pt$writeToExcelWorksheet(wb=wb, wsName="T5", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#------------------------------------------------------------------------------
# Table T6 - Balance of trade by country
#------------------------------------------------------------------------------
tab6 <- dbGetQuery(mydb,"SELECT Country, sum(mcif) AS Import, Month, Year, sum(xcif) AS Export, Type
                          FROM trade
                          GROUP BY Year, Month, Country")

#Apply filter to get the required records
tab6 <- tab6 |>
  filter((Year >= startYear & Year <= endYear) & (Month >= startMonth & Month <= endMonth))

tab6$Balance <- tab6$Export - tab6$Import

pt <- PivotTable$new()
pt$addData(tab6)
pt$addColumnDataGroups("Year")
pt$addColumnDataGroups("Month")
pt$addRowDataGroups("Country")
pt$defineCalculation(calculationName="Import", summariseExpression="format(round(sum(Import), 0), big.mark = ',')")
pt$defineCalculation(calculationName="Export", summariseExpression="format(round(sum(Export), 0), big.mark = ',')")
pt$defineCalculation(calculationName="Balance", summariseExpression="format(round(sum(Balance), 0), big.mark = ',')")
pt$addRowCalculationGroups()
pt$theme <- tableTheme
pt$renderPivot()

addWorksheet(wb, "T6")
pt$writeToExcelWorksheet(wb=wb, wsName="T6", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#------------------------------------------------------------------------------
# Table T7 - Balance of trade by country
#------------------------------------------------------------------------------
tab7 <- dbGetQuery(mydb,"SELECT Region, sum(mcif) AS Import, Month, Year, sum(xcif) AS Export, Type
                          FROM trade
                          GROUP BY Year, Month, Region")

#Apply filter to get the required records
tab7 <- tab7 |>
  filter((Year >= startYear & Year <= endYear) & (Month >= startMonth & Month <= endMonth))

tab7$Balance <- tab7$Export - tab7$Import

pt <- PivotTable$new()
pt$addData(tab7)
pt$addColumnDataGroups("Year")
pt$addColumnDataGroups("Month")
pt$addRowDataGroups("Region")
pt$defineCalculation(calculationName="Import", summariseExpression="format(round(sum(Import), 0), big.mark = ',')")
pt$defineCalculation(calculationName="Export", summariseExpression="format(round(sum(Export), 0), big.mark = ',')")
pt$defineCalculation(calculationName="Balance", summariseExpression="format(round(sum(Balance), 0), big.mark = ',')")
pt$addRowCalculationGroups()
pt$theme <- tableTheme
pt$renderPivot()

addWorksheet(wb, "T7")
pt$writeToExcelWorksheet(wb=wb, wsName="T7", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#------------------------------------------------------------------------------
# Table T7 - Balance of trade by country
#------------------------------------------------------------------------------
tab8 <- dbGetQuery(mydb,"SELECT Office, sum(mcif) AS Import, Month, Year, sum(xcif) AS Export, Type
                          FROM trade
                          GROUP BY Year, Month, Office")

#Apply filter to get the required records
tab8 <- tab8 |>
  filter((Year >= startYear & Year <= endYear) & (Month >= startMonth & Month <= endMonth))

tab8$Balance <- tab8$Export - tab8$Import
pt <- PivotTable$new()
pt$addData(tab8)
pt$addRowDataGroups("Year")
pt$addRowDataGroups("Month")
pt$addColumnDataGroups("Office")
pt$defineCalculation(calculationName="Import", summariseExpression="format(round(sum(Import), 0), big.mark = ',')")
pt$defineCalculation(calculationName="Export", summariseExpression="format(round(sum(Export), 0), big.mark = ',')")
pt$defineCalculation(calculationName="Balance", summariseExpression="format(round(sum(Balance), 0), big.mark = ',')")
pt$addRowCalculationGroups()
pt$theme <- tableTheme
pt$renderPivot()

addWorksheet(wb, "T8")
pt$writeToExcelWorksheet(wb=wb, wsName="T8", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#Save Final Excel Workbook to output folder
saveWorkbook(wb, file="output/IMTS_ReleaseTables.xlsx", overwrite = TRUE)
