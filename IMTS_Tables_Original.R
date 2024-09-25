#------------------------------------------------------------------------------
# This script produces the original IMTS tables for Tuvalu.
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

exp1 <- dbGetQuery(mydb, "SELECT Office,`SAD Date` AS Date, `SAD #` AS SAD, `SAD Model` AS Type, Chapter, mcif,
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
tab1 <- dbGetQuery(mydb, "SELECT Year, Month, Type, sum(mcif) AS Import, sum(xcif) As Export
                          FROM trade
                          GROUP BY Year, Month, Type")

#Apply filter to get the required records
tab1 <- tab1 |>
  filter((Year >= startYear & Year <= endYear) & (Month >= startMonth & Month <= endMonth) & (Type == "Export" | Type == "Import" | Type == "Re-export"))

tab1$tradeBal <- tab1$Export - tab1$Import
#Splitting totals for export and re-export
tab1$DomX <- ifelse(tab1$Type == "Export",tab1$Export,0)
tab1$ReX <- ifelse(tab1$Type == "Re-export",tab1$Export,0)

pt <- PivotTable$new()
pt$addData(tab1)
pt$addRowDataGroups("Year")
pt$addRowDataGroups("Month")
pt$defineCalculation(calculationName="Domestic", summariseExpression="format(round(sum(DomX), 0), big.mark = ',')")
pt$defineCalculation(calculationName="Re-Export", summariseExpression="format(round(sum(ReX), 0), big.mark = ',')")
pt$defineCalculation(calculationName="Total Export", summariseExpression="format(round(sum(Export), 0), big.mark = ',')")
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
#Table 3A
#------------------------------------------------------------------------------
tab3A <- dbGetQuery(mydb, "SELECT Year, Month, `SAD Model` AS Type, sum(cif) AS Value, hsGroup AS Chapter, hsDescription AS Desc, hsGroupNum AS Num
                   FROM export
                   GROUP BY Year, Month, Chapter")
tab3A$fullChpt <- paste0(tab3A$Chapter,"-",tab3A$Desc)
#Need to uncomment the line above when there is export data

#Apply filter to get the required records
tab3A <- tab3A |>
  filter((Year >= startYear & Year <= endYear) & (Month >= startMonth & Month <= endMonth) & Type == "Export")

pt <- PivotTable$new()
pt$addData(tab3A)
pt$addColumnDataGroups("Num")
pt$addColumnDataGroups("fullChpt",addTotal = FALSE) #change variable to 'fullChpt' when there is export data
pt$addRowDataGroups("Year")
pt$addRowDataGroups("Month")
pt$defineCalculation(calculationName="TotalExports", summariseExpression="format(round(sum(Value), 0), big.mark = ',')")
pt$theme <- tableTheme
pt$renderPivot()

addWorksheet(wb, "T3A")
pt$writeToExcelWorksheet(wb=wb, wsName="T3A", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
#------------------------------------------------------------------------------
#Table 3B
#------------------------------------------------------------------------------
tab3B <- dbGetQuery(mydb, "SELECT Year, Month, `SAD Model` AS Type, sum(cif) AS Value, hsGroup AS Chapter, hsDescription AS Desc, hsGroupNum AS Num
                   FROM export
                   GROUP BY Year, Month, Chapter")
tab3B$fullChpt <- paste0(tab3B$Chapter,"-",tab3B$Desc)
#Need to uncomment the line above when there is export data

#Apply filter to get the required records
tab3B <- tab3B |>
  filter((Year >= startYear & Year <= endYear) & (Month >= startMonth & Month <= endMonth) & Type == "Re-export")

pt <- PivotTable$new()
pt$addData(tab3B)
pt$addColumnDataGroups("Num")
pt$addColumnDataGroups("fullChpt",addTotal = FALSE) #change variable to 'fullChpt' when there is export data
pt$addRowDataGroups("Year")
pt$addRowDataGroups("Month")
pt$defineCalculation(calculationName="TotalExports", summariseExpression="format(round(sum(Value), 0), big.mark = ',')")
pt$theme <- tableTheme
pt$renderPivot()

addWorksheet(wb, "T3B")
pt$writeToExcelWorksheet(wb=wb, wsName="T3B", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
#------------------------------------------------------------------------------
#Table 3C
#------------------------------------------------------------------------------
tab3C <- dbGetQuery(mydb, "SELECT Year, Month, `SAD Model` AS Type, sum(cif) AS Value, hsGroup AS Chapter, hsDescription AS Desc, hsGroupNum AS Num
                   FROM export
                   GROUP BY Year, Month, Chapter")
tab3C$fullChpt <- paste0(tab3C$Chapter,"-",tab3C$Desc)
#Need to uncomment the line above when there is export data

#Apply filter to get the required records
tab3C <- tab3C |>
  filter((Year >= startYear & Year <= endYear) & (Month >= startMonth & Month <= endMonth)& Type == "Export" | Type == "Re-export")

pt <- PivotTable$new()
pt$addData(tab3C)
pt$addColumnDataGroups("Num")
pt$addColumnDataGroups("fullChpt",addTotal = FALSE) #change variable to 'fullChpt' when there is export data
pt$addRowDataGroups("Year")
pt$addRowDataGroups("Month")
pt$defineCalculation(calculationName="TotalExports", summariseExpression="format(round(sum(Value), 0), big.mark = ',')")
pt$theme <- tableTheme
pt$renderPivot()

addWorksheet(wb, "T3C")
pt$writeToExcelWorksheet(wb=wb, wsName="T3C", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
#------------------------------------------------------------------------------
# Table T4 - Principle exports
#------------------------------------------------------------------------------
tab4 <- dbGetQuery(mydb,"SELECT princ_x_desc AS Commodity, Year, Month, `SAD Model` AS Type, sum(cif) AS Value
                   FROM export
                   GROUP BY Year, Month, princ_x_desc")

#Apply filter to get the required records
tab4 <- tab4 |>
  filter((Year >= startYear & Year <= endYear) & (Month >= startMonth & Month <= endMonth) & Type == "Export" | Type == "Re-export")


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
# Table T5 - Principle imports
#------------------------------------------------------------------------------
tab5 <- dbGetQuery(mydb, "SELECT * FROM impo")
#Apply filter to get the required records
tab5 <- tab5 |>
  filter((Year >= startYear & Year <= endYear) & (Month >= startMonth & Month <= endMonth))


pt <- PivotTable$new()
pt$addData(tab5)
pt$addColumnDataGroups("Year")
pt$addColumnDataGroups("Month")
pt$addRowDataGroups("NUM", addTotal = FALSE)
pt$addRowDataGroups("prinSpecs", addTotal = FALSE)
pt$defineCalculation(calculationName="Import", summariseExpression="format(round(sum(cif), 0), big.mark = ',')")
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
                          GROUP BY Year, Month, Country, Type")

#Apply filter to get the required records
tab6 <- tab6 |>
  filter((Year >= startYear & Year <= endYear) & (Month >= startMonth & Month <= endMonth) & (Type == "Export" | Type == "Import" | Type == "Re-export"))

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
# Table T7 - Balance of trade by region
#------------------------------------------------------------------------------
tab7 <- dbGetQuery(mydb,"SELECT Region, sum(mcif) AS Import, Month, Year, sum(xcif) AS Export, Type
                          FROM trade
                          GROUP BY Year, Month, Region, Type")

#Apply filter to get the required records
tab7 <- tab7 |>
  filter((Year >= startYear & Year <= endYear) & (Month >= startMonth & Month <= endMonth) & (Type == "Export"  | Type == "Import"| Type == "Re-export"))

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
# Table T8 - Balance of trade by transport mode
#------------------------------------------------------------------------------
tab8 <- dbGetQuery(mydb,"SELECT Office, sum(mcif) AS Import, Month, Year, sum(xcif) AS Export, Type
                          FROM trade
                          GROUP BY Year, Month, Office, Type")

#Apply filter to get the required records
tab8 <- tab8 |>
  filter((Year >= startYear & Year <= endYear) & (Month >= startMonth & Month <= endMonth) & (Type == "Export"  | Type == "Import"| Type == "Re-export"))

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
saveWorkbook(wb, file="output/IMTS_ReleaseTables_Original.xlsx", overwrite = TRUE)
