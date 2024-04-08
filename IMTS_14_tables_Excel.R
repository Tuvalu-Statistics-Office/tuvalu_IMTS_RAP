#Load libraries
library(pivottabler)
library(openxlsx)

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

#Create Excel workbook
wb <- createWorkbook(creator = Sys.getenv("USERNAME"))

#Load file which contains all the functions producing the 14 tables
source("functions/IMTS_14_Tables_Processing.R")

#Creating table1A - Trade Balance by Year
t1 <- table1A(table1A)
pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("Type")
pt$addRowDataGroups("Period")
pt$defineCalculation(calculationName="TotalImports", summariseExpression="format(round(sum(Import), 0), big.mark = ',')")
pt$theme <- tableTheme
pt$renderPivot()

#Writing final results to Excel worksheet
addWorksheet(wb, "Table1A")
pt$writeToExcelWorksheet(wb=wb, wsName="Table1A", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)


#Creating table1B - Trade Balance by Month
t2 <- table1B(table1B)
pt <- PivotTable$new()
pt$addData(t2)
pt$addColumnDataGroups("Type")
pt$addRowDataGroups("Period")
pt$defineCalculation(calculationName="TotalImports", summariseExpression="format(round(sum(Import), 0), big.mark = ',')")
pt$theme <- tableTheme
pt$renderPivot()

#Writing final results to Excel worksheet
addWorksheet(wb, "Table1B")
pt$writeToExcelWorksheet(wb=wb, wsName="Table1B", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)


#Creating table2A - Trade By HS BY Year
t3 <- table2A(table2A)
pt <- PivotTable$new()
pt$addData(t3)
pt$addColumnDataGroups("Year")
pt$addRowDataGroups("HSClass")
pt$defineCalculation(calculationName="TotalImports", summariseExpression="format(round(sum(Value), 0), big.mark = ',')")
pt$theme <- tableTheme
pt$renderPivot()

#Writing final results to Excel worksheet
addWorksheet(wb, "Table2A")
pt$writeToExcelWorksheet(wb=wb, wsName="Table2A", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)


#Creating table2A - Trade By HS BY Month
t4 <- table2B(table2B)
pt <- PivotTable$new()
pt$addData(t4)
pt$addColumnDataGroups("Year")
pt$addColumnDataGroups("monthName")
pt$addRowDataGroups("HSClass")
pt$defineCalculation(calculationName="TotalImports", summariseExpression="format(round(sum(Value), 0), big.mark = ',')")
pt$theme <- tableTheme
pt$renderPivot()

#Writing final results to Excel worksheet
addWorksheet(wb, "Table2B")
pt$writeToExcelWorksheet(wb=wb, wsName="Table2B", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#Creating table5A - Trade by principal Imports By Year
t5A <- table5A(table5A)
pt <- PivotTable$new(tableStyle=list("border-color"="maroon"),
                     headingStyle=list("color"="cornsilk", "background-color"="maroon", 
                                       "font-style"="italic", "border-color"="maroon"), 
                     cellStyle=list("color"="maroon", "background-color"="cornsilk", 
                                    "border-color"="maroon"),
                     totalStyle=list("color"="maroon", "background-color"="cornsilk", 
                                     "border-color"="maroon", "font-weight"="bold"))
pt$addData(t5A)
pt$addColumnDataGroups("Year")
pt$addRowDataGroups("Commodity")
pt$defineCalculation(calculationName="TotalImports", summariseExpression="format(round(sum(Value), 0), big.mark = ',')")
pt$renderPivot()

#Writing final results to Excel worksheet
addWorksheet(wb, "Table5A")
pt$writeToExcelWorksheet(wb=wb, wsName="Table5A", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)


#Creating table5A - Trade by principal Imports By Year
t5B <- table5B(table5B)
t5B$monthName <- factor(t5B$monthName, levels = month.name)
pt <- PivotTable$new(tableStyle=list("border-color"="maroon"),
                     headingStyle=list("color"="cornsilk", "background-color"="maroon", 
                                       "font-style"="italic", "border-color"="maroon"), 
                     cellStyle=list("color"="maroon", "background-color"="cornsilk", 
                                    "border-color"="maroon"),
                     totalStyle=list("color"="maroon", "background-color"="cornsilk", 
                                     "border-color"="maroon", "font-weight"="bold"))
pt$addData(t5B)
pt$addColumnDataGroups("Year")
pt$addColumnDataGroups("monthName")
pt$addRowDataGroups("Commodity")
pt$defineCalculation(calculationName="TotalImports", summariseExpression="format(round(sum(Value), 0), big.mark = ',')")
pt$renderPivot()

#Writing final results to Excel worksheet
addWorksheet(wb, "Table5B")
pt$writeToExcelWorksheet(wb=wb, wsName="Table5B", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)




#Trade by Partner Country by Year
t6 <- table6A(table6A)

pt <- PivotTable$new()
pt$addData(t6)
pt$addColumnDataGroups("Year")
pt$addRowDataGroups("country")
pt$defineCalculation(calculationName="TotalImports", summariseExpression="format(round(sum(Value), 0), big.mark = ',')")
pt$theme <- tableTheme
pt$renderPivot()

#Writing final results to Excel worksheet
addWorksheet(wb, "Table6A")
pt$writeToExcelWorksheet(wb=wb, wsName="Table6A", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)


#Trade by Partner Country by Month
t7 <- table6B(table6B)

pt <- PivotTable$new()
pt$addData(t7)
pt$addColumnDataGroups("Year")
pt$addColumnDataGroups("monthName")
pt$addRowDataGroups("country")
pt$defineCalculation(calculationName="TotalImports", summariseExpression="format(round(sum(Value), 0), big.mark = ',')")
pt$theme <- tableTheme
pt$renderPivot()

#Writing final results to Excel worksheet
addWorksheet(wb, "Table6B")
pt$writeToExcelWorksheet(wb=wb, wsName="Table6B", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)


#Trade by Region by Year
t8 <- table7A(table7A)

pt <- PivotTable$new()
pt$addData(t8)
pt$addColumnDataGroups("Year")
pt$addRowDataGroups("Region")
pt$defineCalculation(calculationName="TotalImports", summariseExpression="format(round(sum(Value), 0), big.mark=',')")
pt$theme <- tableTheme
pt$renderPivot()

#Writing final results to Excel worksheet
addWorksheet(wb, "Table7A")
pt$writeToExcelWorksheet(wb=wb, wsName="Table7A", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)


#Trade by Region by Month
t9 <- table7B(table7B)

pt <- PivotTable$new()
pt$addData(t9)
pt$addColumnDataGroups("Year")
pt$addColumnDataGroups("monthName")
pt$addRowDataGroups("Region")
pt$defineCalculation(calculationName="TotalImports", summariseExpression="format(round(sum(Value), 0), big.mark = ',')")
pt$theme <- tableTheme
pt$renderPivot()

#Writing final results to Excel worksheet
addWorksheet(wb, "Table7B")
pt$writeToExcelWorksheet(wb=wb, wsName="Table7B", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#Trade by mode of Transport by year

t10 <- table8A(table8A)

pt <- PivotTable$new()
pt$addData(t10)
pt$addColumnDataGroups("transportMode")
pt$addRowDataGroups("Period")
pt$defineCalculation(calculationName="TotalImports", summariseExpression="format(round(sum(Value), 0), big.mark = ',') ")
pt$theme <- tableTheme
pt$renderPivot()

#Writing final results to Excel worksheet
addWorksheet(wb, "Table8A")
pt$writeToExcelWorksheet(wb=wb, wsName="Table8A", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#Trade by mode of Transport by month

t11 <- table8B(table8B)

pt <- PivotTable$new()
pt$addData(t11)
pt$addColumnDataGroups("transportMode")
pt$addRowDataGroups("Period")
pt$defineCalculation(calculationName="TotalImports", summariseExpression="format(round(sum(Value), 0), big.mark = ',')")
pt$theme <- tableTheme
pt$renderPivot()

#Writing final results to Excel worksheet
addWorksheet(wb, "Table8B")
pt$writeToExcelWorksheet(wb=wb, wsName="Table8B", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#Save Final Excel Workbook to output folder
saveWorkbook(wb, file="output/IMTS_14tables_Releases.xlsx", overwrite = TRUE)
