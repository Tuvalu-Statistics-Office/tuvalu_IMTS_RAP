#### Calculating the total Imports and Exports ####
#Gathering data from SQLite database needed to compute trade balances 

importMonth <- dbGetQuery(mydb, "SELECT import.Year, import.Month, tblmonth.monthName AS importMonth, 
                                        sum(import.[Invoice Value]) AS importTotal 
                                 FROM import
                                 INNER JOIN tblmonth ON import.Month = tblMonth.month
                                 WHERE import.Year > 2000
                                 GROUP BY import.Year, import.Month
                          ")

#Export by Month Summary

exportMonth <- dbGetQuery(mydb, "SELECT export.Year, export.Month, tblmonth.monthName AS exportMonth, 
                                        sum(export.[Invoice Value]) AS exportTotal 
                                 FROM export
                                 INNER JOIN tblmonth ON export.Month = tblMonth.month
                                 WHERE export.Year > 2000
                                 GROUP BY export.Year, export.Month
                          ")

#Import Analysis
imports_analysis <- dbGetQuery(mydb, "SELECT import.Year,
                                               tblmonth.Month AS monthID,  
                                               tblmonth.monthName AS Month,
                                               SUM(import.CIF) AS totImport
                                        FROM import
                                        INNER JOIN tblmonth on import.Month = tblmonth.Month
                                        WHERE import.Year > 2000
                                        GROUP BY import.Year, monthID
                                        ORDER BY Year, MonthID
                                 ")


#Import by Month Summary
importExport <- function(importExport_tab){
  
  importExport <- merge(importMonth, exportMonth, by = c("Year", "Month"), all = TRUE)
  importExport$Balance <- importExport$exportTotal - importExport$importTotal
  
  impExpBal <- importExport %>%
    select(Year, importMonth, importTotal, exportTotal)
  colnames(impExpBal)[2] <- "Month"
  colnames(impExpBal)[3] <- "Import"
  colnames(impExpBal)[4] <- "Export"
  
  importExport_tab <- impExpBal
  
  #Return table back to the main document processing script
  return(importExport_tab)
  
}

#producing the ggplot of the Balance of Trade
importExport_GG <- function(imexGG){
  
  exp$type <- "Export"
  impexpGG <- rbind(imp, exp)
  
  impexpGG$monthName <- factor(impexpGG$monthName, levels = month.name)
  impexpGG$grp <- "month"
  imexGG <- ggplot(impexpGG, aes(x = monthName, y = Value, group=grp,  fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Column Graph Import", x = "Month", y = "Value(000)") +
    theme_minimal()
  
  return(imexGG)
  
  ggsave("image/importExport_GG.png", plot = imexGG, width = 6, height = 4, units = "in")
  
}

impexpBalance_yr <- function(balAnnual){
  
  #Annual Trade Balances
  impBal_yr <- imp %>%
    group_by(Year) %>%
    summarise(import = sum(Value))
  
  expBal_yr <- exp %>%
    group_by(Year) %>%
    summarise(export = sum(Value))
  
  
  balAnnual <- merge(impBal_yr, expBal_yr, by = "Year")
  balAnnual$balance <- balAnnual$export - balAnnual$import
  
  return(balAnnual)
  
}

monthBal_analysis <- function(analysis) {
  
  curmonth_fig <- tail(imports_analysis, 1)
  prevmonth_fig <- head(tail(imports_analysis, 2), 1)
  prevprevmonth_fig <- head(tail(imports_analysis, 3), 1)
  curPercentChange <- (curmonth_fig$totImport - prevmonth_fig$totImport)/prevmonth_fig$totImport * 100
  prevPercentChange <- (prevmonth_fig$totImport - prevprevmonth_fig$totImport)/prevprevmonth_fig$totImport * 100
  
  analysis <- paste0("Figure 1 illustrates the value of imports and exports as of ",curmonth_fig$Month, " ", curmonth_fig$Year,
                     ", and it shows; total imports in ", curmonth_fig$Month, " ", curmonth_fig$Year, " were $", format(round(curmonth_fig$totImport, 0), big.mark = ","),
                     " which is an approximate ", round(curPercentChange, 0), "% ", if (curPercentChange < 0){paste0("decrease")} else{paste0("increase")},
                     " from total imports in ", prevmonth_fig$Month, " ", prevmonth_fig$Year, " ($",format(round(prevmonth_fig$totImport, 0), big.mark = ","), "). There is also a slight ",
                     if (prevPercentChange < 0){paste0("decrease")} else {paste0("increase")}, " in total imports by approximately ", round(prevPercentChange, 0), "% from ",
                     prevprevmonth_fig$Month, " ", prevprevmonth_fig$Year, " to ", prevmonth_fig$Month, " ", prevmonth_fig$Year, "."
                     
  )
  
  return(analysis)
  
}