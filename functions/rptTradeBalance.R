#### Calculating the Trade Balances ####

#Monthly Trade Balances
imp <- dbGetQuery(mydb, "SELECT impo.Year, tblmonth.Month, tblmonth.monthName AS monthName,
                                 'Import' AS type,
                                        round(sum(impo.[cif]/1000),0) AS Value 
                                 FROM impo
                                 INNER JOIN tblmonth ON impo.Month = tblMonth.month
                                 WHERE impo.Year > 2000
                                 GROUP BY impo.Year, tblmonth.Month
                          ")

exp <- dbGetQuery(mydb, "SELECT export.Year, tblmonth.Month, tblmonth.monthName AS monthName,
                                 'Export' AS type,
                                        round(sum(export.[cif]/1000),0) AS Value 
                                 FROM export
                                 INNER JOIN tblmonth ON export.Month = tblMonth.month
                                 WHERE export.Year > 2000
                                 GROUP BY export.Year, tblmonth.Month
                          ")

#Generating the Monthly Trade Balance table

monthlyBalance <- function(balMonthly){

  impBal_mh <- imp %>%
    group_by(Month, monthName) %>%
    arrange(Month) %>%
    summarise(import = sum(Value))
  
  expBal_mh <- exp %>%
    group_by(Month, monthName) %>%
    arrange(Month) %>%
    summarise(export = sum(Value))
  
  balMonthly <- merge(impBal_mh, expBal_mh, by = "Month", all = TRUE)
  balMonthly$export[is.na(balMonthly$export)] <- 0
  
  #Calculate Trade balance
  balMonthly$balance <- balMonthly$export - balMonthly$import
  balMonthly <- balMonthly %>%
    select(monthName.x, import, export, balance) %>%
    rename(Month = monthName.x)
  
  balMonthly$Month <- factor(balMonthly$Month, levels = month.name)
  
  return(balMonthly)
  
}

monthBal_GG <- function(imexGG_mh){
  
#Creating the GGPlot line graph for the monthly balance
  balMonthly$grp <- "month"
  imexGG_mh <- ggplot(balMonthly, aes(x = Month, y = balance, group = grp)) +
    geom_point(colour = "red", size = 5) +
    geom_line(colour = "red", linewidth = 2) +
    geom_text(aes(label = balance), vjust = -0.5) +
    labs(title = "Line graph of monthly trade balance", x = "Month", y = "Value(000)") +
    theme_minimal()
  
  ggsave("image/monthtradebal.png", plot = imexGG_mh, width = 6, height = 5, units = "in")
  
  return(imexGG_mh)
}





