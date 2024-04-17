#*********************************************************************************#
#************************ Table 1 - Balance Of Trade *****************************#
#********************************************************************************#

table1 <- function(statFrame){
  statFrame <- data.frame()
  impo <- dbGetQuery(mydb, "SELECT * FROM impo")

  
  #### Annual Import Summary - 1_BOT ####
  
  annualImport <- impo %>%
    group_by(Year) %>%
    summarise(OBS_VALUE = sum(cif))
  
  colnames(annualImport)[colnames(annualImport) == "Year"] <- "TIME_PERIOD"
  annualImport$FREQ  = "A"
  annualImport$TRADE_FLOW = "M"
  
  #Monthly Import Summary
  monthlyImport <- impo %>%
    group_by(yearMonth) %>%
    summarise(OBS_VALUE = sum(cif))
  
  dbWriteTable(mydb, "monthImport", monthlyImport, overwrite = TRUE )
  names(monthlyImport)[names(monthlyImport) == "yearMonth"] <- "TIME_PERIOD"
  monthlyImport$FREQ  = "M"
  monthlyImport$TRADE_FLOW = "M"
  
  imports <- rbind(annualImport, monthlyImport)
  
  # Annual Export Processing
  
  annualExport <- export %>%
    group_by(Year) %>%
    summarise(OBS_VALUE = sum(CIF))
  
  colnames(annualExport)[colnames(annualExport) == "Year"] <- "TIME_PERIOD"
  annualExport$FREQ  = "A"
  annualExport$TRADE_FLOW = "X"
  
  # Monthly Export Processing
  
  monthlyExport <- export %>%
    group_by(yearMonth) %>%
    summarise(OBS_VALUE = sum(CIF))
  
  colnames(monthlyExport)[colnames(monthlyExport) == "yearMonth"] <- "TIME_PERIOD"
  monthlyExport$FREQ  = "M"
  monthlyExport$TRADE_FLOW = "X"
  
  exports <- rbind(annualExport, monthlyExport)
  
  # Annual Trade Balabce
  tradeBalance <- merge(annualExport, annualImport, by = "TIME_PERIOD")
  tradeBalance$tradeBalance <- tradeBalance$OBS_VALUE.x - tradeBalance$OBS_VALUE.y 
  
  tradeBalance_df <- tradeBalance %>%
    select(TIME_PERIOD, tradeBalance) %>%
    group_by(TIME_PERIOD) %>%
    summarise(OBS_VALUE = sum(tradeBalance))
  
  tradeBalance_df$FREQ  = "A"
  tradeBalance_df$TRADE_FLOW = "TB"
  
  # Monthly Trade Balance
  monthTradeBalance <- merge(monthlyExport, monthlyImport, by = "TIME_PERIOD", all = TRUE)
  monthTradeBalance$OBS_VALUE.x[is.na(monthTradeBalance$OBS_VALUE.x)] <- 0
  monthTradeBalance$FREQ.x[is.na(monthTradeBalance$FREQ.x)] <- "M"
  monthTradeBalance$TRADE_FLOW.x[is.na(monthTradeBalance$TRADE_FLOW.x)] <- "X"
  
  monthTradeBalance$tradeBalance <- monthTradeBalance$OBS_VALUE.x - monthTradeBalance$OBS_VALUE.y
  
  monthTradeBalance_df <- monthTradeBalance %>%
    select(TIME_PERIOD, tradeBalance) %>%
    group_by(TIME_PERIOD) %>%
    summarise(OBS_VALUE = sum(tradeBalance))
  
  monthTradeBalance_df$FREQ  = "M"
  monthTradeBalance_df$TRADE_FLOW = "TB"
  
  
  balance <- rbind(tradeBalance_df, monthTradeBalance_df)
  
  statFrame <- rbind(imports, exports, balance)
  
  #Add the rest of the columns to the dataframe
  statFrame$GEO_PICT = "TV"
  statFrame$INDICATOR = "AMT"
  statFrame$COMMODITY = "_T"
  statFrame$COUNTERPART = "_T"
  statFrame$TRANSPORT = "_T"
  statFrame$CURRENCY = "DOM"
  statFrame$UNIT_MEASURE ="AUD"
  statFrame$UNIT_MULT = 3
  statFrame$OBS_STATUS = ""
  statFrame$DATA_SOURCE = ""
  statFrame$OBS_COMMENT = ""
  
  return(statFrame)
  
}



