#*********************************************************************************#
#************************ Table 3 - Export by HS *********************************#
#********************************************************************************#

table3 <- function(statFrame){
  statFrame <- data.frame()
  #### Import by Classifications 2_M #####
  impo <- dbGetQuery(mydb, "SELECT * FROM impo")
  export <- dbGetQuery(mydb, "SELECT * FROM export")
  
  # Annual Data preparation
  
  export_ann <- export %>%
    group_by(Year) %>%
    summarise(totExport = sum(cif))
  
   
  exportHS <- export %>%
    select(Year, Month, hs2Code, cif)
   
  exportHS_merge_class <- merge(exportHS, hsClass, by = "hs2Code")
  
  exportHS_merge_class_summary <- exportHS_merge_class %>%
    group_by(Year, Month, hsGroup, hsDescription) %>%
    summarise(total = sum(cif))
  
  # Annual Import by Commodity
  
  annualExportCommodity <- exportHS_merge_class_summary %>%
    group_by(Year, hsGroup) %>%
    summarise(OBS_VALUE = sum(total))
  
  colnames(annualExportCommodity)[colnames(annualExportCommodity) == "Year"] <- "TIME_PERIOD"
  annualExportCommodity$TIME_PERIOD <- as.character(annualExportCommodity$TIME_PERIOD)
  colnames(annualExportCommodity)[colnames(annualExportCommodity) == "hsGroup"] <- "COMMODITY"
  
  annualExportCommodity$FREQ  = "A"
  annualExportCommodity$TRADE_FLOW = "X"
  
  
  # Monthly data preparation
  
  exportYM <- export %>%
    select(yearMonth, hs2Code, cif) 
    
  
  exportHS_merge_class_YM <- merge(exportYM, hsClass, by = "hs2Code")
  
  monthExportCommodity <- exportHS_merge_class_YM %>%
    group_by(yearMonth, hsGroup) %>%
    summarise(OBS_VALUE = sum(cif))
  
  colnames(monthExportCommodity)[colnames(monthExportCommodity) == "yearMonth"] <- "TIME_PERIOD"
  colnames(monthExportCommodity)[colnames(monthExportCommodity) == "hsGroup"] <- "COMMODITY"
  
  monthExportCommodity$FREQ  = "M"
  monthExportCommodity$TRADE_FLOW = "X"
  
  statFrame <- rbind(annualExportCommodity, monthExportCommodity)
  
  #Add the rest of the columns to the final data frame
  statFrame$GEO_PICT = "TV"
  statFrame$INDICATOR = "AMT"
  statFrame$COUNTERPART = "_T"
  statFrame$TRANSPORT = "_T"
  statFrame$CURRENCY = "DOM"
  statFrame$UNIT_MEASURE ="AUD"
  statFrame$UNIT_MULT = 3
  statFrame$OBS_STATUS = ""
  statFrame$DATA_SOURCE = ""
  statFrame$OBS_COMMENT = ""
  
  #Re-ordering of the columns
  order <- c("FREQ", "TIME_PERIOD", "GEO_PICT", "INDICATOR", "TRADE_FLOW", "COMMODITY", "COUNTERPART", "TRANSPORT", "CURRENCY", "OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "OBS_STATUS", "DATA_SOURCE", "OBS_COMMENT")
  statFrame <- statFrame[, order]
  
  return(statFrame)
  
}
