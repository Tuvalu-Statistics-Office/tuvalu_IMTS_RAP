#*********************************************************************************#
#************************ Table 2 - Import by HS *********************************#
#*********************************************************************************#

table2 <- function(statFrame){
  statFrame <- data.frame()
  #### Import by Classifications 2_M #####
  
  # Annual Data preparation
  
 importHS <- impo %>%
    select(Year, Month, hs2, cif)
  
  importHS_merge_class <- merge(importHS, hsClass, by = "hs2")
  
  importHS_merge_class_summary <- importHS_merge_class %>%
    group_by(Year, Month, hsGroup, hsDescription) %>%
    summarise(total = sum(cif))
  
  # Annual Import by Commodity
  
  annualImportCommodity <- importHS_merge_class_summary %>%
    group_by(Year, hsGroup) %>%
    summarise(OBS_VALUE = sum(total))
  
  colnames(annualImportCommodity)[colnames(annualImportCommodity) == "Year"] <- "TIME_PERIOD"
  annualImportCommodity$TIME_PERIOD <- as.character(annualImportCommodity$TIME_PERIOD)
  colnames(annualImportCommodity)[colnames(annualImportCommodity) == "hsGroup"] <- "COMMODITY"
  
  annualImportCommodity$FREQ  = "A"
  annualImportCommodity$TRADE_FLOW = "M"
  #statFrame <- rbind(statFrame, annualImportCommodity)
  
  
  # Monthly data preparation
  
  importYM <- impo %>%
    select(yearMonth, hs2, cif)
  
  importHS_merge_class_YM <- merge(importYM, hsClass, by = "hs2")
  
  monthImportCommodity <- importHS_merge_class_YM %>%
    group_by(yearMonth, hsGroup) %>%
    summarise(OBS_VALUE = sum(cif))
  
  colnames(monthImportCommodity)[colnames(monthImportCommodity) == "yearMonth"] <- "TIME_PERIOD"
  colnames(monthImportCommodity)[colnames(monthImportCommodity) == "hsGroup"] <- "COMMODITY"
  monthImportCommodity$FREQ  = "M"
  monthImportCommodity$TRADE_FLOW = "M"
  
  statFrame <- rbind(statFrame, monthImportCommodity)
  
  # Annual Export by HS processing
  
  exportHS <- export %>%
    select(Year, Month, HS2, CIF) %>%
    rename(hs2 = HS2,
           cif = CIF,
           )
  
  exportHS_merge_class <- merge(exportHS, hsClass, by = "hs2")
  
  exportHS_merge_class_summary <- exportHS_merge_class %>%
    group_by(Year, Month, hsGroup, hsDescription) %>%
    summarise(total = sum(cif))
  
  annualExportCommodity <- exportHS_merge_class_summary %>%
    group_by(Year, hsGroup) %>%
    summarise(OBS_VALUE = sum(total))
  
  colnames(annualExportCommodity)[colnames(annualExportCommodity) == "Year"] <- "TIME_PERIOD"
  annualExportCommodity$TIME_PERIOD <- as.character(annualExportCommodity$TIME_PERIOD)
  colnames(annualExportCommodity)[colnames(annualExportCommodity) == "hsGroup"] <- "COMMODITY"
  
  annualExportCommodity$FREQ  = "A"
  annualExportCommodity$TRADE_FLOW = "X"
  
  # Monthly Export by HS processing
  
  exportYM <- export %>%
    select(yearMonth, HS2, CIF) %>%
    rename(hs2 = HS2,
           cif = CIF
           )
  
  exportYM_merge_class <- merge(exportYM, hsClass, by = "hs2")
  
  exportYM_merge_class_summary <- exportYM_merge_class %>%
    group_by(yearMonth, hsGroup) %>%
    summarise(OBS_VALUE = sum(cif))
  
  colnames(exportYM_merge_class_summary)[colnames(exportYM_merge_class_summary) == "yearMonth"] <- "TIME_PERIOD"
  exportYM_merge_class_summary$TIME_PERIOD <- as.character(exportYM_merge_class_summary$TIME_PERIOD)
  colnames(exportYM_merge_class_summary)[colnames(exportYM_merge_class_summary) == "hsGroup"] <- "COMMODITY"
  
  exportYM_merge_class_summary$FREQ  = "M"
  exportYM_merge_class_summary$TRADE_FLOW = "X"
  
  statFrame <- rbind(annualImportCommodity, monthImportCommodity, annualExportCommodity, exportYM_merge_class_summary)
  
  #Add the rest of the columns
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

