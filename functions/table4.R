#*********************************************************************************#
#************************ Table 4 - Trade by Parter Country **********************#
#********************************************************************************#

# Declare function

table4 <- function(statFrame){
  
  statFrame <- data.frame()
  impo <- dbGetQuery(mydb, "SELECT * FROM impo")
  
  
  importPartnerAnnual <- impo %>%
    group_by(coeID, Year) %>%
    summarise(OBS_VALUE = sum(cif))
  
  # Renaming columns
  
  colnames(importPartnerAnnual)[colnames(importPartnerAnnual) == "Year"] <- "TIME_PERIOD"
  colnames(importPartnerAnnual)[colnames(importPartnerAnnual) == "coeID"] <- "COUNTERPART"
  
  importPartnerAnnual$FREQ <- "A"
  importPartnerAnnual$TRADE_FLOW <- "M"
  
  #Export by Partner Countries
  exportPartnerAnnual <- export %>%
    group_by(Year, coeID) %>%
    summarise(OBS_VALUE = sum(cif))
  
  colnames(exportPartnerAnnual)[colnames(exportPartnerAnnual) == "Year"] <- "TIME_PERIOD"
  colnames(exportPartnerAnnual)[colnames(exportPartnerAnnual) == "coeID"] <- "COUNTERPART"
  
  exportPartnerAnnual$FREQ <- "A"
  exportPartnerAnnual$TRADE_FLOW <- "X"
  
  tradeByPartnerAnnual <- rbind(importPartnerAnnual, exportPartnerAnnual)
  tradeByPartnerAnnual$TIME_PERIOD <- as.character(tradeByPartnerAnnual$TIME_PERIOD)
  
  # Import by partner country by month
  
  importPartnerMonthly <- impo %>%
    group_by(coeID, yearMonth) %>%
    summarise(OBS_VALUE = sum(cif))
  
  # Renaming columns
  
  colnames(importPartnerMonthly)[colnames(importPartnerMonthly) == "yearMonth"] <- "TIME_PERIOD"
  colnames(importPartnerMonthly)[colnames(importPartnerMonthly) == "COE"] <- "COUNTERPART"
  
  importPartnerMonthly$FREQ <- "M"
  importPartnerMonthly$TRADE_FLOW <- "M"
  
 
  # Export by partner country by month
  
  exportPartnerMonthly <- export %>%
    group_by(coeID, yearMonth) %>%
    summarise(OBS_VALUE = sum(cif))
  
  # Renaming columns
  
  colnames(exportPartnerMonthly)[colnames(exportPartnerMonthly) == "yearMonth"] <- "TIME_PERIOD"
  colnames(exportPartnerMonthly)[colnames(exportPartnerMonthly) == "COD"] <- "COUNTERPART"
  
  exportPartnerMonthly$FREQ <- "M"
  exportPartnerMonthly$TRADE_FLOW <- "X"
  
  tradeByPartnerMonthly <- rbind(importPartnerMonthly, exportPartnerMonthly)
  
  statFrame <- rbind(tradeByPartnerAnnual, tradeByPartnerMonthly )
  
  statFrame$GEO_PICT <- "TV"
  statFrame$INDICATOR <- "AMT"
  statFrame$COMMODITY <- "_T"
  statFrame$TRANSPORT <- "_T"
  statFrame$CURRENCY <- "DOM"
  statFrame$UNIT_MEASURE <- "NZD"
  statFrame$UNIT_MULT <- "3"
  statFrame$OBS_STATUS <- ""
  statFrame$DATA_SOURCE <- ""
  statFrame$OBS_COMMENT <- ""
  
  #Re-ordering of the columns
  order <- c("FREQ", "TIME_PERIOD", "GEO_PICT", "INDICATOR", "TRADE_FLOW", "COMMODITY", "COUNTERPART", "TRANSPORT", "CURRENCY", "OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "OBS_STATUS", "DATA_SOURCE", "OBS_COMMENT")
  statFrame <- statFrame[, order]
  
  return(statFrame)

}





