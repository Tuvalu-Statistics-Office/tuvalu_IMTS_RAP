#*********************************************************************************#
#************************ Table 4 - Trade by Parter Country **********************#
#********************************************************************************#

# Declare function

table4 <- function(statFrame){
  
  statFrame <- data.frame()
  importPartnerAnnual <- import %>%
    filter(Year > 2000) %>%
    group_by(COE, Year) %>%
    summarise(OBS_VALUE = sum(CIF))
  
  # Renaming columns
  
  colnames(importPartnerAnnual)[colnames(importPartnerAnnual) == "Year"] <- "TIME_PERIOD"
  colnames(importPartnerAnnual)[colnames(importPartnerAnnual) == "COE"] <- "COUNTERPART"
  
  importPartnerAnnual$FREQ <- "A"
  importPartnerAnnual$TRADE_FLOW <- "M"
  
  
  #Export by Partner Countries
  exportPartnerAnnual <- export %>%
    filter(Year > 2000) %>%
    group_by(Year, COD) %>%
    summarise(OBS_VALUE = sum(CIF))
  
  colnames(exportPartnerAnnual)[colnames(exportPartnerAnnual) == "Year"] <- "TIME_PERIOD"
  colnames(exportPartnerAnnual)[colnames(exportPartnerAnnual) == "COD"] <- "COUNTERPART"
  
  exportPartnerAnnual$FREQ <- "A"
  exportPartnerAnnual$TRADE_FLOW <- "X"
  
  
  tradeByPartnerAnnual <- rbind(importPartnerAnnual, exportPartnerAnnual)
  tradeByPartnerAnnual$TIME_PERIOD <- as.character(tradeByPartnerAnnual$TIME_PERIOD)
  
  
  # Import by partner country by month
  
  importPartnerMonthly <- import %>%
    filter(Year > 2000) %>%
    group_by(COE, yearMonth) %>%
    summarise(OBS_VALUE = sum(CIF))
  
  # Renaming columns
  
  colnames(importPartnerMonthly)[colnames(importPartnerMonthly) == "yearMonth"] <- "TIME_PERIOD"
  colnames(importPartnerMonthly)[colnames(importPartnerMonthly) == "COE"] <- "COUNTERPART"
  
  importPartnerMonthly$FREQ <- "M"
  importPartnerMonthly$TRADE_FLOW <- "M"
  
 
  # Export by partner country by month
  
  exportPartnerMonthly <- export %>%
    filter(Year > 2000) %>%
    group_by(COD, yearMonth) %>%
    summarise(OBS_VALUE = sum(CIF))
  
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
  
  return(statFrame)

}





