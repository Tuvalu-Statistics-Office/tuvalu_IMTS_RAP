#*********************************************************************************#
#************************ Table 4 - Trade by Parter Country **********************#
#********************************************************************************#

# Declare function

table4 <- function(statFrame){
  
  statFrame <- data.frame()
  impo <- dbGetQuery(mydb, "SELECT * FROM impo")
  
  # Import by partner country by year
  importPartnerAnnual <- impo %>%
    group_by(coeID, Year) %>%
    summarise(totImp = sum(cif))
  
  #Reformat table
  importPartnerAnnual_cube <- importPartnerAnnual |>
    mutate(TRADE_FLOW = "M",
           FREQ = "A",
           Year = as.character(Year)
           ) |>
    rename(COUNTERPART = coeID, TIME_PERIOD = Year, OBS_VALUE = totImp)
  
  # Import by partner country by month
  importPartnerMonthly <- impo %>%
    group_by(coeID, yearMonth) %>%
    summarise(totImp = sum(cif))
  
  #Reformat table
  importPartnerMonthly_cube <- importPartnerMonthly |>
    mutate(TRADE_FLOW = "M",
           FREQ = "M") |>
    rename(COUNTERPART = coeID, TIME_PERIOD = yearMonth, OBS_VALUE = totImp)
  
  
#*********************************** Export processing ********************************* 
  
  #Export by Partner Countries by year
  export <- dbGetQuery(mydb, "SELECT * FROM export")
  
  exportPartnerAnnual <- export %>%
    group_by(Year, coeID) %>%
    summarise(totExp = sum(cif))
  
  #Reformat table
  exportPartnerAnnual_cube <- exportPartnerAnnual |>
    mutate(TRADE_FLOW = "X",
           FREQ = "A",
           Year = as.character(Year)
           ) |>
    rename(COUNTERPART = coeID, TIME_PERIOD = Year, OBS_VALUE = totExp)
  
  # Export by partner country by month
  
  exportPartnerMonthly <- export %>%
    group_by(coeID, yearMonth) %>%
    summarise(totExp = sum(cif))
  
  #Reformat table
  exportPartnerMonthly_cube <- exportPartnerMonthly |>
    mutate(TRADE_FLOW = "X",
           FREQ = "M") |>
    rename(COUNTERPART = coeID, TIME_PERIOD = yearMonth, OBS_VALUE = totExp)
  
  statFrame <- rbind(importPartnerAnnual_cube,
                     importPartnerMonthly_cube,
                     exportPartnerAnnual_cube,
                     exportPartnerMonthly_cube
                     )
  
  
  #Add the rest of the columns
  statFrame <- statFrame |>
    mutate(GEO_PICT = "TV", INDICATOR = "AMT", CURRENCY = "DOM", TRANSPORT = "_T",
           UNIT_MEASURE = "AUD", UNIT_MULT = "3", OBS_STATUS = "", DATA_SOURCE = "",
           OBS_COMMENT = "", COMMODITY = "_T")
  
  #Reorder the columns in the proper order
  statFrame <- statFrame |>
    select(FREQ, TIME_PERIOD, GEO_PICT, INDICATOR, TRADE_FLOW, COMMODITY, COUNTERPART, 
           TRANSPORT, CURRENCY, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT)
  
  return(statFrame)

}





