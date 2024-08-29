#*********************************************************************************#
#************************ Table 2 - Import by HS *********************************#
#*********************************************************************************#

table2 <- function(statFrame){
  statFrame <- data.frame()

#******************************* Import table generation ****************************** #

  #### Import by Classifications 2_M #####
  impo <- dbGetQuery(mydb, "SELECT * FROM impo")
  hsClass <- dbGetQuery(mydb, "SELECT * FROM hsClass")
  hsClass$hs2Code <- hsClass$hs2
  
  # Annual Data preparation
 importHS <- impo %>%
    select(Year, Month, hs2, cif) |>
    rename(hs2Code = hs2)
  
  importHS_merge_class <- merge(importHS, hsClass, by = "hs2Code")
  
  importHS_merge_class_summary <- importHS_merge_class %>%
    group_by(Year, Month, hsGroup, hsDescription) %>%
    summarise(total = sum(cif))
  
  # Annual Import by Commodity
  annualImportCommodity <- importHS_merge_class_summary %>%
    group_by(Year, hsGroup) %>%
    summarise(totImp = sum(total))
  
  # Creating the data cube
  annualImportCommodity <- as.data.table(annualImportCommodity)
  annualImportCommodity_cube <- cube(annualImportCommodity, j = sum(totImp), by = c("Year", "hsGroup"), id = FALSE)
  annualImportCommodity_cube <- annualImportCommodity_cube[!is.na(annualImportCommodity_cube$Year)]
  
  #Reformat table
  annualImportCommodity_cube <- annualImportCommodity_cube |>
    filter(!is.na(hsGroup)) |>
    mutate(hsGroup = ifelse(is.na(hsGroup), "_T", hsGroup),
           TRADE_FLOW = "M",
           FREQ = "A") |>
    rename(COMMODITY = hsGroup, TIME_PERIOD = Year, OBS_VALUE = V1)
  
  # Monthly data preparation
  importYM <- impo %>%
    select(yearMonth, hs2, cif)
  
  importHS_merge_class_YM <- merge(importYM, hsClass, by = "hs2")
  
  monthImportCommodity <- importHS_merge_class_YM %>%
    group_by(yearMonth, hsGroup) %>%
    summarise(totImp = sum(cif))
  
  # Creating the data cube
  monthImportCommodity <- as.data.table(monthImportCommodity)
  monthImportCommodity_cube <- cube(monthImportCommodity, j = sum(totImp), by = c("yearMonth", "hsGroup"), id = FALSE)
  
  #Reformat table
  monthImportCommodity_cube <- monthImportCommodity_cube |>
    filter(!is.na(yearMonth)) |>
    filter(!is.na(hsGroup)) |>
    mutate(hsGroup = ifelse(is.na(hsGroup), "_T", hsGroup),
           TRADE_FLOW = "M",
           FREQ = "M") |>
    rename(COMMODITY = hsGroup, TIME_PERIOD = yearMonth, OBS_VALUE = V1)
  
  #### Export by Classifications 3_M #####
    export <- dbGetQuery(mydb, "SELECT * FROM export")
  # Annual Export by HS processing
  exportHS <- export %>%
    select(Year, Month, hs2Code, cif)
      
  exportHS_merge_class <- merge(exportHS, hsClass, by = "hs2Code")
  
  exportHS_merge_class_summary <- exportHS_merge_class %>%
    group_by(Year, Month, hsGroup, hsDescription) %>%
    summarise(total = sum(cif))
  
  annualExportCommodity <- exportHS_merge_class_summary %>%
    group_by(Year, hsGroup) %>%
    summarise(totExp = sum(total))
  
  # Creating the data cube
  annualExportCommodity <- as.data.table(annualExportCommodity)
  annualExportCommodity_cube <- cube(annualExportCommodity, j = sum(totExp), by = c("Year", "hsGroup"), id = FALSE)
  
  #Reformat table
  annualExportCommodity_cube <- annualExportCommodity_cube |>
    filter(!is.na(Year)) |>
    filter(!is.na(hsGroup)) |>
    mutate(hsGroup = ifelse(is.na(hsGroup), "_T", hsGroup),
           TRADE_FLOW = "X",
           FREQ = "A") |>
    rename(COMMODITY = hsGroup, TIME_PERIOD = Year, OBS_VALUE = V1)
  
  # Monthly Export by HS processing
  exportYM <- export %>%
    select(yearMonth, hs2Code, cif)
      
  exportYM_merge_class <- merge(exportYM, hsClass, by = "hs2Code")
  
  monthlyExportCommodity <- exportYM_merge_class %>%
    group_by(yearMonth, hsGroup) %>%
    summarise(totExp = sum(cif))
  
  # Creating the data cube
  monthlyExportCommodity <- as.data.table(monthlyExportCommodity)
  monthlyExportCommodity_cube <- cube(monthlyExportCommodity, j = sum(totExp), by = c("yearMonth", "hsGroup"), id = FALSE)
  
  #Reformat table
  monthlyExportCommodity_cube <- monthlyExportCommodity_cube |>
    filter(!is.na(yearMonth)) |>
    filter(!is.na(hsGroup)) |>
    mutate(hsGroup = ifelse(is.na(hsGroup), "_T", hsGroup),
           TRADE_FLOW = "X",
           FREQ = "M") |>
    rename(COMMODITY = hsGroup, TIME_PERIOD = yearMonth, OBS_VALUE = V1)
  
  statFrame <- rbind(annualImportCommodity_cube, monthImportCommodity_cube, annualExportCommodity_cube, monthlyExportCommodity_cube)
  
  #Add the rest of the columns
  statFrame <- statFrame |>
    mutate(GEO_PICT = "TV", INDICATOR = "AMT", CURRENCY = "DOM", TRANSPORT = "_T",
           UNIT_MEASURE = "AUD", UNIT_MULT = "3", OBS_STATUS = "", DATA_SOURCE = "",
           OBS_COMMENT = "", COUNTERPART = "_T")
  
  #Reorder the columns in the proper order
  statFrame <- statFrame |>
    select(FREQ, TIME_PERIOD, GEO_PICT, INDICATOR, TRADE_FLOW, COMMODITY, COUNTERPART, 
           TRANSPORT, CURRENCY, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT)
  
  return(statFrame)
  
}

