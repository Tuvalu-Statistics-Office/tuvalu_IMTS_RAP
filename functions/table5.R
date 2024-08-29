#*********************************************************************************#
#************************ Table 05 - Trade by Transport Mode *********************#
#********************************************************************************#

table5 <- function(statFrame) {
  
  statFrame <- data.frame()
  
  #************************************ Annual Transport *********************************** #  
  
  # Import Transport mode Annual
  impo <- dbGetQuery(mydb, "SELECT * FROM impo")
  
  importTransportAnnual <- impo %>%
    mutate(Office = case_when(
      Office == "FFSEA" ~ 21,
      Office == "FFAIR" ~ 1,
      Office == "LAND" ~ 3
    )) %>%
    group_by(Office, Year) %>%
    summarise(totImp = sum(cif))
  
  #Reformat table
  importTransportAnnual_cube <- importTransportAnnual |>
    mutate(Year = as.character(Year),
           TRADE_FLOW = "M",
           FREQ = "A") |>
    rename(TRANSPORT = Office, TIME_PERIOD = Year, OBS_VALUE = totImp)
  
  #Export Transport mode Annual
  export <- dbGetQuery(mydb, "SELECT * FROM export")
  
  exportTransportAnnual <- export %>%
    mutate(Office = case_when(
      Office == "FFSEA" ~ 21,
      Office == "FFAIR" ~ 1,
      Office == "LAND" ~ 3
    )) %>%
    group_by(Office, Year) %>%
    summarise(totExp = sum(cif))
  
  #Reformat table
  exportTransportAnnual_cube <- exportTransportAnnual |>
    mutate(Year = as.character(Year),
           TRADE_FLOW = "X",
           FREQ = "A") |>
    rename(TRANSPORT = Office, TIME_PERIOD = Year, OBS_VALUE = totExp)
  
#************************************ Monthly Transport *********************************** #  
  
  # Import Transport mode Monthly
  importTransportMonthly <- impo %>%
    mutate(Office = case_when(
      Office == "FFSEA" ~ 21,
      Office == "FFAIR" ~ 1,
      Office == "LAND" ~ 3
    )) %>%
    group_by(Office, yearMonth) %>%
    summarise(totImp = sum(cif))
  
  #Reformat table
  importTransportMonthly_cube <- importTransportMonthly |>
    mutate(TRADE_FLOW = "M",
           FREQ = "M") |>
    rename(TRANSPORT = Office, TIME_PERIOD = yearMonth, OBS_VALUE = totImp)
  
  #Export Transport mode Monthly
  exportTransportMonthly <- export %>%
    mutate(Office = case_when(
      Office == "FFSEA" ~ 21,
      Office == "FFAIR" ~ 1,
      Office == "LAND" ~ 3
    )) %>%
    group_by(Office, yearMonth) %>%
    summarise(totExp = sum(cif))
  
  #Reformat table
  exportTransportMonthly_cube <- exportTransportMonthly |>
    mutate(TRADE_FLOW = "X",
           FREQ = "M") |>
    rename(TRANSPORT = Office, TIME_PERIOD = yearMonth, OBS_VALUE = totExp)
  
  #combine all the generated tables together
  statFrame <- rbind(importTransportAnnual_cube,
                     importTransportMonthly_cube,
                     exportTransportAnnual_cube,
                     exportTransportMonthly_cube
                    )
  
  #Add the rest of the columns
  statFrame <- statFrame |>
    mutate(GEO_PICT = "TV", INDICATOR = "AMT", COMMODITY = "_T", CURRENCY = "DOM", 
           UNIT_MEASURE = "AUD", UNIT_MULT = "3", OBS_STATUS = "", DATA_SOURCE = "",
           OBS_COMMENT = "", COUNTERPART = "_T")

  #Reorder the columns in the proper order
  statFrame <- statFrame |>
    select(FREQ, TIME_PERIOD, GEO_PICT, INDICATOR, TRADE_FLOW, COMMODITY, COUNTERPART, 
           TRANSPORT, CURRENCY, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT)
  
  return(statFrame)
  
}
