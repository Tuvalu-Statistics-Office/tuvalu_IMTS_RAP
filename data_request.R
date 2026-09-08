#A script to handle data requests
#Person requesting to provide 
##tariff code for target items
##target years
#Person producing the table to make the necessary changes to placeholders

library(dplyr)
library(readxl)
#-------------------------------------------------------------------------------
#Define key variables and data
target_tariff <- c("2711","0202")#Enter tariff delimited  by ','
years <- c(2026) #Enter years to tabulate
target_level <- 4 #Change this to the target level of HS
clean_qty_data <- read_csv("output/imports_qty_correct.csv")
#-------------------------------------------------------------------------------
clean_qty_data$year <- year(clean_qty_data$sadDate) #Generate year
str_hs <- paste("HS_",target_level) #Define label for target HS level
clean_qty_data$coln <- substr(clean_qty_data$tariff,1,target_level) #get hs level
clean_qty_data$tariff <- sprintf("%0*d", 8, as.numeric(clean_qty_data$tariff))
clean_qty_data$tariff <- as.character(clean_qty_data$tariff)
#Remove other records
clean_qty_data_filtered <- clean_qty_data %>% 
  filter(clean_qty_data$coln %in% target_tariff)
names(clean_qty_data_filtered)[names(clean_qty_data)=="coln"] <- trimws(str_hs) #rename column
#Keep target attributes columns

cols_to_keep <- c("year",str_hs, "cif", "uomCode", "qtyCorr")
clean_qty_data_filtered <- clean_qty_data_filtered[, cols_to_keep]

#generate collapsed data frame
collapsed_clean_qty_data_filtered <- clean_qty_data_filtered %>%
  group_by(year, uomCode, .data[[str_hs]]) %>%
  summarise(
    total_cif = sum(cif, na.rm = TRUE),    # Adds up numeric values
    total_qty   = sum(qtyCorr, na.rm = TRUE),   # Calculates averages
    row_count   = n()                          # Counts number of rows grouped
  ) %>%
  ungroup() # Best practice: always ungroup when finished

#Write output table
#Important: change the name of the output file to desired name
write.csv(collapsed_clean_qty_data_filtered, "output/data_request.csv", row.names = FALSE)
