####Data Processing and Cleaning ####

#Load required libraries
library(dplyr) #Library to manipulate the data
library(readxl) #Library to read excel files 
library(openxlsx) #Library to write excel files
library(tidyr) # Library to format dataframe
library(RSQLite) #Library to allow you to be abale to connect to SQLite database

#Dynamic directory path mapping
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#Connect to a MySQLite database which will be used for reporting
mydb <- dbConnect(RSQLite::SQLite(), "data/imts.db")
imports <- dbGetQuery(mydb, "SELECT * FROM import")


# Create an auto-incrementing column using row_number()
imports <- imports %>% 
  mutate(UID = row_number())

#Rename quantity or weight column to be used later on
colnames(imports)[colnames(imports) == "UOM CODE2"] <- "qtywgt"

#Compute Unit Value by taking CIF and dividing quantity/weight/volume
imports$unitValue <- round((imports$CIF / imports$qtywgt), 2)

#Construct the summary table
imports_check <- imports %>%
  group_by(Tariff) %>%
  summarise(minValue = round(min(unitValue), 2),
            maxValue = round(max(unitValue), 2),
            meanValue = round(mean(unitValue),2),
            medianValue = round(median(unitValue), 2),
            frquency = n()
            )

#Determining the Upper and Lower bounds using mean
imports_check$mean_lower_value <- round(imports_check$meanValue - (imports_check$meanValue * 25/100), 2)
imports_check$mean_upper_value <- round(imports_check$meanValue + (imports_check$meanValue * 25/100), 2)
imports_check$diff_upper_lower_mean <- imports_check$mean_upper_value - imports_check$mean_lower_value

#Determining the Upper and Lower bounds using median
imports_check$median_lower_value <- round(imports_check$medianValue - (imports_check$medianValue * 25/100), 2)
imports_check$median_upper_value <- round(imports_check$medianValue + (imports_check$medianValue * 25/100), 2)
imports_check$diff_upper_lower_median <- imports_check$median_upper_value - imports_check$median_lower_value


#Remove records with frequency of 1

imports_check_rem_freq_one <- imports_check %>%
  filter(frquency > 1, diff_upper_lower_median > 200 )

imports_chk <- imports_check_rem_freq_one %>%
  select(Tariff, medianValue, median_lower_value, median_upper_value, diff_upper_lower_median)

imports_chk_imports_merge <- merge(imports, imports_chk, by = "Tariff")
imports_chk_imports_merge$myUnitValue <- round(imports_chk_imports_merge$CIF / imports_chk_imports_merge$qtywgt, 2)

write.csv(imports_chk_imports_merge, "processing/imports_chk.csv", row.names = FALSE)


write.csv(imports_check_rem_freq_one, "output/HS_checks.csv", row.names = FALSE)
write.csv(imports, "c:/temp/imports.csv", row.names = FALSE)

#Example where no checking should be done
boxCheck_2043000 <- imports %>%
  filter(Tariff == 2043000

)

boxplot(boxCheck_2043000$unitValue)

#Example where checking should be done
boxCheck_19053100 <- imports %>%
  filter(Tariff == 19053100
         
  )

boxplot(boxCheck_19053100$unitValue)


#Example where checking should be done
boxCheck_27101990 <- imports %>%
  filter(Tariff == 27101990
         
  )

boxplot(boxCheck_27101990$unitValue)


#Example where checking should be done
boxCheck_39249000 <- imports %>%
  filter(Tariff == 39249000
         
  )

boxplot(boxCheck_39249000$unitValue)


#Example where checking should be done
boxCheck_82141000 <- imports %>%
  filter(Tariff == 82141000
         
  )

boxplot(boxCheck_82141000$unitValue)

#Write tables to SQLite database
'/* dbWriteTable(mydb, "imports-check", imports_check, overwrite = TRUE)
dbWriteTable(mydb, "imports", imports, overwrite = TRUE)

#Checking outliers

outliers_mean <- dbGetQuery(mydb, "SELECT * FROM imports
                              INNER JOIN [imports-check] ON imports.Tariff = [imports-check].Tariff
                              WHERE imports.unitValue < [imports-check].mean_lower_value OR imports.unitValue > [imports-check].mean_upper_value
                       ")

outliers_median <- dbGetQuery(mydb, "SELECT * FROM imports
                              INNER JOIN [imports-check] ON imports.Tariff = [imports-check].Tariff
                              WHERE imports.unitValue < [imports-check].median_lower_value OR imports.unitValue > [imports-check].median_upper_value
                       ")
*/'

#Disconnect from the SQLite database
dbDisconnect(mydb)