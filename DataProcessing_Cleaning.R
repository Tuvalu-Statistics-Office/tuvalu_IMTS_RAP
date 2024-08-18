#Dynamic directory path mapping
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#Referencing the setup source file
source("setup.R")

#Connect to a MySQLite database which will be used for reporting
mydb <- dbConnect(RSQLite::SQLite(), "data/imts.db")

#### *************************************** DATA PREPARATION ************************************************* #### 

impo <- read.csv("data/import.csv")
selCountry <- read.csv("other/selCountry.csv")
country <- read.csv("other/country.csv")
countries <- read.csv("other/countries.csv")
hsClass <- read.csv("other/importClassification.csv")
principalImports <- read.csv("other/principalImports.csv")


hsClass$hs2 <- sprintf("%0*d", hs2digits, hsClass$hs2)

#Write the tables to the SQLite database exept for the impo table
dbWriteTable(mydb, "selCountry", selCountry, overwrite = TRUE)
dbWriteTable(mydb, "country", country, overwrite = TRUE)
dbWriteTable(mydb, "countries", countries, overwrite = TRUE)
dbWriteTable(mydb, "hsClass", hsClass, overwrite = TRUE)

#******************************************************************************* 
#*****************  Import table preparation ***********************************
#*******************************************************************************

impo <- impo %>%
  rename(sadDate = SAD.Date,
         sadNo = SAD..,
         importerName = Importer.Name,
         sadModel = SAD.Model,
         coeID = COE,
         coeName = COE.Nam,
         itemNo = Item..,
         tariff = Tariff,
         goodsDesc = Goods.Dsc.,
         natProc = Nat..Proc,
         preference = Preferenc,
         coo = COO,
         cooName = COO.Name,
         invoiceValue = Invoice.Value,
         freightIns = Freight...Ins.,
         cif = CIF,
         totalTax = Total.Tax,
         imd = IMD,
         iex = IEX,
         tct = TCT,
         wal = WAL,
         uomCode = UOM.CODE,
         uomCode2 = UOM.CODE.1
  )

#merge to get Selected country
impo <- merge(impo, selCountry, by = "coeID", all = TRUE)

#Converting date from text to date format and extract month and year
impo$sadDate = dmy(impo$sadDate)
impo$Month = month(impo$sadDate)
impo$Year = year(impo$sadDate)

impo$MonthID <- sprintf("%02d", impo$Month)
impo$yearMonth <- paste(impo$Year, impo$MonthID, sep = "-")

#Drop records where the Tariff has NA value
impo <- impo[!is.na(impo$tariff), ]

impo$tariff <- sprintf("%0*d", hsdigits, impo$tariff)

#Extract and Create HS2 and HS4 and Chapter columns
impo$hs2 <- substr(impo$tariff, 1, hs2digits)
impo$Chapter <- substr(impo$tariff, 1, hs2digits)
impo$hs4 <- substr(impo$tariff, 1, hs4digits)
impo$hs6 <- substr(impo$tariff, 1, hs6digits)

#Merge main table with the Import by HS Classification
impo <- merge(impo, hsClass)

#Adding Principal imports to the main table
pImport <- read.csv("other/prImport.csv")
pImport$PM <- sprintf("%0*d", hs4digits, pImport$PM)

colnames(pImport)[colnames(pImport) == "PM"] <- "hs4"
colnames(pImport)[colnames(pImport) == "HS"] <- "myHS"

#Write impo and pImport table to the SQLite database
dbWriteTable(mydb, "impo", impo, overwrite = TRUE)
dbWriteTable(mydb, "pImport", pImport, overwrite = TRUE)

#Run queries needed for merging
hs4_import <- dbGetQuery(mydb, "SELECT impo.Tariff,
                                      pImport.hs4,  
                                      pImport.prinCode,
                                      COUNT(Tariff) AS freq
                               FROM impo
                               INNER JOIN pImport ON impo.hs4 = pImport.hs4
                               GROUP BY impo.Tariff, pImport.hs4, pImport.prinCode  
                        ")

hs2_import <- dbGetQuery(mydb, "SELECT impo.Tariff,
                                       pImport.hs4,  
                                       pImport.prinCode,
                                       COUNT(Tariff) AS freq
                                FROM impo
                                INNER JOIN pImport ON impo.hs2 = pImport.prinCode
                                GROUP BY impo.Tariff, pImport.hs4, pImport.prinCode  
                         ")

pImportDesc <- read.csv("other/pImportDesc.csv")

hs_pImport_append <- rbind(hs2_import, hs4_import)
hs_pImport_append_Desc <- merge(hs_pImport_append, pImportDesc, by = "prinCode")

hs_pImport_append_Desc <- hs_pImport_append_Desc %>%
  select(tariff, prinCode, prinSpecs)

impo_Tariff_unique <- impo %>%
  group_by(tariff) %>%
  summarise(totCount = n())

impo_Tariff_unique_Desc <- merge(impo_Tariff_unique, hs_pImport_append_Desc, by="tariff", all = TRUE)
impo <- merge(impo, impo_Tariff_unique_Desc, by = "tariff", all = TRUE)
impo <- impo[, -which(names(impo) == "totCount")]

#Replace NA with other
impo$prinCode[is.na(impo$prinCode)] <- 9999
impo$prinSpecs[is.na(impo$prinSpecs)] <- "Other imports"

# Create an auto-incrementing column using row_number()
impo <- impo %>% 
  mutate(UID = row_number())

#Rename quantity or weight column to be used later on
colnames(impo)[colnames(impo) == "uomCode2"] <- "qtywgt"
dbWriteTable(mydb, "impo", impo, overwrite = TRUE)

#************************************ Checking for Outliers in imports *********************************** ####

#Compute Unit Value by taking CIF and dividing quantity/weight/volume
impo$qty <- as.numeric(as.vector(impo$tct))

impo$unitValue <- round((impo$cif / impo$qty), 2)

#Construct the summary table
imports_check <- impo %>%
  group_by(tariff) %>%
  summarise(minValue = round(min(unitValue), 2),
            maxValue = round(max(unitValue), 2),
            meanValue = round(mean(unitValue),2),
            medianValue = round(median(unitValue), 2),
            frquency = n()
            )
imports_check$mean_median_diff <- imports_check$meanValue - imports_check$medianValue


#Determining the Upper and Lower bounds using mean
imports_check$mean_lower_value <- round(imports_check$meanValue - (imports_check$meanValue * 5/100), 2)
imports_check$mean_upper_value <- round(imports_check$meanValue + (imports_check$meanValue * 5/100), 2)
imports_check$diff_upper_lower_mean <- imports_check$mean_upper_value - imports_check$mean_lower_value

#Determining the Upper and Lower bounds using median
imports_check$median_lower_value <- round(imports_check$medianValue - (imports_check$medianValue * 5/100), 2)
imports_check$median_upper_value <- round(imports_check$medianValue + (imports_check$medianValue * 5/100), 2)
imports_check$diff_upper_lower_median <- imports_check$median_upper_value - imports_check$median_lower_value


#Remove records with frequency of 1

imports_check_rem_freq_one <- imports_check %>%
  filter(frquency > 1, diff_upper_lower_median > 200 )

imports_chk <- imports_check_rem_freq_one %>%
  select(tariff, medianValue, median_lower_value, median_upper_value, diff_upper_lower_median)

imports_chk_imports_merge <- merge(impo, imports_chk, by = "tariff", all = TRUE)
imports_chk_imports_merge$myUnitValue <- round(imports_chk_imports_merge$cif / imports_chk_imports_merge$qtywgt, 2)

write.csv(imports_chk_imports_merge, "processing/imports_chk.csv", row.names = FALSE)


write.csv(imports_check_rem_freq_one, "output/HS_checks.csv", row.names = FALSE)
#write.csv(imports, "c:/temp/imports.csv", row.names = FALSE)

#Example where checking should be done
boxCheck_19053100 <- impo %>%
  filter(tariff == 19053100
         
  )

boxplot(boxCheck_19053100$unitValue)


#Example where checking should be done
boxCheck_27101990 <- impo %>%
  filter(tariff == 27101990
         
  )

#boxplot(boxCheck_27101990$unitValue)


#Example where checking should be done
boxCheck_39249000 <- impo %>%
  filter(tariff == 39249000
         
  )

boxplot(boxCheck_39249000$unitValue)


#Example where checking should be done
boxCheck_82141000 <- impo %>%
  filter(tariff == 82141000
         
  )

boxplot(boxCheck_82141000$unitValue)

#-------------------------------------------------------------------------------
#     EXPORTS
#-------------------------------------------------------------------------------
#Read in the export excel file
export <- read_excel("data/export.xlsx")

#Getting year and month
export <- export |>
  #Filter records that contain NA values
  filter(!is.na(Tariff)) |>
  #generate month, year and yearmonth columns
  mutate(Year = year(`SAD Date`),
         Month = month(`SAD Date`),
         sMonth = sprintf("%02d", month(`SAD Date`)),
         yearMonth = paste(year(`SAD Date`), sprintf("%02d", month(`SAD Date`)), sep = "-")
         )

#Reformat tariff to 8 digits and create hs2 and hs4 columns
export$Tariff <- sprintf("%08d", export$Tariff)
export$hs2Code <- substr(export$Tariff, 1, hs2digits)
export$hs4 <- substr(export$Tariff, 1, hs4digits)
export$Chapter <- sprintf("%02d", export$Chapter)

#Change values for EX 1
export$`SAD Model`[export$`SAD Model`=="EX 1"] <- "Export"

#Getting country names
colnames(export)[colnames(export) == "COD"] <- "coeID"
export <- merge(export, selCountry, by = "coeID", all = TRUE)

#Renaming the columns
export <- export |>
  rename(
    Country = coeSelected,
    Region = regionSelected,
    cif = CIF
  )

export <- merge(export, hsClass, by = "hs2Code")
export$mcif <- 0

#Getting principle exports
princ_x <- data.frame(
  hs4 = c("7204","7311","9703","2710"),
  princ_x_desc = c("Ferrous waste and scrap",
                   "Containers for compressed or liquified gas",
                   "Sculptures and statuary",
                   "Mineral Fuel")
)

export <- merge(export, princ_x, by = "hs4", all = TRUE)
export$princ_x_desc[is.na(export$princ_x_desc)] <- "Other"
dbWriteTable(mydb, "export", export, overwrite = TRUE)

dbDisconnect(mydb)