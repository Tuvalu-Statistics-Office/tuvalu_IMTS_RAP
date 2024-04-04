#Load Library
library(officer)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(pivottabler)
library(RSQLite)

#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#Connect to a MySQLite database which will be used for reporting
mydb <- dbConnect(RSQLite::SQLite(), "data/imts.db")

#Global variables
cmonth <- curMonth
cyear <- curYear

# Create blank word document object
word_doc <- read_docx()

logo <- "image/logo.png"
staticText <- read.csv("other/staticText.csv")
intro1 <- staticText$intro1
intro2 <- staticText$intro2
intro3 <- staticText$intro3

dbWriteTable(mydb, "staticText", staticText, overwrite = TRUE )

heading_format <- fp_text(color = "#191970",
                          bold = T,
                          font.size = 25,
                          underlined = T,
                          font.family = "Calibri")

table_format <- fp_text(color = "#191970",
                        italic = T,
                        font.size = 10,
                        font.family = "Calibri"
                        )

format_heading <- ftext("Tuvalu IMTS Reporting", heading_format) %>% fpar()

#Sourcing all the functions that are generating the tables
source("functions/rptTradeBalance.R")
source("functions/rptImportHSClass_yr.R")
source("functions/rptimportExport.R")
source("functions/rpttradeCountry.R")

#### Reporting by HS Classifications ####
#Imports performances

impHSClass_yr <- importHSClass_yr(importHSClass_tb)
balance_analysis <- monthBal_analysis(analysis)
ggClassImport <- importHSClass_GG(hsclassGG)
balMonthly <- monthlyBalance(balMonthly)
ggImage_mh <- monthBal_GG(imexGG_mh)
impExpBal <- importExport(importExport_tab)
ggImage <- importExport_GG(imexGG)
ggImpCountry <- tradeCountry(importCountry_pie)
balAnnual <- impexpBalance_yr(balAnnual)
importByCountry <- impByCountry(importByCountry)
tradeByCountry_Analysis_a <- tradeByCountry_Analysis(analysis)
#pivtest <- region_country(pt)


#### Add contents of the Word Document ####

word_doc <- word_doc %>%
  body_add_img(logo, width = 4, height = 1.5, style = "centered") %>%
  body_add_par("") %>%
  body_add(format_heading, style = "centered") %>%
  body_add_par("") %>%
  body_add_par("Table of Content", style = "heading 1") %>%
  body_add_toc() %>% #Add table of contents
  body_add_par("") %>%
  body_add_break() %>% #Adds Page break
  body_add_par("Introduction", style = "heading 1") %>%
  body_add_par("") %>%
  body_add_par(intro1, style = "Normal") %>%
  body_add_par("") %>%
  body_add_par(intro2, style = "Normal") %>%
  body_add_par("") %>%
  body_add_par(intro3, style = "Normal") %>%
  body_add_break() %>%
  body_add_par("Analysis", style = "heading 1") %>%
  body_add_par("") %>%
  body_add_par("Value of Imports and Exports:", style = "heading 2") %>%
  body_add_par("") %>%
  body_add_par("Table 1: Table showing Imports and Exports", style = "heading 3") %>%
  body_add_par("") %>%
  body_add_table(impExpBal, style = "Table Professional") %>%
  body_add_par("") %>%
  body_add_par("Figure 1: Column Graph showing Imports and Exports", style = "heading 3") %>%
  body_add_par("") %>%
  body_add_gg(ggImage, width = 5, height = 3, style = "centered") %>%
  body_add_par("") %>%
  body_add_par(balance_analysis, style = "Normal") %>%
  body_add_par("") %>%
  body_add_break() %>%
  body_add_par("Balance of Trade:", style = "heading 2") %>%
  body_add_par("") %>%
  body_add_par("Table 2: Table of trade balance by month", style = "heading 3") %>%
  body_add_par("") %>%
  body_add_table(balMonthly, style = "Table Professional") %>%
  body_add_par("") %>%
  body_add_par("Figure 2: Line Graph showing trade balance by month", style = "heading 3") %>%
  body_add_par("") %>%
  body_add_gg(ggImage_mh, width = 6, height = 4, style = "centered") %>%
  body_add_break() %>%
  body_add_par("Table 3: Table of trade balance by year", style = "heading 3") %>%
  body_add_par("") %>%
  body_add_table(balAnnual, style = "Table Professional") %>%
  body_add_par("") %>%
  body_add_par("Figure 3: Line Graph showing trade balance by year", style = "heading 3") %>%
  body_add_break() %>%
  body_add_par("Import and Export Performances:", style = "heading 2") %>%
  body_add_par("") %>%
  body_add_par("Performance of Imports", style = "heading 3") %>%
  body_add_par("") %>%
  body_add_gg(ggClassImport, width = 7, height = 8, style = "centered") %>%
  body_add_par("") %>%
  body_add_break() %>%
  body_add_par("Performance of Exports", style = "heading 3") %>%
  body_add_par("") %>%
  body_add_par("Trade by Country:", style = "heading 2") %>%
  body_add_par("") %>%
  body_add_par("Import by Country", style = "heading 3") %>%
  body_add_par("") %>%
  body_add_table(importByCountry, style = "Table Professional") %>%
  body_add_par("") %>%
  body_add_gg(ggImpCountry, width = 6, height = 6,  style = "centered")
  
#Save Document
print(word_doc, "output/IMTS_Publication_Report_TV.docx")
