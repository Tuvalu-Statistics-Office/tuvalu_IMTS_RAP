#### Table 1 Trade balances ####

#Trade Balance by year
table1A <- function(table1A){
  table1A <- dbGetQuery(mydb, "SELECT Year AS Period, 
                                        sum(CIF) AS Import 
                                 FROM impo 
                                 WHERE year > 2000
                                 GROUp BY Year")
  table1A$Type <- "Import"
  return(table1A)
  
}
  
  
#Trade Balance by month

table1B <- function(table1B){
  table1B <- dbGetQuery(mydb, "SELECT impo.Year,
                                      impo.Month,
                                      tblmonth.monthName AS Period,
                                      sum(impo.cif) AS Import
                               FROM impo
                               INNER JOIN tblmonth ON impo.Month = tblmonth.Month
                               WHERE Year > 2000
                               GROUP BY Year, impo.Month, monthName
                        
                        ")
  
  table1B$Period <- factor(table1B$Period, levels = month.name)
  table1B <- table1B %>%
    select(Period, Import)
  
  table1B$Type <- "Import"
  
  return(table1B)

}  


#### Imports By HS by year ####

table2A <- function(table2A){
  table2A <- dbGetQuery(mydb, "SELECT impo.Year, 
                                          hsclass.hsGroup,
                                          hsclass.hsDescription,
                                          sum(impo.cif) AS Value
                                   FROM impo
                                   INNER JOIN hsclass ON impo.hs2 = hsclass.hs2
                                   WHERE impo.Year > 2000
                                   GROUP BY impo.Year, hsclass.hsGroup, hsclass.hsDescription
                            
                            ")
  table2A$HSClass <- paste0(table2A$hsGroup , "-", table2A$hsDescription)
  
  return(table2A)
  
} 


#### Imports By HS by Month ####

table2B <- function(table2B){
  table2B <- dbGetQuery(mydb, "SELECT impo.Month,
                                          impo.Year,
                                          tblmonth.monthName,
                                          hsclass.hsGroup,
                                          hsclass.hsDescription,
                                          sum(impo.CIF) AS Value
                                   FROM impo
                                   INNER JOIN tblmonth ON impo.Month = tblmonth.monthCode
                                   INNER JOIN hsclass ON impo.HS2 = hsclass.HS2
                                   WHERE impo.Year > 2000
                                   GROUP BY impo.Year, impo.Month, tblmonth.monthName, hsclass.hsGroup, hsclass.hsDescription
                            
                            ")
  table2B$HSClass <- paste0(table2B$hsGroup , "-", table2B$hsDescription)
  table2B$monthName <- factor(table2B$monthName, levels = month.name)
  
  return(table2B)
  
}

#### Trade by principal Imports ####

table5A <- function(table5A){
  table5A <- dbGetQuery(mydb, "SELECT impo.Year,
                                      tblprinImports.PRINC_desc AS Commodity,
                                      sum(impo.CIF) AS Value
                               FROM impo
                               INNER JOIN tblprinImports ON impo.prinCode = tblprinImports.PRINC_IMP
                               WHERE impo.Year > 2000
                               GROUP BY impo.Year, Commodity
                        ")
return(table5A)  

}

table5B <- function(table5B){
  table5B <- dbGetQuery(mydb, "SELECT impo.Year,
                                      impo.Month,
                                      tblmonth.monthName,
                                      tblprinImports.PRINC_desc AS Commodity,
                                      sum(impo.CIF) AS Value
                               FROM impo
                               INNER JOIN tblprinImports ON impo.prinCode = tblprinImports.PRINC_IMP
                               INNER JOIN tblmonth ON impo.Month = tblmonth.Month
                               WHERE impo.Year > 2000
                               GROUP BY Year, impo.Month, monthName, Commodity
                               ORDER BY Year, impo.Month
                        ")
  
  return(table5B)
  
  #table5B$monthName <- factor(table5B$monthName, levels = month.name)
  
}




#### Trade by Partner Countries ####

table6A <- function(table6A){
  table6A <- dbGetQuery(mydb, "SELECT impo.Year, 
                                 impo.coeSelected AS country,
                                 sum(impo.cif) AS Value
                           FROM impo
                           WHERE impo.Year > 2000
                           GROUP BY impo.Year, impo.coeSelected
                           ")
  
  table6A$country[is.na(table6A$country)] <- "99. Other Country"
  table6A <- table6A %>%
    arrange(country)
  
  return(table6A)

}

table6B <- function(table6B){
  table6B <- dbGetQuery(mydb, "SELECT impo.Year,
                                 impo.Month,
                                 tblmonth.monthName,
                                 impo.coeSelected AS country,
                                 sum(impo.CIF) AS Value
                           FROM impo
                           INNER JOIN tblmonth ON impo.Month = tblmonth.monthCode
                           WHERE impo.Year > 2000
                           GROUP BY impo.Year, impo.Month, tblmonth.monthName, impo.coeSelected
                           ")
  
  table6B$country[is.na(table6B$country)] <- "99. Other Country"
  table6B$monthName <- factor(table6B$monthName, levels = month.name)
  
  return(table6B)
  
}

#### Trade by Reagion ####

table7A <- function(table7A){
  table7A <- dbGetQuery(mydb, "SELECT impo.Year, 
                                 impo.regionSelected AS Region,
                                 sum(impo.CIF) AS Value
                           FROM impo
                           WHERE impo.Year > 2000
                           GROUP BY impo.Year, impo.regionSelected
                           ")
  
  table7A$Region[is.na(table7A$Region)] <- "99. Other Region"
  
  return(table7A)
  
}

table7B <- function(table7B){
  table7B <- dbGetQuery(mydb, "SELECT impo.Year,
                                 impo.Month,
                                 tblmonth.monthName,
                                 impo.regionSelected AS Region,
                                 sum(impo.CIF) AS Value
                           FROM impo
                           INNER JOIN tblmonth ON impo.Month = tblmonth.monthCode
                           WHERE impo.Year > 2000
                           GROUP BY impo.Year, impo.Month, tblmonth.monthName, impo.regionSelected
                           ")
  
  table7B$Region[is.na(table7B$Region)] <- "99. Other Region"
  table7B$monthName <- factor(table7B$monthName, levels = month.name)
  
  return(table7B)
  
}


#### Trade by Transport Mode ####

table8A <- function(table8A){
  #By Year
  table8A <- dbGetQuery(mydb, "SELECT Year AS Period, 
                                     CASE 
                                        WHEN office = 'FFAIR' THEN 'Air'
                                        WHEN office = 'FFSEA' THEN 'Sea'
                                        ELSE 'Other'
                                     END AS transportMode, 
                                     sum(cif) AS Value 
                              FROM impo
                              WHERE Year > 2000
                              GROUP BY Period, transportMode")  
  
  return(table8A)
  
}
  
  #By Month
  
table8B <- function(table8B){
  table8B <- dbGetQuery(mydb, "SELECT impo.Year,
                                        impo.Month,
                                        tblmonth.monthName AS Period,
                                        CASE 
                                          WHEN impo.office = 'FFAIR' THEN 'Air'
                                          WHEN impo.office = 'FFSEA' THEN 'Sea'
                                          ELSE 'Other'
                                        END AS transportMode, 
                                        sum(cif) AS Value 
                              FROM impo
                              INNER JOIN tblmonth ON impo.Month = tblmonth.monthCode
                              WHERE impo.Year > 2000
                              GROUP BY impo.Year, impo.Month, Period, transportMode")
  
  table8B$Period <- factor(table8B$Period, levels = month.name)
  
  return(table8B)
  
  } 
