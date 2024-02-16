#### Table 1 Trade balances ####

#Trade Balance by year
table1A <- function(table1A){
  table1A <- dbGetQuery(mydb, "SELECT Year AS Period, 
                                        sum(CIF) AS Import 
                                 FROM import 
                                 WHERE year > 2000
                                 GROUp BY Year")
  table1A$Type <- "Import"
  return(table1A)
  
}
  
  
#Trade Balance by month

table1B <- function(table1B){
  table1B <- dbGetQuery(mydb, "SELECT import.Year,
                                      import.Month,
                                      tblmonth.monthName AS Period,
                                      sum(import.CIF) AS Import
                               FROM import
                               INNER JOIN tblmonth ON import.Month = tblmonth.monthCode
                               WHERE Year > 2000
                               GROUP BY Year, import.Month, monthName
                        
                        ")
  
  table1B$Period <- factor(table1B$Period, levels = month.name)
  table1B <- table1B %>%
    select(Period, Import)
  
  table1B$Type <- "Import"
  
  return(table1B)

}  


#### Imports By HS by year ####

table2A <- function(table2A){
  table2A <- dbGetQuery(mydb, "SELECT import.Year, 
                                          hsclass.hsGroup,
                                          hsclass.Description,
                                          sum(import.CIF) AS Value
                                   FROM import
                                   INNER JOIN hsclass ON import.HS2 = hsclass.HS2
                                   WHERE import.Year > 2000
                                   GROUP BY import.Year,hsclass.hsGroup, hsclass.Description
                            
                            ")
  table2A$HSClass <- paste0(table2A$hsGroup , "-", table2A$Description)
  
  return(table2A)
  
} 


#### Imports By HS by Month ####

table2B <- function(table2B){
  table2B <- dbGetQuery(mydb, "SELECT import.Month,
                                          import.Year,
                                          tblmonth.monthName,
                                          hsclass.hsGroup,
                                          hsclass.Description,
                                          sum(import.CIF) AS Value
                                   FROM import
                                   INNER JOIN tblmonth ON import.Month = tblmonth.monthCode
                                   INNER JOIN hsclass ON import.HS2 = hsclass.HS2
                                   WHERE import.Year > 2000
                                   GROUP BY import.Year, import.Month, tblmonth.monthName, hsclass.hsGroup, hsclass.Description
                            
                            ")
  table2B$HSClass <- paste0(table2B$hsGroup , "-", table2B$Description)
  table2B$monthName <- factor(table2B$monthName, levels = month.name)
  
  return(table2B)
  
}

#### Trade by principal Imports ####

table5A <- function(table5A){
  table5A <- dbGetQuery(mydb, "SELECT import.Year,
                                      tblprinImports.PRINC_desc AS Commodity,
                                      sum(import.CIF) AS Value
                               FROM import
                               INNER JOIN tblprinImports ON import.PRINC_IMP = tblprinImports.PRINC_IMP
                               WHERE import.Year > 2000
                               GROUP BY import.Year, Commodity
                        ")
  
}

table5B <- function(table5B){
  table5B <- dbGetQuery(mydb, "SELECT import.Year,
                                      import.Month,
                                      tblmonth.monthName,
                                      tblprinImports.PRINC_desc AS Commodity,
                                      sum(import.CIF) AS Value
                               FROM import
                               INNER JOIN tblprinImports ON import.PRINC_IMP = tblprinImports.PRINC_IMP
                               INNER JOIN tblmonth ON import.Month = tblmonth.monthCode
                               WHERE import.Year > 2000
                               GROUP BY Year, import.Month, monthName, Commodity
                               ORDER BY Year, import.Month
                        ")
  
  #table5B$monthName <- factor(table5B$monthName, levels = month.name)
  
}











#### Trade by Partner Countries ####

table6A <- function(table6A){
  table6A <- dbGetQuery(mydb, "SELECT import.Year, 
                                 import.[SELECTED COE] AS country,
                                 sum(import.CIF) AS Value
                           FROM import
                           WHERE import.Year > 2000
                           GROUP BY import.Year, import.[SELECTED COE]
                           ")
  return(table6A)

}

table6B <- function(table6B){
  table6B <- dbGetQuery(mydb, "SELECT import.Year,
                                 import.Month,
                                 tblmonth.monthName,
                                 import.[SELECTED COE] AS country,
                                 sum(import.CIF) AS Value
                           FROM import
                           INNER JOIN tblmonth ON import.Month = tblmonth.monthCode
                           WHERE import.Year > 2000
                           GROUP BY import.Year, import.Month, tblmonth.monthName, import.[SELECTED COE]
                           ")
  
  table6B$monthName <- factor(table6B$monthName, levels = month.name)
  
  return(table6B)
  
}

#### Trade by Reagion ####

table7A <- function(table7A){
  table7A <- dbGetQuery(mydb, "SELECT import.Year, 
                                 import.[SELECTED REGION] AS Region,
                                 sum(import.CIF) AS Value
                           FROM import
                           WHERE import.Year > 2000
                           GROUP BY import.Year, import.[SELECTED REGION]
                           ")
  return(table7A)
  
}

table7B <- function(table7B){
  table7B <- dbGetQuery(mydb, "SELECT import.Year,
                                 import.Month,
                                 tblmonth.monthName,
                                 import.[SELECTED REGION] AS Region,
                                 sum(import.CIF) AS Value
                           FROM import
                           INNER JOIN tblmonth ON import.Month = tblmonth.monthCode
                           WHERE import.Year > 2000
                           GROUP BY import.Year, import.Month, tblmonth.monthName, import.[SELECTED REGION]
                           ")
  
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
                                     sum(CIF) AS Value 
                              FROM import
                              WHERE Year > 2000
                              GROUP BY Period, transportMode")  
  
  return(table8A)
  
}
  
  #By Month
  
table8B <- function(table8B){
  table8B <- dbGetQuery(mydb, "SELECT Year,
                                        import.Month,
                                        tblmonth.monthName AS Period,
                                        CASE 
                                          WHEN office = 'FFAIR' THEN 'Air'
                                          WHEN office = 'FFSEA' THEN 'Sea'
                                          ELSE 'Other'
                                        END AS transportMode, 
                                        sum(CIF) AS Value 
                              FROM import
                              INNER JOIN tblmonth ON import.Month = tblmonth.monthCode
                              WHERE Year > 2000
                              GROUP BY Year, import.Month, Period, transportMode")
  
  table8B$Period <- factor(table8B$Period, levels = month.name)
  
  return(table8B)
  
  } 
