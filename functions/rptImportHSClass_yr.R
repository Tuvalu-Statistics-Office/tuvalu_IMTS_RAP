#### Reporting by HS Classifications ####

#SQL query to get import by current year
importHSClass <- dbGetQuery(mydb, "SELECT impo.Year, 
                                          hsclass.hsGroup,
                                          hsclass.hsDescription,
                                          sum(impo.cif) AS Value
                                   FROM impo
                                   INNER JOIN hsclass ON impo.hs2 = hsclass.HS2
                                   GROUP BY impo.Year, hsclass.hsGroup, hsclass.hsDescription
                            
                            ")
#Remove records which contain NULL values
importHSClass <- importHSClass[complete.cases(importHSClass), ]

totImport <- dbGetQuery(mydb, "SELECT year, 
                                      sum(CIF) as Value 
                               FROM impo 
                               WHERE Year > 2000
                               GROUP BY Year
                               ")


#Imports performances

importHSClass_yr <- function(importHSClass_tb){
  
  importHSClass$totImport <- totImport$Value
  importHSClass <- importHSClass[complete.cases(importHSClass), ]
  
  importHSClass$percent <- (importHSClass$Value/importHSClass$totImport) * 100
  importHSClass$percent <- round(importHSClass$percent, 0)
  
  importHSClass$class <- paste0(importHSClass$hsGroup, " - ", importHSClass$Description)
  
  importHSClass_tb <<- importHSClass %>%
    select(class, Value, percent)
  
  return(importHSClass_tb)
  
}

importHSClass_GG <- function(hsclassGG){
  
  hsclassGG <- ggplot(importHSClass_tb, aes(x = class, y = percent)) +
    geom_bar(stat = "identity", position = "dodge", fill = "blue") +
    labs(title = "Column Graph HS Class", x = "HS Class", y = "percent(%)") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  ggsave("image/hsclass_cur.png", plot = hsclassGG, width = 7, height = 8, units = "in")
  return(hsclassGG)
  
}



