#### Reporting by HS Classifications ####

#SQL query to get import by current year
importHSClass <- dbGetQuery(mydb, "SELECT import.Year, 
                                          hsclass.hsGroup,
                                          hsclass.Description,
                                          sum(import.CIF) AS Value
                                   FROM import
                                   INNER JOIN hsclass ON import.HS2 = hsclass.HS2
                                   GROUP BY import.Year,hsclass.hsGroup, hsclass.Description
                            
                            ")
#Remove records which contain NULL values
importHSClass <- importHSClass[complete.cases(importHSClass), ]

totImport <- dbGetQuery(mydb, "SELECT year, 
                                      sum(CIF) as Value 
                               FROM import 
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



