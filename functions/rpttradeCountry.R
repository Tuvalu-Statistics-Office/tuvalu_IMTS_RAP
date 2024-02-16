#### Trade By Country ####
library(pivottabler)
# Import by Country

importByCountry <- dbGetQuery(mydb, "SELECT [SELECTED COE] AS country, 
                                            sum(CIF) AS Value 
                                     FROM import 
                                     GROUP BY country")

# get total import for purpose of calculating the percentage per country
totImport <- dbGetQuery(mydb, "SELECT Year, 
                                      sum(CIF) AS Value 
                               FROM import 
                               WHERE Year > 2000
                               GROUP BY Year")

# Add total import to the table for purpose of calculating the percentage
importByCountry$total <- totImport$Value
importByCountry$percent <- (importByCountry$Value / importByCountry$total) * 100
importByCountry$percent <- round(importByCountry$percent, 2)

# Remove rows with missing value
importByCountry <- importByCountry[complete.cases(importByCountry), ]
importByCountry <- importByCountry %>%
  select(country, Value, percent) %>%
  arrange(desc(percent))


tradeCountry <- function(importCountry_pie){
  
  # Create a pie chart
  importCountry_pie <- ggplot(importByCountry, aes(x = "", y = percent, fill = country)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    labs(title = "Import by Country (%)") +
    theme_void() # Remove unnecessary elements
    
    
  ggsave("image/importCountry_pie.png", plot = importCountry_pie, width = 6, height = 6, units = "in")
  
  return(importCountry_pie)
}

#Function to generate the Import by Country table
impByCountry <- function(importByCountry){
  
  importByCountry <- importByCountry
  return(importByCountry)
  
}

#Function to generate a simple analysis of the trade by country

tradeByCountry_Analysis <- function(analysis){
  
  importLead <- importByCountry %>% 
    mutate(Rank = rank(-percent, ties.method = "min"))
  
  index <- 1
  
  # Define the while loop condition
  while (index <= nrow(importLead)) {
    current_row <- paste0("- ", importLead$country, " ", round(importLead$percent, 0), "%")
    index <- index + 1
  }
  
  analysis <- current_row
  
  return(analysis)
  
}
