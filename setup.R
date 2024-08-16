#Load required libraries
library(dplyr)
library(readxl)
library(openxlsx)
library(tidyr)
library(RSQLite)
library(lubridate) #Date conversions and manipulations
library(officer)
library(tidyverse)
library(ggplot2)
library(pivottabler)

#Global variable declarations
hsdigits <- 8
hs6digits <- 6
hs4digits <- 4
hs2digits <- 2