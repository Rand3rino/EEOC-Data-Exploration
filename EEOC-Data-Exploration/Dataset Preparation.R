library(openxlsx)
library(tidyverse)



### Data Load ###

eeoc_2021 <- read.xlsx("https://www.eeoc.gov/sites/default/files/2023-03/EEO1%202021%20PUF.xlsx")
eeoc_2020 <- read.xlsx("https://www.eeoc.gov/sites/default/files/2022-12/EEO1_2020_PUF.xlsx")
eeoc_2019 <- read.xlsx("https://www.eeoc.gov/sites/default/files/2022-09/EEO1_2019_PUF.xlsx")
eeoc_2018 <- read.xlsx("https://www.eeoc.gov/sites/default/files/2021-12/EEO1%202018%20PUF.xlsx")
eeoc_2017 <- read.xlsx("https://www.eeoc.gov/sites/default/files/2021-12/EEO1%202017%20PUF.xlsx")
eeoc_2016 <- read.xlsx("https://www.eeoc.gov/sites/default/files/2021-12/EEO1%202016%20PUF.xlsx")
eeoc_2015 <- read.xlsx("https://www.eeoc.gov/sites/default/files/2021-12/EEO1%202015%20PUF.xlsx")
eeoc_2014 <- read.xlsx("https://www.eeoc.gov/sites/default/files/2021-12/EEO1%202014%20PUF.xlsx")

### Data Filtering to only the National Level ###

filter_national <- function(df) {
  # Get variable name as a string
  # https://stackoverflow.com/questions/44281656/get-object-name-in-r-as-string
  year <- substr(deparse(match.call()$df), 6, 9)
  
  # Capitalize Column Names
  colnames(df) <- toupper(colnames(df))
  
  df %>% 
    filter(is.na(REGION) & !is.na(NAICS2_NAME) & is.na(CBSA) & !is.na(NAICS3_NAME)) %>% 
    select(-NATION, -REGION, -DIVISION, -STATE, -CBSA, -COUNTY, -NAICS2, -NAICS3) %>% 
    mutate(Year = year)
}

eeoc_2021_national <- filter_national(eeoc_2021)
eeoc_2020_national <- filter_national(eeoc_2020)
eeoc_2019_national <- filter_national(eeoc_2019)
eeoc_2018_national <- filter_national(eeoc_2018)
eeoc_2017_national <- filter_national(eeoc_2017)
eeoc_2016_national <- filter_national(eeoc_2016)
eeoc_2015_national <- filter_national(eeoc_2015)
eeoc_2014_national <- filter_national(eeoc_2014)

### Dataset Stacking ###
# https://stackoverflow.com/questions/8169323/r-concatenate-two-dataframes

library(plyr)

eeoc_national <- rbind.fill(
  eeoc_2021_national,
  eeoc_2020_national,
  eeoc_2019_national,
  eeoc_2018_national,
  eeoc_2017_national,
  eeoc_2016_national,
  eeoc_2015_national,
  eeoc_2014_national
)

# Some Industries aren't present in more recent years
eeoc_national %>% 
  group_by(Year) %>% 
  dplyr::summarise(n()) %>% 
  dplyr::arrange(Year)


### Rename Column Names
column_names <- colnames(eeoc_national)
column_names[1] <- "Industry Group"
column_names[2] <- "Industry"
column_names[3] <- "Establishments"

# Job types
column_names <- str_replace(column_names, "1_2", "/Middle Management")
column_names <- str_replace(column_names, "10", "/All Jobs")
column_names <- str_replace(column_names, "9", "/Service")
column_names <- str_replace(column_names, "8", "/Labor")
column_names <- str_replace(column_names, "7", "/Operatives")
column_names <- str_replace(column_names, "6", "/Craft")
column_names <- str_replace(column_names, "5", "/Clericals")
column_names <- str_replace(column_names, "4", "/Sales Workers")
column_names <- str_replace(column_names, "3", "/Techinicians")
column_names <- str_replace(column_names, "2", "/Professionals")
column_names <- str_replace(column_names, "1", "/Senior Management")

# Sexes
column_names <- str_replace(column_names, "M/", "/Male/")
column_names <- str_replace(column_names, "F/", "/Female/")
column_names <- str_replace(column_names, "MT/", "All Races/Male/")
column_names <- str_replace(column_names, "FT/", "All Races/Female/")
column_names <- str_replace(column_names, "T/", "/All Sexes/")

# Races
column_names <- str_replace(column_names, "AIAN/", "American Indian or Alaskan Native/")
column_names <- str_replace(column_names, "WH/", "White/")
column_names <- str_replace(column_names, "BLK/", "Black/")
column_names <- str_replace(column_names, "ASIAN/", "Asian/")
column_names <- str_replace(column_names, "NHOPI/", "Native Hawaiian or Pacific Islander/")
column_names <- str_replace(column_names, "TOMR/", "Two or More Races/")
column_names <- str_replace(column_names, "HISP/", "Hispanic/")

# Totals
column_names <- str_replace(column_names, "TOTAL/", "All Races/All Sexes/")

# Apply new column names
colnames(eeoc_national) <- column_names

### Pivot Data and split race/sex/profession column
eeoc_pivot <- eeoc_national %>% 
  pivot_longer(col = -c(Year,`Industry Group`,Industry,Establishments)) %>% 
  separate(name, c("Race", "Sex", "Profession"), "/")

# Replace * values with 0
eeoc_pivot$value <- str_replace(eeoc_pivot$value, "\\*", "")
eeoc_pivot$value <- as.integer(eeoc_pivot$value)
