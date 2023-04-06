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
  df %>% 
    filter(is.na(Region) & !is.na(NAICS2_Name) & is.na(CBSA) & !is.na(NAICS3_Name)) %>% 
    select(-Nation, -Region, -Division, -State, -CBSA, -County, -NAICS2, -NAICS3) %>% 
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

