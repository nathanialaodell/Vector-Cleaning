library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)

az1 <- read_csv('/Users/nathanialodell/Library/CloudStorage/OneDrive-UW/Thesis/Data/AZ/2013-2019.csv') %>%
  as_tibble()
az2 <- read_csv('/Users/nathanialodell/Library/CloudStorage/OneDrive-UW/Thesis/Data/AZ/2020-2024.csv') %>%
  as_tibble()

str(az1)
str(az2)

az1 <- az1 %>%
  dplyr::select(
    `ID Number`,
    `Lab Date`,
    Species,
    Males,
    Females,
    Disease,
    `Test Result`,
    Latitude,
    Longitude
  ) %>%
  dplyr::rename(
    TrapID = "ID Number",
    Lab_Date = "Lab Date",
    Result = "Test Result"
  ) %>%
  dplyr::mutate(
    Males = as.integer(Males),
    Females = as.integer(Females),
    Lab_Date = as.Date(Lab_Date, "%m/%d/%y"),
    Disease = ifelse(
      Disease == "None", NA, Disease
    )
  )

az2 <- az2 %>%
  dplyr::select(
    TrapID,
    Lab_Date,
    Species,
    Males,
    Females,
    Disease,
    Result,
    Latitude,
    Longitude
  ) %>%
  dplyr::mutate(
    Males = as.integer(Males),
    Females = as.integer(Females),
    Lab_Date = as.Date(Lab_Date, "%m/%d/%y")
  )

