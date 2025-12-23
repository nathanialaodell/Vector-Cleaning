library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(sf)
library(here)
library(janitor)
# remotes::install_github("ropensci/parzer")
library(parzer) # incredibly powerful tool for parsing messy lat/long coordinates

# use this for readxl! https://github.com/rstudio/cheatsheets/blob/main/data-import.pdf
dal.path = here("TX/Dallas County/Dallas County 2008-2025.xlsx") # multiple sheets here
ca.path = here("CA")
ca.extensions <- c(
  paste(ca.path, "/abundance_", "2003-10.csv", sep = ""),
  paste(ca.path, "/abundance_", "20011-15.csv", sep = ""),
  paste(ca.path, "/abundance_", "20016-20.csv", sep = ""),
  paste(ca.path, "/abundance_", "2021-25.csv", sep = "")
)

#----------------------------------------------------------
# SWEEP step--cleans up the basic that can be automated
#----------------------------------------------------------

sweep_fun <- function(data, path, state, extensions, excel){ # extension is the years for cali; final line to format excel objects if needed
  if(excel == TRUE){
    temp.list <- path %>%
      excel_sheets() %>%
      set_names() %>%
      map(read_excel, path = path) %>%
      lapply(., clean_names)
    
    for(i in 1:length(temp.list)){
      
      # INSERT STATE IDENTIFIER
      temp.list[[i]]$state <- state
      
      # CLEANING LAT
      # the purpose of doing this is for list_rbind() (if there are eastings in 
      # coordinates they will be character and not double. Can create a big mess)
      # have to use regex to remove the northing/eastings in lat and long respectively
      
      # finally, there should be no spacing between numbers (this can be present 
      # when coordinates are in DMS; bad practice as opposed to using decimals 
      # in effect we are doing all this work because if these issues are present
      # we are going to have to just geocode trap sites
      
      temp.list[[i]]$latitude <- gsub("^[A-Z] ", "", temp.list[[i]]$latitude)
      temp.list[[i]]$latitude <- gsub("^[A-Z]", "", temp.list[[i]]$latitude)
      temp.list[[i]]$latitude <- gsub(" [A-Z]$", "", temp.list[[i]]$latitude)
      temp.list[[i]]$latitude <- gsub("[A-Z]$", "", temp.list[[i]]$latitude)
      
      
      # CLEANING LONG
      temp.list[[i]]$longitude <- gsub("^[A-Z] ", "", temp.list[[i]]$longitude)
      temp.list[[i]]$longitude <- gsub("^[A-Z]", "", temp.list[[i]]$longitude)
      temp.list[[i]]$longitude <- gsub(" [A-Z]$", "", temp.list[[i]]$longitude)
      temp.list[[i]]$longitude <- gsub("[A-Z]$", "", temp.list[[i]]$longitude)
      
      # from here, use parzer!
      temp.list[[i]]$latitude <- parse_lat(temp.list[[i]]$latitude)
      temp.list[[i]]$longitude <- parse_lon(temp.list[[i]]$longitude)
      
    }
    data.temp <- temp.list
  }
  
  else{
    data.list <- list()
    for(i in 1:length(extensions)){
      data.list[[i]] <- read.csv(cextensions[i])
    }
    data.temp <- lapply(data.list, clean_names) %>%
      list_bind() %>%
      dplyr::mutate(
        state = state
      )
  }
 return(data.temp) 
}

#----------------------------------------------------------
# MOP step--cleans up the basic that cannot be automated
#----------------------------------------------------------

dal.list <- sweep_fun(data = NULL, path = dal.path, state = "TX", extensions = NULL, excel = TRUE)

for(i in 1:length(dal.list)){
  dal.list[[i]] <- dal.list[[i]] %>%
    dplyr::select(
      county, sampled_date, address, collection_method,
      latitude, longitude, mosquito_id, number_of_mosquitoes, state
    ) %>%
    dplyr::mutate(
      sampled_date = as.Date(sampled_date, format = "%m/%d/%y")# make sure it's same format as AZ
    )
}

dal <- list_rbind(dal.list)

