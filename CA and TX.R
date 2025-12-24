library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(sf)
library(here)
library(janitor)
# remotes::install_github("ropensci/parzer")
library(parzer) # incredibly powerful tool for parsing messy lat/long coordinates
library(tidygeocoder)
library(postmastr) # for cleaning addresses

# use this for readxl! https://github.com/rstudio/cheatsheets/blob/main/data-import.pdf

#-------
# DALLAS
#-------

dal.path = here("TX/Dallas County/Dallas County 2008-2025.xlsx") # multiple sheets here

#-------
# NUECES
#-------

nu.path = here("TX/Dallas County/Nueces County 2008-2025.xlsx")

#-----------
# CALIFORNIA
#-----------

ca.path = here("CA")
ca.extensions <- c(
  paste(ca.path, "/abundance_", "2003-10.csv", sep = ""),
  paste(ca.path, "/abundance_", "2011-15.csv", sep = ""),
  paste(ca.path, "/abundance_", "2016-20.csv", sep = ""),
  paste(ca.path, "/abundance_", "2021-25.csv", sep = "")
)

#----------------------------------------------------------
# SWEEP step--cleans up the basic that can be automated
#----------------------------------------------------------

# PREAMBLE TO THE FUNCTION!
# this function assumes that you have a dataset that has at least the following 
# (CASE SENSITIVE!) column names when working w/ excel sheets: 
# county, sampled_date, address, collection_method,
# latitude, longitude, mosquito_id, number_of_mosquitoes, state

geo.list <- list()
temp.na <- list()
min <- list()
parsed <- list()

# street direction dictionary for cleaning
dirs <- pm_dictionary(type = "directional", filter = c("N", "S", "E", "W"), locale = "us")

sweep_fun <- function(path, state_name, extensions, excel, dirs = dirs){ 
  
  if(excel == TRUE){
    temp.list <- path %>%
      excel_sheets() %>%
      set_names() %>%
      map(read_excel, path = path) %>%
      lapply(., clean_names)
    
    for(i in 1:length(temp.list)){
      
      # INSERT STATE IDENTIFIER
      temp.list[[i]]$state <- state_name
      
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
      
      # make sure long is negative and vice versa for lat (sometimes it isn't)
      temp.list[[i]] <- temp.list[[i]] %>%
        dplyr::mutate(
          longitude = ifelse(
            longitude > 0, longitude * -1, longitude
          ),
          latitude = abs(latitude)
        )
      
      
      # geocoding where coords are NA
      # subset to only coords with NA values
      temp.na[[i]] <- temp.list[[i]] %>%
        dplyr::filter(is.na(latitude) | is.na(longitude)) %>%
        dplyr::select(
          address, city, state, county
        )
      
      if(any(!is.na(temp.na[[i]]$address))){ # don't waste time otherwise!
      
      # prepping addresses prior to geocoding
      temp.na[[i]] <- pm_identify(temp.na[[i]], var = address, locale = "us")
      
      min[[i]] <- pm_prep(temp.na[[i]], var = address, type = "street")
      
      min[[i]] <- pm_postal_parse(min[[i]])
      
      min[[i]] <- pm_house_parse(min[[i]])
      
      min[[i]] <- pm_streetDir_parse(min[[i]], dictionary = dirs)
      
      # error checking
      str(temp.na[[i]]$address)
      table(temp.na[[i]]$address, useNA = "ifany")
      
      str(min[[i]]$pm.address)
      table(min[[i]]$pm.address, useNA = "ifany")
      
      
      min[[i]] <- pm_streetSuf_parse(min[[i]])
      
      min[[i]] <- pm_street_parse(min[[i]], ordinal = TRUE, drop = TRUE)
      
      parsed[[i]] <- pm_replace(min[[i]], source = temp.na[[i]])
      
      parsed[[i]]$pm.city <- parsed[[i]]$city
      
      parsed[[i]]$pm.state <- state_name
      
      parsed[[i]] <- pm_rebuild(parsed[[i]], output = "full", side = "right", 
                                keep_parsed = "limited")
      
      
  geo.list[[i]] <- geocode(
    .tbl = parsed[[i]],
  street = pm.address,
  city = city,
  state = pm.state,
  return_input = TRUE,
  timeout = 20,
  method = 'census'
  ) %>%
    dplyr::select(
      address, lat, long
    ) %>%
    dplyr::rename(
      latitude = "lat",
      longitude = "long"
    )

      temp.list[[i]] <- temp.list[[i]] %>% # normalize address formatting
        dplyr::mutate(address_clean = str_to_upper(str_trim(address))) %>%
        left_join(
          geo.list[[i]] %>%
            dplyr::mutate(address_clean = str_to_upper(str_trim(address))) %>%
            dplyr::select(address_clean, geo_lat = latitude, geo_lon = longitude),
          by = "address_clean" ,
          relationship = 'many-to-many') %>%
        dplyr::mutate(latitude = ifelse(is.na(latitude),
                                         geo_lat,
                                         latitude),
                longitude = ifelse(is.na(longitude),
                                    geo_lon,
                                    longitude) ) %>%
        select(-address_clean, -geo_lat, -geo_lon)
      
      # propagate known coordinates across identical addresses
      # essentially, the problem is that there are rare occurences where
      # an address has a full lat/long in one observation, but a partial in another
      # coalesce makes sure we don't get -Inf for addresses where there are NO 
      # coordinates at all
      
      temp.list[[i]] <- temp.list[[i]] %>%
        group_by(address) %>%
        mutate(
          latitude  = coalesce(latitude, 
                               ifelse(is.finite(max(latitude, na.rm = TRUE)), 
                                      max(latitude, na.rm = TRUE), NA_real_)),
          longitude = coalesce(longitude, 
                               ifelse(is.finite(max(longitude, na.rm = TRUE)), 
                                     max(longitude, na.rm = TRUE), NA_real_))
        ) %>%
        ungroup()
      
      
      }

    }
    
    data.temp <- temp.list
  }
  
  else{
    data.list <- list()
    for(i in 1:length(extensions)){
      data.list[[i]] <- read.csv(ca.extensions[i])
    }
    
    data.temp <- lapply(data.list, clean_names) %>%
      list_bind() %>%
      dplyr::mutate(
        state = state_name
      )
  }
  
  for(i in 1:length(data.temp)){
    data.temp[[i]] <- data.temp[[i]] %>%
      dplyr::select(
        county, city, sampled_date, address, collection_method,
        latitude, longitude, mosquito_id, number_of_mosquitoes, state
      ) %>%
      dplyr::mutate(
        sampled_date = as.Date(sampled_date, format = "%m/%d/%y")
      )
  }
  
  data.temp <- list_rbind(data.temp)
  data.temp <- data.temp[!duplicated(data.temp), ] # left join can create some duplicates
  
 return(data.temp) 
}

nueces <- sweep_fun(path = nu.path, state_name = "TX", 
                     extensions = NULL, excel = TRUE)

dallas <- sweep_fun(path = dal.path, state_name = "TX", 
                      extensions = NULL, excel = TRUE)

#----------------------------------------------------------
# MOP step--cleans up the basic that cannot be automated
#----------------------------------------------------------
# the bounding box will be very telling, and for Nueces it's very obvious
# I think anything less than 27 and over 29 are clear, incorrect coordinates we need
# to just remove

nueces_filtered <- nueces %>% dplyr::filter(latitude >= 27.558358, 
                                            latitude <= 27.995659, 
                                             longitude >= -97.942146, 
                                             longitude <= -96.984281 ) # bb for Nueces

nrow(nueces) - nrow(nueces_filtered) # not bad (506)

CRS = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
test <- nueces_filtered %>% 
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = CRS,
           na.fail = F)

st_bbox(test)

ggplot() +
  geom_sf(data = test)

write_rds(nueces_filtered, "Nueces (pre-processed).RDS")
