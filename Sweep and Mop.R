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
library(tigris)

#----------------------------------------------------------
# SWEEP step--cleans up the basic that can be automated
#----------------------------------------------------------

# PREAMBLE TO THE FUNCTION!
# this function assumes that you have a dataset that has at least the following
# column names when working w/ excel sheets:
# county, sampled_date, address, collection_method,
# latitude, longitude, mosquito_id, number_of_mosquitoes, state


# street direction dictionary for cleaning
dirs_dict <- pm_dictionary(type = "directional", filter = c("N", "S", "E", "W"), 
                           locale = "us")

geo.list <- list()
temp.na <- list()
min <- list()
parsed <- list()
temp.list <- list()

#-------------
# LOAD IN DATA
#-------------

loader_fun <- function(path, extensions = NULL, sheets = FALSE){
  
  if(sheets == TRUE){
    
    temp.list <- path %>%
      excel_sheets() %>%
      set_names() %>%
      map(read_excel, path = path)
    
  }
  
  else if(!is.null(extensions)){
    
    temp.list <- map(extensions, read.csv) # nicer syntax than base [[i]]
    
  }
  
  else{
    temp.list <- map(path, read.csv)
  }
  
  temp.list %>%
    lapply(clean_names)
}

#----------------------------------------
# STANDARDIZE GENUS (two letter no period)
#----------------------------------------

standard_genus <- function(df){
  df$mosquito_id <- gsub("^Aedes ", "Ae ", df$mosquito_id)
  df$mosquito_id <- gsub("^Ae. ", "Ae ", df$mosquito_id)
  df$mosquito_id <- gsub("^Culex ", "Cx ", df$mosquito_id)
  df$mosquito_id <- gsub("^Cx. ", "Cx ", df$mosquito_id)
  df$mosquito_id <- gsub("^Anopheles ", "An ", df$mosquito_id)
  df$mosquito_id <- gsub("^An. ", "An ", df$mosquito_id)
  df$mosquito_id <- gsub("^Psorophora ", "P ", df$mosquito_id)
  df$mosquito_id <- gsub("^P. ", "P ", df$mosquito_id)
  
  df
}

#---------------------------------
# PARSE COORDS TO STANDARDIZE THEM
#---------------------------------

parse_coords <- function(df){
  
  clean_dir <- function(z){
    z %>%
      gsub("^[A-Z] ?", "", .) %>%
      gsub(" ?[A-Z]$", "", .)
  }
  
  df$latitude  <- clean_dir(df$latitude)
  df$longitude <- clean_dir(df$longitude)
  
  df$latitude  <- parse_lat(df$latitude)
  df$longitude <- parse_lon(df$longitude)
  
  df %>%
    dplyr::mutate(
      longitude = ifelse(longitude > 0, longitude * -1, longitude),
      latitude  = abs(latitude)
    )
  
}

#--------------------------
# GEOCODE COORDINATES W/ NA 
#--------------------------

geocode_missing_coords <- function(df, state_name, dirs) {
  
  df$state <- state_name
  
  if (!("city" %in% names(df)) ||
      sum(is.na(df$latitude) | is.na(df$longitude)) == 0) {
    return(df) 
    warning("No city data present in provided datasheet. Missing coordinates will not be geocoded.")
  }
  
  na_df <- df %>%
    dplyr::filter(is.na(latitude) | is.na(longitude)) %>%
    dplyr::select(address, city, state, county)
  
  na_df <- pm_identify(na_df, var = address, locale = "us")
  
  min <- pm_prep(na_df, var = address, type = "street")
  
  min <- pm_postal_parse(min)
  
  min <- pm_house_parse(min)
  
  min <- pm_streetDir_parse(min, dictionary = dirs)
  
  # error checking
  str(na_df$address)
  table(na_df$address, useNA = "ifany")
  
  str(min$pm.address)
  table(min$pm.address, useNA = "ifany")
  
  
  min <- pm_streetSuf_parse(min)
  
  min <- pm_street_parse(min, ordinal = TRUE, drop = TRUE)
  
  parsed <- pm_replace(min, source = na_df)
  
  parsed$pm.city <- parsed$city
  
  parsed$pm.state <- state_name
  
  parsed <- pm_rebuild(parsed, output = "full", side = "right",
                       keep_parsed = "limited")
  
  geo.df <- geocode(
    .tbl = parsed,
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
  
  df <- df %>%
    dplyr::mutate(address_clean = str_to_upper(str_trim(address))) %>%
    left_join(
      geo.df %>%
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
  
  df[!duplicated(df),]
}

#------------------------------------------
# FILTER OUT FEMALES (CAN SKIP) 
#------------------------------------------

filter_females <- function(df) {
  
  if (!"sex" %in% names(df)) return(df)
  
  df$sex <- gsub("^Females.*", "Female", df$sex)
  df$sex <- gsub("^Female.*", "Female", df$sex)
  df$sex <- gsub("^F.*", "Female", df$sex)
  df$sex <- gsub("^f.*", "Female", df$sex)
  
  df <- df %>%
    dplyr::filter(
      sex == "Female"
    ) %>%
    dplyr::select(-sex)
}

#-------------------------------------
# MAKE OUTPUT STANDARD FOR RBIND LATER
#-------------------------------------

standardize_output <- function(df) {
  
  df %>%
    dplyr::select(
      county, sampled_date, address, collection_method,
      latitude, longitude, mosquito_id,
      number_of_mosquitoes, state
    ) %>%
    dplyr::mutate(
      sampled_date = as.Date(sampled_date, "%m/%d/%y")
    ) %>%
    dplyr::rename(trapID = address,
                  species = mosquito_id,
                  total = number_of_mosquitoes)
}

standardize_output_pools <- function(df) {
  
  df %>%
    dplyr::select(
      county, sampled_date, address, collection_method,
      latitude, longitude, mosquito_id,
      number_of_mosquitoes, state, disease, result
    ) %>%
    dplyr::mutate(
      sampled_date = as.Date(sampled_date, "%m/%d/%y"),
      result = ifelse(
        result %in% c("Positive", "Confirmed", 1), 1, 0
      )
    ) %>%
    dplyr::rename(trapID = address,
                  species = mosquito_id,
                  total = number_of_mosquitoes
                  )
}

#--------------------------
# REMOVE UNREALISTIC COORDS
#--------------------------

filter_outside_counties <- function(df, state_name) {
  
  county_shapes <- counties(state = state_name, cb = TRUE)
  valid_counties <- county_shapes$NAME
  
  for (county_name in unique(df$county)) {
    
    bbox <- st_bbox(
      dplyr::filter(county_shapes, NAME == county_name)
    )
    
    idx <- df$county == county_name
    
    # identify points outside the bbox
    outside_bbox <- idx & (
      df$longitude < bbox$xmin |
        df$longitude > bbox$xmax |
        df$latitude < bbox$ymin |
        df$latitude > bbox$ymax
    )
    df <- df[!outside_bbox, ]
  }
  
  df %>% dplyr::filter(county %in% valid_counties)
}

#-----------------------------------------------
# IF ADDRESS HAS LAT/LONG ONCE, ENSURE IT ALWAYS DOES
#-----------------------------------------------

propagate_coords <- function(df) {
  df %>%
    group_by(address) %>%
    mutate(
      latitude = coalesce(latitude,
                          ifelse(is.finite(max(latitude, na.rm = TRUE)),
                                 max(latitude, na.rm = TRUE), NA_real_)),
      longitude = coalesce(longitude,
                           ifelse(is.finite(max(longitude, na.rm = TRUE)),
                                  max(longitude, na.rm = TRUE), NA_real_))
    ) %>%
    ungroup()
}

#--------
# COMPILE
#--------

sweep_fun <- function(path = NULL, 
                      state_name,
                      extensions = NULL,
                      sheets = FALSE,
                      dirs = dirs_dict,
                      type = "abundance") {
  
  if (is.null(path) & is.null(extensions)) stop("No file type specifed ('path' and 'extensions' args cannot be simultaneously NULL).")
  
  if(missing(state_name)) stop("No state provided! Please specify 'state_name' arg (e.g. 'TX'', 'WA', etc.)")
  
  if(missing(type)) warning("'type' arg defaults to 'abundance'; ensure this is appropriate for your purposes!")
  
  message("Loading data...")
  
  temp.list <- loader_fun(path, extensions, sheets)
  
  message("Data loaded successfully!")
  
  std_fun <- switch( # praise stack overflow!
    type,
    abundance = standardize_output,
    pool = standardize_output_pools,
    stop("Not a valid datasheet type! Must be either 'abundance' or 'pool'.")
  )
  
  message("Sweeping up. This may take a while...")
  
  temp.list <- purrr::map(
    temp.list,
    function(df) {
      df %>%
        dplyr::mutate(state = state_name) %>%
        standard_genus() %>%
        parse_coords() %>%
        geocode_missing_coords(state_name, dirs) %>%
        propagate_coords() %>%
        filter_females() %>%
        std_fun()
    }
  )
  
  message("Sweep complete! Binding data...")
  
  data.temp <- list_rbind(temp.list)
  
  filter_outside_counties(data.temp, state_name) %>%
    dplyr::distinct()
}


# TEST SHEETS
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

ca.extensions.pool <- c(
  paste(ca.path, "/pool_", "2003-15.csv", sep = ""),
  paste(ca.path, "/pool_", "2016-20.csv", sep = ""),
  paste(ca.path, "/pool_", "2021-25.csv", sep = "")
)


ca <- sweep_fun(state_name = "CA", extensions = ca.extensions)
ca.pool <- sweep_fun(extensions = ca.extensions.pool, state_name = "CA", type = "pool")

#-----------
# ARIZONA
#-----------
az.path = here("AZ")

az.extensions.pool <- c(
  paste(az.path, "/MCVC Lab Data Jan 2013 until Dec 2019.csv", sep = ""),
  paste(az.path, "/2020-2024 MCVC Lab Data.csv", sep = "")
)

az.extensions <- c(
  paste(az.path, "/2013-2019.csv", sep = ""),
  paste(az.path, "/2020-2024.csv", sep = "")
)

# TEST EXCEL ABUNDANCE

nueces <- sweep_fun(path = nu.path, state_name = "TX", sheets = TRUE)

# TEST CSV POOLS


az <- sweep_fun(
  path = az.extensions.pool,
  state_name = "AZ",
  type = "pool"
)

# TEST SF
CRS = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

library(leaflet)

leaflet(az %>%
          st_as_sf(coords = c("longitude", "latitude"),
                   crs = CRS,
                   na.fail = FALSE)) %>%
  addTiles() %>%
  addCircleMarkers()


#---------------------------------------------------------------------
# MOP step--cleans up the basic that cannot or should not be automated
#---------------------------------------------------------------------