#-------------
# LOAD IN DATA
#-------------

loader_fun <- function(path, extensions = NULL, sheets = FALSE){

  if(sheets == TRUE){

    temp.list <- path %>%
      readxl::excel_sheets() %>%
      janitor::set_names() %>%
      purrr::map(read_excel, path = path)

  }

  else if(!is.null(extensions)){

    temp.list <- purrr::map(extensions, read.csv) # nicer syntax than base [[i]]

  }

  else{
    temp.list <- purrr::map(path, read.csv)
  }

  temp.list %>%
    lapply(janitor::clean_names)
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

  df$latitude  <- parzer::parse_lat(df$latitude)
  df$longitude <- parzer::parse_lon(df$longitude)

  df %>%
    dplyr::mutate(
      longitude = ifelse(longitude > 0, longitude * -1, longitude),
      latitude  = abs(latitude)
    )

}

#--------------------------
# GEOCODE COORDINATES W/ NA
#--------------------------

geocode_missing_coords <- function(df, state_name) {

  dirs <- postmastr::pm_dictionary(type = "directional",
                                   filter = c("N", "S", "E", "W"),
                                   locale = "us")

  df$state <- state_name

  if (!("city" %in% names(df)) ||
      sum(is.na(df$latitude) | is.na(df$longitude)) == 0) {
    return(df)
    warning("No city data present in provided datasheet. Missing coordinates will not be geocoded.")
  }

  na_df <- df %>%
    dplyr::filter(is.na(latitude) | is.na(longitude)) %>%
    dplyr::select(address, city, state, county)

  na_df <- postmastr::pm_identify(na_df, var = address, locale = "us")

  min <- postmastr::pm_prep(na_df, var = address, type = "street")

  min <- postmastr::pm_postal_parse(min)

  min <- postmastr::pm_house_parse(min)

  min <- postmastr::pm_streetDir_parse(min, dictionary = dirs)

  # error checking
  str(na_df$address)
  table(na_df$address, useNA = "ifany")

  str(min$pm.address)
  table(min$pm.address, useNA = "ifany")


  min <- postmastr::pm_streetSuf_parse(min)

  min <- postmastr::pm_street_parse(min, ordinal = TRUE, drop = TRUE)

  parsed <- postmastr::pm_replace(min, source = na_df)

  parsed$pm.city <- parsed$city

  parsed$pm.state <- state_name

  parsed <- postmastr::pm_rebuild(parsed, output = "full", side = "right",
                                  keep_parsed = "limited")

  geo.df <- tidygeocoder::geocode(
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
    dplyr::left_join(
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
    dplyr::select(-address_clean, -geo_lat, -geo_lon)

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

  county_shapes <- tigris::counties(state = state_name, cb = TRUE)
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
    dplyr::group_by(address) %>%
    dplyr::mutate(
      latitude = dplyr::coalesce(latitude,
                                 ifelse(is.finite(max(latitude, na.rm = TRUE)),
                                        max(latitude, na.rm = TRUE), NA_real_)),
      longitude = dplyr::coalesce(longitude,
                                  ifelse(is.finite(max(longitude, na.rm = TRUE)),
                                         max(longitude, na.rm = TRUE), NA_real_))
    ) %>%
    ungroup()
}
