#' Preprocess mosquito surveillance data
#'
#' This functions takes a raw, potentially messy .csv, .xlsx, or .xls file that
#' contains mosquito abundance of pool data and both cleans and standardizes
#' species collection and geospatial information.
#'
#' @param path Character. The directory of a single datasheet. Defaults to NULL.
#' @param state_name Character. Abbreviated state--e.g. "TX", "WA"--that vector data comes from.
#' @param extensions If working with multi-year data that is spread across multiple files, a list of path directories. Defaults to NULL.
#' @param sheets TRUE or FALSE. Indicates whether data is stored within multiple excel sheets. Defaults to FALSE.
#' @param type The type of datasheet to be cleaned: either 'abundance' or 'pool'. Defaults to 'abundance'.
#' @return A preprocessed datasheet.
#' @export

sweep_fun <- function(path = NULL,
                      state_name,
                      extensions = NULL,
                      sheets = FALSE,
                      type = "abundance") {

  if (is.null(path) & is.null(extensions)) stop("No file type specifed ('path' and 'extensions' args cannot be simultaneously NULL).")

  if(missing(state_name)) stop("No state provided! Please specify 'state_name' arg (e.g. 'TX'', 'WA', etc.)")

  if(missing(type)) warning("'type' arg defaults to 'abundance'; ensure this is appropriate for your purposes!")

  message("Loading data...")

  geo.list <- list()
  temp.na <- list()
  min <- list()
  parsed <- list()
  temp.list <- list()

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

  data.temp <- purrr::list_rbind(temp.list)

  filter_outside_counties(data.temp, state_name) %>%
    dplyr::distinct()
}
