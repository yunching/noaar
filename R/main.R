library(tidyverse)

raw_data <-
  readr::read_tsv(file.path(".", "data", "signif.txt.tsv"))

#' Takes raw NOAA data frame and returns a clean data frame
#'
#' @return
#' @export
#'
#' @examples
eq_clean_data <- function(raw_data) {
  # The clean data frame should have the following:
  #   A date column created by uniting the year, month, day and converting it to the Date class
  # LATITUDE and LONGITUDE columns converted to numeric class
  # In addition, write a function eq_location_clean() that cleans the LOCATION_NAME column by stripping out the country name (including the colon) and converts names to title case (as opposed to all caps). This will be needed later for annotating visualizations. This function should be applied to the raw data to produce a cleaned up version of the LOCATION_NAME column.
  raw_data %>%
    dplyr::transmute(
      date = lubridate::ymd(paste(YEAR, MONTH, DAY)),
      longitude = LONGITUDE,
      latitude = LATITUDE,
      location_name = eq_location_clean(LOCATION_NAME),
      country = get_country_clean(LOCATION_NAME),
      intensity = INTENSITY,
      deaths = DEATHS
    ) %>%
    as_tibble()
}

#' Cleans the raw LOCATION_NAME column
#'
#' @param mystring raw LOCATION_NAME string to be cleaned
#'
#' @return LOCATION_NAME without country name and in title case
#' @examples eq_location_clean("ITALY: LACUS CIMINI")
eq_location_clean <- function(mystring) {
  mystring %>%
    stringr::str_replace(";.+", "") %>%
    stringr::str_replace("[A-Z]+: ", "") %>%
    stringr::str_to_title()
}

get_country_clean <- function(mystring) {
  mystring %>%
    stringr::str_replace(":.+", "") %>%
    stringr::str_to_upper()
}

cleaned_data <- eq_clean_data(raw_data)

cleaned_data %>%
  filter(date > lubridate::ymd("2000-1-1") &
           date <= lubridate::ymd("2000-1-31")) -> tmp

tmp %>% ggplot(aes(x = date)) + geom_timeline()
