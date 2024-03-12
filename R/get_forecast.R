#'Donnees meteo
#'
#'@description permet d obtenir des donnes de previsions meteorologiques en fonction de la lat. et de la long. ou d une adresse
#'
#' @param x Latitude (num) ou adresse (string)
#' @param y Longitude (num) (Obligatoire uniquement si x est la latitude).
#'
#' @return Un tibble contenant des donnees de previsions meteorologiques
#'
#' @export
#'
#' @import tibble
#' @import jsonlite
#' @import httr2
#' @import tidygeocoder
#' @import dplyr
#' @import devtools
#'
#' @examples
#' get_forecast(42.35, 2.65)
#' get_forecast("Paris")


get_forecast <- function(x, y = NULL) {

  perform_request <- function(lat, lon) {
    url <- "https://api.open-meteo.com/v1/forecast"
    response_table <-
      httr2::request(url) |>
      httr2::req_url_query(latitude = lat, longitude = lon,
                           hourly = c("temperature_2m",
                                      "apparent_temperature",
                                      "precipitation_probability",
                                      "precipitation"),
                           .multi = "comma") |>
      httr2::req_perform() |>
      httr2::resp_body_json() |>
      tibble::as_tibble()
    return(response_table)
  }


  unnest_data <- function(resp) {
    tibble::tibble(date_heure = unlist(resp$hourly[1][[1]]),
                   temperature_celsius = unlist(resp$hourly[2][[1]]),
                   apparent_temperature_celsius = unlist(resp$hourly[3][[1]]),
                   precipitation_proba = unlist(resp$hourly[4][[1]]),
                   precipitation = unlist(resp$hourly[5][[1]]))
  }


  get_gps_coordonnee <- function(adresse) {
    adresse_tibble <- tibble::tribble(
      ~addr,
      adresse
    ) |>
      tidygeocoder::geocode(addr)

    lat <- adresse_tibble$lat
    long <- adresse_tibble$long

    coordonnees <- c(lat, long)
    return(coordonnees)
  }



  if (!is.null(y)) {

    lat <- x
    lon <- y
    response <- perform_request(lat, lon)
  } else if (is.character(x)) {

    coordonnees <- get_gps_coordonnee(x)
    lat <- coordonnees[1]
    lon <- coordonnees[2]
    response <- perform_request(lat, lon)
  } else {
    stop("erreur")
  }

  data <- unnest_data(response)
  return(data)
}

