% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualiser_forecast_d.R
\name{visualiser_forecast_d}
\alias{visualiser_forecast_d}
\title{Visualisation de la meteo par jour}
\usage{
visualiser_forecast_d(x, y = NULL)
}
\arguments{
\item{x}{Latitude (num) ou adresse (string).}

\item{y}{Longitude (num) (Obligatoire uniquement si x est la latitude).}
}
\value{
Des graphiques ggplot.
}
\description{
Cette fonction permet de visualiser les donnees de previsions meteorologiques par jours en fonction de la latitude et de la longitude ou d une adresse
}
\examples{
visualiser_forecast_d(42.35, 2.65)
visualiser_forecast_d("Paris")
}
