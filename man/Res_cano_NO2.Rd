% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Res_cano_NO2.R
\name{Res_cano_NO2}
\alias{Res_cano_NO2}
\title{Res_cano_NO2 - Calculates hourly canopy resistance for NO2}
\usage{
Res_cano_NO2(x)
}
\arguments{
\item{x}{A data frame containing hourly data of weather variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight))}
}
\value{
Hourly data of canopy resistance for NO2 (s m-1)
}
\description{
Res_cano_NO2 - Calculates hourly canopy resistance for NO2
}
\examples{

data(Bizkaia_data)
Res_cano_NO2(x = Bizkaia_data)
}
