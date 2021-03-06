% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Res_cano_CO.R
\name{Res_cano_CO}
\alias{Res_cano_CO}
\title{Res_cano_CO - Calculates hourly canopy resistance for CO}
\usage{
Res_cano_CO(x)
}
\arguments{
\item{x}{A data frame containing hourly data of weather variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight))}
}
\value{
Hourly data of canopy resistance for CO (s m-1)
}
\description{
Res_cano_CO - Calculates hourly canopy resistance for CO
}
\examples{

data(Bizkaia_data)
Res_cano_CO(x = Bizkaia_data)
}
