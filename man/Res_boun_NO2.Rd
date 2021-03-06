% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Res_boun_NO2.R
\name{Res_boun_NO2}
\alias{Res_boun_NO2}
\title{Res_boun_NO2 - Calculates hourly boundary layer resistance for NO2}
\usage{
Res_boun_NO2(x, z_0 = 1)
}
\arguments{
\item{x}{A data frame containing hourly data of weather variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight))}

\item{z_0}{Roughness length value (m)}
}
\value{
Hourly data of  boundary layer resistance for NO2 (s m-1)
}
\description{
Res_boun_NO2 - Calculates hourly boundary layer resistance for NO2
}
\examples{

data(Bizkaia_data)
Res_boun_NO2(x = Bizkaia_data, z_0 = 1)
}
