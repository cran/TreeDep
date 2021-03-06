% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Res_cuti_O3.R
\name{Res_cuti_O3}
\alias{Res_cuti_O3}
\title{Res_cuti_O3 - Calculates hourly cuticular resistance for O3}
\usage{
Res_cuti_O3(x)
}
\arguments{
\item{x}{A data frame containing hourly data of weather variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight))}
}
\value{
Hourly data of cuticular resistance for O3 (s m-1)
}
\description{
Res_cuti_O3 - Calculates hourly cuticular resistance for O3
}
\examples{

data(Bizkaia_data)
Res_cuti_O3(x = Bizkaia_data)
}
