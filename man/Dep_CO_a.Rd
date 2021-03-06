% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Dep_CO_a.R
\name{Dep_CO_a}
\alias{Dep_CO_a}
\title{Dep_CO_a - Calculates the annual value of deposition of CO on vegetation}
\usage{
Dep_CO_a(x, z_0 = 1)
}
\arguments{
\item{x}{A data frame containing hourly data of CO concentration and other variables (Dates (e.g. 01/01/2016  00:00:00), Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight), BAI, LAI)}

\item{z_0}{Roughness length value (m)}
}
\value{
Annual value of deposition of CO on vegetation (g m-2 yr-1)
}
\description{
Dep_CO_a - Calculates the annual value of deposition of CO on vegetation
}
\examples{

data(Bizkaia_data)
Dep_CO_a(x = Bizkaia_data, z_0 = 1)
}
