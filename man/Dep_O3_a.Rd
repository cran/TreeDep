% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Dep_O3_a.R
\name{Dep_O3_a}
\alias{Dep_O3_a}
\title{Dep_O3_a - Calculates the annual value of deposition of O3 on vegetation}
\usage{
Dep_O3_a(x, z_0 = 1)
}
\arguments{
\item{x}{A data frame containing hourly data of O3 concentration and other variables (Dates (e.g. 01/01/2016  00:00:00), Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight), BAI, LAI)}

\item{z_0}{Roughness length value (m)}
}
\value{
Annual value of deposition of O3 on vegetation (g m-2 yr-1)
}
\description{
Dep_O3_a - Calculates the annual value of deposition of O3 on vegetation
}
\examples{

data(Bizkaia_data)
Dep_O3_a(x = Bizkaia_data, z_0 = 1)
}
