% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Dep_vel_SO2.R
\name{Dep_vel_SO2}
\alias{Dep_vel_SO2}
\title{Dep_vel_SO2 - Calculates hourly deposition velocity for SO2}
\usage{
Dep_vel_SO2(x, z_0 = 1)
}
\arguments{
\item{x}{A data frame containing hourly data of weather variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight))}

\item{z_0}{Roughness length value (m)}
}
\value{
Hourly data of deposition velocity for SO2 (m s-1)
}
\description{
Dep_vel_SO2 - Calculates hourly deposition velocity for SO2
}
\examples{

data(Bizkaia_data)
Dep_vel_SO2(x = Bizkaia_data, z_0 = 1)
}
