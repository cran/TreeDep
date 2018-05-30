#' Dep_vel_PM10 - Calculates hourly deposition velocity for PM10
#' @param  x A data frame containing hourly data of weather variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight))
#' @return Hourly data of deposition velocity for PM10 (m s-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Dep_vel_PM10(x = Bizkaia_data)
Dep_vel_PM10 <- function(x){
  db <- x
  Vd_PM10_average <- 0.0064 # m/s
  Vd_PM10 <- cbind.data.frame(Dates = db$Dates, Depos_vel = Vd_PM10_average)
  return(Vd_PM10)
}
