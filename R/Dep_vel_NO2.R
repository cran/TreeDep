#' Dep_vel_NO2 - Calculates hourly deposition velocity for NO2
#' @param  x A data frame containing hourly data of weather variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight))
#' @param  z_0 Roughness length value (m)
#' @return Hourly data of deposition velocity for NO2 (m s-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Dep_vel_NO2(x = Bizkaia_data, z_0 = 1)
Dep_vel_NO2 <- function(x, z_0 = 1){
  db <- x
  Vd_NO20 <- 1 / Res_Tot_NO2(db, z_0 = z_0)[,"Resist_Tot"]
  Vd_NO2 <- cbind.data.frame(Dates = db$Dates, Depos_vel = Vd_NO20)
  return(Vd_NO2)
}
