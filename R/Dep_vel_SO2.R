#' Dep_vel_SO2 - Calculates hourly deposition velocity for SO2
#' @param  x A data frame containing hourly data of weather variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight))
#' @param  z_0 Roughness length value (m)
#' @return Hourly data of deposition velocity for SO2 (m s-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Dep_vel_SO2(x = Bizkaia_data, z_0 = 1)
Dep_vel_SO2 <- function(x, z_0 = 1){
  db <- x
  Vd_SO20 <- 1 / Res_Tot_SO2(db, z_0 = z_0)[,"Resist_Tot"]
  Vd_SO2 <- cbind.data.frame(Dates = db$Dates, Depos_vel = Vd_SO20)
  return(Vd_SO2)
}
