#' Dep_vel_O3 - Calculates hourly deposition velocity for O3
#' @param  x A data frame containing hourly data of weather variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight))
#' @return Hourly data of deposition velocity for O3 (m s-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Dep_vel_O3(x = Bizkaia_data)
Dep_vel_O3 <- function(x){
  db <- x
  Vd_O30 <- 1 / Res_Tot_O3(db)[,"Resist_Tot"]
  Vd_O3 <- cbind.data.frame(Dates = db$Dates, Depos_vel = Vd_O30)
  return(Vd_O3)
}
