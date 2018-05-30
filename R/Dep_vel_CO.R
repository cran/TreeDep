#' Dep_vel_CO - Calculates hourly deposition velocity for CO
#' @param  x A data frame containing hourly data of weather variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight))
#' @return Hourly data of deposition velocity for CO (m s-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Dep_vel_CO(x = Bizkaia_data)
Dep_vel_CO <- function(x){
  db <- x
  Vd_CO0 <- 1 / Res_Tot_CO(db)[,"Resist_Tot"]
  Vd_CO <- cbind.data.frame(Dates = db$Dates, Depos_vel = Vd_CO0)
  return(Vd_CO)
}
