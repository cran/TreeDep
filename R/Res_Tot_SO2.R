#' Res_Tot_SO2 - Calculates hourly total resistance for SO2
#' @param  x A data frame containing hourly data of weather variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight))
#' @param  z_0 Roughness length value (m)
#' @return Hourly data of total resistance for SO2 (s m-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Res_Tot_SO2(x = Bizkaia_data, z_0 = 1)
Res_Tot_SO2 <- function(x, z_0 = 1){
  db <- x
  Rt_SO20 <- Res_aero(db, z_0 = z_0)[,"Resist_aero"] + Res_boun_SO2(db, z_0 = z_0)[,"Resist_bound_SO2"] +
    Res_cano_SO2(db)[,"Resist_cano"]
  Rt_SO2 <- cbind.data.frame(Dates = db$Dates, Resist_Tot = Rt_SO20)
  return(Rt_SO2)
}
