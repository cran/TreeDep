#' Res_Tot_O3 - Calculates hourly total resistance for O3
#' @param  x A data frame containing hourly data of weather variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight))
#' @param  z_0 Roughness length value (m)
#' @return Hourly data of total resistance for O3 (s m-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Res_Tot_O3(x = Bizkaia_data, z_0 = 1)
Res_Tot_O3 <- function(x, z_0 = 1){
  db <- x
  Rt_O30 <- Res_aero(db, z_0 = z_0)[,"Resist_aero"] + Res_boun_O3(db, z_0 = z_0)[,"Resist_bound_O3"] +
    Res_cano_O3(db)[,"Resist_cano"]
  Rt_O3 <- cbind.data.frame(Dates = db$Dates, Resist_Tot = Rt_O30)
  return(Rt_O3)
}
