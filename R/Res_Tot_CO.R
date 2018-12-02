#' Res_Tot_CO - Calculates hourly total resistance for CO
#' @param  x A data frame containing hourly data of weather variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight))
#' @param  z_0 Roughness length value (m)
#' @return Hourly data of total resistance for CO (s m-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Res_Tot_CO(x = Bizkaia_data, z_0 = 1)
Res_Tot_CO <- function(x, z_0 = 1){
  db <- x
  Rt_CO0 <- Res_aero(db, z_0 = z_0)[,"Resist_aero"] + Res_boun_CO(db, z_0 = z_0)[,"Resist_bound_CO"] +
    Res_cano_CO(db)[,"Resist_cano"]
  Rt_CO <- cbind.data.frame(Dates = db$Dates, Resist_Tot = Rt_CO0)
  return(Rt_CO)
}
